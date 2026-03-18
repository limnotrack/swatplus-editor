#' Watershed delineation and existing-watershed import
#'
#' Provides two high-level functions that mirror the
#' \strong{"Delineate watershed"} and \strong{"Use existing watershed"} options
#' found in the SWAT+ Editor GUI:
#'
#' \describe{
#'   \item{\code{\link{delineate_watershed}}}{Runs a complete TauDEM pipeline
#'     on a digital elevation model (DEM) to delineate a new watershed from
#'     scratch, producing all \code{gis_*} tables needed by
#'     \code{\link{import_gis}}.}
#'   \item{\code{\link{use_existing_watershed}}}{Reads pre-delineated
#'     shapefiles produced by QSWAT+ or ArcSWAT+ and writes them to a SWAT+
#'     project database, mirroring the "Use existing watershed" workflow.}
#' }
#'
#' Both functions optionally write the result directly to a project SQLite
#' database and set \code{project_config.delineation_done = TRUE}.
#'
#' @keywords internal
NULL

# ===========================================================================
# Internal helpers
# ===========================================================================

# Check that the traudem package is installed and TauDEM is on PATH.
.check_traudem <- function() {
  if (!requireNamespace("traudem", quietly = TRUE)) {
    stop(
      "Package 'traudem' is required for watershed delineation.\n",
      "Install it with: install.packages('traudem')\n",
      "Then install TauDEM following the guide at:\n",
      "  vignette('taudem-installation', package = 'traudem')",
      call. = FALSE
    )
  }
  if (!traudem::can_register_taudem()) {
    stop(
      "TauDEM executables not found on PATH.\n",
      "Run traudem::taudem_sitrep() for a diagnostic report.\n",
      "Installation guide: vignette('taudem-installation', package = 'traudem')",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Validate a DEM input, reproject to projected CRS (UTM) if needed, and
# write it as a GeoTIFF file.  Returns a list:
#   dem_file  – path to the (projected) GeoTIFF
#   rast      – SpatRaster (projected)
#   crs_proj  – CRS string of the projected raster (for outlets etc.)
.prepare_dem <- function(dem, work_dir, verbose) {
  if (is.character(dem)) {
    if (!file.exists(dem)) stop("DEM file not found: ", dem, call. = FALSE)
    dem_rast <- terra::rast(dem)
  } else if (inherits(dem, "SpatRaster")) {
    dem_rast <- dem
  } else {
    stop("'dem' must be a file path or a SpatRaster object.", call. = FALSE)
  }

  # Reproject from geographic (lon/lat) to the appropriate UTM zone
  if (terra::is.lonlat(dem_rast)) {
    ext      <- terra::ext(dem_rast)
    lon_mid  <- (ext$xmin + ext$xmax) / 2
    lat_mid  <- (ext$ymin + ext$ymax) / 2
    utm_zone <- floor((lon_mid + 180) / 6) + 1
    epsg     <- if (lat_mid >= 0) 32600L + utm_zone else 32700L + utm_zone
    crs_proj <- paste0("EPSG:", epsg)
    if (verbose)
      emit_progress(sprintf(
        "Reprojecting DEM to UTM zone %d (%s) for TauDEM processing.",
        utm_zone, crs_proj
      ))
    dem_rast <- terra::project(dem_rast, crs_proj)
  } else {
    crs_desc <- terra::crs(dem_rast, describe = TRUE)
    crs_proj <- if (!is.null(crs_desc) && !is.na(crs_desc$authority) &&
                   !is.na(crs_desc$code)) {
      paste0(crs_desc$authority, ":", crs_desc$code)
    } else {
      terra::crs(dem_rast)
    }
  }

  dem_file <- file.path(work_dir, "DEM.tif")
  terra::writeRaster(dem_rast, dem_file, overwrite = TRUE)

  list(dem_file = dem_file, rast = dem_rast, crs_proj = crs_proj)
}

# Validate outlet input (sf, data.frame, or file path), reproject to the DEM
# CRS, and write as a shapefile.  Returns list: file, sf.
.prepare_outlet <- function(outlet, crs_proj, work_dir) {
  if (is.character(outlet)) {
    if (!file.exists(outlet))
      stop("Outlet file not found: ", outlet, call. = FALSE)
    outlet_sf <- sf::st_read(outlet, quiet = TRUE)
  } else if (inherits(outlet, c("sf", "sfc"))) {
    outlet_sf <- outlet
  } else if (is.data.frame(outlet)) {
    lon_col <- intersect(c("lon", "longitude", "x", "X"), names(outlet))
    lat_col <- intersect(c("lat", "latitude",  "y", "Y"), names(outlet))
    if (!length(lon_col) || !length(lat_col))
      stop("outlet data.frame must contain lon/lat (or x/y) columns.",
           call. = FALSE)
    outlet_sf <- sf::st_as_sf(outlet,
                               coords = c(lon_col[[1L]], lat_col[[1L]]),
                               crs    = 4326)
  } else {
    stop("'outlet' must be an sf object, data.frame, or shapefile path.",
         call. = FALSE)
  }

  # Ensure the outlet contains point geometry
  geom_types <- as.character(sf::st_geometry_type(outlet_sf))
  if (!all(geom_types %in% c("POINT", "MULTIPOINT")))
    stop("'outlet' must contain POINT geometry.", call. = FALSE)

  # Assign WGS84 if CRS is missing
  if (is.na(sf::st_crs(outlet_sf))) {
    message("No CRS on outlet; assuming WGS84 (EPSG:4326).")
    sf::st_crs(outlet_sf) <- 4326L
  }

  # Reproject to DEM CRS
  if (!isTRUE(sf::st_crs(outlet_sf) == sf::st_crs(crs_proj))) {
    outlet_sf <- sf::st_transform(outlet_sf, crs_proj)
  }

  outlet_file <- file.path(work_dir, "outlet.shp")
  sf::st_write(outlet_sf, outlet_file, quiet = TRUE, delete_dsn = TRUE)

  list(file = outlet_file, sf = outlet_sf)
}

# Compute per-polygon mean slope (radians) from the DEM.
.polygon_mean_slope <- function(dem_rast, poly_sf) {
  slope_r <- terra::terrain(dem_rast, "slope", unit = "radians")
  poly_v  <- terra::vect(poly_sf)
  if (!tryCatch(terra::same.crs(poly_v, slope_r), error = function(e) FALSE))
    poly_v <- terra::project(poly_v, terra::crs(slope_r))
  vals   <- terra::extract(slope_r, poly_v, fun = NULL)
  col    <- names(vals)[[2L]]
  by_id  <- split(vals[[col]], vals[[1L]])
  vapply(by_id, mean, numeric(1L), na.rm = TRUE)
}

# Compute per-polygon elevation statistics from the DEM.
.polygon_elev_stats <- function(dem_rast, poly_sf) {
  poly_v <- terra::vect(poly_sf)
  if (!tryCatch(terra::same.crs(poly_v, dem_rast), error = function(e) FALSE))
    poly_v <- terra::project(poly_v, terra::crs(dem_rast))
  vals   <- terra::extract(dem_rast, poly_v, fun = NULL)
  col    <- names(vals)[[2L]]
  by_id  <- split(vals[[col]], vals[[1L]])
  list(
    mean = vapply(by_id, mean, numeric(1L), na.rm = TRUE),
    min  = vapply(by_id, min,  numeric(1L), na.rm = TRUE),
    max  = vapply(by_id, max,  numeric(1L), na.rm = TRUE)
  )
}

# Build the gis_subbasins data.frame from TauDEM's watershed polygon shapefile.
# Adds an internal ".wsno" column (original TauDEM watershed number) for
# cross-referencing with channels; caller removes it before DB insert.
.build_gis_subbasins <- function(watershed_file, dem_rast) {
  wshd     <- sf::st_read(watershed_file, quiet = TRUE)
  # TauDEM watershed shapefile uses "DN" for the watershed (subbasin) number
  dn_col   <- if ("DN" %in% names(wshd)) "DN" else names(wshd)[[1L]]
  ws_ids   <- as.integer(wshd[[dn_col]])

  # Areas in ha from projected geometry (already in metres)
  area_ha  <- as.numeric(sf::st_area(wshd)) / 10000

  # Centroids in WGS84
  wshd_geo  <- sf::st_transform(wshd, 4326L)
  cents_geo <- sf::st_coordinates(sf::st_centroid(wshd_geo))

  # Elevation stats
  elev <- .polygon_elev_stats(dem_rast, wshd)

  # Mean slope in radians
  slo1 <- .polygon_mean_slope(dem_rast, wshd)

  # Overland flow length and slope-length parameter (empirical estimates)
  len1 <- pmax(sqrt(area_ha * 10000) * 0.5, 1)   # √(area_m²) / 2 in m
  sll  <- pmax(sqrt(len1 * slo1), 0)

  data.frame(
    id      = seq_along(ws_ids),
    area    = area_ha,
    slo1    = slo1,
    len1    = len1,
    sll     = sll,
    lat     = cents_geo[, 2L],
    lon     = cents_geo[, 1L],
    elev    = elev$mean,
    elevmin = elev$min,
    elevmax = elev$max,
    .wsno   = ws_ids,          # internal; removed before DB insert
    stringsAsFactors = FALSE
  )
}

# Build the gis_channels data.frame from TauDEM's stream network shapefile.
# Adds internal ".linkno" and ".dslinkno" columns for building routing.
.build_gis_channels <- function(net_file, sub_df, dem_rast) {
  net <- sf::st_read(net_file, quiet = TRUE)

  # TauDEM streamnet field names
  .col <- function(sf_obj, candidates, default) {
    hit <- candidates[candidates %in% names(sf_obj)]
    if (length(hit)) sf_obj[[hit[[1L]]]] else default
  }

  linkno   <- .col(net, c("LINKNO",    "linkno"),   seq_len(nrow(net)))
  dslinkno <- .col(net, c("DSLINKNO",  "dslinkno"), rep(-1L, nrow(net)))
  wsno     <- .col(net, c("WSNO",      "wsno"),     seq_len(nrow(net)))
  order_v  <- .col(net, c("Order",     "order"),    rep(1L,  nrow(net)))
  len_m    <- .col(net, c("Length",    "length"),   as.numeric(sf::st_length(net)))
  slope_v  <- .col(net, c("Slope",     "slope"),    rep(0.001, nrow(net)))
  da_km2   <- .col(net, c("TotDASqKm","totdasqkm"), rep(NA_real_, nrow(net)))

  # Drainage area fallback: use corresponding subbasin area
  if (anyNA(da_km2)) {
    wsno_to_area <- setNames(sub_df$area / 100, sub_df$.wsno)   # ha → km²
    da_km2[is.na(da_km2)] <-
      as.numeric(wsno_to_area[as.character(wsno[is.na(da_km2)])])
    da_km2[is.na(da_km2)] <- 0.01
  }
  da_km2 <- pmax(da_km2, 0.01)

  # Empirical channel width/depth from contributing drainage area (km²)
  # (power-law relationships commonly used in SWAT+)
  wid2 <- 2.6  * da_km2^0.4
  dep2 <- 0.12 * da_km2^0.4

  # Midpoints of reaches in WGS84
  net_geo    <- sf::st_transform(net, 4326L)
  mid_pts    <- sf::st_point_on_surface(sf::st_geometry(net_geo))
  mid_coords <- sf::st_coordinates(mid_pts)

  # Map TauDEM WSNO → sequential subbasin id
  wsno_to_id <- setNames(sub_df$id, sub_df$.wsno)
  sub_id     <- as.integer(wsno_to_id[as.character(as.integer(wsno))])
  sub_id[is.na(sub_id)] <- 1L

  # Elevation stats along each reach from DEM
  net_proj <- sf::st_transform(net, terra::crs(dem_rast))
  net_v    <- terra::vect(net_proj)
  ext_vals <- terra::extract(dem_rast, net_v, fun = NULL)
  col_nm   <- names(ext_vals)[[2L]]
  by_id    <- split(ext_vals[[col_nm]], ext_vals[[1L]])
  elevmin  <- vapply(by_id, min,  numeric(1L), na.rm = TRUE)
  elevmax  <- vapply(by_id, max,  numeric(1L), na.rm = TRUE)
  n        <- min(length(elevmin), nrow(net))

  ch_elevmin <- rep(0, nrow(net))
  ch_elevmax <- rep(0, nrow(net))
  ch_elevmin[seq_len(n)] <- elevmin[seq_len(n)]
  ch_elevmax[seq_len(n)] <- elevmax[seq_len(n)]

  data.frame(
    id       = seq_along(linkno),
    subbasin = sub_id,
    areac    = da_km2 * 100,            # km² → ha
    strahler = as.integer(pmax(order_v, 1L)),
    len2     = pmax(len_m, 1),
    slo2     = pmax(slope_v, 1e-5),
    wid2     = wid2,
    dep2     = dep2,
    elevmin  = ch_elevmin,
    elevmax  = ch_elevmax,
    midlat   = mid_coords[, 2L],
    midlon   = mid_coords[, 1L],
    .linkno  = as.integer(linkno),      # internal; removed before DB insert
    .dslinkno = as.integer(dslinkno),   # internal; removed before DB insert
    stringsAsFactors = FALSE
  )
}

# Build gis_lsus – one landscape unit per subbasin (simple delineation).
.build_gis_lsus <- function(sub_df, ch_df) {
  n <- nrow(sub_df)
  # For each subbasin, find the channel that drains it
  ch_for_sub <- ch_df$id[match(seq_len(n), ch_df$subbasin)]
  ch_for_sub[is.na(ch_for_sub)] <- 1L

  csl <- ch_df$slo2[match(seq_len(n), ch_df$subbasin)]
  csl[is.na(csl)] <- 0.001

  data.frame(
    id       = seq_len(n),
    category = 1L,                        # 1 = upland
    channel  = ch_for_sub,
    area     = sub_df$area,
    slope    = sub_df$slo1,
    len1     = sub_df$len1,
    csl      = csl,
    wid1     = pmax(sub_df$len1 * 0.1, 1),
    dep1     = 0.01,
    lat      = sub_df$lat,
    lon      = sub_df$lon,
    elev     = sub_df$elev,
    stringsAsFactors = FALSE
  )
}

# Build gis_hrus – one HRU per LSU with placeholder land-use / soil values.
# (A full HRU overlay requires land-use and soil rasters.)
.build_gis_hrus_simple <- function(lsu_df) {
  data.frame(
    id      = seq_len(nrow(lsu_df)),
    lsu     = lsu_df$id,
    arsub   = 1.0,
    arlsu   = 1.0,
    landuse = "AGRL",       # placeholder: agricultural general
    arland  = 1.0,
    soil    = "DEFAULT",    # placeholder; replaced after soils import
    arso    = 1.0,
    slp     = "0",
    arslp   = 1.0,
    slope   = lsu_df$slope,
    lat     = lsu_df$lat,
    lon     = lsu_df$lon,
    elev    = lsu_df$elev,
    stringsAsFactors = FALSE
  )
}

# Build gis_routing from TauDEM stream network topology.
# LSU → CH  (each landscape unit routes to its draining channel)
# CH  → CH  (each channel routes to its downstream reach)
# CH  → X   (the outlet reach routes to the watershed outlet)
.build_gis_routing <- function(lsu_df, ch_df) {
  rows <- vector("list", nrow(lsu_df) + nrow(ch_df))
  idx  <- 0L

  # LSU → CH
  for (i in seq_len(nrow(lsu_df))) {
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      sourceid  = lsu_df$id[[i]],
      sourcecat = "LSU",
      hyd_typ   = NA_character_,
      sinkid    = lsu_df$channel[[i]],
      sinkcat   = "CH",
      percent   = 100.0,
      stringsAsFactors = FALSE
    )
  }

  # CH → CH or CH → X
  linkno_to_id <- stats::setNames(ch_df$id, ch_df$.linkno)

  for (i in seq_len(nrow(ch_df))) {
    ds  <- ch_df$.dslinkno[[i]]
    key <- as.character(ds)
    if (!is.na(ds) && ds != -1L && key %in% names(linkno_to_id)) {
      sink_id  <- as.integer(linkno_to_id[[key]])
      sink_cat <- "CH"
    } else {
      sink_id  <- 1L
      sink_cat <- "X"   # watershed outlet
    }
    idx <- idx + 1L
    rows[[idx]] <- data.frame(
      sourceid  = ch_df$id[[i]],
      sourcecat = "CH",
      hyd_typ   = NA_character_,
      sinkid    = sink_id,
      sinkcat   = sink_cat,
      percent   = 100.0,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

# Build gis_points from the (snapped) outlet shapefile.
.build_gis_points <- function(outlet_file, watershed_file, dem_rast) {
  outlet_sf <- sf::st_read(outlet_file, quiet = TRUE)
  wshd_sf   <- sf::st_read(watershed_file, quiet = TRUE)

  # Geographic coordinates (WGS84) for storage
  outlet_geo  <- sf::st_transform(outlet_sf, 4326L)
  outlet_proj <- if (!tryCatch(terra::same.crs(terra::vect(outlet_sf),
                                               dem_rast),
                               error = function(e) FALSE)) {
    sf::st_transform(outlet_sf, terra::crs(dem_rast))
  } else {
    outlet_sf
  }

  coords_geo  <- sf::st_coordinates(outlet_geo)
  coords_proj <- sf::st_coordinates(outlet_proj)

  # Elevation at outlet
  ol_v      <- terra::vect(outlet_proj)
  elev_vals <- terra::extract(dem_rast, ol_v)[[2L]]
  elev_vals[is.na(elev_vals)] <- 0.0

  # Find which subbasin polygon contains each outlet
  wshd_geo <- sf::st_transform(wshd_sf, 4326L)
  sub_idx  <- sf::st_nearest_feature(outlet_geo, wshd_geo)

  data.frame(
    id       = seq_len(nrow(outlet_geo)),
    subbasin = as.integer(sub_idx),
    ptype    = "outlet",
    xpr      = coords_proj[, 1L],
    ypr      = coords_proj[, 2L],
    lat      = coords_geo[,  2L],
    lon      = coords_geo[,  1L],
    elev     = elev_vals,
    stringsAsFactors = FALSE
  )
}

# Set project_config.delineation_done = TRUE (and store gis_type/version).
.update_delineation_config <- function(project_db,
                                       gis_type    = "r-taudem",
                                       gis_version = NULL,
                                       verbose     = FALSE) {
  if (is.null(project_db) || !file.exists(project_db))
    return(invisible(NULL))

  con <- swat_open_db(project_db)
  on.exit(swat_close_db(con), add = TRUE)

  if (!swat_exists_table(con, "project_config"))
    return(invisible(NULL))

  version <- gis_version %||%
    as.character(utils::packageVersion("swatplusEditoR"))

  DBI::dbExecute(
    con,
    "UPDATE project_config SET delineation_done = 1, gis_type = ?, gis_version = ?",
    params = list(gis_type, version)
  )
  if (verbose)
    emit_progress("Updated project_config: delineation_done = TRUE")
  invisible(NULL)
}

# ===========================================================================
# Exported functions
# ===========================================================================

#' Delineate a watershed from a DEM using TauDEM
#'
#' @description
#' Runs the full TauDEM hydrological analysis pipeline on a digital elevation
#' model (DEM) and one or more outlet points, producing all \code{gis_*} tables
#' required by \code{\link{import_gis}}.  This mirrors the
#' \strong{"Delineate watershed"} option in the SWAT+ Editor GUI, which
#' normally delegates this step to QSWAT+ in QGIS using TauDEM.
#'
#' @details
#' The function executes the following TauDEM steps in order:
#' \enumerate{
#'   \item \strong{Pit Remove} (\code{taudem_pitremove}) — fills sinks/pits in
#'     the DEM to ensure continuous flow paths.
#'   \item \strong{D8 Flow Directions} (\code{taudem_d8flowdir}) — computes
#'     steepest-descent flow directions and D8 slopes.
#'   \item \strong{D8 Contributing Area — full DEM} (\code{taudem_aread8}) —
#'     accumulates upstream contributing area across the entire DEM.
#'   \item \strong{Stream Definition by Threshold} (\code{taudem_threshold}) —
#'     defines the stream network from cells whose contributing area equals or
#'     exceeds \code{stream_threshold}.
#'   \item \strong{Move Outlets to Streams} (\code{taudem_moveoutletstostream})
#'     — snaps the supplied outlet point(s) to the nearest stream cell within
#'     \code{snap_distance} cells.
#'   \item \strong{D8 Contributing Area — from outlet} (\code{taudem_aread8})
#'     — re-accumulates contributing area bounded by the snapped outlet,
#'     masking areas outside the watershed.
#'   \item \strong{Stream Net} (\code{taudem_exec("streamnet", ...)}) —
#'     delineates the watershed polygon(s) and labelled stream-reach network,
#'     including Strahler order, length, slope, and total drainage area.
#' }
#'
#' Subbasin statistics (area, mean slope, centroid coordinates, elevation) are
#' derived from the DEM and the watershed polygons.  Channel geometry (width,
#' depth) is estimated from drainage area using power-law relationships
#' (\eqn{w = 2.6 \, A^{0.4}}, \eqn{d = 0.12 \, A^{0.4}}, where \eqn{A} is
#' drainage area in km\ifelse{html}{\out{&sup2;}}{\eqn{^2}}).  Simple HRUs
#' (one per landscape unit, placeholder land use \code{"AGRL"}, placeholder
#' soil \code{"DEFAULT"}) are created; supply land-use and soil rasters later
#' through \code{\link{read_swatplus_gis}} or the SWAT+ Editor to override
#' these defaults.
#'
#' @param dem Path to a GeoTIFF DEM (\code{.tif}), or a \pkg{terra}
#'   \code{SpatRaster} object.  Geographic CRS (degrees) DEMs are
#'   automatically reprojected to the appropriate UTM zone before processing;
#'   TauDEM requires a projected (metric) coordinate system.
#' @param outlet Outlet location(s) in one of these formats:
#'   \itemize{
#'     \item An \code{sf} POINT object.
#'     \item A \code{data.frame} with \code{lon}/\code{lat} columns
#'       (or \code{x}/\code{y}).
#'     \item A path to an OGR-readable vector dataset (e.g. \code{.shp}).
#'   }
#' @param project_db Optional path to a SWAT+ project SQLite database.  When
#'   provided, the delineated \code{gis_*} tables are written to the database
#'   and \code{project_config.delineation_done} is set to \code{TRUE}.
#' @param stream_threshold Minimum number of upstream grid cells required to
#'   initiate a stream (passed to \code{\link[traudem]{taudem_threshold}} as
#'   \code{threshold_parameter}).  Larger values produce fewer, longer reaches.
#'   Default \code{1000}.
#' @param snap_distance Maximum number of grid cells to traverse when moving
#'   the outlet to the nearest stream cell (passed as \code{-md} to
#'   \code{\link[traudem]{taudem_moveoutletstostream}}).  Default \code{50}.
#' @param n_processes Number of MPI processes for TauDEM.  Default \code{1}.
#'   Increase for large DEMs if MPI is available.
#' @param work_dir Directory where TauDEM intermediate files are written.
#'   Defaults to a temporary directory that is removed on exit unless
#'   \code{keep_work_dir = TRUE}.
#' @param keep_work_dir If \code{TRUE}, \code{work_dir} is retained after the
#'   function returns, allowing inspection of all TauDEM intermediate outputs.
#'   Default \code{FALSE}.
#' @param verbose Print progress messages.  Default \code{TRUE}.
#'
#' @return A named list:
#' \describe{
#'   \item{\code{subbasins}}{data.frame for \code{gis_subbasins}}
#'   \item{\code{channels}}{data.frame for \code{gis_channels}}
#'   \item{\code{lsus}}{data.frame for \code{gis_lsus}}
#'   \item{\code{hrus}}{data.frame for \code{gis_hrus} (one per LSU)}
#'   \item{\code{water}}{NULL (water bodies not delineated)}
#'   \item{\code{points}}{data.frame for \code{gis_points} (snapped outlets)}
#'   \item{\code{aquifers}}{NULL}
#'   \item{\code{routing}}{data.frame for \code{gis_routing}}
#' }
#' Returned invisibly when \code{project_db} is provided.
#'
#' @seealso \code{\link{use_existing_watershed}},
#'   \code{\link{write_gis_to_db}}, \code{\link{setup_project}},
#'   \code{\link[traudem]{taudem_pitremove}}
#' @export
delineate_watershed <- function(dem,
                                outlet,
                                project_db       = NULL,
                                stream_threshold = 1000,
                                snap_distance    = 50,
                                n_processes      = 1L,
                                work_dir         = NULL,
                                keep_work_dir    = FALSE,
                                verbose          = TRUE) {
  .check_traudem()

  # --- Working directory ---------------------------------------------------
  own_dir <- is.null(work_dir)
  if (own_dir) {
    work_dir <- tempfile("swat_delineate_")
    dir.create(work_dir, recursive = TRUE)
    if (!keep_work_dir)
      on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
  }

  if (verbose) emit_progress("Preparing DEM and outlet...")

  dem_info    <- .prepare_dem(dem, work_dir, verbose)
  dem_file    <- dem_info$dem_file
  dem_rast    <- dem_info$rast
  crs_proj    <- dem_info$crs_proj

  outlet_info <- .prepare_outlet(outlet, crs_proj, work_dir)

  # -------------------------------------------------------------------------
  # TauDEM pipeline
  # -------------------------------------------------------------------------

  if (verbose) emit_progress("Step 1/7: Pit Remove...")
  dem_fel <- traudem::taudem_pitremove(dem_file, quiet = !verbose)

  if (verbose) emit_progress("Step 2/7: D8 Flow Directions...")
  flow_out     <- traudem::taudem_d8flowdir(dem_fel, quiet = !verbose)
  flowdir_file <- flow_out$output_d8flowdir_grid

  if (verbose) emit_progress("Step 3/7: D8 Contributing Area (full DEM)...")
  ad8_full <- traudem::taudem_aread8(flowdir_file, quiet = !verbose)

  if (verbose) emit_progress("Step 4/7: Stream Definition by Threshold...")
  src_file <- traudem::taudem_threshold(
    ad8_full,
    threshold_parameter = stream_threshold,
    quiet               = !verbose
  )

  if (verbose) emit_progress("Step 5/7: Moving outlet to nearest stream...")
  outlet_snapped <- traudem::taudem_moveoutletstostream(
    input_d8flowdir_grid     = flowdir_file,
    input_stream_raster_grid = src_file,
    outlet_file              = outlet_info$file,
    max_dist                 = snap_distance,
    quiet                    = !verbose
  )

  if (verbose) emit_progress("Step 6/7: D8 Contributing Area (from outlet)...")
  ad8_outlet <- traudem::taudem_aread8(
    flowdir_file,
    outlet_file = outlet_snapped,
    quiet       = !verbose
  )

  if (verbose) emit_progress("Step 7/7: Stream Net (stream network + watershed polygons)...")
  net_file       <- file.path(work_dir, "net.shp")
  watershed_file <- file.path(work_dir, "watershed.shp")
  order_file     <- file.path(work_dir, "ord.tif")
  tree_file      <- file.path(work_dir, "tree.dat")
  coord_file     <- file.path(work_dir, "coord.dat")

  traudem::taudem_exec(
    n_processes = n_processes,
    program     = "streamnet",
    args        = c(
      "-fel",   dem_fel,
      "-p",     flowdir_file,
      "-ad8",   ad8_outlet,
      "-src",   src_file,
      "-o",     outlet_snapped,
      "-ord",   order_file,
      "-tree",  tree_file,
      "-coord", coord_file,
      "-net",   net_file,
      "-w",     watershed_file
    ),
    quiet = !verbose
  )

  if (!file.exists(net_file) || !file.exists(watershed_file))
    stop(
      "TauDEM streamnet did not produce expected output files. ",
      "Check TauDEM messages above for details.",
      call. = FALSE
    )

  # -------------------------------------------------------------------------
  # Parse TauDEM outputs → gis_* data.frames
  # -------------------------------------------------------------------------

  if (verbose) emit_progress("Parsing delineation results into SWAT+ GIS tables...")

  sub_df  <- .build_gis_subbasins(watershed_file, dem_rast)
  ch_df   <- .build_gis_channels(net_file, sub_df, dem_rast)
  lsu_df  <- .build_gis_lsus(sub_df, ch_df)
  hru_df  <- .build_gis_hrus_simple(lsu_df)
  rout_df <- .build_gis_routing(lsu_df, ch_df)
  pts_df  <- .build_gis_points(outlet_snapped, watershed_file, dem_rast)

  # Remove internal helper columns before returning / inserting
  sub_df$.wsno    <- NULL
  ch_df$.linkno   <- NULL
  ch_df$.dslinkno <- NULL

  gis_data <- list(
    subbasins = sub_df,
    channels  = ch_df,
    lsus      = lsu_df,
    hrus      = hru_df,
    water     = NULL,
    points    = pts_df,
    aquifers  = NULL,
    routing   = rout_df
  )

  if (verbose)
    emit_progress(sprintf(
      "Delineation complete: %d subbasin(s), %d channel(s), %d HRU(s).",
      nrow(sub_df), nrow(ch_df), nrow(hru_df)
    ))

  # -------------------------------------------------------------------------
  # Write to project database
  # -------------------------------------------------------------------------

  if (!is.null(project_db)) {
    if (verbose) emit_progress("Writing GIS tables to project database...")
    con <- swat_open_db(project_db)
    write_gis_to_db(
      con           = con,
      subbasins     = gis_data$subbasins,
      channels      = gis_data$channels,
      lsus          = gis_data$lsus,
      hrus          = gis_data$hrus,
      water         = NULL,
      points        = gis_data$points,
      aquifers      = NULL,
      deep_aquifers = NULL,
      routing       = gis_data$routing
    )
    swat_close_db(con)
    .update_delineation_config(
      project_db  = project_db,
      gis_type    = "r-taudem",
      gis_version = as.character(utils::packageVersion("swatplusEditoR")),
      verbose     = verbose
    )
    return(invisible(gis_data))
  }

  gis_data
}

# ===========================================================================

#' Use an existing watershed delineation
#'
#' @description
#' Reads pre-delineated shapefiles produced by QSWAT+ or ArcSWAT+ (or by
#' \code{\link{delineate_watershed}}) from a directory and optionally writes
#' them to a SWAT+ project SQLite database.
#'
#' This mirrors the \strong{"Use existing watershed"} option in the SWAT+
#' Editor GUI, which accepts GIS output from QSWAT+ or ArcSWAT+ and populates
#' the \code{gis_*} tables in the project database ready for
#' \code{\link{setup_project}} and \code{\link{import_gis}}.
#'
#' @details
#' The function is a convenience wrapper around \code{\link{read_swatplus_gis}}
#' and \code{\link{write_gis_to_db}}.  It additionally records the source GIS
#' software in \code{project_config} and sets
#' \code{project_config.delineation_done = TRUE}.
#'
#' Standard QSWAT+ output shapefile names expected in \code{gis_dir}:
#' \tabular{ll}{
#'   \code{subs1.shp}         \tab Subbasins \cr
#'   \code{rivs1.shp}         \tab Channels / rivers \cr
#'   \code{lsus2.shp}         \tab Landscape units (LSUs) \cr
#'   \code{hrus1.shp}         \tab HRUs \cr
#'   \code{reservoirs.shp}    \tab Reservoirs / ponds (optional) \cr
#'   \code{outlets.shp}       \tab Point sources / inlets (optional) \cr
#'   \code{aquifers.shp}      \tab Shallow aquifers (optional) \cr
#'   \code{deepaquifers.shp}  \tab Deep aquifers (optional) \cr
#'   \code{dem.tif}           \tab DEM raster for elevation stats (optional) \cr
#' }
#'
#' @param gis_dir Path to the directory containing the QSWAT+/ArcSWAT+ output
#'   shapefiles listed above.
#' @param project_db Optional path to a SWAT+ project SQLite database.  When
#'   provided, the GIS tables are written to the database and
#'   \code{project_config.delineation_done} is set to \code{TRUE}.
#' @param dem_file Optional path to a DEM raster.  Overrides the default
#'   \code{dem.tif} inside \code{gis_dir} for elevation extraction.
#' @param gis_type GIS software that produced the shapefiles: \code{"qgis"}
#'   (default) or \code{"arcgis"}.  Stored in \code{project_config.gis_type}.
#' @param gis_version Version string of the GIS software (e.g.
#'   \code{"3.40.3-Bratislava"}).  Stored in \code{project_config.gis_version}.
#' @param verbose Print progress messages.  Default \code{TRUE}.
#'
#' @return A named list with components \code{subbasins}, \code{channels},
#'   \code{lsus}, \code{hrus}, \code{water}, \code{points},
#'   \code{aquifers}, \code{deep_aquifers} (as returned by
#'   \code{\link{read_swatplus_gis}}).
#'   Returned invisibly when \code{project_db} is provided.
#'
#' @seealso \code{\link{delineate_watershed}}, \code{\link{read_swatplus_gis}},
#'   \code{\link{write_gis_to_db}}, \code{\link{setup_project}}
#' @export
use_existing_watershed <- function(gis_dir,
                                   project_db  = NULL,
                                   dem_file    = NULL,
                                   gis_type    = "qgis",
                                   gis_version = NULL,
                                   verbose     = TRUE) {
  if (!dir.exists(gis_dir))
    stop("gis_dir not found: ", gis_dir, call. = FALSE)

  if (verbose) emit_progress("Reading existing watershed shapefiles...")

  gis_data <- read_swatplus_gis(gis_dir = gis_dir, dem_file = dem_file)

  n_sub <- if (!is.null(gis_data$subbasins)) nrow(gis_data$subbasins) else 0L
  n_ch  <- if (!is.null(gis_data$channels))  nrow(gis_data$channels)  else 0L
  n_hru <- if (!is.null(gis_data$hrus))      nrow(gis_data$hrus)      else 0L

  if (verbose)
    emit_progress(sprintf(
      "Read %d subbasin(s), %d channel(s), %d HRU(s) from %s",
      n_sub, n_ch, n_hru, gis_dir
    ))

  if (!is.null(project_db)) {
    if (verbose) emit_progress("Writing GIS tables to project database...")

    con <- swat_open_db(project_db)
    write_gis_to_db(
      con           = con,
      subbasins     = gis_data$subbasins,
      channels      = gis_data$channels,
      lsus          = gis_data$lsus,
      hrus          = gis_data$hrus,
      water         = gis_data$water,
      points        = gis_data$points,
      aquifers      = gis_data$aquifers,
      deep_aquifers = gis_data$deep_aquifers
    )
    swat_close_db(con)

    .update_delineation_config(
      project_db  = project_db,
      gis_type    = gis_type,
      gis_version = gis_version,
      verbose     = verbose
    )

    if (verbose)
      emit_progress(sprintf(
        "Done: %d subbasin(s), %d channel(s), %d HRU(s) written to database.",
        n_sub, n_ch, n_hru
      ))

    return(invisible(gis_data))
  }

  gis_data
}
