#' Read shapefiles and rasters for SWAT+ GIS setup
#'
#' Provides functions to read spatial data from shapefiles (\pkg{sf}) and
#' raster files (\pkg{terra}) and convert them into the GIS tables expected
#' by the SWAT+ project database.  This module adds new functionality that the
#' Python API does not contain; in the Python workflow these tables are
#' populated by an external GIS plug-in (QSWAT+ or ArcSWAT+).
#'
#' @keywords internal
NULL

# ===========================================================================
# Shapefile readers
# ===========================================================================

#' Read a subbasins shapefile and return a data.frame for \code{gis_subbasins}
#'
#' Expected attribute columns (case-insensitive lookup):
#' \code{area}, \code{slo1}, \code{len1}, \code{sll},
#' \code{lat}, \code{lon}, \code{elev}, \code{elevmin}, \code{elevmax}.
#' If \code{lat}/\code{lon} are absent they are derived from the centroid of
#' each feature.  If \code{elev} is absent and \code{dem} is provided,
#' mean elevation is extracted from the raster.
#'
#' @param path  Path to the shapefile (\code{.shp}) or any format readable by
#'   \pkg{sf}.
#' @param dem   Optional \pkg{terra} \code{SpatRaster} for elevation
#'   extraction (used when \code{elev}/\code{elevmin}/\code{elevmax} columns
#'   are absent in the shapefile).
#' @param area_col   Column name for area (ha).  Default \code{"area"}.
#' @param slope_col  Column name for mean slope (\%).  Default \code{"slo1"}.
#' @return A \code{data.frame} suitable for inserting into \code{gis_subbasins}.
#' @importFrom sf st_read st_centroid st_coordinates st_transform st_crs
#' @importFrom terra extract
#' @export
read_subbasins_shp <- function(path, dem = NULL,
                               area_col  = "area",
                               slope_col = "slo1") {
  shp <- sf::st_read(path, quiet = TRUE)
  shp <- .normalize_crs(shp)

  cols  <- tolower(names(shp))
  nrows <- nrow(shp)

  out <- data.frame(
    area    = .col_or_default(shp, cols, area_col,  0.0),
    slo1    = .col_or_default(shp, cols, slope_col, 0.0),
    len1    = .col_or_default(shp, cols, "len1",    0.0),
    sll     = .col_or_default(shp, cols, "sll",     0.0),
    lat     = NA_real_,
    lon     = NA_real_,
    elev    = .col_or_default(shp, cols, "elev",    NA_real_),
    elevmin = .col_or_default(shp, cols, "elevmin", NA_real_),
    elevmax = .col_or_default(shp, cols, "elevmax", NA_real_),
    stringsAsFactors = FALSE
  )

  # Derive lat / lon from centroids if not in shapefile
  if (!("lat" %in% cols) || !("lon" %in% cols)) {
    cents <- sf::st_coordinates(sf::st_centroid(shp))
    out$lon <- cents[, "X"]
    out$lat <- cents[, "Y"]
  } else {
    out$lat <- shp[[which(cols == "lat")]]
    out$lon <- shp[[which(cols == "lon")]]
  }

  # Extract elevation from DEM if needed
  if (!is.null(dem) && any(is.na(out$elev))) {
    ev <- .extract_dem_stats(dem, shp)
    out$elev[is.na(out$elev)]    <- ev$mean[is.na(out$elev)]
    out$elevmin[is.na(out$elevmin)] <- ev$min[is.na(out$elevmin)]
    out$elevmax[is.na(out$elevmax)] <- ev$max[is.na(out$elevmax)]
  }

  out$elev[is.na(out$elev)]       <- 0.0
  out$elevmin[is.na(out$elevmin)] <- 0.0
  out$elevmax[is.na(out$elevmax)] <- 0.0

  out
}

#' Read a channels shapefile and return a data.frame for \code{gis_channels}
#'
#' Expected columns: \code{subbasin}, \code{areac}, \code{strahler},
#' \code{len2}, \code{slo2}, \code{wid2}, \code{dep2},
#' \code{elevmin}, \code{elevmax}, \code{midlat}, \code{midlon}.
#'
#' @param path Path to the shapefile.
#' @param dem  Optional DEM \code{SpatRaster} for elevation extraction.
#' @return A \code{data.frame} for \code{gis_channels}.
#' @importFrom sf st_read st_centroid st_coordinates
#' @export
read_channels_shp <- function(path, dem = NULL) {
  shp  <- sf::st_read(path, quiet = TRUE)
  shp  <- .normalize_crs(shp)
  cols <- tolower(names(shp))

  cents <- sf::st_coordinates(sf::st_centroid(shp))

  out <- data.frame(
    subbasin  = as.integer(.col_or_default(shp, cols, "subbasin",  NA_integer_)),
    areac     = .col_or_default(shp, cols, "areac",    0.0),
    strahler  = as.integer(.col_or_default(shp, cols, "strahler", 1L)),
    len2      = .col_or_default(shp, cols, "len2",     0.0),
    slo2      = .col_or_default(shp, cols, "slo2",     0.0),
    wid2      = .col_or_default(shp, cols, "wid2",     0.0),
    dep2      = .col_or_default(shp, cols, "dep2",     0.0),
    elevmin   = .col_or_default(shp, cols, "elevmin",  NA_real_),
    elevmax   = .col_or_default(shp, cols, "elevmax",  NA_real_),
    midlat    = if ("midlat" %in% cols) shp[[which(cols == "midlat")]] else cents[, "Y"],
    midlon    = if ("midlon" %in% cols) shp[[which(cols == "midlon")]] else cents[, "X"],
    stringsAsFactors = FALSE
  )

  if (!is.null(dem)) {
    ev <- .extract_dem_stats(dem, shp)
    out$elevmin[is.na(out$elevmin)] <- ev$min[is.na(out$elevmin)]
    out$elevmax[is.na(out$elevmax)] <- ev$max[is.na(out$elevmax)]
  }
  out$elevmin[is.na(out$elevmin)] <- 0.0
  out$elevmax[is.na(out$elevmax)] <- 0.0
  out
}

#' Read a landscape units shapefile and return a data.frame for \code{gis_lsus}
#'
#' @param path Path to the shapefile.
#' @param dem  Optional DEM \code{SpatRaster}.
#' @return A \code{data.frame} for \code{gis_lsus}.
#' @importFrom sf st_read st_centroid st_coordinates
#' @export
read_lsus_shp <- function(path, dem = NULL) {
  shp  <- sf::st_read(path, quiet = TRUE)
  shp  <- .normalize_crs(shp)
  cols <- tolower(names(shp))
  cents <- sf::st_coordinates(sf::st_centroid(shp))

  out <- data.frame(
    category = as.integer(.col_or_default(shp, cols, "category", 1L)),
    channel  = as.integer(.col_or_default(shp, cols, "channel",  NA_integer_)),
    area     = .col_or_default(shp, cols, "area",    0.0),
    slope    = .col_or_default(shp, cols, "slope",   0.0),
    len1     = .col_or_default(shp, cols, "len1",    0.0),
    csl      = .col_or_default(shp, cols, "csl",     0.0),
    wid1     = .col_or_default(shp, cols, "wid1",    0.0),
    dep1     = .col_or_default(shp, cols, "dep1",    0.0),
    lat      = cents[, "Y"],
    lon      = cents[, "X"],
    elev     = .col_or_default(shp, cols, "elev",    NA_real_),
    stringsAsFactors = FALSE
  )

  if (!is.null(dem) && any(is.na(out$elev))) {
    ev <- .extract_dem_stats(dem, shp)
    out$elev[is.na(out$elev)] <- ev$mean[is.na(out$elev)]
  }
  out$elev[is.na(out$elev)] <- 0.0
  out
}

#' Read an HRUs shapefile and return a data.frame for \code{gis_hrus}
#'
#' Expected columns: \code{lsu}, \code{arsub}, \code{arlsu}, \code{landuse},
#' \code{arland}, \code{soil}, \code{arso}, \code{slp}, \code{arslp},
#' \code{slope}, \code{lat}, \code{lon}, \code{elev}.
#'
#' @param path Path to the shapefile.
#' @param dem  Optional DEM \code{SpatRaster}.
#' @return A \code{data.frame} for \code{gis_hrus}.
#' @importFrom sf st_read st_centroid st_coordinates
#' @export
read_hrus_shp <- function(path, dem = NULL) {
  shp  <- sf::st_read(path, quiet = TRUE)
  shp  <- .normalize_crs(shp)
  cols <- tolower(names(shp))
  cents <- sf::st_coordinates(sf::st_centroid(shp))

  out <- data.frame(
    lsu     = as.integer(.col_or_default(shp, cols, "lsu",     NA_integer_)),
    arsub   = .col_or_default(shp, cols, "arsub",   0.0),
    arlsu   = .col_or_default(shp, cols, "arlsu",   0.0),
    landuse = .col_or_default(shp, cols, "landuse", NA_character_),
    arland  = .col_or_default(shp, cols, "arland",  0.0),
    soil    = .col_or_default(shp, cols, "soil",    ""),
    arso    = .col_or_default(shp, cols, "arso",    0.0),
    slp     = .col_or_default(shp, cols, "slp",     "0"),
    arslp   = .col_or_default(shp, cols, "arslp",   0.0),
    slope   = .col_or_default(shp, cols, "slope",   0.0),
    lat     = if ("lat" %in% cols) shp[[which(cols == "lat")]] else cents[, "Y"],
    lon     = if ("lon" %in% cols) shp[[which(cols == "lon")]] else cents[, "X"],
    elev    = .col_or_default(shp, cols, "elev",    NA_real_),
    stringsAsFactors = FALSE
  )

  if (!is.null(dem) && any(is.na(out$elev))) {
    ev <- .extract_dem_stats(dem, shp)
    out$elev[is.na(out$elev)] <- ev$mean[is.na(out$elev)]
  }
  out$elev[is.na(out$elev)] <- 0.0
  out
}

#' Read a water bodies shapefile and return a data.frame for \code{gis_water}
#'
#' @param path Path to the shapefile.
#' @param dem  Optional DEM \code{SpatRaster}.
#' @return A \code{data.frame} for \code{gis_water}.
#' @importFrom sf st_read st_centroid st_coordinates
#' @export
read_water_shp <- function(path, dem = NULL) {
  shp  <- sf::st_read(path, quiet = TRUE)
  shp  <- .normalize_crs(shp)
  cols <- tolower(names(shp))
  cents <- sf::st_coordinates(sf::st_centroid(shp))

  out <- data.frame(
    wtype   = .col_or_default(shp, cols, "wtype",   "WTR"),
    lsu     = as.integer(.col_or_default(shp, cols, "lsu",  NA_integer_)),
    subbasin = as.integer(.col_or_default(shp, cols, "subbasin", NA_integer_)),
    area    = .col_or_default(shp, cols, "area",    0.0),
    xpr     = if ("xpr" %in% cols) shp[[which(cols == "xpr")]] else cents[, "X"],
    ypr     = if ("ypr" %in% cols) shp[[which(cols == "ypr")]] else cents[, "Y"],
    lat     = if ("lat" %in% cols) shp[[which(cols == "lat")]] else cents[, "Y"],
    lon     = if ("lon" %in% cols) shp[[which(cols == "lon")]] else cents[, "X"],
    elev    = .col_or_default(shp, cols, "elev",    NA_real_),
    stringsAsFactors = FALSE
  )

  if (!is.null(dem) && any(is.na(out$elev))) {
    ev <- .extract_dem_stats(dem, shp)
    out$elev[is.na(out$elev)] <- ev$mean[is.na(out$elev)]
  }
  out$elev[is.na(out$elev)] <- 0.0
  out
}

#' Read a point sources shapefile and return a data.frame for \code{gis_points}
#'
#' @param path Path to the shapefile.
#' @param dem  Optional DEM \code{SpatRaster}.
#' @return A \code{data.frame} for \code{gis_points}.
#' @importFrom sf st_read st_coordinates
#' @export
read_points_shp <- function(path, dem = NULL) {
  shp  <- sf::st_read(path, quiet = TRUE)
  shp  <- .normalize_crs(shp)
  cols <- tolower(names(shp))
  coords <- sf::st_coordinates(shp)

  out <- data.frame(
    subbasin = as.integer(.col_or_default(shp, cols, "subbasin", NA_integer_)),
    ptype    = .col_or_default(shp, cols, "ptype", "P"),
    xpr      = coords[, "X"],
    ypr      = coords[, "Y"],
    lat      = if ("lat" %in% cols) shp[[which(cols == "lat")]] else coords[, "Y"],
    lon      = if ("lon" %in% cols) shp[[which(cols == "lon")]] else coords[, "X"],
    elev     = .col_or_default(shp, cols, "elev", NA_real_),
    stringsAsFactors = FALSE
  )

  if (!is.null(dem) && any(is.na(out$elev))) {
    ev <- .extract_dem_stats_points(dem, shp)
    out$elev[is.na(out$elev)] <- ev[is.na(out$elev)]
  }
  out$elev[is.na(out$elev)] <- 0.0
  out
}

#' Read aquifer shapefiles and return data.frames for the GIS aquifer tables
#'
#' @param shallow_path Path to the shallow aquifer shapefile.
#' @param deep_path    Path to the deep aquifer shapefile (optional).
#' @param dem          Optional DEM \code{SpatRaster}.
#' @return A named list with elements \code{aquifers} and \code{deep_aquifers}.
#' @importFrom sf st_read st_centroid st_coordinates
#' @export
read_aquifers_shp <- function(shallow_path, deep_path = NULL, dem = NULL) {
  shp  <- sf::st_read(shallow_path, quiet = TRUE)
  shp  <- .normalize_crs(shp)
  cols <- tolower(names(shp))
  cents <- sf::st_coordinates(sf::st_centroid(shp))

  aquifers <- data.frame(
    category     = as.integer(.col_or_default(shp, cols, "category",    1L)),
    subbasin     = as.integer(.col_or_default(shp, cols, "subbasin",    NA_integer_)),
    deep_aquifer = as.integer(.col_or_default(shp, cols, "deep_aquifer", 1L)),
    area         = .col_or_default(shp, cols, "area", 0.0),
    lat          = if ("lat" %in% cols) shp[[which(cols == "lat")]] else cents[, "Y"],
    lon          = if ("lon" %in% cols) shp[[which(cols == "lon")]] else cents[, "X"],
    elev         = .col_or_default(shp, cols, "elev", NA_real_),
    stringsAsFactors = FALSE
  )
  if (!is.null(dem) && any(is.na(aquifers$elev))) {
    ev <- .extract_dem_stats(dem, shp)
    aquifers$elev[is.na(aquifers$elev)] <- ev$mean[is.na(aquifers$elev)]
  }
  aquifers$elev[is.na(aquifers$elev)] <- 0.0

  deep_aquifers <- data.frame()
  if (!is.null(deep_path)) {
    dshp  <- sf::st_read(deep_path, quiet = TRUE)
    dshp  <- .normalize_crs(dshp)
    dcols <- tolower(names(dshp))
    dcents <- sf::st_coordinates(sf::st_centroid(dshp))

    deep_aquifers <- data.frame(
      subbasin = as.integer(.col_or_default(dshp, dcols, "subbasin", NA_integer_)),
      area     = .col_or_default(dshp, dcols, "area", 0.0),
      lat      = if ("lat" %in% dcols) dshp[[which(dcols == "lat")]] else dcents[, "Y"],
      lon      = if ("lon" %in% dcols) dshp[[which(dcols == "lon")]] else dcents[, "X"],
      elev     = .col_or_default(dshp, dcols, "elev", NA_real_),
      stringsAsFactors = FALSE
    )
    if (!is.null(dem) && any(is.na(deep_aquifers$elev))) {
      ev <- .extract_dem_stats(dem, dshp)
      deep_aquifers$elev[is.na(deep_aquifers$elev)] <- ev$mean[is.na(deep_aquifers$elev)]
    }
    deep_aquifers$elev[is.na(deep_aquifers$elev)] <- 0.0
  }

  list(aquifers = aquifers, deep_aquifers = deep_aquifers)
}

# ===========================================================================
# Raster readers
# ===========================================================================

#' Load a Digital Elevation Model raster
#'
#' A thin wrapper around \code{terra::rast()} that reprojects to WGS84 if the
#' raster has a geographic CRS, so that elevation extraction is consistent with
#' the WGS84 shapefiles used throughout the package.
#'
#' @param path Path to the DEM raster file (any format supported by
#'   \pkg{terra}, e.g. GeoTIFF, NetCDF, etc.).
#' @return A \code{terra::SpatRaster} object.
#' @importFrom terra rast project
#' @export
read_dem_raster <- function(path) {
  r <- terra::rast(path)
  if (!terra::is.lonlat(r)) {
    r <- terra::project(r, "EPSG:4326")
  }
  r
}

#' Load a land-use raster and return a lookup table of cover codes
#'
#' @param path    Path to the land-use raster.
#' @param lookup  Optional named character vector mapping raster integer values
#'   to SWAT+ land-use codes (e.g. \code{c("1" = "corn", "2" = "frst")}).
#'   If \code{NULL}, integer cell values are used as-is.
#' @return A list with \code{raster} (the \code{SpatRaster}) and \code{lookup}
#'   (named character vector of code -> SWAT+ name).
#' @importFrom terra rast levels
#' @export
read_landuse_raster <- function(path, lookup = NULL) {
  r <- terra::rast(path)
  if (is.null(lookup)) {
    levs <- terra::levels(r)[[1]]
    if (!is.null(levs)) {
      lookup <- stats::setNames(as.character(levs[[2]]), as.character(levs[[1]]))
    }
  }
  list(raster = r, lookup = lookup)
}

#' Load a soils raster and return a lookup table of soil codes
#'
#' @param path    Path to the soils raster.
#' @param lookup  Optional named character vector mapping raster values to
#'   soil names used in the SWAT+ soils database.
#' @return A list with \code{raster} and \code{lookup}.
#' @importFrom terra rast levels
#' @export
read_soils_raster <- function(path, lookup = NULL) {
  r <- terra::rast(path)
  if (is.null(lookup)) {
    levs <- terra::levels(r)[[1]]
    if (!is.null(levs)) {
      lookup <- stats::setNames(as.character(levs[[2]]), as.character(levs[[1]]))
    }
  }
  list(raster = r, lookup = lookup)
}

# ===========================================================================
# Write GIS tables to database
# ===========================================================================

#' Write all GIS data.frames to the project database
#'
#' Populates the \code{gis_*} tables from the outputs of the shapefile reader
#' functions.  All existing rows in the GIS tables are deleted first.
#'
#' @param con          An open \code{DBIConnection} to the project database.
#' @param subbasins    \code{data.frame} from \code{\link{read_subbasins_shp}}.
#' @param channels     \code{data.frame} from \code{\link{read_channels_shp}}.
#' @param lsus         \code{data.frame} from \code{\link{read_lsus_shp}}.
#' @param hrus         \code{data.frame} from \code{\link{read_hrus_shp}}.
#' @param water        Optional \code{data.frame} from \code{\link{read_water_shp}}.
#' @param points       Optional \code{data.frame} from \code{\link{read_points_shp}}.
#' @param aquifers     Optional \code{data.frame} for \code{gis_aquifers}.
#' @param deep_aquifers Optional \code{data.frame} for \code{gis_deep_aquifers}.
#' @param routing      Optional \code{data.frame} for \code{gis_routing}
#'   (columns: \code{sourceid}, \code{sourcecat}, \code{hyd_typ},
#'   \code{sinkid}, \code{sinkcat}, \code{percent}).
#' @return Invisibly, a named list of row counts inserted into each table.
#' @export
write_gis_to_db <- function(con,
                            subbasins,
                            channels,
                            lsus,
                            hrus,
                            water        = NULL,
                            points       = NULL,
                            aquifers     = NULL,
                            deep_aquifers = NULL,
                            routing      = NULL) {
  counts <- list()

  # Clear existing GIS data
  for (tbl in c("gis_subbasins", "gis_channels", "gis_lsus", "gis_hrus",
                "gis_water", "gis_points", "gis_routing",
                "gis_aquifers", "gis_deep_aquifers")) {
    if (swat_exists_table(con, tbl)) {
      DBI::dbExecute(con, paste0("DELETE FROM ", tbl))
    }
  }

  counts$subbasins <- swat_bulk_insert(con, "gis_subbasins", subbasins)
  counts$channels  <- swat_bulk_insert(con, "gis_channels",  channels)
  counts$lsus      <- swat_bulk_insert(con, "gis_lsus",      lsus)
  counts$hrus      <- swat_bulk_insert(con, "gis_hrus",      hrus)

  if (!is.null(water) && nrow(water) > 0L)
    counts$water <- swat_bulk_insert(con, "gis_water", water)
  if (!is.null(points) && nrow(points) > 0L)
    counts$points <- swat_bulk_insert(con, "gis_points", points)
  if (!is.null(aquifers) && nrow(aquifers) > 0L)
    counts$aquifers <- swat_bulk_insert(con, "gis_aquifers", aquifers)
  if (!is.null(deep_aquifers) && nrow(deep_aquifers) > 0L)
    counts$deep_aquifers <- swat_bulk_insert(con, "gis_deep_aquifers", deep_aquifers)
  if (!is.null(routing) && nrow(routing) > 0L)
    counts$routing <- swat_bulk_insert(con, "gis_routing", routing)

  invisible(counts)
}

# ===========================================================================
# Convenience wrapper: read all shapefiles from a QSWAT+ / ArcSWAT+ folder
# ===========================================================================

#' Read all standard SWAT+ GIS shapefiles from a directory
#'
#' Looks for the standard shapefile names produced by QSWAT+ or ArcSWAT+ in
#' \code{gis_dir} and returns a named list of \code{data.frame}s ready for
#' \code{\link{write_gis_to_db}}.
#'
#' Standard filenames:
#' \itemize{
#'   \item \code{subs1.shp} – subbasins
#'   \item \code{rivs1.shp} – channels / rivers
#'   \item \code{lsus2.shp} – landscape units
#'   \item \code{hrus1.shp} – HRUs
#'   \item \code{reservoirs.shp} – water bodies (optional)
#'   \item \code{outlets.shp}   – point sources (optional)
#'   \item \code{aquifers.shp}  – shallow aquifers (optional)
#'   \item \code{deepaquifers.shp} – deep aquifers (optional)
#'   \item \code{dem.tif}       – DEM raster (optional)
#' }
#'
#' @param gis_dir Directory containing the shapefiles.
#' @param dem_file Optional path to DEM raster (uses
#'   \code{file.path(gis_dir, "dem.tif")} by default if present).
#' @return A named list suitable for passing to \code{\link{write_gis_to_db}}.
#' @export
read_swatplus_gis <- function(gis_dir, dem_file = NULL) {
  f <- function(name) file.path(gis_dir, name)

  # Load DEM if available
  dem <- NULL
  if (!is.null(dem_file) && file.exists(dem_file)) {
    dem <- read_dem_raster(dem_file)
  } else if (file.exists(f("dem.tif"))) {
    dem <- read_dem_raster(f("dem.tif"))
  }

  result <- list()

  if (file.exists(f("subs1.shp")))
    result$subbasins <- read_subbasins_shp(f("subs1.shp"), dem = dem)
  if (file.exists(f("rivs1.shp")))
    result$channels  <- read_channels_shp(f("rivs1.shp"),  dem = dem)
  if (file.exists(f("lsus2.shp")))
    result$lsus      <- read_lsus_shp(f("lsus2.shp"),      dem = dem)
  if (file.exists(f("hrus1.shp")))
    result$hrus      <- read_hrus_shp(f("hrus1.shp"),       dem = dem)
  if (file.exists(f("reservoirs.shp")))
    result$water     <- read_water_shp(f("reservoirs.shp"), dem = dem)
  if (file.exists(f("outlets.shp")))
    result$points    <- read_points_shp(f("outlets.shp"),   dem = dem)

  aqu_path  <- f("aquifers.shp")
  daqu_path <- f("deepaquifers.shp")
  if (file.exists(aqu_path)) {
    aqu_data <- read_aquifers_shp(
      aqu_path,
      deep_path = if (file.exists(daqu_path)) daqu_path else NULL,
      dem       = dem
    )
    result$aquifers      <- aqu_data$aquifers
    result$deep_aquifers <- aqu_data$deep_aquifers
  }

  result
}

# ===========================================================================
# Internal helpers
# ===========================================================================

# Reproject to WGS84 geographic if needed
.normalize_crs <- function(shp) {
  wgs84 <- sf::st_crs(4326)
  if (is.na(sf::st_crs(shp)) || sf::st_crs(shp) == wgs84) return(shp)
  sf::st_transform(shp, wgs84)
}

# Safely pull a column by (lowercase) name, returning a default vector
.col_or_default <- function(shp, cols_lower, col_name, default) {
  idx <- which(cols_lower == tolower(col_name))
  if (length(idx) == 0L) {
    return(rep(default, nrow(shp)))
  }
  as.vector(shp[[idx[[1L]]]])
}

# Extract mean/min/max elevation from a DEM raster for polygon features
.extract_dem_stats <- function(dem, shp) {
  shp_v  <- terra::vect(shp)
  if (!tryCatch(terra::same.crs(shp_v, dem), error = function(e) FALSE)) {
    shp_v <- terra::project(shp_v, terra::crs(dem))
  }
  vals <- terra::extract(dem, shp_v, fun = NULL)
  col  <- names(vals)[[2L]]  # first layer column
  by_id <- split(vals[[col]], vals[[1L]])
  list(
    mean = vapply(by_id, mean, numeric(1L), na.rm = TRUE),
    min  = vapply(by_id, min,  numeric(1L), na.rm = TRUE),
    max  = vapply(by_id, max,  numeric(1L), na.rm = TRUE)
  )
}

# Extract elevation from a DEM raster at point features
.extract_dem_stats_points <- function(dem, shp) {
  shp_v <- terra::vect(shp)
  if (!tryCatch(terra::same.crs(shp_v, dem), error = function(e) FALSE)) {
    shp_v <- terra::project(shp_v, terra::crs(dem))
  }
  vals <- terra::extract(dem, shp_v)
  vals[[2L]]
}
