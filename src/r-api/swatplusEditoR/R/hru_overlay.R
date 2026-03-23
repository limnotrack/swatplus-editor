#' HRU overlay: land-use × soil × slope delineation
#'
#' Functions for delineating Hydrologic Response Units (HRUs) from land-use
#' and soil rasters overlaid on subbasin polygons, mirroring the QSWAT+ HRU
#' delineation step.
#'
#' @keywords internal
NULL

# ===========================================================================
# Internal helpers
# ===========================================================================

# Return slope-class labels, e.g. c("0-2","2-8","8-25","25-9999")
.make_slope_labels <- function(thresholds) {
  breaks <- c(0, thresholds)
  paste0(utils::head(breaks, -1L), "-", utils::tail(breaks, -1L))
}

# Assign each slope value (percent) to a 1-based class index.
.classify_slope_values <- function(slope_vals, thresholds) {
  findInterval(slope_vals, thresholds, rightmost.closed = FALSE) + 1L
}

# Reproject a SpatRaster to a target WKT CRS only when the CRS differs.
.project_raster_if_needed <- function(r, target_crs_wkt, method = "bilinear") {
  if (isTRUE(terra::same.crs(r, target_crs_wkt))) return(r)
  tryCatch(
    terra::project(r, target_crs_wkt, method = method),
    error = function(e) r
  )
}

# Internal: extract (lu, soil, slope_class) area fractions for one polygon.
# Returns a data.frame with columns:
#   lu_val, soil_val, slp_cls, n_cells, area_ha, frac_sub
# or NULL when no valid raster cells are found inside the polygon.
.extract_hru_fractions <- function(poly, lu_rast, soil_rast, slope_rast,
                                   slope_thresholds) {
  # Project the polygon to the raster CRS for masking
  rast_crs <- terra::crs(lu_rast)
  poly_proj <- tryCatch(
    sf::st_transform(poly, sf::st_crs(rast_crs)),
    error = function(e) poly
  )
  poly_v <- terra::vect(poly_proj)

  lu_c <- tryCatch(terra::crop(lu_rast,    poly_v), error = function(e) NULL)
  if (is.null(lu_c) || terra::ncell(lu_c) == 0L) return(NULL)

  soil_c  <- tryCatch(terra::crop(soil_rast,  poly_v), error = function(e) NULL)
  slope_c <- tryCatch(terra::crop(slope_rast, poly_v), error = function(e) NULL)
  if (is.null(soil_c) || is.null(slope_c)) return(NULL)

  # Resample soil/slope to landuse resolution if they differ
  if (!isTRUE(tryCatch(terra::compareGeom(lu_c, soil_c,  stopOnError = FALSE),
                       error = function(e) FALSE)))
    soil_c <- terra::resample(soil_c, lu_c, method = "near")
  if (!isTRUE(tryCatch(terra::compareGeom(lu_c, slope_c, stopOnError = FALSE),
                       error = function(e) FALSE)))
    slope_c <- terra::resample(slope_c, lu_c, method = "bilinear")

  # Mask all layers to the polygon
  lu_m    <- terra::mask(lu_c,    poly_v)
  soil_m  <- terra::mask(soil_c,  poly_v)
  slope_m <- terra::mask(slope_c, poly_v)

  lu_v    <- terra::values(lu_m,    mat = FALSE)
  soil_v  <- terra::values(soil_m,  mat = FALSE)
  slope_v <- terra::values(slope_m, mat = FALSE)

  ok <- !is.na(lu_v) & !is.na(soil_v) & !is.na(slope_v)
  if (!any(ok)) return(NULL)

  lu_v    <- as.character(round(lu_v[ok]))
  soil_v  <- as.character(round(soil_v[ok]))
  slp_cls <- .classify_slope_values(slope_v[ok], slope_thresholds)

  # Cell area in ha (accounts for projected CRS units of metres)
  res_xy  <- terra::res(lu_m)
  cell_ha <- prod(res_xy) / 10000

  # Aggregate counts per (lu, soil, slp_cls) combination.
  # Use a separator string unlikely to appear in land-use or soil codes.
  sep    <- "||SWAT||"
  keys   <- paste(lu_v, soil_v, slp_cls, sep = sep)
  cnts   <- table(keys)
  splits <- strsplit(names(cnts), sep, fixed = TRUE)

  result <- data.frame(
    lu_val   = vapply(splits, `[[`, character(1L), 1L),
    soil_val = vapply(splits, `[[`, character(1L), 2L),
    slp_cls  = as.integer(vapply(splits, `[[`, character(1L), 3L)),
    n_cells  = as.integer(cnts),
    stringsAsFactors = FALSE
  )
  result$area_ha  <- result$n_cells * cell_ha
  result$frac_sub <- result$area_ha / sum(result$area_ha)
  result
}

# Internal: remove HRUs below the area-fraction threshold and renormalise.
.apply_hru_threshold <- function(fracs, threshold) {
  keep <- fracs$frac_sub >= threshold
  # Always keep at least one HRU (the largest)
  if (!any(keep)) keep[which.max(fracs$frac_sub)] <- TRUE
  fracs <- fracs[keep, , drop = FALSE]
  fracs$frac_sub <- fracs$frac_sub / sum(fracs$frac_sub)
  fracs
}

# Internal: add arland, arso, arslp columns to a fractions data.frame.
# Definitions (mirroring QSWAT+ area-fraction fields):
#   arland = fraction of subbasin with this land-use
#   arso   = fraction of land-use area with this soil (arland_ls / arland_l)
#   arslp  = fraction of (lu × soil) area with this slope class
.compute_hru_fracs <- function(fracs) {
  # arland: total subbasin fraction for each land-use class
  lu_sums      <- tapply(fracs$frac_sub, fracs$lu_val, sum)
  fracs$arland <- as.numeric(lu_sums[fracs$lu_val])

  # arso: fraction of land-use area that has this (lu, soil) combination
  ls_key    <- paste(fracs$lu_val, fracs$soil_val, sep = "||SWAT||")
  ls_sums   <- tapply(fracs$frac_sub, ls_key, sum)
  fracs$arso <- as.numeric(ls_sums[ls_key]) / fracs$arland
  fracs$arso[!is.finite(fracs$arso)] <- 1.0

  # arslp: fraction of (lu × soil) area with this slope class
  fracs$arslp <- fracs$frac_sub / (fracs$arland * fracs$arso)
  fracs$arslp[!is.finite(fracs$arslp)] <- 1.0
  fracs
}

# Translate a raster integer value to a code using an optional lookup.
.lookup_code <- function(val, lookup) {
  key <- as.character(val)
  if (!is.null(lookup) && key %in% names(lookup)) lookup[[key]] else key
}

# ===========================================================================
# Public function
# ===========================================================================

#' Delineate HRUs from land-use and soil rasters
#'
#' Replaces the placeholder one-HRU-per-subbasin table created by
#' \code{\link{delineate_watershed}} with real Hydrologic Response Units
#' derived from the spatial intersection of land-use and soil rasters with
#' subbasin boundaries.  An optional slope classification adds a third
#' dimension to the HRU definition, matching the QSWAT+ HRU delineation step.
#'
#' Each unique combination of (land-use class, soil class, slope class) within
#' a subbasin becomes one HRU.  Combinations that cover less than
#' \code{hru_threshold} of the subbasin area are removed and the remaining
#' fractions are renormalised to sum to 1 within the subbasin.
#'
#' Area fraction columns (mirrors QSWAT+ \code{gis_hrus} fields):
#' \describe{
#'   \item{\code{arsub}}{area of this HRU / total subbasin area}
#'   \item{\code{arlsu}}{area of this HRU / LSU area (= \code{arsub} in
#'     simple single-LSU-per-subbasin mode)}
#'   \item{\code{arland}}{area of this land-use class / subbasin area}
#'   \item{\code{arso}}{area of (land-use × soil) / land-use area}
#'   \item{\code{arslp}}{area of this HRU / (land-use × soil) area}
#' }
#'
#' @param subbasins An \code{sf} polygon object as returned by
#'   \code{\link{delineate_watershed}} (\code{gis_data$subbasins}).  The
#'   object must have an \code{area} column (ha) and a defined CRS.
#' @param dem A \code{terra::SpatRaster} or file path to a DEM used to
#'   compute slope.  The DEM need not be in the same CRS as \code{subbasins};
#'   it is reprojected automatically.
#' @param landuse A \code{terra::SpatRaster} or file path to a land-use
#'   raster.  Integer cell values are mapped to SWAT+ land-use codes via
#'   \code{landuse_lookup}.
#' @param soil A \code{terra::SpatRaster} or file path to a soils raster.
#'   Integer cell values are mapped to SWAT+ soil names via
#'   \code{soil_lookup}.
#' @param landuse_lookup Named character vector mapping integer raster values
#'   (as character keys) to SWAT+ land-use codes
#'   (e.g. \code{c("1" = "AGRL", "2" = "FRST")}).
#'   When \code{NULL} (default), raw integer values are used as codes.
#' @param soil_lookup Named character vector mapping raster values to SWAT+
#'   soil names.  When \code{NULL}, raw integer values are used.
#' @param slope_thresholds Numeric vector of slope (\%) break-points defining
#'   slope classes.  Default \code{c(2, 8, 25, 9999)} produces four classes:
#'   \code{"0-2"}, \code{"2-8"}, \code{"8-25"}, \code{"25-9999"}.
#'   Supply a single very large value (e.g. \code{9999}) to collapse all cells
#'   into one slope class and effectively ignore slope in HRU definition.
#' @param hru_threshold Minimum fractional area (0–1) for an HRU to be
#'   retained.  Combinations below this are discarded and fractions are
#'   renormalised.  Default \code{0.02} (2\%).
#' @param verbose Print progress messages.  Default \code{TRUE}.
#'
#' @return A \code{data.frame} with one row per HRU containing the columns
#'   required by the \code{gis_hrus} database table:
#'   \code{id}, \code{lsu}, \code{arsub}, \code{arlsu},
#'   \code{landuse}, \code{arland},
#'   \code{soil}, \code{arso},
#'   \code{slp}, \code{arslp},
#'   \code{slope}, \code{lat}, \code{lon}, \code{elev}.
#'
#' @seealso \code{\link{delineate_watershed}},
#'   \code{\link{read_landuse_raster}}, \code{\link{read_soils_raster}},
#'   \code{\link{write_gis_to_db}}
#' @importFrom sf st_crs st_centroid st_coordinates st_transform
#' @importFrom terra rast terrain crop mask resample values res crs project
#' @export
delineate_hrus <- function(subbasins,
                           dem,
                           landuse,
                           soil,
                           landuse_lookup   = NULL,
                           soil_lookup      = NULL,
                           slope_thresholds = c(2, 8, 25, 9999),
                           hru_threshold    = 0.02,
                           verbose          = TRUE) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for HRU delineation.", call. = FALSE)
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required for HRU delineation.", call. = FALSE)
  if (!inherits(subbasins, "sf"))
    stop("'subbasins' must be an sf object with polygon geometry.", call. = FALSE)
  if (is.null(sf::st_crs(subbasins)$wkt))
    stop("'subbasins' must have a defined CRS.", call. = FALSE)

  # Load rasters from paths if needed
  if (is.character(dem))     dem     <- terra::rast(dem)
  if (is.character(landuse)) landuse <- terra::rast(landuse)
  if (is.character(soil))    soil    <- terra::rast(soil)

  # Compute slope in percent from DEM
  if (verbose) emit_progress("Computing slope from DEM...")
  slope_rast <- terra::terrain(dem, v = "slope", unit = "percent")

  # Project all rasters to the subbasin CRS so spatial operations are consistent
  sub_crs_wkt <- sf::st_crs(subbasins)$wkt
  if (verbose) emit_progress("Projecting rasters to subbasin CRS...")
  lu_proj  <- .project_raster_if_needed(landuse,    sub_crs_wkt, method = "near")
  so_proj  <- .project_raster_if_needed(soil,       sub_crs_wkt, method = "near")
  sl_proj  <- .project_raster_if_needed(slope_rast, sub_crs_wkt, method = "bilinear")

  slope_labels <- .make_slope_labels(slope_thresholds)
  n_sub        <- nrow(subbasins)
  hru_rows     <- vector("list", n_sub)

  if (verbose)
    emit_progress(sprintf("Delineating HRUs for %d subbasin(s)...", n_sub))

  for (i in seq_len(n_sub)) {
    poly_i  <- subbasins[i, ]

    # Centroid in WGS84 and subbasin-level attributes for fallback / context
    cent_geo <- sf::st_coordinates(
      sf::st_centroid(sf::st_transform(poly_i, 4326L)))
    sub_lat  <- cent_geo[, "Y"][[1L]]
    sub_lon  <- cent_geo[, "X"][[1L]]
    sub_elev <- if ("elev" %in% names(subbasins)) subbasins$elev[[i]] else 0.0
    sub_slp  <- if ("slo1" %in% names(subbasins)) subbasins$slo1[[i]] else 0.0

    fracs_i <- tryCatch(
      .extract_hru_fractions(poly_i, lu_proj, so_proj, sl_proj, slope_thresholds),
      error = function(e) {
        if (verbose)
          warning("Subbasin ", i, ": could not extract raster fractions — ",
                  conditionMessage(e), call. = FALSE)
        NULL
      }
    )

    # Fallback: single default HRU if no raster data in this polygon
    if (is.null(fracs_i) || nrow(fracs_i) == 0L) {
      hru_rows[[i]] <- data.frame(
        lsu     = i,
        arsub   = 1.0,   arlsu   = 1.0,
        landuse = "AGRL", arland  = 1.0,
        soil    = "DEFAULT", arso = 1.0,
        slp     = slope_labels[[length(slope_labels)]],
        arslp   = 1.0,
        slope   = sub_slp,
        lat     = sub_lat, lon = sub_lon, elev = sub_elev,
        stringsAsFactors = FALSE
      )
      next
    }

    fracs_i <- .apply_hru_threshold(fracs_i, hru_threshold)
    fracs_i <- .compute_hru_fracs(fracs_i)

    rows_i <- lapply(seq_len(nrow(fracs_i)), function(j) {
      fr       <- fracs_i[j, ]
      lu_code  <- .lookup_code(fr$lu_val,   landuse_lookup)
      so_code  <- .lookup_code(fr$soil_val, soil_lookup)
      slp_lbl  <- if (fr$slp_cls >= 1L && fr$slp_cls <= length(slope_labels))
        slope_labels[[fr$slp_cls]] else slope_labels[[length(slope_labels)]]

      data.frame(
        lsu     = i,
        arsub   = fr$frac_sub,
        arlsu   = fr$frac_sub,
        landuse = lu_code,
        arland  = fr$arland,
        soil    = so_code,
        arso    = fr$arso,
        slp     = slp_lbl,
        arslp   = fr$arslp,
        slope   = sub_slp,
        lat     = sub_lat,
        lon     = sub_lon,
        elev    = sub_elev,
        stringsAsFactors = FALSE
      )
    })
    hru_rows[[i]] <- do.call(rbind, rows_i)
  }

  hrus    <- do.call(rbind, hru_rows)
  hrus$id <- seq_len(nrow(hrus))
  hrus[, c("id", setdiff(names(hrus), "id")), drop = FALSE]
}
