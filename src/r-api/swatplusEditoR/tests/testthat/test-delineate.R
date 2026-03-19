## Tests for delineate.R
## ─────────────────────────────────────────────────────────────────────────────
## Tests are organised in three tiers:
##   1. Structural tests using synthetic mock data — always run, no dependencies
##   2. Real-file tests using the bundled extdata — require sf + terra, no TauDEM
##   3. TauDEM integration tests — skipped unless TauDEM is installed

library(swatplusEditoR)

# ===========================================================================
# Shared helpers
# ===========================================================================

# Resolve a path inside the package's inst/extdata directory.
extdata <- function(file) {
  system.file("extdata", file, package = "swatplusEditoR")
}

# Minimal synthetic gis_* tables (mirrors what delineate_watershed returns)
.make_mock_gis <- function(n = 3L) {
  sub <- data.frame(
    id = seq_len(n), area = rep(150, n), slo1 = rep(0.05, n),
    len1 = rep(300, n), sll = rep(1.2, n),
    lat = seq(43.0, by = 0.01, length.out = n),
    lon = seq(-88.0, by = 0.01, length.out = n),
    elev = rep(200, n), elevmin = rep(180, n), elevmax = rep(220, n),
    stringsAsFactors = FALSE
  )
  ch <- data.frame(
    id = seq_len(n), subbasin = seq_len(n), areac = rep(150, n),
    strahler = rep(1L, n), len2 = rep(500, n), slo2 = rep(0.005, n),
    wid2 = rep(3, n), dep2 = rep(0.5, n),
    elevmin = rep(180, n), elevmax = rep(200, n),
    midlat = seq(43.0, by = 0.01, length.out = n),
    midlon = seq(-88.0, by = 0.01, length.out = n),
    stringsAsFactors = FALSE
  )
  lsu <- data.frame(
    id = seq_len(n), category = rep(1L, n), channel = seq_len(n),
    area = rep(150, n), slope = rep(0.05, n), len1 = rep(300, n),
    csl = rep(0.005, n), wid1 = rep(30, n), dep1 = rep(0.01, n),
    lat = seq(43.0, by = 0.01, length.out = n),
    lon = seq(-88.0, by = 0.01, length.out = n),
    elev = rep(200, n),
    stringsAsFactors = FALSE
  )
  hru <- data.frame(
    id = seq_len(n), lsu = seq_len(n),
    arsub = rep(1.0, n), arlsu = rep(1.0, n),
    landuse = rep("AGRL", n), arland = rep(1.0, n),
    soil = rep("DEFAULT", n), arso = rep(1.0, n),
    slp = rep("0", n), arslp = rep(1.0, n),
    slope = rep(0.05, n),
    lat = seq(43.0, by = 0.01, length.out = n),
    lon = seq(-88.0, by = 0.01, length.out = n),
    elev = rep(200, n),
    stringsAsFactors = FALSE
  )
  pts <- data.frame(
    id = 1L, subbasin = n, ptype = "outlet",
    xpr = -8800000, ypr = 4300000,
    lat = 43.0, lon = -88.0, elev = 180,
    stringsAsFactors = FALSE
  )
  list(subbasins = sub, channels = ch, lsus = lsu, hrus = hru,
       water = NULL, points = pts, aquifers = NULL, routing = NULL)
}

# ===========================================================================
# 1. Structural tests — synthetic data, no file I/O
# ===========================================================================

test_that(".prepare_outlet rejects non-point geometry", {
  skip_if_not_installed("sf")
  tmp <- tempfile(fileext = ".shp")
  # A simple square polygon (not a point)
  polygon_coords <- matrix(
    c(0, 0,  1, 0,  1, 1,  0, 1,  0, 0),
    ncol = 2, byrow = TRUE
  )
  poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(polygon_coords)),
    crs = 4326
  ))
  sf::st_write(poly, tmp, quiet = TRUE)
  expect_error(
    .prepare_outlet(tmp, "EPSG:32616", tempdir()),
    "POINT"
  )
  unlink(tmp)
})

test_that(".prepare_outlet accepts sf POINT input", {
  skip_if_not_installed("sf")
  pt <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(-88.0, 43.0)),
                                         crs = 4326))
  wd <- tempfile("prep_outlet_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE))
  res <- .prepare_outlet(pt, "EPSG:32616", wd)
  expect_true(file.exists(res$file))
  expect_s3_class(res$sf, "sf")
})

test_that(".prepare_outlet accepts data.frame with lon/lat columns", {
  skip_if_not_installed("sf")
  df <- data.frame(lon = -88.0, lat = 43.0)
  wd <- tempfile("prep_outlet_df_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE))
  res <- .prepare_outlet(df, "EPSG:32616", wd)
  expect_true(file.exists(res$file))
})

test_that(".prepare_outlet rejects data.frame without coordinates", {
  df <- data.frame(a = 1, b = 2)
  expect_error(
    .prepare_outlet(df, "EPSG:32616", tempdir()),
    "lon/lat"
  )
})

test_that(".build_gis_lsus has one LSU per subbasin", {
  mock <- .make_mock_gis(4L)
  lsu  <- .build_gis_lsus(mock$subbasins, mock$channels)
  expect_equal(nrow(lsu), nrow(mock$subbasins))
  expect_equal(lsu$category, rep(1L, 4L))
})

test_that(".build_gis_hrus_simple creates one HRU per LSU", {
  mock <- .make_mock_gis(5L)
  hru  <- .build_gis_hrus_simple(mock$lsus)
  expect_equal(nrow(hru), nrow(mock$lsus))
  expect_true(all(hru$arsub  == 1.0))
  expect_true(all(hru$arlsu  == 1.0))
  expect_true(all(hru$arland == 1.0))
  expect_true(all(hru$arso   == 1.0))
})

test_that(".build_gis_routing produces correct row count and categories", {
  mock <- .make_mock_gis(3L)
  # Add internal routing columns used by .build_gis_routing
  mock$channels$.linkno   <- c(1L, 2L, 3L)
  mock$channels$.dslinkno <- c(2L, 3L, -1L)   # channel 3 routes to outlet
  rout <- .build_gis_routing(mock$lsus, mock$channels)
  # Expect n LSU rows + n CH rows
  expect_equal(nrow(rout), nrow(mock$lsus) + nrow(mock$channels))
  expect_true("LSU" %in% rout$sourcecat)
  expect_true("CH"  %in% rout$sourcecat)
  # Outlet channel should route to "X"
  outlet_row <- rout[rout$sourcecat == "CH" & rout$sinkcat == "X", ]
  expect_equal(nrow(outlet_row), 1L)
})

test_that(".build_gis_routing (sourceid, sourcecat) pairs are globally unique", {
  # Regression test: LSU and CH share integer ID sequences starting at 1, so
  # sourceid alone is NOT unique — the composite (sourceid, sourcecat) must be.
  mock <- .make_mock_gis(3L)
  mock$channels$.linkno   <- c(1L, 2L, 3L)
  mock$channels$.dslinkno <- c(2L, 3L, -1L)
  rout <- .build_gis_routing(mock$lsus, mock$channels)

  # Every (sourceid, sourcecat) pair must be unique
  pairs <- paste(rout$sourceid, rout$sourcecat, sep = "_")
  expect_equal(length(pairs), length(unique(pairs)))

  # sourceid alone is NOT unique (LSU and CH overlap at 1, 2, 3)
  expect_lt(length(unique(rout$sourceid)), nrow(rout))
})

test_that(".build_gis_routing rows can be inserted into gis_routing DB table", {
  # Ensures the composite PK schema allows LSU + CH rows without constraint error
  mock <- .make_mock_gis(3L)
  mock$channels$.linkno   <- c(1L, 2L, 3L)
  mock$channels$.dslinkno <- c(2L, 3L, -1L)
  rout <- .build_gis_routing(mock$lsus, mock$channels)

  con <- new_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "DELETE FROM gis_routing")
  expect_no_error(
    swatplusEditoR:::swat_bulk_insert(con, "gis_routing", rout)
  )
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM gis_routing")$n
  expect_equal(n, nrow(rout))
})

test_that(".build_gis_routing handles all channels routing to outlet", {
  mock <- .make_mock_gis(2L)
  mock$channels$.linkno   <- c(1L, 2L)
  mock$channels$.dslinkno <- c(-1L, -1L)   # both go to outlet
  rout <- .build_gis_routing(mock$lsus, mock$channels)
  ch_rows <- rout[rout$sourcecat == "CH", ]
  expect_true(all(ch_rows$sinkcat == "X"))
})

test_that(".vectorise_watershed_raster returns sf with DN column", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  # Create a tiny synthetic watershed raster (2x2, values 1 and 2)
  wd <- tempfile("vect_ws_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE))
  r <- terra::rast(
    nrows = 4L, ncols = 4L,
    xmin = 0, xmax = 4, ymin = 0, ymax = 4,
    crs = "EPSG:32632"
  )
  # Left two columns → watershed 1, right two columns → watershed 2
  terra::values(r) <- rep(c(1L, 1L, 2L, 2L), times = 4L)
  tmp_rast <- file.path(wd, "wstream.tif")
  terra::writeRaster(r, tmp_rast, overwrite = TRUE)

  result <- .vectorise_watershed_raster(tmp_rast)

  expect_s3_class(result, "sf")
  expect_true("DN" %in% names(result))
  expect_equal(sort(unique(result$DN)), c(1L, 2L))
  expect_equal(nrow(result), 2L)
})

# ===========================================================================
# 2. Real-file tests — use bundled extdata, no TauDEM required
# ===========================================================================

test_that("extdata files are present and readable", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  dem_path     <- extdata("dem.tif")
  outlet_path  <- extdata("outlets.shp")
  channels_path <- extdata("rivs1.shp")
  subs_path    <- extdata("subs1.shp")

  expect_true(file.exists(dem_path))
  expect_true(file.exists(outlet_path))
  expect_true(file.exists(channels_path))
  expect_true(file.exists(subs_path))

  # DEM
  dem <- terra::rast(dem_path)
  expect_s4_class(dem, "SpatRaster")
  expect_gt(terra::ncell(dem), 0L)

  # Outlet point
  outlet <- sf::st_read(outlet_path, quiet = TRUE)
  expect_s3_class(outlet, "sf")
  expect_true(all(sf::st_geometry_type(outlet) %in% c("POINT", "MULTIPOINT")))
  expect_equal(nrow(outlet), 1L)

  # Channels
  channels <- sf::st_read(channels_path, quiet = TRUE)
  expect_s3_class(channels, "sf")
  expect_gt(nrow(channels), 0L)

  # Subbasins
  subs <- sf::st_read(subs_path, quiet = TRUE)
  expect_s3_class(subs, "sf")
  expect_gt(nrow(subs), 0L)
})

test_that(".build_gis_subbasins parses the bundled subs1.shp correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  subs_path <- extdata("subs1.shp")
  dem_rast  <- terra::rast(extdata("dem.tif"))

  sub_df <- .build_gis_subbasins(subs_path, dem_rast)

  expect_s3_class(sub_df, "data.frame")
  # All required SWAT+ gis_subbasins columns
  expect_true(all(c("id", "area", "slo1", "len1", "sll",
                    "lat", "lon", "elev", "elevmin", "elevmax",
                    ".wsno") %in% names(sub_df)))
  # Row count matches the source shapefile
  expect_equal(nrow(sub_df), nrow(sf::st_read(subs_path, quiet = TRUE)))
  expect_true(all(sub_df$area  > 0))
  expect_true(all(sub_df$elev  > 0))
  expect_true(all(sub_df$len1  > 0))
  # Lat/lon in WGS84 range for New Zealand
  expect_true(all(sub_df$lat > -48 & sub_df$lat < -35))
  expect_true(all(sub_df$lon > 165 & sub_df$lon < 179))
})

test_that(".build_gis_channels parses the bundled rivs1.shp correctly", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")

  subs_path <- extdata("subs1.shp")
  ch_path   <- extdata("rivs1.shp")
  dem_rast  <- terra::rast(extdata("dem.tif"))

  # Build sub_df first (needed for drainage area fallback and subbasin mapping)
  sub_df <- .build_gis_subbasins(subs_path, dem_rast)

  ch_df <- .build_gis_channels(ch_path, sub_df, dem_rast)

  expect_s3_class(ch_df, "data.frame")
  # All required SWAT+ gis_channels columns present
  expect_true(all(c("id", "subbasin", "areac", "strahler",
                    "len2", "slo2", "wid2", "dep2",
                    "elevmin", "elevmax", "midlat", "midlon",
                    ".linkno", ".dslinkno") %in% names(ch_df)))
  # One channel per feature in the source shapefile
  expect_equal(nrow(ch_df), nrow(sf::st_read(ch_path, quiet = TRUE)))
  # Strahler order read from strmOrder field
  expect_true(all(ch_df$strahler >= 1L))
  # Channel lengths from LEN field (positive)
  expect_true(all(ch_df$len2 > 0))
  # Width/depth always positive
  expect_true(all(ch_df$wid2 > 0))
  expect_true(all(ch_df$dep2 > 0))
  # Mid-point coordinates in WGS84 range for New Zealand
  expect_true(all(ch_df$midlat > -48 & ch_df$midlat < -35))
  expect_true(all(ch_df$midlon > 165 & ch_df$midlon < 179))
})

test_that(".prepare_outlet handles the bundled outlets.shp", {
  skip_if_not_installed("sf")

  outlet_path <- extdata("outlets.shp")
  wd <- tempfile("prep_outlet_real_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE))

  # The example outlet is in NZTM (EPSG:2193) — pass the same CRS
  res <- .prepare_outlet(outlet_path, "EPSG:2193", wd)
  expect_true(file.exists(res$file))
  expect_s3_class(res$sf, "sf")
  expect_equal(nrow(res$sf), 1L)
})

test_that("use_existing_watershed errors on missing directory", {
  expect_error(
    use_existing_watershed(gis_dir = "/no/such/path"),
    "gis_dir not found"
  )
})

test_that("use_existing_watershed returns a list from an empty dir", {
  wd <- tempfile("existing_wshd_")
  dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE))
  # No shapefiles → read_swatplus_gis returns an empty list
  res <- use_existing_watershed(wd, verbose = FALSE)
  expect_type(res, "list")
})

test_that("use_existing_watershed with example data", {
  wd <- system.file("extdata", package = "swatplusEditoR")
  dem_file <- list.files(wd, pattern = "dem\\.tif$", full.names = TRUE)
  # No shapefiles → read_swatplus_gis returns an empty list
  res <- use_existing_watershed(wd, dem_file = dem_file)
  expect_type(res, "list")
  expect_named(res,
    c("subbasins", "channels", "lsus", "hrus", "water",
      "points", "aquifers", "routing"))
})

# ===========================================================================
# 3. TauDEM integration tests — skipped unless TauDEM is installed
# ===========================================================================

test_that("delineate_watershed errors when traudem/TauDEM is not available", {
  skip_if(
    requireNamespace("traudem", quietly = TRUE) &&
      traudem::can_register_taudem(),
    "TauDEM is available – skipping absence test"
  )
  expect_error(
    delineate_watershed(dem    = "/tmp/fake.tif",
                        outlet = data.frame(lon = 176.2, lat = -38.1)),
    "traudem|TauDEM"
  )
})

test_that("delineate_watershed rejects channel_threshold > stream_threshold", {
  # This validation runs before any TauDEM call, so no TauDEM needed.
  skip_if(
    requireNamespace("traudem", quietly = TRUE) &&
      traudem::can_register_taudem(),
    "TauDEM available – use full integration test instead"
  )
  # When TauDEM is absent the error comes from .check_traudem(), which is fine.
  # We only test the new validation when traudem is present but executable absent.
  skip_if_not_installed("traudem")
  expect_error(
    delineate_watershed(
      dem               = "/tmp/fake.tif",
      outlet            = data.frame(lon = 176.2, lat = -38.1),
      stream_threshold  = 500L,
      channel_threshold = 1000L   # invalid: greater than stream_threshold
    ),
    "channel_threshold.*<=.*stream_threshold|stream_threshold"
  )
})

test_that("delineate_watershed runs end-to-end with bundled DEM and outlet", {
  skip_if_not_installed("traudem")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if(!traudem::can_register_taudem(), "TauDEM executables not found")

  dem_path    <- extdata("dem.tif")
  outlet_path <- extdata("outlets.shp")
  skip_if(dem_path == "" || outlet_path == "",
          "Bundled example files not found")

  outlet <- sf::st_read(outlet_path, quiet = TRUE)

  res <- delineate_watershed(
    dem              = dem_path,
    outlet           = outlet,
    stream_threshold = 200L,
    snap_distance    = 20L,
    verbose          = FALSE
  )

  expect_type(res, "list")
  expect_named(res,
    c("subbasins", "channels", "lsus", "hrus", "water",
      "points", "aquifers", "routing"))

  expect_s3_class(res$subbasins, "data.frame")
  expect_s3_class(res$channels,  "data.frame")
  expect_s3_class(res$lsus,      "data.frame")
  expect_s3_class(res$hrus,      "data.frame")
  expect_s3_class(res$routing,   "data.frame")

  expect_gt(nrow(res$subbasins), 0L)
  expect_gt(nrow(res$channels),  0L)
  expect_equal(nrow(res$lsus), nrow(res$subbasins))
  expect_equal(nrow(res$hrus), nrow(res$lsus))
  expect_null(res$water)
  expect_null(res$aquifers)
  expect_true(all(c("id", "area", "slo1", "lat", "lon") %in%
                    names(res$subbasins)))
  expect_true(all(c("id", "subbasin", "strahler", "len2", "slo2") %in%
                    names(res$channels)))
  expect_true(all(c("sourceid", "sourcecat", "sinkid", "sinkcat") %in%
                    names(res$routing)))
  expect_true("outlet" %in% res$points$ptype)
})

test_that("delineate_watershed with channel_threshold produces more channels", {
  skip_if_not_installed("traudem")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if(!traudem::can_register_taudem(), "TauDEM executables not found")

  dem_path    <- extdata("dem.tif")
  outlet_path <- extdata("outlets.shp")
  skip_if(dem_path == "" || outlet_path == "",
          "Bundled example files not found")

  outlet <- sf::st_read(outlet_path, quiet = TRUE)

  # Single-threshold run (stream only)
  res_single <- delineate_watershed(
    dem              = dem_path,
    outlet           = outlet,
    stream_threshold = 500L,
    snap_distance    = 20L,
    verbose          = TRUE
  )

  # Dual-threshold run: more detailed channel network
  res_dual <- delineate_watershed(
    dem               = dem_path,
    outlet            = outlet,
    stream_threshold  = 500L,
    channel_threshold = 100L,   # finer channel network
    snap_distance     = 20L,
    verbose           = TRUE
  )

  # Dual run should have at least as many channels as single run
  expect_gte(nrow(res_dual$channels), nrow(res_single$channels))
  # Subbasin count should be the same (same stream threshold defines subbasins)
  expect_equal(nrow(res_dual$subbasins), nrow(res_single$subbasins))
  # All channels must be assigned to a valid subbasin
  expect_true(all(res_dual$channels$subbasin %in% res_dual$subbasins$id))
})

test_that("delineate_watershed writes gis_* tables to project DB", {
  skip_if_not_installed("traudem")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if(!traudem::can_register_taudem(), "TauDEM executables not found")

  dem_path    <- extdata("dem.tif")
  outlet_path <- extdata("outlets.shp")
  skip_if(dem_path == "" || outlet_path == "",
          "Bundled example files not found")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  create_project_db(tmp_db)
  con <- swat_open_db(tmp_db)
  create_project_tables(con)
  swat_close_db(con)

  outlet <- sf::st_read(outlet_path, quiet = TRUE)

  delineate_watershed(
    dem              = dem_path,
    outlet           = outlet,
    project_db       = tmp_db,
    stream_threshold = 200L,
    snap_distance    = 20L,
    verbose          = FALSE
  )

  con   <- swat_open_db(tmp_db)
  n_sub <- swat_count(con, "gis_subbasins")
  n_ch  <- swat_count(con, "gis_channels")
  n_hru <- swat_count(con, "gis_hrus")
  swat_close_db(con)

  expect_gt(n_sub, 0L)
  expect_gt(n_ch,  0L)
  expect_gt(n_hru, 0L)
})
