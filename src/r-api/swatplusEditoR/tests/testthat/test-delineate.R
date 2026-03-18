## Tests for delineate.R
## ─────────────────────────────────────────────────────────────────────────────
## Most tests are data-structure checks that do NOT require TauDEM.
## Tests that call delineate_watershed() are skipped unless TauDEM is present.

library(swatplusEditoR)

# ===========================================================================
# Helpers shared across tests
# ===========================================================================

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
# Structural tests (no TauDEM required)
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
  mock$channels$.linkno  <- c(1L, 2L, 3L)
  mock$channels$.dslinkno <- c(2L, 3L, -1L)   # 3 → outlet
  rout <- .build_gis_routing(mock$lsus, mock$channels)
  # Expect n LSU rows + n CH rows
  expect_equal(nrow(rout), nrow(mock$lsus) + nrow(mock$channels))
  expect_true("LSU" %in% rout$sourcecat)
  expect_true("CH"  %in% rout$sourcecat)
  # Outlet channel should route to "X"
  outlet_row <- rout[rout$sourcecat == "CH" & rout$sinkcat == "X", ]
  expect_equal(nrow(outlet_row), 1L)
})

test_that(".build_gis_routing handles all channels routing to outlet", {
  mock <- .make_mock_gis(2L)
  mock$channels$.linkno  <- c(1L, 2L)
  mock$channels$.dslinkno <- c(-1L, -1L)   # both go to outlet
  rout <- .build_gis_routing(mock$lsus, mock$channels)
  ch_rows <- rout[rout$sourcecat == "CH", ]
  expect_true(all(ch_rows$sinkcat == "X"))
})

# ===========================================================================
# use_existing_watershed() – structural tests (no TauDEM / no real shapefiles)
# ===========================================================================

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

# ===========================================================================
# delineate_watershed() – skip unless TauDEM is available
# ===========================================================================

test_that("delineate_watershed errors when traudem is not installed", {
  # We can only test this if traudem IS installed but TauDEM is absent,
  # or if traudem is absent.  Just verify the error condition is checked.
  skip_if(
    requireNamespace("traudem", quietly = TRUE) &&
      traudem::can_register_taudem(),
    "TauDEM is available – skipping absence test"
  )
  expect_error(
    delineate_watershed(dem = "/tmp/fake.tif",
                        outlet = data.frame(lon = -88, lat = 43)),
    "traudem|TauDEM"
  )
})

test_that("delineate_watershed runs end-to-end with traudem test DEM", {
  skip_if_not_installed("traudem")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if(!traudem::can_register_taudem(),
              "TauDEM executables not found")

  dem_path <- system.file("extdata/dem_example.tif", package = "swatplusEditoR")
  outlet_path <- system.file("extdata/hydro_id_outlet.shp", package = "swatplusEditoR")
  skip_if(dem_path == "", " test DEM not found")

  # The traudem test DEM is small; use a low threshold so streams are found
  dem_r  <- terra::rast(dem_path)
  outlet <- sf::st_read(outlet_path, quiet = TRUE)

  res <- delineate_watershed(
    dem              = dem_path,
    outlet           = outlet,
    stream_threshold = 200,
    snap_distance    = 20,
    verbose          = FALSE
  )

  expect_type(res, "list")
  expect_named(res,
    c("subbasins", "channels", "lsus", "hrus", "water", "points",
      "aquifers", "routing"))

  expect_s3_class(res$subbasins, "data.frame")
  expect_s3_class(res$channels,  "data.frame")
  expect_s3_class(res$lsus,      "data.frame")
  expect_s3_class(res$hrus,      "data.frame")
  expect_s3_class(res$routing,   "data.frame")

  expect_gt(nrow(res$subbasins), 0L)
  expect_gt(nrow(res$channels),  0L)
  expect_equal(nrow(res$lsus),   nrow(res$subbasins))
  expect_equal(nrow(res$hrus),   nrow(res$lsus))
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

test_that("delineate_watershed writes to project DB", {
  skip_if_not_installed("traudem")
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if(!traudem::can_register_taudem(),
          "TauDEM executables not found")
  
  dem_path <- system.file("test-data", "DEM.tif", package = "traudem")
  skip_if(dem_path == "", "traudem test DEM not found")

  tmp_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp_db))

  create_project_db(tmp_db)
  con <- swat_open_db(tmp_db)
  create_project_tables(con)
  swat_close_db(con)

  dem_r  <- terra::rast(dem_path)
  terra::ext(dem_r) <- terra::ext(1748000, 1748200, 5427000, 5427120)
  terra::crs(dem_r) <- "EPSG:2193"
  ext_r  <- terra::ext(dem_r)
  dem_tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(dem_r, dem_tmp, overwrite = TRUE)
  outlet <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_point(c((ext_r$xmin + ext_r$xmax) / 2,
                     ext_r$ymin + (ext_r$ymax - ext_r$ymin) * 0.1)),
      crs = sf::st_crs(dem_r)
    )
  )
  
  library(tmap)
  tm_shape(outlet) + tm_dots() + tm_shape(dem_r) + tm_raster() + tm_layout(frame = FALSE)

  delineate_watershed(
    dem              = dem_tmp,
    outlet           = outlet,
    project_db       = tmp_db,
    stream_threshold = 200,
    snap_distance    = 20,
    verbose          = T
  )

  con <- swat_open_db(tmp_db)
  n_sub <- swat_count(con, "gis_subbasins")
  n_ch  <- swat_count(con, "gis_channels")
  n_hru <- swat_count(con, "gis_hrus")
  swat_close_db(con)

  expect_gt(n_sub, 0L)
  expect_gt(n_ch,  0L)
  expect_gt(n_hru, 0L)
})
