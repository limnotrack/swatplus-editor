# Test GWFLOW configuration functions

create_gwflow_test_project <- function() {
  skip_if_not_installed("rQSWATPlus")

  dem         <- system.file("extdata", "ravn_dem.tif",    package = "rQSWATPlus")
  landuse     <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil        <- system.file("extdata", "ravn_soil.tif",   package = "rQSWATPlus")
  lu_lookup   <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv",   package = "rQSWATPlus")
  outlet      <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")

  rQSWATPlus::qswat_run(
    project_dir     = file.path(tempdir(), "ravn_gwflow"),
    dem_file        = dem,
    landuse_file    = landuse,
    soil_file       = soil,
    landuse_lookup  = lu_lookup,
    soil_lookup     = soil_lookup,
    outlet_file     = outlet,
    threshold       = 500,
    slope_breaks    = c(0, 5, 15, 9999),
    landuse_threshold = 5,
    soil_threshold  = 5,
    db_file         = "swat.db",
    quiet           = TRUE
  )
}

test_that("get_gwflow_status returns status", {
  project <- create_gwflow_test_project()
  on.exit(unlink(project$db_file))

  status <- get_gwflow_status(project)
  expect_true(is.list(status))
  expect_false(status$use_gwflow)
  expect_false(status$can_enable)
})

test_that("init_gwflow creates GWFLOW tables and config", {
  project <- create_gwflow_test_project()

  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)

  # Check base config
  base <- get_gwflow_base(project)
  expect_equal(base$cell_size, 200)
  expect_equal(base$row_count, 50)
  expect_equal(base$col_count, 60)
  expect_equal(base$recharge, 1)

  # Check default zone created
  zones <- get_gwflow_zones(project)
  expect_equal(nrow(zones), 1)
  expect_equal(zones$zone_id[1], 1)
  expect_equal(zones$aquifer_k[1], 10.0)

  # Check project config updated
  config <- get_project_config(project)
  expect_equal(config$use_gwflow, 1L)
})

test_that("update_gwflow_base modifies settings", {
  project <- create_gwflow_test_project()
  on.exit(unlink(project$db_file))

  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)
  update_gwflow_base(project, recharge = 2, daily_output = 1)

  base <- get_gwflow_base(project)
  expect_equal(base$recharge, 2)
  expect_equal(base$daily_output, 1)
})

test_that("update_gwflow_zones modifies zone parameters", {
  project <- create_gwflow_test_project()
  on.exit(unlink(project$db_file))

  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)
  update_gwflow_zones(project, zone_id = 1, aquifer_k = 15.0,
                      specific_yield = 0.15)

  zones <- get_gwflow_zones(project)
  expect_equal(zones$aquifer_k[1], 15.0)
  expect_equal(zones$specific_yield[1], 0.15)
})
