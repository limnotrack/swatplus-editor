# Tests for run_swatplus() and swatplus_exe()

# Helper: build a full project with rQSWATPlus + ERA5 weather
setup_swat_sim <- function() {
  skip_if_not_installed("rQSWATPlus")

  dem         <- system.file("extdata", "ravn_dem.tif",     package = "rQSWATPlus")
  landuse     <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil        <- system.file("extdata", "ravn_soil.tif",    package = "rQSWATPlus")
  lu_lookup   <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv",    package = "rQSWATPlus")
  outlet      <- system.file("extdata", "ravn_outlet.shp",  package = "rQSWATPlus")

  project <- rQSWATPlus::qswat_run(
    project_dir      = file.path(tempdir(), "ravn_sim"),
    dem_file         = dem,
    landuse_file     = landuse,
    soil_file        = soil,
    landuse_lookup   = lu_lookup,
    soil_lookup      = soil_lookup,
    outlet_file      = outlet,
    threshold        = 500,
    slope_breaks     = c(0, 5, 15, 9999),
    landuse_threshold = 5,
    soil_threshold   = 5,
    db_file          = "swat.db",
    quiet            = TRUE
  )

  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2000)

  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")

  # Register ERA5 as the weather station
  stations <- data.frame(
    name     = "IDera5",
    lat      = -38.1,
    lon      = 176.3,
    pcp      = "pcp.cli",
    tmp      = "tmp.cli",
    slr      = "slr.cli",
    hmd      = "hmd.cli",
    wnd      = "wnd.cli",
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, stations)

  list(project = project, era5_dir = era5_dir,
       output_dir = tempfile("txtinout_"))
}

test_that("swatplus_exe returns NULL or a valid path", {
  exe <- swatplus_exe()
  expect_true(is.null(exe) || (is.character(exe) && file.exists(exe)))
})

test_that("write_config_files writes SWAT+ files from rQSWATPlus project", {
  skip_if_not_installed("rQSWATPlus")

  sim        <- setup_swat_sim()
  project    <- sim$project
  output_dir <- sim$output_dir
  era5_dir   <- sim$era5_dir
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  write_config_files(project, output_dir = output_dir,
                     weather_dir = era5_dir)

  expect_true(file.exists(file.path(output_dir, "file.cio")))
  expect_true(file.exists(file.path(output_dir, "time.sim")))
  expect_true(file.exists(file.path(output_dir, "weather-sta.cli")))
})

test_that("run_swatplus executes SWAT+ simulation", {
  skip_if_not_installed("rQSWATPlus")
  exe <- swatplus_exe()
  skip_if(is.null(exe), "No SWAT+ executable available on this platform")

  sim        <- setup_swat_sim()
  project    <- sim$project
  output_dir <- sim$output_dir
  era5_dir   <- sim$era5_dir
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  }, add = TRUE)

  write_config_files(project, output_dir = output_dir,
                     weather_dir = era5_dir)

  result <- run_swatplus(swat_exe = exe, working_dir = output_dir,
                         verbose = FALSE)

  expect_true(result$success)
  expect_equal(result$status, 0L)
})

test_that("run_swatplus errors clearly when no executable supplied", {
  skip_on_os("windows")
  exe <- swatplus_exe()
  skip_if(!is.null(exe), "SWAT+ executable present; skipping error test")

  output_dir <- tempfile("txtinout_")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE))

  expect_error(
    run_swatplus(swat_exe = NULL, working_dir = output_dir),
    regexp = "No SWAT\\+ executable",
    ignore.case = TRUE
  )
})


