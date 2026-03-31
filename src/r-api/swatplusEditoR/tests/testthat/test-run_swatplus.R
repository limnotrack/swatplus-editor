# Tests for run_swatplus() and swatplus_exe()

setup_swat_sim <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempfile("txtinout_")
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)

  project <- list(
    project_dir = project_dir,
    db_file = db_path
  )
  project <- create_project_db(project, db_path, overwrite = TRUE)
  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2000)

  # Add a single ERA5-based weather station
  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")
  stations <- data.frame(
    name      = "IDera5",
    lat       = -38.1,
    lon       = 176.3,
    pcp       = "pcp.cli",
    tmp       = "tmp.cli",
    slr       = "slr.cli",
    hmd       = "hmd.cli",
    wnd       = "wnd.cli",
    pet       = "null",
    atmo_dep  = "null",
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, stations)

  list(project = project, output_dir = project_dir, era5_dir = era5_dir)
}

test_that("swatplus_exe returns NULL or a valid path", {
  exe <- swatplus_exe()
  # On non-Windows or source installs without the binary, NULL is expected
  expect_true(is.null(exe) || (is.character(exe) && file.exists(exe)))
})

test_that("write_config_files writes files for ERA5 weather", {
  sim <- setup_swat_sim()
  project    <- sim$project
  output_dir <- sim$output_dir
  era5_dir   <- sim$era5_dir
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  write_config_files(project, output_dir = output_dir,
                     weather_dir = era5_dir)

  expect_true(file.exists(file.path(output_dir, "file.cio")))
  expect_true(file.exists(file.path(output_dir, "time.sim")))
  expect_true(file.exists(file.path(output_dir, "weather-sta.cli")))
})

test_that("run_swatplus skips gracefully when no executable available", {
  skip_on_os("windows")  # only skip on non-Windows where binary is absent
  exe <- swatplus_exe()
  skip_if(!is.null(exe), "SWAT+ executable found; skipping graceful-skip test")

  sim <- setup_swat_sim()
  output_dir <- sim$output_dir
  on.exit({
    unlink(sim$project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  expect_error(
    run_swatplus(swat_exe = NULL, working_dir = output_dir),
    regexp = "No SWAT\\+ executable",
    ignore.case = TRUE
  )
})

