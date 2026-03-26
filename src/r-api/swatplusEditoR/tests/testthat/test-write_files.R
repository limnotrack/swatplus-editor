# Test file writing functions

create_write_test_project <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()

  project <- list(
    project_dir = project_dir,
    db_file = db_path,
    hru_data = NULL,
    basin_data = NULL
  )

  project <- create_project_db(project, db_path, overwrite = TRUE)

  # Add simulation time
  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2010)

  # Add weather stations
  stations <- data.frame(
    name = c("sta1", "sta2"),
    lat = c(-38.1, -38.2),
    lon = c(176.3, 176.4),
    pcp = c("pcp1.cli", "pcp2.cli"),
    tmp = c("tmp1.cli", "tmp2.cli"),
    slr = c("slr1.cli", "slr2.cli"),
    hmd = c("hmd1.cli", "hmd2.cli"),
    wnd = c("wnd1.cli", "wnd2.cli"),
    pet = c("pet1.cli", "pet2.cli"),
    atmo_dep = c("atmo1.cli", "atmo2.cli"),
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, stations)

  project
}

test_that("write_config_files writes basic files", {
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  write_config_files(project, output_dir = output_dir)

  # Check files were created
  expect_true(file.exists(file.path(output_dir, "time.sim")))
  expect_true(file.exists(file.path(output_dir, "weather-sta.cli")))
  expect_true(file.exists(file.path(output_dir, "file.cio")))

  # Check time.sim content
  time_lines <- readLines(file.path(output_dir, "time.sim"))
  expect_true(length(time_lines) >= 2)
  expect_true(grepl("2000", time_lines[2]))
  expect_true(grepl("2010", time_lines[2]))

  # Check weather station file content
  weather_lines <- readLines(file.path(output_dir, "weather-sta.cli"))
  expect_true(length(weather_lines) >= 3)
  expect_true(grepl("sta1", weather_lines[3]))
})

test_that("write_config_files updates project config", {
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  write_config_files(project, output_dir = output_dir)

  config <- get_project_config(project)
  expect_false(is.null(config$input_files_last_written))
})
