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

  # Check time.sim content - now uses SWAT+ format with meta line
  time_lines <- readLines(file.path(output_dir, "time.sim"))
  expect_true(length(time_lines) >= 2)
  # Meta line is first, then headers, then data
  time_content <- paste(time_lines, collapse = " ")
  expect_true(grepl("2000", time_content))
  expect_true(grepl("2010", time_content))

  # Check weather station file content
  weather_lines <- readLines(file.path(output_dir, "weather-sta.cli"))
  expect_true(length(weather_lines) >= 3)
  weather_content <- paste(weather_lines, collapse = " ")
  expect_true(grepl("sta1", weather_content))
  expect_true(grepl("sta2", weather_content))
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

test_that("write_config_files writes proper SWAT+ formatting", {
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  write_config_files(project, output_dir = output_dir)

  # Check meta line in time.sim
  time_lines <- readLines(file.path(output_dir, "time.sim"))
  expect_true(grepl("^time\\.sim: written by SWAT\\+ editor", time_lines[1]))

  # Check weather-sta.cli has proper column headers
  weather_lines <- readLines(file.path(output_dir, "weather-sta.cli"))
  expect_true(grepl("name", weather_lines[2]))
  expect_true(grepl("wgn", weather_lines[2]))
  expect_true(grepl("pcp", weather_lines[2]))

  # Check file.cio meta line
  cio_lines <- readLines(file.path(output_dir, "file.cio"))
  expect_true(grepl("^file\\.cio: written by SWAT\\+ editor", cio_lines[1]))
})

test_that("write_config_files defaults to native R writing", {
  # Verify that without editor_exe or api_url, it writes natively
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  # Call without exe or api - should write directly
  expect_message(
    write_config_files(project, output_dir = output_dir),
    "Writing SWAT\\+ input files"
  )
  expect_true(file.exists(file.path(output_dir, "time.sim")))
  expect_true(file.exists(file.path(output_dir, "file.cio")))
})

test_that("swat_write_table writes generic table correctly", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "CREATE TABLE test_tbl (
    id INTEGER PRIMARY KEY, name TEXT, val REAL, cnt INTEGER)")
  DBI::dbExecute(con, "INSERT INTO test_tbl VALUES (1, 'alpha', 1.5, 10)")
  DBI::dbExecute(con, "INSERT INTO test_tbl VALUES (2, 'beta', 2.5, 20)")

  output_dir <- tempfile("out_")
  dir.create(output_dir)
  fp <- file.path(output_dir, "test.tbl")

  swatplusEditoR:::swat_write_table(con, "test_tbl", fp,
                   version = "3.2", swat_version = "60",
                   ignore_id = TRUE)

  DBI::dbDisconnect(con)

  lines <- readLines(fp)
  # Meta line
  expect_true(grepl("test.tbl: written by SWAT\\+ editor", lines[1]))
  # Data should contain alpha and beta
  content <- paste(lines, collapse = " ")
  expect_true(grepl("alpha", content))
  expect_true(grepl("beta", content))

  unlink(output_dir, recursive = TRUE)
})
