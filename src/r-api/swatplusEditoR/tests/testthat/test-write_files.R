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

# -------------------------------------------------------------------
# Tests for ensure_write_tables
# -------------------------------------------------------------------

test_that("ensure_write_tables creates all required tables", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  # Start with a minimal database: only project_config
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE project_config (
      id INTEGER PRIMARY KEY, project_name TEXT,
      editor_version TEXT, input_files_dir TEXT DEFAULT 'TxtInOut'
    )")
  DBI::dbExecute(con, "INSERT INTO project_config (project_name) VALUES ('test')")
  DBI::dbDisconnect(con)

  # Open via package function and run ensure_write_tables
  con <- swatplusEditoR:::open_project_db(db_path)
  swatplusEditoR:::ensure_write_tables(con)

  tables <- DBI::dbListTables(con)

  # Check critical tables that must exist
  critical <- c(
    "time_sim", "print_prt", "print_prt_object", "object_prt",
    "object_cnt", "constituents_cs",
    "codes_bsn", "parameters_bsn",
    "file_cio_classification", "file_cio",
    "weather_sta_cli", "weather_wgn_cli",
    "hru_con", "channel_con", "reservoir_con", "aquifer_con",
    "hydrology_hyd", "topography_hyd",
    "soils_sol", "nutrients_sol"
  )
  for (tbl in critical) {
    expect_true(tbl %in% tables, info = paste("Missing table:", tbl))
  }

  DBI::dbDisconnect(con)
})

test_that("ensure_write_tables populates default data", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE project_config (
      id INTEGER PRIMARY KEY, project_name TEXT,
      editor_version TEXT, input_files_dir TEXT DEFAULT 'TxtInOut'
    )")
  DBI::dbExecute(con, "INSERT INTO project_config (project_name) VALUES ('myproj')")
  DBI::dbDisconnect(con)

  con <- swatplusEditoR:::open_project_db(db_path)
  swatplusEditoR:::ensure_write_tables(con)

  # time_sim should have a default row
  ts <- DBI::dbGetQuery(con, "SELECT * FROM time_sim")
  expect_equal(nrow(ts), 1)
  expect_equal(ts$yrc_start, 1980L)
  expect_equal(ts$step, 0L)

  # print_prt should have a default row
  pp <- DBI::dbGetQuery(con, "SELECT * FROM print_prt")
  expect_equal(nrow(pp), 1)
  expect_equal(pp$nyskip, 1L)
  expect_equal(pp$crop_yld, "b")

  # print_prt_object should have 52 rows
  ppo <- DBI::dbGetQuery(con, "SELECT * FROM print_prt_object")
  expect_equal(nrow(ppo), 52)

  # object_cnt should have project name

  oc <- DBI::dbGetQuery(con, "SELECT * FROM object_cnt")
  expect_equal(nrow(oc), 1)
  expect_equal(oc$name, "myproj")

  # codes_bsn should have defaults
  cb <- DBI::dbGetQuery(con, "SELECT * FROM codes_bsn")
  expect_equal(nrow(cb), 1)
  expect_equal(cb$pet, 1L)
  expect_equal(cb$atmo_dep, "a")
  expect_equal(cb$gwflow, 0L)

  # parameters_bsn should have defaults
  pb <- DBI::dbGetQuery(con, "SELECT * FROM parameters_bsn")
  expect_equal(nrow(pb), 1)
  expect_equal(pb$lai_noevap, 3.0)
  expect_equal(pb$co2, 400.0)

  # file_cio_classification should have 31 rows
  fcc <- DBI::dbGetQuery(con, "SELECT * FROM file_cio_classification")
  expect_equal(nrow(fcc), 31)
  expect_true("simulation" %in% fcc$name)
  expect_true("basin" %in% fcc$name)
  expect_true("climate" %in% fcc$name)

  # file_cio should have entries
  fc <- DBI::dbGetQuery(con, "SELECT * FROM file_cio")
  expect_true(nrow(fc) > 100)

  DBI::dbDisconnect(con)
})

test_that("ensure_write_tables is idempotent", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE project_config (
      id INTEGER PRIMARY KEY, project_name TEXT,
      editor_version TEXT, input_files_dir TEXT DEFAULT 'TxtInOut'
    )")
  DBI::dbExecute(con, "INSERT INTO project_config (project_name) VALUES ('test')")
  DBI::dbDisconnect(con)

  con <- swatplusEditoR:::open_project_db(db_path)

  # Run twice - should not duplicate data
  swatplusEditoR:::ensure_write_tables(con)
  swatplusEditoR:::ensure_write_tables(con)

  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM time_sim")$n, 1L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM print_prt")$n, 1L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM print_prt_object")$n, 52L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM object_cnt")$n, 1L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM codes_bsn")$n, 1L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM parameters_bsn")$n, 1L)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM file_cio_classification")$n, 31L)

  DBI::dbDisconnect(con)
})

test_that("ensure_write_tables does not overwrite existing data", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE project_config (
      id INTEGER PRIMARY KEY, project_name TEXT,
      editor_version TEXT, input_files_dir TEXT DEFAULT 'TxtInOut'
    )")
  DBI::dbExecute(con, "INSERT INTO project_config (project_name) VALUES ('test')")
  # Pre-create time_sim with custom values
  DBI::dbExecute(con, "
    CREATE TABLE time_sim (
      id INTEGER PRIMARY KEY,
      day_start INTEGER, yrc_start INTEGER,
      day_end INTEGER, yrc_end INTEGER, step INTEGER
    )")
  DBI::dbExecute(con, "INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
                        VALUES (1, 2000, 365, 2020, 0)")
  DBI::dbDisconnect(con)

  con <- swatplusEditoR:::open_project_db(db_path)
  swatplusEditoR:::ensure_write_tables(con)

  # Custom time_sim should be preserved
  ts <- DBI::dbGetQuery(con, "SELECT * FROM time_sim")
  expect_equal(nrow(ts), 1)
  expect_equal(ts$yrc_start, 2000L)
  expect_equal(ts$yrc_end, 2020L)

  DBI::dbDisconnect(con)
})

test_that("write_config_files works on minimal db with ensure_write_tables", {
  # Create a bare-minimum project (no tables except project_config + weather)
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()
  output_dir <- tempfile("txtinout_")
  on.exit({
    unlink(db_path)
    unlink(output_dir, recursive = TRUE)
  })

  project <- list(
    project_dir = project_dir,
    db_file = db_path,
    hru_data = NULL,
    basin_data = NULL
  )

  project <- create_project_db(project, db_path, overwrite = TRUE)
  set_simulation_time(project, day_start = 1, yrc_start = 2005,
                      day_end = 365, yrc_end = 2015)

  # Write config files - should auto-create all missing tables
  write_config_files(project, output_dir = output_dir)

  # Check that critical output files exist
  expect_true(file.exists(file.path(output_dir, "time.sim")))
  expect_true(file.exists(file.path(output_dir, "file.cio")))
  expect_true(file.exists(file.path(output_dir, "codes.bsn")))
  expect_true(file.exists(file.path(output_dir, "parameters.bsn")))
  expect_true(file.exists(file.path(output_dir, "print.prt")))
  expect_true(file.exists(file.path(output_dir, "object.cnt")))

  # Verify file.cio references all sections (not just static fallback)
  cio_lines <- readLines(file.path(output_dir, "file.cio"))
  cio_content <- paste(cio_lines, collapse = " ")
  expect_true(grepl("simulation", cio_content))
  expect_true(grepl("basin", cio_content))
  expect_true(grepl("climate", cio_content))
  expect_true(grepl("time\\.sim", cio_content))
  expect_true(grepl("codes\\.bsn", cio_content))
})
