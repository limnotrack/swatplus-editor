# Test file writing functions
library(testthat)
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

# -------------------------------------------------------------------
# Test for sequential ID bug fix (matching Python hru.py / reservoir.py fix)
# -------------------------------------------------------------------

test_that("swat_write_table uses sequential row numbers for id column", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  # Create table with non-sequential IDs (simulating gaps from deletions)
  DBI::dbExecute(con, "CREATE TABLE test_gap (
    id INTEGER PRIMARY KEY, name TEXT, val REAL)")
  DBI::dbExecute(con, "INSERT INTO test_gap VALUES (5, 'alpha', 1.5)")
  DBI::dbExecute(con, "INSERT INTO test_gap VALUES (10, 'beta', 2.5)")
  DBI::dbExecute(con, "INSERT INTO test_gap VALUES (15, 'gamma', 3.5)")

  output_dir <- tempfile("out_")
  dir.create(output_dir)
  fp <- file.path(output_dir, "test_gap.tbl")

  # Write WITH id column (ignore_id = FALSE)
  swatplusEditoR:::swat_write_table(con, "test_gap", fp,
                                    version = "3.2", swat_version = "60",
                                    ignore_id = FALSE)

  DBI::dbDisconnect(con)

  lines <- readLines(fp)
  # Line 1 = meta, line 2 = header, lines 3-5 = data
  expect_true(length(lines) >= 5)

  # The id values should be sequential 1, 2, 3 - not the DB ids 5, 10, 15
  # Extract the first number from each data line
  data_lines <- lines[3:5]
  ids <- as.integer(trimws(substring(data_lines, 1, 8)))
  expect_equal(ids, c(1L, 2L, 3L))

  # Also verify names are there
  content <- paste(lines, collapse = " ")
  expect_true(grepl("alpha", content))
  expect_true(grepl("beta", content))
  expect_true(grepl("gamma", content))

  unlink(output_dir, recursive = TRUE)
})

# -------------------------------------------------------------------
# Comprehensive test for writing all necessary simulation files
# -------------------------------------------------------------------

test_that("write_config_files produces all necessary files for SWAT+ simulation", {
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_full_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  # Set weather dir to ERA5 data from package
  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")
  skip_if(nchar(era5_dir) == 0, "ERA5 data not installed")

  set_weather_dir(project, era5_dir)

  # Write all config files with weather data
  write_config_files(project, output_dir = output_dir, weather_dir = era5_dir)

  # === Simulation section files ===
  expect_true(file.exists(file.path(output_dir, "time.sim")),
              info = "time.sim is required for SWAT+ simulation")
  expect_true(file.exists(file.path(output_dir, "print.prt")),
              info = "print.prt is required for SWAT+ simulation")
  expect_true(file.exists(file.path(output_dir, "object.cnt")),
              info = "object.cnt is required for SWAT+ simulation")

  # === Basin section files ===
  expect_true(file.exists(file.path(output_dir, "codes.bsn")),
              info = "codes.bsn is required for SWAT+ simulation")
  expect_true(file.exists(file.path(output_dir, "parameters.bsn")),
              info = "parameters.bsn is required for SWAT+ simulation")

  # === Climate section files ===
  expect_true(file.exists(file.path(output_dir, "weather-sta.cli")),
              info = "weather-sta.cli is required for SWAT+ simulation")

  # === Master config file ===
  expect_true(file.exists(file.path(output_dir, "file.cio")),
              info = "file.cio is the master config, required for SWAT+")

  # === Weather data files (copied from ERA5 dir) ===
  expect_true(file.exists(file.path(output_dir, "pcp.cli")),
              info = "pcp.cli weather data should be copied")
  expect_true(file.exists(file.path(output_dir, "tmp.cli")),
              info = "tmp.cli weather data should be copied")
  expect_true(file.exists(file.path(output_dir, "slr.cli")),
              info = "slr.cli weather data should be copied")
  expect_true(file.exists(file.path(output_dir, "hmd.cli")),
              info = "hmd.cli weather data should be copied")
  expect_true(file.exists(file.path(output_dir, "wnd.cli")),
              info = "wnd.cli weather data should be copied")

  # === Verify file contents ===
  # time.sim should contain correct simulation period
  time_content <- paste(readLines(file.path(output_dir, "time.sim")),
                        collapse = " ")
  expect_true(grepl("2000", time_content))
  expect_true(grepl("2010", time_content))

  # weather-sta.cli should reference our stations
  weather_content <- paste(readLines(file.path(output_dir, "weather-sta.cli")),
                           collapse = " ")
  expect_true(grepl("sta1", weather_content))
  expect_true(grepl("sta2", weather_content))

  # file.cio should reference simulation and basin sections
  cio_content <- paste(readLines(file.path(output_dir, "file.cio")),
                       collapse = " ")
  expect_true(grepl("simulation", cio_content))
  expect_true(grepl("basin", cio_content))
  expect_true(grepl("climate", cio_content))
  expect_true(grepl("connect", cio_content))
  expect_true(grepl("time\\.sim", cio_content))
  expect_true(grepl("codes\\.bsn", cio_content))
  expect_true(grepl("weather-sta\\.cli", cio_content))

  # codes.bsn should contain properly formatted default values
  codes_lines <- readLines(file.path(output_dir, "codes.bsn"))
  expect_true(grepl("^codes\\.bsn: written by SWAT\\+ editor", codes_lines[1]))
  expect_true(length(codes_lines) >= 3, info = "codes.bsn should have meta + header + data")

  # parameters.bsn should contain properly formatted default values
  params_lines <- readLines(file.path(output_dir, "parameters.bsn"))
  expect_true(grepl("^parameters\\.bsn: written by SWAT\\+ editor", params_lines[1]))
  expect_true(length(params_lines) >= 3, info = "parameters.bsn should have meta + header + data")

  # print.prt should have proper structure
  prt_lines <- readLines(file.path(output_dir, "print.prt"))
  expect_true(grepl("^print\\.prt: written by SWAT\\+ editor", prt_lines[1]))

  # object.cnt should have object counts
  cnt_lines <- readLines(file.path(output_dir, "object.cnt"))
  expect_true(grepl("^object\\.cnt: written by SWAT\\+ editor", cnt_lines[1]))

  # Copied weather files should have correct content
  pcp_content <- readLines(file.path(output_dir, "pcp.cli"))
  expect_true(grepl("pcp\\.cli", pcp_content[1]))
  expect_true(grepl("IDera5\\.pcp", pcp_content[3]))
})

test_that("write_config_files with weather_dir copies ERA5 climate files", {
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_weather_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")
  skip_if(nchar(era5_dir) == 0, "ERA5 data not installed")

  write_config_files(project, output_dir = output_dir, weather_dir = era5_dir)

  # All ERA5 files should be copied to output
  for (f in c("pcp.cli", "tmp.cli", "slr.cli", "hmd.cli", "wnd.cli")) {
    expect_true(file.exists(file.path(output_dir, f)),
                info = paste("ERA5 file not copied:", f))
    # Verify it's a faithful copy
    src_content <- readLines(file.path(era5_dir, f))
    dst_content <- readLines(file.path(output_dir, f))
    expect_equal(src_content, dst_content,
                 info = paste("Content mismatch for:", f))
  }
})

# -------------------------------------------------------------------
# Tests for file.cio completeness and naming consistency
# -------------------------------------------------------------------

test_that("file.cio references use consistent file names matching write specs", {
  # Verify that file names in file_cio table match what write_config_files generates
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
  swatplusEditoR:::ensure_write_tables(con)

  # Verify pesticide file name matches (.pst not .pes)
  pst_file <- DBI::dbGetQuery(con,
    "SELECT f.file_name FROM file_cio f
     JOIN file_cio_classification c ON f.classification_id = c.id
     WHERE c.name = 'hru_parm_db' AND f.order_in_class = 4")
  expect_equal(pst_file$file_name, "pesticide.pst",
               info = "pesticide file in file_cio should be .pst not .pes")

  # Verify cons_prac file name matches
  cp_file <- DBI::dbGetQuery(con,
    "SELECT f.file_name FROM file_cio f
     JOIN file_cio_classification c ON f.classification_id = c.id
     WHERE c.name = 'lum' AND f.order_in_class = 4")
  expect_equal(cp_file$file_name, "cons_prac.lum",
               info = "conservation practice file should be cons_prac.lum")

  DBI::dbDisconnect(con)
})

test_that("file.cio has all 31 classification sections", {
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
  swatplusEditoR:::ensure_write_tables(con)

  fcc <- DBI::dbGetQuery(con, "SELECT * FROM file_cio_classification ORDER BY id")
  expect_equal(nrow(fcc), 31)

  # Verify all expected sections exist
  expected_sections <- c(
    "simulation", "basin", "climate", "connect", "channel",
    "reservoir", "routing_unit", "hru", "exco", "recall",
    "dr", "aquifer", "herd", "water_rights", "link",
    "hydrology", "structural", "hru_parm_db", "ops", "lum",
    "chg", "init", "soils", "decision_table", "regions",
    "pcp_path", "tmp_path", "slr_path", "hmd_path", "wnd_path",
    "out_path")

  for (section in expected_sections) {
    expect_true(section %in% fcc$name,
                info = paste("Missing file_cio section:", section))
  }

  DBI::dbDisconnect(con)
})

test_that("file.cio section file counts match expected", {
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
  swatplusEditoR:::ensure_write_tables(con)

  # Check file counts per section
  counts <- DBI::dbGetQuery(con,
    "SELECT c.name, COUNT(f.id) as cnt
     FROM file_cio_classification c
     LEFT JOIN file_cio f ON f.classification_id = c.id
     GROUP BY c.name ORDER BY c.id")

  expected_counts <- list(
    simulation = 5, basin = 2, climate = 9, connect = 13,
    channel = 8, reservoir = 8, routing_unit = 4, hru = 2,
    exco = 6, recall = 1, dr = 6, aquifer = 2,
    herd = 3, water_rights = 3, link = 2, hydrology = 3,
    structural = 5, hru_parm_db = 10, ops = 6, lum = 5,
    chg = 9, init = 11, soils = 3, decision_table = 4,
    regions = 17
  )

  for (section in names(expected_counts)) {
    actual <- counts$cnt[counts$name == section]
    expect_equal(actual, expected_counts[[section]],
                 info = paste("Wrong file count for section:", section,
                              "- expected", expected_counts[[section]],
                              "got", actual))
  }

  DBI::dbDisconnect(con)
})

# -------------------------------------------------------------------
# Tests for new tables created by ensure_write_tables
# -------------------------------------------------------------------

test_that("ensure_write_tables creates all missing section tables", {
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
  swatplusEditoR:::ensure_write_tables(con)

  tables <- DBI::dbListTables(con)

  # Check that new tables from recent additions exist
  new_tables <- c(
    # Water rights
    "element_wro", "define_wro",
    # Link
    "chan_aqu_lin",
    # Channel
    "temperature_cha",
    # Init (additional)
    "hmet_hru_ini", "hmet_water_ini", "salt_hru_ini", "salt_water_ini",
    # DR (should already exist)
    "dr_pest_del", "dr_path_del", "dr_hmet_del", "dr_salt_del"
  )

  for (tbl in new_tables) {
    expect_true(tbl %in% tables,
                info = paste("Missing table:", tbl))
  }

  DBI::dbDisconnect(con)
})

# -------------------------------------------------------------------
# Full integration test: write_config_files generates all file.cio files
# -------------------------------------------------------------------

test_that("write_config_files generates files for all file.cio sections", {
  # Create a project with data in every section to verify full file generation
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()
  output_dir <- tempfile("txtinout_full_cio_")
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
  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2010)

  # Add weather stations
  stations <- data.frame(
    name = c("sta1"), lat = c(-38.1), lon = c(176.3),
    pcp = c("pcp1.cli"), tmp = c("tmp1.cli"),
    slr = c("slr1.cli"), hmd = c("hmd1.cli"),
    wnd = c("wnd1.cli"),
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, stations)

  # Open DB directly and insert minimal data into key tables
  # First ensure all tables exist
  con <- swatplusEditoR:::open_project_db(db_path)
  swatplusEditoR:::ensure_write_tables(con)

  # Insert data into connect tables for connect section
  DBI::dbExecute(con, "INSERT INTO hru_con (name, lat, lon, elev, area, wst_id, obj_id)
                        VALUES ('hru1', -38.1, 176.3, 100, 10.0, 1, 1)")
  DBI::dbExecute(con, "INSERT INTO channel_con (name, lat, lon, elev, area, wst_id, obj_id)
                        VALUES ('cha1', -38.1, 176.3, 100, 1.0, 1, 1)")
  DBI::dbExecute(con, "INSERT INTO aquifer_con (name, lat, lon, elev, area, wst_id, obj_id)
                        VALUES ('aqu1', -38.1, 176.3, 100, 10.0, 1, 1)")

  # Insert data into channel tables
  DBI::dbExecute(con, "INSERT INTO initial_cha (name) VALUES ('init_cha1')")
  DBI::dbExecute(con, "INSERT INTO hydrology_cha (name, wd, dp, slp, len, mann, k)
                        VALUES ('hyd_cha1', 5.0, 1.0, 0.01, 100, 0.05, 0.01)")
  DBI::dbExecute(con, "INSERT INTO channel_cha (name, init_id, hyd_id)
                        VALUES ('cha1', 1, 1)")

  # Insert data into hydrology tables
  DBI::dbExecute(con, "INSERT INTO hydrology_hyd (name) VALUES ('hyd1')")
  DBI::dbExecute(con, "INSERT INTO topography_hyd (name, slp, slp_len)
                        VALUES ('topo1', 0.05, 50.0)")

  # Insert data into aquifer tables
  DBI::dbExecute(con, "INSERT INTO initial_aqu (name) VALUES ('init_aqu1')")
  DBI::dbExecute(con, "INSERT INTO aquifer_aqu (name, init_id) VALUES ('aqu1', 1)")

  # Insert data into HRU tables
  DBI::dbExecute(con, "INSERT INTO hru_data_hru (name) VALUES ('hru1')")

  # Insert data into soils tables
  DBI::dbExecute(con, "INSERT INTO soils_sol (name) VALUES ('soil1')")
  DBI::dbExecute(con, "INSERT INTO nutrients_sol (name) VALUES ('nut1')")

  # Insert into LUM tables
  DBI::dbExecute(con, "INSERT INTO landuse_lum (name) VALUES ('agrl')")

  DBI::dbDisconnect(con)

  # Write all config files
  write_config_files(project, output_dir = output_dir)

  # Verify all expected files exist
  # -- Simulation --
  expect_true(file.exists(file.path(output_dir, "time.sim")))
  expect_true(file.exists(file.path(output_dir, "print.prt")))
  expect_true(file.exists(file.path(output_dir, "object.cnt")))

  # -- Basin --
  expect_true(file.exists(file.path(output_dir, "codes.bsn")))
  expect_true(file.exists(file.path(output_dir, "parameters.bsn")))

  # -- Climate --
  expect_true(file.exists(file.path(output_dir, "weather-sta.cli")))

  # -- Connect (written because we have data) --
  expect_true(file.exists(file.path(output_dir, "hru.con")) ||
              file.exists(file.path(output_dir, "hru-lte.con")),
              info = "At least one connect file should exist")

  # -- Channel (written because we have data) --
  expect_true(file.exists(file.path(output_dir, "initial.cha")),
              info = "initial.cha should exist with channel data")
  expect_true(file.exists(file.path(output_dir, "hydrology.cha")),
              info = "hydrology.cha should exist with channel data")

  # -- Hydrology --
  expect_true(file.exists(file.path(output_dir, "hydrology.hyd")),
              info = "hydrology.hyd should exist with hydro data")
  expect_true(file.exists(file.path(output_dir, "topography.hyd")),
              info = "topography.hyd should exist with topo data")

  # -- Aquifer --
  expect_true(file.exists(file.path(output_dir, "initial.aqu")),
              info = "initial.aqu should exist with aquifer data")
  expect_true(file.exists(file.path(output_dir, "aquifer.aqu")),
              info = "aquifer.aqu should exist with aquifer data")

  # -- HRU --
  expect_true(file.exists(file.path(output_dir, "hru-data.hru")),
              info = "hru-data.hru should exist with HRU data")

  # -- Soils --
  expect_true(file.exists(file.path(output_dir, "soils.sol")),
              info = "soils.sol should exist with soils data")
  expect_true(file.exists(file.path(output_dir, "nutrients.sol")),
              info = "nutrients.sol should exist with soils data")

  # -- LUM --
  expect_true(file.exists(file.path(output_dir, "landuse.lum")),
              info = "landuse.lum should exist with LUM data")

  # -- file.cio (always) --
  expect_true(file.exists(file.path(output_dir, "file.cio")))

  # Verify file.cio includes all key sections with proper references
  cio_content <- paste(readLines(file.path(output_dir, "file.cio")),
                       collapse = "\n")
  # Sections that should appear in file.cio
  for (section in c("simulation", "basin", "climate", "connect",
                     "channel", "hydrology", "aquifer", "hru",
                     "soils", "lum")) {
    expect_true(grepl(section, cio_content),
                info = paste("file.cio missing section:", section))
  }

  # Verify generated files have correct format
  # All written files should have meta line as first line
  written_files <- list.files(output_dir, pattern = "\\.(sim|prt|cnt|bsn|cli|cio|cha|hyd|aqu|hru|sol|lum)$")
  for (f in written_files) {
    fp <- file.path(output_dir, f)
    first_line <- readLines(fp, n = 1)
    # Meta line should contain "written by SWAT+ editor" or be a CLI data file
    if (!grepl("\\.cli$", f) || f == "weather-sta.cli") {
      expect_true(
        grepl("written by SWAT\\+ editor", first_line) ||
        grepl("^\\w+\\.\\w+$", first_line),  # CLI data file header
        info = paste("File", f, "missing meta line, got:", first_line))
    }
  }
})

# -------------------------------------------------------------------
# Test: weather data from ERA5 CLI files integrates into database
# -------------------------------------------------------------------

test_that("ERA5 climate data integrates with weather stations and file writing", {
  project <- create_write_test_project()
  output_dir <- tempfile("txtinout_era5_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })

  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")
  skip_if(nchar(era5_dir) == 0, "ERA5 data not installed")

  # Read the ERA5 CLI files to get the referenced data file names
  pcp_lines <- readLines(file.path(era5_dir, "pcp.cli"))
  tmp_lines <- readLines(file.path(era5_dir, "tmp.cli"))

  # Verify CLI file format (3 lines: header, "filename", data reference)
  expect_equal(length(pcp_lines), 3)
  expect_equal(pcp_lines[1], "pcp.cli")
  expect_equal(pcp_lines[2], "filename")
  expect_true(grepl("IDera5\\.pcp", pcp_lines[3]))

  # Now remove existing stations and add ERA5-referenced stations
  remove_weather_stations(project)
  era5_stations <- data.frame(
    name = "era5_sta",
    lat = -38.15, lon = 176.35,
    pcp = trimws(pcp_lines[3]),
    tmp = trimws(tmp_lines[3]),
    slr = trimws(readLines(file.path(era5_dir, "slr.cli"))[3]),
    hmd = trimws(readLines(file.path(era5_dir, "hmd.cli"))[3]),
    wnd = trimws(readLines(file.path(era5_dir, "wnd.cli"))[3]),
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, era5_stations)

  # Verify stations are in database
  stations <- list_weather_stations(project)
  expect_equal(nrow(stations), 1)
  expect_equal(stations$name, "era5_sta")
  expect_equal(stations$pcp, "IDera5.pcp")

  # Write config files with weather dir
  write_config_files(project, output_dir = output_dir, weather_dir = era5_dir)

  # weather-sta.cli should reference the ERA5 station
  weather_content <- paste(readLines(file.path(output_dir, "weather-sta.cli")),
                           collapse = " ")
  expect_true(grepl("era5_sta", weather_content))
  expect_true(grepl("IDera5\\.pcp", weather_content))

  # CLI data files should be copied to output
  for (f in c("pcp.cli", "tmp.cli", "slr.cli", "hmd.cli", "wnd.cli")) {
    expect_true(file.exists(file.path(output_dir, f)),
                info = paste("ERA5 data file not copied:", f))
  }
})

# ---- populate_from_datasets tests ----

test_that("populate_from_datasets fills reference tables from rQSWATPlus", {
  skip_if_not_installed("rQSWATPlus")

  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Create empty tables with minimal schema (as ensure_write_tables does)
  DBI::dbExecute(con, "CREATE TABLE plants_plt (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE fertilizer_frt (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE cal_parms_cal (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE snow_sno (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE landuse_lum (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE d_table_dtl (id INTEGER PRIMARY KEY, name TEXT)")

  # Verify they are empty

  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM plants_plt")$n, 0L)

  # Run populate_from_datasets
  swatplusEditoR:::populate_from_datasets(con)

  # Verify reference tables are now populated with full schemas
  plants <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM plants_plt")$n
  expect_true(plants > 200,
              info = paste("Expected 266+ plants_plt rows, got", plants))

  fert <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM fertilizer_frt")$n
  expect_true(fert > 50,
              info = paste("Expected 59+ fertilizer_frt rows, got", fert))

  cal <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM cal_parms_cal")$n
  expect_true(cal > 200,
              info = paste("Expected 221+ cal_parms_cal rows, got", cal))

  snow <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM snow_sno")$n
  expect_true(snow >= 1,
              info = paste("Expected 1+ snow_sno rows, got", snow))

  lum <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM landuse_lum")$n
  expect_true(lum > 200,
              info = paste("Expected 284+ landuse_lum rows, got", lum))

  # Decision tables should be filtered (lum.dtl + select res_rel.dtl)
  dtl <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM d_table_dtl")$n
  expect_true(dtl > 10,
              info = paste("Expected 43+ d_table_dtl rows, got", dtl))

  # Verify full schema was restored (not just id+name)
  plant_cols <- DBI::dbGetQuery(con, "PRAGMA table_info(plants_plt)")
  expect_true(nrow(plant_cols) > 40,
              info = paste("Expected 50+ columns in plants_plt, got",
                           nrow(plant_cols)))
})

test_that("populate_from_datasets does not overwrite existing data", {
  skip_if_not_installed("rQSWATPlus")

  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Create plants_plt with one row of data (simulating user-modified table)
  DBI::dbExecute(con, "CREATE TABLE plants_plt (id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "INSERT INTO plants_plt (name) VALUES ('my_custom_plant')")

  # Run populate_from_datasets
  swatplusEditoR:::populate_from_datasets(con)

  # The existing table with data should NOT be touched
  plants <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM plants_plt")$n
  expect_equal(plants, 1L)
  custom <- DBI::dbGetQuery(con, "SELECT name FROM plants_plt")$name
  expect_equal(custom, "my_custom_plant")
})

test_that("populate_from_datasets is idempotent", {
  skip_if_not_installed("rQSWATPlus")

  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Run twice
  swatplusEditoR:::populate_from_datasets(con)
  n1 <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM plants_plt")$n

  swatplusEditoR:::populate_from_datasets(con)
  n2 <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM plants_plt")$n

  expect_equal(n1, n2)
})
