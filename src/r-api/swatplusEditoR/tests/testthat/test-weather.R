# Test weather station management functions

# Helper to create a project with weather capability
create_weather_test_project <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()

  project <- list(
    project_dir = project_dir,
    db_file = db_path,
    hru_data = NULL,
    basin_data = NULL
  )

  project <- create_project_db(project, db_path, overwrite = TRUE)
  project
}

test_that("add_weather_stations inserts stations", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  stations <- data.frame(
    name = c("station1", "station2"),
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

  result <- list_weather_stations(project)
  expect_equal(nrow(result), 2)
  expect_equal(result$name[1], "station1")
  expect_equal(result$lat[1], -38.1)
  expect_equal(result$pcp[1], "pcp1.cli")
})

test_that("add_weather_stations validates input", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  # Not a data.frame
  expect_error(add_weather_stations(project, "not_a_df"),
               "must be a data.frame")

  # Missing required columns
  bad_df <- data.frame(x = 1)
  expect_error(add_weather_stations(project, bad_df),
               "missing required columns")

  # Empty data.frame
  empty_df <- data.frame(name = character(0), lat = numeric(0),
                         lon = numeric(0))
  expect_error(add_weather_stations(project, empty_df),
               "empty")
})

test_that("update_weather_station updates station", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  stations <- data.frame(
    name = "test_station",
    lat = -38.1, lon = 176.3,
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, stations)

  update_weather_station(project, station_id = 1, lat = -38.5, lon = 176.8)

  result <- list_weather_stations(project)
  expect_equal(result$lat[1], -38.5)
  expect_equal(result$lon[1], 176.8)
})

test_that("remove_weather_stations removes stations", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  stations <- data.frame(
    name = c("s1", "s2", "s3"),
    lat = c(-38.1, -38.2, -38.3),
    lon = c(176.3, 176.4, 176.5),
    stringsAsFactors = FALSE
  )
  add_weather_stations(project, stations)
  expect_equal(nrow(list_weather_stations(project)), 3)

  # Remove specific stations
  remove_weather_stations(project, station_ids = c(1, 2))
  expect_equal(nrow(list_weather_stations(project)), 1)

  # Remove all
  remove_weather_stations(project)
  expect_equal(nrow(list_weather_stations(project)), 0)
})

test_that("add_weather_generators inserts WGN data", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  wgn <- data.frame(
    name = "wgn_test",
    lat = -38.1, lon = 176.3, elev = 350, rain_yrs = 30,
    stringsAsFactors = FALSE
  )

  add_weather_generators(project, wgn)

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  result <- DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli")
  DBI::dbDisconnect(con)

  expect_equal(nrow(result), 1)
  expect_equal(result$name[1], "wgn_test")
  expect_equal(result$rain_yrs[1], 30)
})

# -------------------------------------------------------------------
# Tests for ERA5 climate data integration
# -------------------------------------------------------------------

test_that("ERA5 climate data files exist in package", {
  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")
  expect_true(nchar(era5_dir) > 0, info = "ERA5 directory not found in package")
  expect_true(dir.exists(era5_dir))

  expected_files <- c("pcp.cli", "tmp.cli", "slr.cli", "hmd.cli", "wnd.cli")
  for (f in expected_files) {
    expect_true(file.exists(file.path(era5_dir, f)),
                info = paste("Missing ERA5 file:", f))
  }
})

test_that("add_weather_stations with ERA5 climate file references", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  # Add a station that references ERA5-style climate files
  stations <- data.frame(
    name = "era5_station",
    lat = 55.5, lon = 12.1,
    pcp = "IDera5.pcp",
    tmp = "IDera5.tmp",
    slr = "IDera5.slr",
    hmd = "IDera5.hmd",
    wnd = "IDera5.wnd",
    stringsAsFactors = FALSE
  )

  add_weather_stations(project, stations)

  result <- list_weather_stations(project)
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "era5_station")
  expect_equal(result$pcp, "IDera5.pcp")
  expect_equal(result$tmp, "IDera5.tmp")
  expect_equal(result$slr, "IDera5.slr")
  expect_equal(result$hmd, "IDera5.hmd")
  expect_equal(result$wnd, "IDera5.wnd")
})

test_that("set_weather_dir works with ERA5 data directory", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  era5_dir <- system.file("extdata", "era5", package = "swatplusEditoR")
  skip_if(nchar(era5_dir) == 0, "ERA5 data not installed")

  set_weather_dir(project, era5_dir)

  config <- get_project_config(project)
  expect_equal(normalizePath(config$weather_data_dir),
               normalizePath(era5_dir))
})

test_that("set_weather_dir rejects non-existent directory", {
  project <- create_weather_test_project()
  on.exit(unlink(project$db_file))

  expect_error(set_weather_dir(project, "/nonexistent/path"),
               "does not exist")
})

# -------------------------------------------------------------------
# Tests for get_wgn_cfsr_world
# -------------------------------------------------------------------

#' Create a minimal swatplus_wgn.sqlite with wgn_cfsr_world + _mon tables
create_mock_wgn_db <- function() {
  wgn_db_path <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), wgn_db_path)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "CREATE TABLE wgn_cfsr_world (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    lat REAL NOT NULL,
    lon REAL NOT NULL,
    elev REAL NOT NULL,
    rain_yrs INTEGER NOT NULL
  )")

  DBI::dbExecute(con, "CREATE TABLE wgn_cfsr_world_mon (
    id INTEGER PRIMARY KEY,
    wgn_id INTEGER NOT NULL,
    month INTEGER NOT NULL,
    tmp_max_ave REAL, tmp_min_ave REAL, tmp_max_sd REAL, tmp_min_sd REAL,
    pcp_ave REAL, pcp_sd REAL, pcp_skew REAL,
    wet_dry REAL, wet_wet REAL, pcp_days REAL, pcp_hhr REAL,
    slr_ave REAL, dew_ave REAL, wnd_ave REAL
  )")

  # Insert two WGN stations
  DBI::dbExecute(con, "INSERT INTO wgn_cfsr_world VALUES
    (1, 'wgn_s1', -38.1, 176.3, 350.0, 30),
    (2, 'wgn_s2', -39.5, 177.0, 120.0, 25)")

  # Insert 12 monthly rows for each station (use simple placeholder values)
  for (wgn_id in 1:2) {
    for (month in 1:12) {
      DBI::dbExecute(con,
        "INSERT INTO wgn_cfsr_world_mon
         (wgn_id, month, tmp_max_ave, tmp_min_ave, tmp_max_sd, tmp_min_sd,
          pcp_ave, pcp_sd, pcp_skew, wet_dry, wet_wet, pcp_days, pcp_hhr,
          slr_ave, dew_ave, wnd_ave)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(wgn_id, month,
                      20.0 + month * 0.1, 10.0 + month * 0.1,
                      2.0, 1.5,
                      50.0, 10.0, 0.5,
                      0.3, 0.4, 8.0, 15.0,
                      18.0, 12.0, 3.0))
    }
  }

  wgn_db_path
}

test_that("get_wgn_cfsr_world inserts WGN data into weather_wgn_cli", {
  project <- create_weather_test_project()
  wgn_db <- create_mock_wgn_db()
  on.exit({
    unlink(project$db_file)
    unlink(wgn_db)
  })

  # Two stations - both nearest to wgn_s1 (-38.1, 176.3)
  stations <- data.frame(
    lat = c(-38.1, -38.15),
    lon = c(176.3, 176.35),
    stringsAsFactors = FALSE
  )

  result <- get_wgn_cfsr_world(project, stations, wgn_db)

  # Returns project invisibly
  expect_identical(result, project)

  # One unique WGN site should have been written
  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  wgn_rows <- DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli")
  expect_equal(nrow(wgn_rows), 1)
  expect_equal(wgn_rows$name[1], "wgn_s1")
  expect_equal(wgn_rows$rain_yrs[1], 30)

  mon_rows <- DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli_mon")
  expect_equal(nrow(mon_rows), 12)
})

test_that("get_wgn_cfsr_world writes two unique WGN sites when stations differ", {
  project <- create_weather_test_project()
  wgn_db <- create_mock_wgn_db()
  on.exit({
    unlink(project$db_file)
    unlink(wgn_db)
  })

  # One station near wgn_s1, one near wgn_s2
  stations <- data.frame(
    lat = c(-38.1, -39.5),
    lon = c(176.3, 177.0),
    stringsAsFactors = FALSE
  )

  get_wgn_cfsr_world(project, stations, wgn_db)

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  wgn_rows <- DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli ORDER BY id")
  expect_equal(nrow(wgn_rows), 2)
  expect_equal(sort(wgn_rows$name), c("wgn_s1", "wgn_s2"))

  mon_rows <- DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli_mon")
  expect_equal(nrow(mon_rows), 24)  # 12 months x 2 stations
})

test_that("get_wgn_cfsr_world is idempotent (INSERT OR IGNORE)", {
  project <- create_weather_test_project()
  wgn_db <- create_mock_wgn_db()
  on.exit({
    unlink(project$db_file)
    unlink(wgn_db)
  })

  stations <- data.frame(lat = -38.1, lon = 176.3, stringsAsFactors = FALSE)

  get_wgn_cfsr_world(project, stations, wgn_db)
  get_wgn_cfsr_world(project, stations, wgn_db)  # second call must not duplicate

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli")), 1)
  expect_equal(nrow(DBI::dbGetQuery(con, "SELECT * FROM weather_wgn_cli_mon")), 12)
})

test_that("get_wgn_cfsr_world validates inputs", {
  project <- create_weather_test_project()
  wgn_db <- create_mock_wgn_db()
  on.exit({
    unlink(project$db_file)
    unlink(wgn_db)
  })

  # Not a data.frame
  expect_error(get_wgn_cfsr_world(project, "not_a_df", wgn_db),
               "non-empty data.frame")

  # Empty data.frame
  expect_error(get_wgn_cfsr_world(project, data.frame(), wgn_db),
               "non-empty data.frame")

  # Missing required columns
  expect_error(
    get_wgn_cfsr_world(project, data.frame(x = 1), wgn_db),
    "missing required columns"
  )

  # Non-existent WGN database
  stations <- data.frame(lat = -38.1, lon = 176.3)
  expect_error(get_wgn_cfsr_world(project, stations, "/no/such/file.sqlite"),
               "not found")
})

