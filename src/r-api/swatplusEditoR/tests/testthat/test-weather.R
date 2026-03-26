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
