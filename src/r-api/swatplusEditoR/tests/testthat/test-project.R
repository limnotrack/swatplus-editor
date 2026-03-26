# Test project management functions

# Helper to create a test project with database
create_test_project <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()

  project <- list(
    project_dir = project_dir,
    db_file = db_path,
    dem_file = NULL,
    landuse_file = NULL,
    soil_file = NULL,
    hru_data = NULL,
    basin_data = NULL
  )

  # Create the database using the package function
  project <- create_project_db(project, db_path, overwrite = TRUE)
  project
}

test_that("create_project_db creates database with GIS tables", {
  project <- create_test_project()
  on.exit(unlink(project$db_file))

  expect_true(file.exists(project$db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)

  # Check all required GIS tables exist
  expected_tables <- c("gis_aquifers", "gis_channels", "gis_deep_aquifers",
                       "gis_hrus", "gis_lsus", "gis_points", "gis_routing",
                       "gis_subbasins", "gis_water")
  for (tbl in expected_tables) {
    expect_true(tbl %in% tables, info = paste("Missing table:", tbl))
  }

  # Check weather tables
  expect_true("weather_sta_cli" %in% tables)
  expect_true("weather_wgn_cli" %in% tables)

  # Check project config
  expect_true("project_config" %in% tables)
})

test_that("load_project validates project object", {
  project <- create_test_project()
  on.exit(unlink(project$db_file))

  # Should work
  expect_message(load_project(project), "SWAT\\+ project loaded")

  # Missing db_file
  bad_project <- list(project_dir = tempdir())
  expect_error(load_project(bad_project), "missing required fields")

  # NULL db_file
  bad_project2 <- list(project_dir = tempdir(), db_file = NULL)
  expect_error(load_project(bad_project2), "db_file is NULL")
})

test_that("get_project_config returns configuration", {
  project <- create_test_project()
  on.exit(unlink(project$db_file))

  config <- get_project_config(project)
  expect_true(is.list(config))
  expect_true("project_name" %in% names(config))
  expect_equal(config$editor_version, "3.2.0")
})

test_that("get_project_info returns summary", {
  project <- create_test_project()
  on.exit(unlink(project$db_file))

  info <- get_project_info(project)
  expect_true(is.list(info))
  expect_true("gis_counts" %in% names(info))
  expect_true("status" %in% names(info))
  expect_equal(info$status$weather_stations, 0L)
})
