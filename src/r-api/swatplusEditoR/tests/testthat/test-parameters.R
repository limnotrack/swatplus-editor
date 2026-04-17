# Test parameter update functions

# Helper
create_param_test_project <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()

  project <- list(
    project_dir = project_dir,
    db_file = db_path,
    hru_data = NULL,
    basin_data = NULL
  )

  project <- create_project_db(project, db_path, overwrite = TRUE)

  # Create a test parameter table (hydrology_hyd)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "
    CREATE TABLE hydrology_hyd (
      id INTEGER PRIMARY KEY,
      name TEXT, cn2 REAL, awc REAL, esco REAL, epco REAL
    )")
  DBI::dbExecute(con,
    "INSERT INTO hydrology_hyd (id, name, cn2, awc, esco, epco)
     VALUES (1, 'hyd1', 70, 0.5, 0.95, 1.0)")
  DBI::dbExecute(con,
    "INSERT INTO hydrology_hyd (id, name, cn2, awc, esco, epco)
     VALUES (2, 'hyd2', 75, 0.6, 0.90, 0.95)")
  DBI::dbExecute(con,
    "INSERT INTO hydrology_hyd (id, name, cn2, awc, esco, epco)
     VALUES (3, 'hyd3', 80, 0.4, 0.85, 0.90)")
  DBI::dbDisconnect(con)

  project
}

test_that("update_parameters updates all rows", {
  project <- create_param_test_project()
  on.exit(unlink(project$db_file))

  n <- update_parameters(project, "hydrology_hyd", list(cn2 = 65))
  expect_equal(n, 3)

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  result <- DBI::dbGetQuery(con, "SELECT cn2 FROM hydrology_hyd")
  DBI::dbDisconnect(con)

  expect_true(all(result$cn2 == 65))
})

test_that("update_parameters updates by IDs", {
  project <- create_param_test_project()
  on.exit(unlink(project$db_file))

  n <- update_parameters(project, "hydrology_hyd", list(cn2 = 90), ids = c(1, 2))
  expect_equal(n, 2)

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  result <- DBI::dbGetQuery(con, "SELECT id, cn2 FROM hydrology_hyd ORDER BY id")
  DBI::dbDisconnect(con)

  expect_equal(result$cn2[1], 90)
  expect_equal(result$cn2[2], 90)
  expect_equal(result$cn2[3], 80)  # unchanged
})

test_that("update_parameters updates with WHERE clause", {
  project <- create_param_test_project()
  on.exit(unlink(project$db_file))

  n <- update_parameters(project, "hydrology_hyd",
                         list(esco = 0.5), where = "cn2 > 74")
  expect_equal(n, 2)
})

test_that("update_parameters validates table existence", {
  project <- create_param_test_project()
  on.exit(unlink(project$db_file))

  expect_error(
    update_parameters(project, "nonexistent_table", list(cn2 = 65)),
    "not found in database"
  )
})

test_that("update_parameters validates column names", {
  project <- create_param_test_project()
  on.exit(unlink(project$db_file))

  expect_error(
    update_parameters(project, "hydrology_hyd", list(invalid_col = 65)),
    "Invalid column"
  )
})

test_that("set_simulation_time sets time period", {
  project <- create_param_test_project()
  on.exit(unlink(project$db_file))

  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2010)

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  result <- DBI::dbGetQuery(con, "SELECT * FROM time_sim")
  DBI::dbDisconnect(con)

  expect_equal(nrow(result), 1)
  expect_equal(result$yrc_start, 2000)
  expect_equal(result$yrc_end, 2010)
})
