# Test database connection utilities

test_that("open_project_db fails on missing file", {
  expect_error(
    open_project_db("/nonexistent/path.sqlite"),
    "Database file not found"
  )
})

test_that("create and open project database", {
  db_path <- tempfile(fileext = ".sqlite")
  on.exit(unlink(db_path))

  # Create minimal database
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "CREATE TABLE test_table (id INTEGER, name TEXT)")
  DBI::dbExecute(con, "INSERT INTO test_table VALUES (1, 'test')")
  DBI::dbDisconnect(con)

  # Open with our function

  con <- open_project_db(db_path)
  expect_true(inherits(con, "SQLiteConnection"))

  tables <- DBI::dbListTables(con)
  expect_true("test_table" %in% tables)

  DBI::dbDisconnect(con)
})
