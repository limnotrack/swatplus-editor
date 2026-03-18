library(SWATplusR)
library(testthat)
library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# swat_open_db / swat_close_db
# ---------------------------------------------------------------------------

test_that("swat_open_db creates and returns a valid DBIConnection", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  con <- swat_open_db(tmp)
  expect_true(DBI::dbIsValid(con))
  swat_close_db(con)
  expect_false(DBI::dbIsValid(con))
})

test_that("swat_open_db on non-existent path creates a new file", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  con <- swat_open_db(tmp)
  swat_close_db(con)
  expect_true(file.exists(tmp))
})

# ---------------------------------------------------------------------------
# swat_exists_table
# ---------------------------------------------------------------------------

test_that("swat_exists_table returns FALSE for missing tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_false(swat_exists_table(con, "nonexistent"))
})

test_that("swat_exists_table returns TRUE after table creation", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE test_tbl (id INTEGER PRIMARY KEY, val TEXT)")
  expect_true(swat_exists_table(con, "test_tbl"))
})

# ---------------------------------------------------------------------------
# swat_bulk_insert
# ---------------------------------------------------------------------------

test_that("swat_bulk_insert inserts rows correctly", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (a INTEGER, b REAL, c TEXT)")
  df  <- data.frame(a = 1:5, b = seq(0.1, 0.5, 0.1), c = letters[1:5],
                    stringsAsFactors = FALSE)
  n   <- swat_bulk_insert(con, "t", df)
  expect_equal(n, 5L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t")$n, 5L)
})

test_that("swat_bulk_insert handles empty data.frame gracefully", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (a INTEGER)")
  n <- swat_bulk_insert(con, "t", data.frame(a = integer(0)))
  expect_equal(n, 0L)
})

test_that("swat_bulk_insert respects batch_size", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (a INTEGER)")
  df <- data.frame(a = 1:105)
  swat_bulk_insert(con, "t", df, batch_size = 50L)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM t")$n, 105L)
})

# ---------------------------------------------------------------------------
# swat_count / swat_max_id
# ---------------------------------------------------------------------------

test_that("swat_count returns correct row count", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (1)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (2)")
  expect_equal(swat_count(con, "t"), 2L)
})

test_that("swat_max_id returns 0 for empty table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER)")
  expect_equal(swat_max_id(con, "t"), 0L)
})

test_that("swat_max_id returns correct max", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE t (id INTEGER)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (7)")
  DBI::dbExecute(con, "INSERT INTO t VALUES (3)")
  expect_equal(swat_max_id(con, "t"), 7L)
})

# ---------------------------------------------------------------------------
# swat_get_table_names
# ---------------------------------------------------------------------------

test_that("swat_get_table_names lists created tables", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE alpha (x INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE beta  (y TEXT)")
  tbls <- swat_get_table_names(con)
  expect_true("alpha" %in% tbls)
  expect_true("beta"  %in% tbls)
})

# ---------------------------------------------------------------------------
# swat_delete_table
# ---------------------------------------------------------------------------

test_that("swat_delete_table drops the table", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE todel (x INTEGER)")
  expect_true(swat_exists_table(con, "todel"))
  swat_delete_table(con, "todel")
  expect_false(swat_exists_table(con, "todel"))
})
