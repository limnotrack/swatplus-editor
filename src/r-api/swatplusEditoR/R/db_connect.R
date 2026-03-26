# Database connection utilities for swatplusEditoR
# Direct SQLite access to SWAT+ Editor project databases

#' Open a connection to the SWAT+ project database
#'
#' @param db_path Character. Path to the SQLite project database file.
#' @return A DBI connection object.
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @examples
#' \dontrun{
#' con <- open_project_db("/path/to/project.sqlite")
#' DBI::dbDisconnect(con)
#' }
open_project_db <- function(db_path) {
  if (!file.exists(db_path)) {
    stop("Database file not found: ", db_path, call. = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  # Enable foreign keys

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  con
}

#' List all tables in the project database
#'
#' @param con A DBI connection object.
#' @return Character vector of table names.
#' @importFrom DBI dbListTables
#' @keywords internal
list_db_tables <- function(con) {
  DBI::dbListTables(con)
}

#' Read a table from the project database
#'
#' @param con A DBI connection object.
#' @param table_name Character. Name of the table to read.
#' @return A data.frame with the table contents.
#' @importFrom DBI dbReadTable
#' @keywords internal
read_db_table <- function(con, table_name) {
  tables <- DBI::dbListTables(con)
  if (!table_name %in% tables) {
    stop("Table '", table_name, "' not found in database. ",
         "Available tables: ", paste(tables, collapse = ", "), call. = FALSE)
  }
  DBI::dbReadTable(con, table_name)
}

#' Execute a query on the project database
#'
#' @param con A DBI connection object.
#' @param query Character. SQL query to execute.
#' @param params List. Optional query parameters.
#' @return A data.frame with query results.
#' @importFrom DBI dbGetQuery
#' @keywords internal
query_db <- function(con, query, params = NULL) {
  if (is.null(params)) {
    DBI::dbGetQuery(con, query)
  } else {
    DBI::dbGetQuery(con, query, params = params)
  }
}

#' Execute a statement on the project database (INSERT, UPDATE, DELETE)
#'
#' @param con A DBI connection object.
#' @param statement Character. SQL statement to execute.
#' @param params List. Optional statement parameters.
#' @return Number of affected rows.
#' @importFrom DBI dbExecute
#' @keywords internal
execute_db <- function(con, statement, params = NULL) {
  if (is.null(params)) {
    DBI::dbExecute(con, statement)
  } else {
    DBI::dbExecute(con, statement, params = params)
  }
}

#' Check if a table exists in the database
#'
#' @param con A DBI connection object.
#' @param table_name Character. Name of the table to check.
#' @return Logical. TRUE if the table exists.
#' @keywords internal
table_exists <- function(con, table_name) {
  table_name %in% DBI::dbListTables(con)
}

#' Safely close a database connection
#'
#' @param con A DBI connection object.
#' @importFrom DBI dbDisconnect
#' @keywords internal
close_db <- function(con) {
  tryCatch(
    DBI::dbDisconnect(con),
    error = function(e) invisible(NULL)
  )
}
