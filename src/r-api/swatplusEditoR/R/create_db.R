#' Create and initialise SWAT+ project and datasets databases
#'
#' Mirrors \code{src/api/actions/create_databases.py}.
#' @keywords internal
NULL

# ===========================================================================
# CreateProjectDb
# ===========================================================================

#' Create a new SWAT+ project SQLite database
#'
#' Creates the database file (if it does not already exist) and builds all
#' required tables via \code{\link{create_project_tables}}.
#'
#' Mirrors \code{CreateProjectDb} in \code{src/api/actions/create_databases.py}.
#'
#' @param project_db Path to the project \code{.sqlite} file to create.
#' @param overwrite  If \code{TRUE}, delete and recreate an existing database.
#'   Default \code{FALSE}.
#' @return Invisibly, the path to the created database.
#' @export
create_project_db <- function(project_db, overwrite = FALSE) {
  if (file.exists(project_db) && overwrite) {
    file.remove(project_db)
  }
  con <- swat_open_db(project_db)
  on.exit(swat_close_db(con), add = TRUE)
  create_project_tables(con)
  invisible(project_db)
}

# ===========================================================================
# CreateOutputDb
# ===========================================================================

#' Create a SWAT+ output SQLite database
#'
#' Creates the file with the minimal schema required by
#' \code{\link{read_output}}.
#'
#' @param output_db  Path to the output \code{.sqlite} file to create.
#' @param overwrite  If \code{TRUE}, delete an existing database first.
#' @return Invisibly, the path.
#' @export
create_output_db <- function(output_db, overwrite = FALSE) {
  if (file.exists(output_db) && overwrite) {
    file.remove(output_db)
  }
  con <- swat_open_db(output_db)
  on.exit(swat_close_db(con), add = TRUE)

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_config (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      project_name TEXT,
      project_db  TEXT,
      output_db   TEXT
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS table_description (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      table_name  TEXT NOT NULL UNIQUE,
      description TEXT,
      obj_type    TEXT
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS column_description (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      table_name   TEXT NOT NULL,
      column_name  TEXT NOT NULL,
      description  TEXT,
      units        TEXT
    )")

  invisible(output_db)
}
