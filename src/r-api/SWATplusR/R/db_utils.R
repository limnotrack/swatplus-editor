#' Database utility functions for SWATplusR
#'
#' Mirrors the functionality of src/api/database/lib.py, providing low-level
#' SQLite helpers used by all higher-level operations.
#' @keywords internal
NULL

# ---------------------------------------------------------------------------
# Connection helpers
# ---------------------------------------------------------------------------

#' Open a DBI/RSQLite connection to a SWAT+ SQLite database
#'
#' @param path Character path to the \code{.sqlite} file.
#' @param journal_mode SQLite journal mode (default \code{"off"} for speed;
#'   use \code{"wal"} for production use).
#' @return A \code{DBIConnection} object.  Remember to close it with
#'   \code{\link{swat_close_db}}.
#' @export
swat_open_db <- function(path, journal_mode = "off") {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, paste0("PRAGMA journal_mode = ", journal_mode, ";"))
  con
}

#' Close a DBI database connection
#'
#' @param con A \code{DBIConnection}.
#' @export
swat_close_db <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Table introspection
# ---------------------------------------------------------------------------

#' Check whether a table exists in a database
#'
#' @param con A \code{DBIConnection}.
#' @param name Table name string.
#' @return Logical \code{TRUE}/\code{FALSE}.
#' @export
swat_exists_table <- function(con, name) {
  DBI::dbExistsTable(con, name)
}

#' List all table names in a database
#'
#' @param con A \code{DBIConnection}.
#' @return Character vector of table names.
#' @export
swat_get_table_names <- function(con) {
  DBI::dbListTables(con)
}

#' List column names of a table
#'
#' @param con A \code{DBIConnection}.
#' @param table Table name string.
#' @return Character vector of column names.
#' @export
swat_get_column_names <- function(con, table) {
  DBI::dbListFields(con, table)
}

#' List table names matching a prefix
#'
#' @param con A \code{DBIConnection}.
#' @param partial_name Prefix to match (SQL \code{LIKE} pattern).
#' @return Character vector of matching table names.
#' @export
swat_get_matching_table_names <- function(con, partial_name) {
  sql <- paste0(
    "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '",
    partial_name, "%'"
  )
  DBI::dbGetQuery(con, sql)$name
}

# ---------------------------------------------------------------------------
# Data manipulation
# ---------------------------------------------------------------------------

#' Bulk-insert rows into a table, respecting the SQLite parameter limit
#'
#' Mirrors Python \code{db_lib.bulk_insert()}.  Inserts \code{data} (a
#' \code{data.frame}) into \code{table_name} in batches no larger than
#' \code{batch_size}.
#'
#' @param con A \code{DBIConnection}.
#' @param table_name Character table name.
#' @param data A \code{data.frame} of rows to insert.
#' @param batch_size Maximum rows per \code{INSERT} call (default 500).
#' @return Invisibly, the total number of rows inserted.
#' @export
swat_bulk_insert <- function(con, table_name, data, batch_size = 500L) {
  if (is.null(data) || nrow(data) == 0L) return(invisible(0L))
  total <- 0L
  n <- nrow(data)
  DBI::dbWithTransaction(con, {
    for (start in seq(1L, n, by = batch_size)) {
      end   <- min(start + batch_size - 1L, n)
      chunk <- data[start:end, , drop = FALSE]
      DBI::dbAppendTable(con, table_name, chunk)
      total <- total + nrow(chunk)
    }
  })
  invisible(total)
}

#' Copy all rows of a table from one database file to another
#'
#' Mirrors Python \code{db_lib.copy_table()}.
#'
#' @param table Table name string.
#' @param src Path to the source \code{.sqlite} file.
#' @param dest Path to the destination \code{.sqlite} file.
#' @param include_id If \code{TRUE}, include the \code{id} column.
#' @param where_clause Optional SQL \code{WHERE} clause (without the keyword).
#' @export
swat_copy_table <- function(table,
                            src,
                            dest,
                            include_id   = FALSE,
                            where_clause = "") {
  src_con  <- swat_open_db(src)
  dest_con <- swat_open_db(dest)
  on.exit({
    swat_close_db(src_con)
    swat_close_db(dest_con)
  }, add = TRUE)

  sql <- if (nchar(trimws(where_clause)) > 0L) {
    paste0("SELECT * FROM ", table, " WHERE ", where_clause)
  } else {
    paste0("SELECT * FROM ", table)
  }
  rows <- DBI::dbGetQuery(src_con, sql)

  if (!include_id && "id" %in% names(rows)) {
    rows <- rows[, !names(rows) %in% "id", drop = FALSE]
  }

  if (nrow(rows) > 0L) {
    swat_bulk_insert(dest_con, table, rows)
  }
  invisible(nrow(rows))
}

#' Drop and remove a table from an SQLite database
#'
#' @param con A \code{DBIConnection}.
#' @param table Table name string.
#' @export
swat_delete_table <- function(con, table) {
  DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", table))
  invisible(NULL)
}

#' Execute an arbitrary SQL statement that returns no rows
#'
#' @param con A \code{DBIConnection}.
#' @param sql SQL string to execute.
#' @return The number of rows affected.
#' @export
swat_execute_sql <- function(con, sql) {
  DBI::dbExecute(con, sql)
}

# ---------------------------------------------------------------------------
# Convenience read helpers
# ---------------------------------------------------------------------------

#' Read an entire table from the database into a data.frame
#'
#' @param con A \code{DBIConnection}.
#' @param table Table name string.
#' @return A \code{data.frame}.
#' @export
swat_read_table <- function(con, table) {
  DBI::dbReadTable(con, table)
}

#' Run a parameterised SELECT and return a data.frame
#'
#' @param con A \code{DBIConnection}.
#' @param sql SQL \code{SELECT} string (may contain \code{?} placeholders).
#' @param params List of parameter values for \code{?} placeholders.
#' @return A \code{data.frame}.
#' @export
swat_query <- function(con, sql, params = list()) {
  if (length(params) == 0L) {
    DBI::dbGetQuery(con, sql)
  } else {
    DBI::dbGetQuery(con, sql, params = params)
  }
}

# ---------------------------------------------------------------------------
# High-level helpers
# ---------------------------------------------------------------------------

#' Return the maximum \code{id} value in a table (or 0 if the table is empty)
#'
#' @param con A \code{DBIConnection}.
#' @param table Table name string.
#' @return Integer maximum id.
#' @export
swat_max_id <- function(con, table) {
  if (!swat_exists_table(con, table)) return(0L)
  res <- DBI::dbGetQuery(con, paste0("SELECT MAX(id) AS max_id FROM ", table))
  val <- res$max_id[[1L]]
  if (is.null(val) || is.na(val)) 0L else as.integer(val)
}

#' Count rows in a table
#'
#' @param con A \code{DBIConnection}.
#' @param table Table name string.
#' @param where Optional WHERE clause (without keyword).
#' @return Integer row count.
#' @export
swat_count <- function(con, table, where = NULL) {
  sql <- paste0("SELECT COUNT(*) AS n FROM ", table)
  if (!is.null(where)) sql <- paste0(sql, " WHERE ", where)
  DBI::dbGetQuery(con, sql)$n[[1L]]
}
