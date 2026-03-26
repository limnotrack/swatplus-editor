# Parameter update functions for swatplusEditoR
# Update HRU, aquifer, channel, and basin parameters

#' Update parameters in the project database
#'
#' A general-purpose function to update parameter values in any table of
#' the SWAT+ project database. Supports updating by ID, by condition, or
#' in bulk.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param table_name Character. Name of the database table to update
#'   (e.g., "hru_data_hru", "aquifer_aqu", "hydrology_hyd",
#'   "topography_hyd", "channel_chn").
#' @param values Named list. Column names and their new values.
#' @param ids Integer vector. Optional IDs of rows to update. If NULL,
#'   updates all rows matching the \code{where} condition.
#' @param where Character. Optional SQL WHERE clause (without the WHERE
#'   keyword). Applied in addition to \code{ids} if both are provided.
#' @return Integer. Number of rows affected.
#' @export
#' @examples
#' \dontrun{
#' # Update CN2 for all HRUs
#' update_parameters(project, "hydrology_hyd", list(cn2 = 65))
#'
#' # Update specific HRUs
#' update_parameters(project, "hydrology_hyd", list(cn2 = 70),
#'                   ids = c(1, 2, 3))
#'
#' # Update with a WHERE condition
#' update_parameters(project, "hydrology_hyd", list(cn2 = 75),
#'                   where = "cn2 > 80")
#' }
update_parameters <- function(project, table_name, values,
                              ids = NULL, where = NULL) {
  validate_project(project)

  if (!is.list(values) || length(values) == 0 || is.null(names(values))) {
    stop("values must be a named list with at least one element.",
         call. = FALSE)
  }

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, table_name)) {
    stop("Table '", table_name, "' not found in database.", call. = FALSE)
  }

  # Verify column names
  cols <- names(query_db(con, paste("SELECT * FROM", table_name, "LIMIT 0")))
  invalid_cols <- setdiff(names(values), cols)
  if (length(invalid_cols) > 0) {
    stop("Invalid column(s) for table '", table_name, "': ",
         paste(invalid_cols, collapse = ", "),
         ". Available: ", paste(cols, collapse = ", "), call. = FALSE)
  }

  # Build WHERE clause
  conditions <- character(0)
  if (!is.null(ids)) {
    id_str <- paste(as.integer(ids), collapse = ", ")
    conditions <- c(conditions, paste0("id IN (", id_str, ")"))
  }
  if (!is.null(where) && nchar(where) > 0) {
    conditions <- c(conditions, paste0("(", where, ")"))
  }

  where_clause <- if (length(conditions) > 0) {
    paste(conditions, collapse = " AND ")
  } else {
    "1 = 1"
  }

  sql <- build_update_sql(table_name, values, where_clause)
  n <- execute_db(con, sql)

  message("Updated ", n, " rows in '", table_name, "'")
  n
}

#' Set simulation time period
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param day_start Integer. Start Julian day (1-366).
#' @param yrc_start Integer. Start year.
#' @param day_end Integer. End Julian day (1-366).
#' @param yrc_end Integer. End year.
#' @param step Integer. Time step (0 = daily, 1 = hourly). Default 0.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' set_simulation_time(project, day_start = 1, yrc_start = 2000,
#'                     day_end = 365, yrc_end = 2010)
#' }
set_simulation_time <- function(project, day_start, yrc_start,
                                day_end, yrc_end, step = 0) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "time_sim")) {
    # Create the table if it doesn't exist
    execute_db(con, "
      CREATE TABLE IF NOT EXISTS time_sim (
        id INTEGER PRIMARY KEY,
        day_start INTEGER, yrc_start INTEGER,
        day_end INTEGER, yrc_end INTEGER, step INTEGER
      )")
    execute_db(con, "
      INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
      VALUES (?, ?, ?, ?, ?)",
      params = list(day_start, yrc_start, day_end, yrc_end, step))
  } else {
    result <- query_db(con, "SELECT COUNT(*) as n FROM time_sim")
    if (result$n == 0) {
      execute_db(con, "
        INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
        VALUES (?, ?, ?, ?, ?)",
        params = list(day_start, yrc_start, day_end, yrc_end, step))
    } else {
      execute_db(con, "
        UPDATE time_sim SET day_start = ?, yrc_start = ?, day_end = ?,
        yrc_end = ?, step = ?",
        params = list(day_start, yrc_start, day_end, yrc_end, step))
    }
  }

  message("Set simulation time: ", yrc_start, "/", day_start,
          " to ", yrc_end, "/", day_end,
          " (step = ", step, ")")
  invisible(project)
}

#' Set print/output options
#'
#' Configures what SWAT+ output is written and at what intervals.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param nyskip Integer. Number of years to skip for warm-up.
#' @param day_start Integer. Print start Julian day.
#' @param day_end Integer. Print end Julian day.
#' @param yrc_start Integer. Print start year.
#' @param yrc_end Integer. Print end year.
#' @param interval Integer. Print interval.
#' @param csvout Logical. Write CSV output files.
#' @param dbout Logical. Write database output.
#' @param cdfout Logical. Write NetCDF output.
#' @param crop_yld Character. Crop yield output option.
#' @param mgtout Logical. Write management output.
#' @param hydcon Logical. Write hydrology connections output.
#' @param fdcout Logical. Write flow duration curve output.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' set_print_options(project, nyskip = 2, csvout = TRUE, dbout = FALSE)
#' }
set_print_options <- function(project, nyskip = NULL, day_start = NULL,
                              day_end = NULL, yrc_start = NULL,
                              yrc_end = NULL, interval = NULL,
                              csvout = NULL, dbout = NULL,
                              cdfout = NULL, crop_yld = NULL,
                              mgtout = NULL, hydcon = NULL,
                              fdcout = NULL) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "print_prt")) {
    stop("print_prt table not found. The database may not be fully ",
         "initialized.", call. = FALSE)
  }

  updates <- list()
  if (!is.null(nyskip))    updates$nyskip <- as.integer(nyskip)
  if (!is.null(day_start)) updates$day_start <- as.integer(day_start)
  if (!is.null(day_end))   updates$day_end <- as.integer(day_end)
  if (!is.null(yrc_start)) updates$yrc_start <- as.integer(yrc_start)
  if (!is.null(yrc_end))   updates$yrc_end <- as.integer(yrc_end)
  if (!is.null(interval))  updates$interval <- as.integer(interval)
  if (!is.null(csvout))    updates$csvout <- as.integer(csvout)
  if (!is.null(dbout))     updates$dbout <- as.integer(dbout)
  if (!is.null(cdfout))    updates$cdfout <- as.integer(cdfout)
  if (!is.null(crop_yld))  updates$crop_yld <- crop_yld
  if (!is.null(mgtout))    updates$mgtout <- as.integer(mgtout)
  if (!is.null(hydcon))    updates$hydcon <- as.integer(hydcon)
  if (!is.null(fdcout))    updates$fdcout <- as.integer(fdcout)

  if (length(updates) == 0) {
    message("No print options to update.")
    return(invisible(project))
  }

  sql <- build_update_sql("print_prt", updates, "1 = 1")
  execute_db(con, sql)

  message("Updated print options")
  invisible(project)
}
