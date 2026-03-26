# Utility functions for swatplusEditoR

#' Validate that a project object has the expected structure
#'
#' @param project List. The SWAT+ project object.
#' @return Logical TRUE if valid, otherwise stops with an error.
#' @keywords internal
validate_project <- function(project) {
  required <- c("project_dir", "db_file")
  missing_fields <- setdiff(required, names(project))
  if (length(missing_fields) > 0) {
    stop("Project object is missing required fields: ",
         paste(missing_fields, collapse = ", "), call. = FALSE)
  }
  if (is.null(project$db_file)) {
    stop("Project db_file is NULL. Please set project$db_file to the path ",
         "of the SWAT+ project SQLite database.", call. = FALSE)
  }
  if (!file.exists(project$db_file)) {
    stop("Project database file not found: ", project$db_file, call. = FALSE)
  }
  invisible(TRUE)
}

#' Generate a weather station name from coordinates
#'
#' Follows the convention used in the SWAT+ Editor Python API.
#'
#' @param lat Numeric. Latitude.
#' @param lon Numeric. Longitude.
#' @return Character. Station name in format "latXXX.XXlonXXX.XX"
#' @keywords internal
weather_sta_name <- function(lat, lon) {
  lat_str <- formatC(abs(lat), format = "f", digits = 2, width = 6,
                     flag = "0")
  lon_str <- formatC(abs(lon), format = "f", digits = 2, width = 7,
                     flag = "0")
  lat_prefix <- ifelse(lat >= 0, "p", "n")
  lon_prefix <- ifelse(lon >= 0, "p", "n")
  paste0(lat_prefix, lat_str, lon_prefix, lon_str)
}

#' Find the closest station by latitude/longitude
#'
#' @param stations A data.frame with lat and lon columns.
#' @param lat Numeric. Target latitude.
#' @param lon Numeric. Target longitude.
#' @return Integer. Row index of the closest station.
#' @keywords internal
find_closest_station <- function(stations, lat, lon) {
  if (nrow(stations) == 0) return(NA_integer_)
  dists <- (stations$lat - lat)^2 + (stations$lon - lon)^2
  which.min(dists)
}

#' Format a data.frame for display
#'
#' @param df A data.frame.
#' @param max_rows Integer. Maximum rows to display.
#' @return The input data.frame (invisibly), with a message showing dimensions.
#' @keywords internal
display_df <- function(df, max_rows = 10) {
  message(sprintf("  %d rows x %d columns", nrow(df), ncol(df)))
  if (nrow(df) > max_rows) {
    message(sprintf("  (showing first %d rows)", max_rows))
  }
  invisible(df)
}

#' Check if a value is a non-empty string
#'
#' @param x Value to check.
#' @return Logical.
#' @keywords internal
is_nonempty_string <- function(x) {
  is.character(x) && length(x) == 1 && nchar(x) > 0
}

#' Build an UPDATE SQL statement from named values
#'
#' @param table_name Character. Table name.
#' @param values Named list of column = value pairs.
#' @param where_clause Character. WHERE clause (without WHERE keyword).
#' @return Character. SQL UPDATE statement.
#' @keywords internal
build_update_sql <- function(table_name, values, where_clause) {
  set_parts <- vapply(names(values), function(col) {
    val <- values[[col]]
    if (is.null(val) || (is.numeric(val) && is.na(val))) {
      paste0(col, " = NULL")
    } else if (is.character(val)) {
      paste0(col, " = '", gsub("'", "''", val), "'")
    } else {
      paste0(col, " = ", val)
    }
  }, character(1))
  paste0("UPDATE ", table_name, " SET ", paste(set_parts, collapse = ", "),
         " WHERE ", where_clause)
}
