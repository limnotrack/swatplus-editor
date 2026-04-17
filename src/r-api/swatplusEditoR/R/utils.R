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

#' Helper to read a SWAT+ text file (skip title + header)
#' 
#' @param file Character. Filename relative to the output directory.
#' @param out_dir Character. Path to the output directory. Default is the project's output
#' directory.
#' @param skip Integer. Number of lines to skip (default 1 for title line
#' and header line).
#' @param ... Additional arguments passed to \code{read.table()}.
#' 
#' @return A data.frame with the contents of the file, or NULL if the file is missing or cannot be read.
#' @keywords internal
#' @export
read_swat <- function(file, out_dir, skip = 1, ...) {
  path <- file.path(out_dir, file)
  if (!file.exists(path)) {
    message("  [MISSING] ", file)
    return(NULL)
  }
  tryCatch({
    lines <- readLines(path)
    
    # Skip metadata line(s)
    lines <- lines[(skip + 1):length(lines)]
    if (length(lines) < 2) return(NULL)
    
    # Parse header (first remaining line)
    header <- scan(text = lines[1], what = character(), quiet = TRUE)
    data_lines <- lines[-1]
    
    # Remove empty lines
    data_lines <- data_lines[nzchar(trimws(data_lines))]
    if (length(data_lines) == 0) return(NULL)
    
    # Count max fields across all data lines
    n_base <- length(header)
    field_counts <- sapply(data_lines, function(x) {
      length(scan(text = x, what = character(), quiet = TRUE))
    })
    n_max <- max(field_counts)
    
    # If all rows fit the header, use standard read
    if (n_max <= n_base) {
      return(read.table(path, skip = skip, header = TRUE, fill = TRUE, ...))
    }
    
    # Extended columns: repeating outflow groups (obj_typ, obj_id, hyd_typ, frac)
    n_extra <- n_max - n_base
    n_groups <- ceiling(n_extra / 4)
    extra_names <- character(0)
    for (g in seq_len(n_groups)) {
      suffix <- paste0("_", g + 1)
      extra_names <- c(extra_names,
                       paste0("obj_typ", suffix),
                       paste0("obj_id", suffix),
                       paste0("hyd_typ", suffix),
                       paste0("frac", suffix))
    }
    all_names <- c(header, extra_names[seq_len(n_extra)])
    
    # Parse each line, padding short rows with NA
    parsed <- lapply(data_lines, function(x) {
      vals <- scan(text = x, what = character(), quiet = TRUE)
      length(vals) <- n_max  # pads with NA
      vals
    })
    
    df <- as.data.frame(do.call(rbind, parsed), stringsAsFactors = FALSE)
    names(df) <- all_names
    
    # Convert numeric columns
    for (col in names(df)) {
      vals <- df[[col]]
      # Try numeric conversion on non-NA values
      non_na <- vals[!is.na(vals)]
      if (length(non_na) > 0) {
        numeric_vals <- suppressWarnings(as.numeric(non_na))
        if (!any(is.na(numeric_vals))) {
          df[[col]] <- as.numeric(df[[col]])
        }
      }
    }
    
    df
  },
  error = function(e) {
    message("  [READ ERROR] ", file, ": ", e$message)
    NULL
  })
}
