#' General utility functions for SWATplusR
#'
#' Mirrors the functionality of src/api/helpers/utils.py
#' @keywords internal
NULL

# Default padding constants (mirrors Python utils.py)
DEFAULT_STR_PAD  <- 16L
DEFAULT_KEY_PAD  <- 16L
DEFAULT_CODE_PAD <- 12L
DEFAULT_NUM_PAD  <- 12L
DEFAULT_INT_PAD  <-  8L
DEFAULT_DECIMALS <-  5L
NULL_STR         <- "null"
NULL_NUM         <- "0"
NON_ZERO_MIN     <- 0.00001

# ---------------------------------------------------------------------------
# String / number formatting helpers
# ---------------------------------------------------------------------------

#' Remove spaces from a string
#'
#' @param s Character string.
#' @param c Replacement character (default \code{"_"}).
#' @return Character string with spaces replaced.
#' @export
remove_space <- function(s, c = "_") {
  if (is.null(s) || s == "") return(s)
  gsub(" ", c, trimws(s))
}

#' Pad a string value for fixed-width SWAT+ output
#'
#' @param val Value to format.
#' @param default_pad Total field width (default \code{DEFAULT_STR_PAD}).
#' @param direction \code{"right"} (right-justify) or \code{"left"}.
#' @param text_if_null Text to use when \code{val} is \code{NULL}/\code{""}.
#' @param spaces_after Number of trailing spaces (default 2).
#' @param no_space_removal If \code{TRUE}, do not replace internal spaces.
#' @return Padded character string.
#' @export
string_pad <- function(val,
                       default_pad    = DEFAULT_STR_PAD,
                       direction      = "right",
                       text_if_null   = NULL_STR,
                       spaces_after   = 2L,
                       no_space_removal = FALSE) {
  if (is.null(val) || val == "") {
    val_text <- text_if_null
  } else if (no_space_removal) {
    val_text <- val
  } else {
    val_text <- remove_space(val)
  }

  trailing <- strrep(" ", spaces_after)

  if (direction == "right") {
    paste0(formatC(as.character(val_text), width = default_pad, flag = " "),
           trailing)
  } else {
    paste0(formatC(as.character(val_text), width = -default_pad, flag = " "),
           trailing)
  }
}

#' @rdname string_pad
#' @export
code_pad <- function(val,
                     default_pad  = DEFAULT_CODE_PAD,
                     direction    = "right",
                     text_if_null = NULL_STR) {
  string_pad(val, default_pad, direction, text_if_null)
}

#' Pad a numeric value for fixed-width SWAT+ output
#'
#' @param val Numeric value.
#' @param decimals Number of decimal places (default \code{DEFAULT_DECIMALS}).
#' @param default_pad Total field width (default \code{DEFAULT_NUM_PAD}).
#' @param direction Field alignment direction.
#' @param text_if_null Text when \code{val} is \code{NULL}/non-numeric.
#' @param use_non_zero_min If \code{TRUE}, replace zero with \code{non_zero_min}.
#' @param non_zero_min Minimum non-zero value (default \code{NON_ZERO_MIN}).
#' @return Padded character string.
#' @export
num_pad <- function(val,
                    decimals      = DEFAULT_DECIMALS,
                    default_pad   = DEFAULT_NUM_PAD,
                    direction     = "right",
                    text_if_null  = NULL_NUM,
                    use_non_zero_min = FALSE,
                    non_zero_min  = NON_ZERO_MIN) {
  if (is_number(val)) {
    v <- as.numeric(val)
    if (use_non_zero_min && v < non_zero_min) v <- non_zero_min
    val_text <- sprintf(paste0("%.", decimals, "f"), v)
  } else {
    val_text <- val
  }
  string_pad(val_text, default_pad, direction, text_if_null)
}

#' Pad an integer value for fixed-width SWAT+ output
#'
#' @param val Integer or numeric value.
#' @param default_pad Total field width (default \code{DEFAULT_INT_PAD}).
#' @param direction Field alignment direction.
#' @return Padded character string.
#' @export
int_pad <- function(val,
                    default_pad = DEFAULT_INT_PAD,
                    direction   = "right") {
  num_pad(val, 0L, default_pad, direction, NULL_NUM)
}

#' Format a number as scientific notation for SWAT+ output
#'
#' @param val Numeric value.
#' @param decimals Decimal places in mantissa (default 4).
#' @param default_pad Total field width.
#' @param direction Field alignment direction.
#' @param text_if_null Text when not numeric.
#' @param use_non_zero_min If \code{TRUE}, replace 0 with tiny positive value.
#' @return Padded character string in scientific notation.
#' @export
exp_pad <- function(val,
                    decimals      = 4L,
                    default_pad   = DEFAULT_NUM_PAD,
                    direction     = "right",
                    text_if_null  = NULL_NUM,
                    use_non_zero_min = FALSE) {
  if (is_number(val)) {
    v <- as.numeric(val)
    if (use_non_zero_min && v == 0) v <- 1e-8
    val_text <- sprintf(paste0("%.", decimals, "E"), v)
  } else {
    val_text <- val
  }
  string_pad(val_text, default_pad, direction, text_if_null)
}

#' Get appropriately-formatted number string (fixed or scientific)
#'
#' @param val Numeric value.
#' @param precision Number of significant decimal places (default 5).
#' @return Formatted character string.
#' @export
get_num_format <- function(val, precision = 5L) {
  if (!is_number(val)) return(val)
  v       <- as.numeric(val)
  min_val <- 10^(-precision)
  if (abs(v) < min_val && v != 0) {
    eprec <- if (precision > 3L) precision - 2L else precision
    return(sprintf(paste0("%.", eprec, "E"), v))
  }
  sprintf(paste0("%.", precision, "f"), v)
}

# ---------------------------------------------------------------------------
# Type-checking helpers
# ---------------------------------------------------------------------------

#' Test whether a value is coercible to numeric
#'
#' @param s Any R object.
#' @return Logical \code{TRUE}/\code{FALSE}.
#' @export
is_number <- function(s) {
  if (is.null(s)) return(FALSE)
  
  tryCatch(!is.na(as.numeric(s)), 
           warning = function(w) FALSE,
           error = function(e) FALSE)
}

# ---------------------------------------------------------------------------
# Path helpers (mirrors Python rel_path / full_path)
# ---------------------------------------------------------------------------

#' Compute a path relative to a reference file
#'
#' @param compare_path Path of the reference file.
#' @param curr_path Path to make relative.
#' @return Relative path string.
#' @export
rel_path <- function(compare_path, curr_path) {
  if (is.null(curr_path)) return(NULL)
  base <- dirname(compare_path)
  # normalizePath needs the path to exist; use string ops as fallback
  tryCatch(
    tools::file_path_as_absolute(
      file.path(base, basename(curr_path))
    ),
    error = function(e) curr_path
  )
}

#' Resolve a (possibly relative) path against a reference file
#'
#' @param compare_path Path of the reference file.
#' @param curr_path Path to resolve (absolute paths are returned unchanged).
#' @return Absolute path string.
#' @export
full_path <- function(compare_path, curr_path) {
  if (is.null(curr_path)) return(NULL)
  if (!grepl("^(/|[A-Za-z]:)", curr_path)) {
    curr_path <- normalizePath(
      file.path(dirname(compare_path), curr_path),
      mustWork = FALSE
    )
  }
  curr_path
}

#' Test whether two paths refer to the same filesystem location
#'
#' @param p1,p2 Path strings to compare.
#' @return Logical.
#' @export
are_paths_equal <- function(p1, p2) {
  n1 <- normalizePath(p1, mustWork = FALSE)
  n2 <- normalizePath(p2, mustWork = FALSE)
  tolower(n1) == tolower(n2)
}

# ---------------------------------------------------------------------------
# String helpers
# ---------------------------------------------------------------------------

#' Sanitise a string for use as a filename
#'
#' @param s Character string.
#' @return Cleaned character string.
#' @export
get_valid_filename <- function(s) {
  s <- trimws(s)
  s <- gsub(" ", "_", s)
  gsub("[^-A-Za-z0-9_.]", "", s)
}

#' Replace a NULL / "null" value with a fallback
#'
#' @param val Value to check.
#' @param ifnull Fallback value (default \code{NULL}).
#' @return \code{val} if not \code{NULL}/\code{"null"}, otherwise \code{ifnull}.
#' @export
val_if_null <- function(val, ifnull = NULL) {
  if (is.null(val) || identical(val, "null")) ifnull else val
}

#' Split a string on multiple delimiter characters
#'
#' @param s Character string.
#' @param delimiters Vector of delimiter strings (default: comma, space, tab, newline).
#' @return Character vector of non-empty tokens.
#' @export
split_multiple_delimiters <- function(s,
                                      delimiters = c(",", " ", "\t", "\n")) {
  pattern <- paste(sapply(delimiters, function(d) paste0("[", d, "]")),
                   collapse = "|")
  tokens <- strsplit(s, pattern)[[1]]
  tokens[nchar(tokens) > 0L]
}

# ---------------------------------------------------------------------------
# SWAT+ name generation
# ---------------------------------------------------------------------------

#' Generate a zero-padded SWAT+ object name
#'
#' Equivalent to Python \code{get_name(name, id, digits)}.
#'
#' @param prefix Name prefix string (e.g. \code{"hru"}).
#' @param id Integer ID for this object.
#' @param max_id Maximum ID in the set (used to determine zero-pad width).
#' @return Character string, e.g. \code{"hru001"}.
#' @export
get_swat_name <- function(prefix, id, max_id) {
  width <- nchar(as.character(max_id))
  paste0(prefix, formatC(id, width = width, flag = "0"))
}

# ---------------------------------------------------------------------------
# Date utilities
# ---------------------------------------------------------------------------

#' Add a number of calendar months to a date
#'
#' @param sourcedate A \code{Date} object.
#' @param months Integer number of months to add.
#' @return A new \code{Date} object.
#' @export
add_months <- function(sourcedate, months) {
  # 1. Calculate new month and year basics
  # Month is 0-indexed (0-11) for the math, then converted back
  orig_m <- as.integer(format(sourcedate, "%m")) - 1L
  total_m <- orig_m + months
  
  new_m <- (total_m %% 12L) + 1L
  new_y <- as.integer(format(sourcedate, "%Y")) + (total_m %/% 12L)
  orig_day <- as.integer(format(sourcedate, "%d"))
  
  # 2. Find the last day of the target month
  # Get the 1st of the NEXT month, then subtract 1 day
  next_m <- (new_m %% 12L) + 1L
  next_y <- if(new_m == 12) new_y + 1L else new_y
  
  first_of_next_month <- as.Date(sprintf("%d-%02d-01", next_y, next_m))
  last_day_of_target <- as.integer(format(first_of_next_month - 1, "%d"))
  
  # 3. Clamp the day and return
  final_day <- min(orig_day, last_day_of_target)
  as.Date(sprintf("%d-%02d-%02d", new_y, new_m, final_day))
}

# ---------------------------------------------------------------------------
# Progress / logging
# ---------------------------------------------------------------------------

#' Emit a JSON-formatted progress message (mirrors ExecutableApi.emit_progress)
#'
#' @param percent Integer 0-100.
#' @param message Human-readable message string.
#' @export
emit_progress <- function(percent, message) {
  cat(jsonlite::toJSON(list(percent = percent, message = message),
                       auto_unbox = TRUE), "\n")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Null-coalescing operator (available package-wide)
# ---------------------------------------------------------------------------

#' Return left-hand side unless it is \code{NULL}, zero-length, or \code{NA}
#'
#' @param a Left-hand side value.
#' @param b Right-hand side fallback.
#' @return \code{a} if usable, otherwise \code{b}.
#' @export
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L ||
      (length(a) == 1L && is.na(a[[1L]]))) b else a
}
