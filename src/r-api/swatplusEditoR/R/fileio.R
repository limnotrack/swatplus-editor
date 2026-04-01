# Low-level SWAT+ file formatting and writing utilities
# These mirror the Python helpers/utils.py and fileio/base.py formatting

# ---- Format constants (matching Python helpers/utils.py) ----
SWAT_STR_PAD   <- 16L
SWAT_KEY_PAD   <- 16L
SWAT_CODE_PAD  <- 12L
SWAT_NUM_PAD   <- 12L
SWAT_INT_PAD   <- 8L
SWAT_DECIMALS  <- 5L
SWAT_SPACES    <- 2L
SWAT_NULL_STR  <- "null"
SWAT_NULL_NUM  <- "0"
SWAT_NON_ZERO_MIN <- 0.00001

#' Format a string value with padding (mirrors Python string_pad)
#' @param val Value to format.
#' @param pad Padding width.
#' @param align "right" or "left".
#' @param null_text Replacement text for NULL/NA values.
#' @return Formatted string.
#' @keywords internal
swat_string_pad <- function(val, pad = SWAT_STR_PAD, align = "right",
                            null_text = SWAT_NULL_STR) {
  txt <- if (is.null(val) || is.na(val) || val == "") null_text else gsub(" ", "_", val)
  fmt <- if (align == "right") {
    paste0("%", pad, "s  ")
  } else {
    paste0("%-", pad, "s  ")
  }
  sprintf(fmt, txt)
}

#' Format a numeric value with padding (mirrors Python num_pad)
#' @param val Numeric value.
#' @param decimals Number of decimal places.
#' @param pad Padding width.
#' @param align "right" or "left".
#' @param null_text Replacement for NULL/NA.
#' @param use_non_zero_min If TRUE, enforce minimum value.
#' @param non_zero_min Minimum value to use.
#' @return Formatted string.
#' @keywords internal
swat_num_pad <- function(val, decimals = SWAT_DECIMALS, pad = SWAT_NUM_PAD,
                         align = "right", null_text = SWAT_NULL_NUM,
                         use_non_zero_min = FALSE, non_zero_min = SWAT_NON_ZERO_MIN) {
  if (is.null(val) || is.na(val)) {
    return(swat_string_pad(null_text, pad, align, null_text))
  }
  if (is.numeric(val)) {
    if (use_non_zero_min && val < non_zero_min) val <- non_zero_min
    txt <- formatC(val, format = "f", digits = decimals, width = 1)
  } else {
    txt <- as.character(val)
  }
  swat_string_pad(txt, pad, align, null_text)
}

#' Format an integer value with padding (mirrors Python int_pad)
#' @param val Integer value.
#' @param pad Padding width.
#' @param align "right" or "left".
#' @return Formatted string.
#' @keywords internal
swat_int_pad <- function(val, pad = SWAT_INT_PAD, align = "right") {
  swat_num_pad(val, decimals = 0, pad = pad, align = align, null_text = SWAT_NULL_NUM)
}

#' Format a boolean value as y/n (mirrors Python write_bool_yn)
#' @param val Logical value.
#' @param pad Padding width.
#' @param align "right" or "left".
#' @return Formatted string.
#' @keywords internal
swat_bool_pad <- function(val, pad = SWAT_CODE_PAD, align = "right") {
  yn <- if (isTRUE(as.logical(val))) "y" else "n"
  swat_string_pad(yn, pad, align, SWAT_NULL_STR)
}

#' Generate SWAT+ file meta line (mirrors Python get_meta_line)
#' @param file_name Output file name (basename only).
#' @param version Editor version string.
#' @param swat_version SWAT+ version string.
#' @return Meta line string.
#' @keywords internal
swat_meta_line <- function(file_name, version = NULL, swat_version = NULL) {
  vtxt <- if (!is.null(version)) paste0(" v", version) else ""
  svtxt <- if (!is.null(swat_version)) paste0("for SWAT+ ", swat_version) else ""
  date_str <- format(Sys.time(), "%Y-%m-%d %H:%M")
  paste0(basename(file_name), ": written by swatplusEditoR R package", vtxt,
         " on ", date_str, " ", svtxt)
}

#' Format a single value based on its R type for SWAT+ output
#' @param val Value to format.
#' @param col_name Column name for special handling.
#' @param is_header If TRUE, format as header text not data.
#' @return Formatted string.
#' @keywords internal
swat_format_value <- function(val, col_name = "", is_header = FALSE) {
  if (is_header) {
    nm <- tolower(col_name)
    if (nm == "description" || nm == "desc") {
      return(if (is.null(nm) || is.na(nm)) "" else nm)
    }
    if (nm == "name" || nm == "file_name") {
      return(swat_string_pad(nm, SWAT_STR_PAD, "left"))
    }
    # Detect type from column name patterns for headers
    return(swat_string_pad(nm, SWAT_STR_PAD, "right"))
  }

  # Data value formatting
  if (col_name == "description" || col_name == "desc") {
    return(if (is.null(val) || is.na(val)) "" else as.character(val))
  }
  if (col_name == "name" || col_name == "file_name") {
    return(swat_string_pad(val, SWAT_STR_PAD, "left"))
  }
  if (is.logical(val)) {
    return(swat_bool_pad(val))
  }
  if (is.integer(val)) {
    return(swat_int_pad(val))
  }
  if (is.numeric(val)) {
    return(swat_num_pad(val))
  }
  swat_string_pad(val)
}

#' Write a database table to a SWAT+ formatted text file
#'
#' Generic writer that queries a database table and writes it in standard
#' SWAT+ fixed-width format. Mimics Python's write_default_table().
#'
#' @param con DBI database connection.
#' @param table_name Character. Name of the database table to query.
#' @param file_path Character. Full path for the output file.
#' @param version Character. Editor version string.
#' @param swat_version Character. SWAT+ version string.
#' @param ignore_id Logical. If TRUE, skip the 'id' column.
#' @param ignored_cols Character vector. Additional columns to skip.
#' @param query Character. Custom SQL query (overrides table_name).
#' @param write_count Logical. If TRUE, write row count line.
#' @param non_zero_min_cols Character vector. Columns to enforce non-zero minimum.
#' @param col_types Named list. Override column types: "int", "num", "str", "bool".
#' @param col_aligns Named list. Override column alignments.
#' @param col_pads Named list. Override column padding widths.
#' @param col_names Named list. Override header names for columns.
#' @return Invisible NULL.
#' @keywords internal
swat_write_table <- function(con, table_name, file_path,
                             version = NULL, swat_version = NULL,
                             ignore_id = FALSE, ignored_cols = character(0),
                             query = NULL, write_count = FALSE,
                             non_zero_min_cols = character(0),
                             col_types = list(), col_aligns = list(),
                             col_pads = list(), col_names = list()) {
  # Get data
  sql <- if (!is.null(query)) query else paste0("SELECT * FROM ", table_name, " ORDER BY id")
  data <- tryCatch(query_db(con, sql), error = function(e) data.frame())
  if (nrow(data) == 0) return(invisible(NULL))

  # Determine columns to write
  cols <- names(data)
  if (ignore_id) cols <- cols[cols != "id"]
  cols <- cols[!cols %in% ignored_cols]

  # Open file and write
  f <- file(file_path, "w")
  on.exit(close(f))

  # Meta line
  writeLines(swat_meta_line(file_path, version, swat_version), f)

  # Count line
  if (write_count) {
    writeLines(as.character(nrow(data)), f)
  }

  # Header line
  header_parts <- vapply(cols, function(cn) {
    display_name <- if (cn %in% names(col_names)) col_names[[cn]] else cn
    align <- if (cn %in% names(col_aligns)) col_aligns[[cn]] else {
      if (cn == "name" || cn == "file_name") "left" else "right"
    }
    pad <- if (cn %in% names(col_pads)) col_pads[[cn]] else NULL

    ctype <- if (cn %in% names(col_types)) col_types[[cn]] else NULL
    if (is.null(ctype)) {
      # Infer type from data
      sample_val <- data[[cn]][!is.na(data[[cn]])]
      if (length(sample_val) == 0) {
        ctype <- "str"
      } else {
        sample_val <- sample_val[1]
        if (is.integer(sample_val)) ctype <- "int"
        else if (is.numeric(sample_val)) ctype <- "num"
        else ctype <- "str"
      }
    }
    if (cn == "description" || cn == "desc") {
      return(tolower(display_name))
    }
    if (is.null(pad)) {
      pad <- switch(ctype,
                    int = SWAT_INT_PAD,
                    num = SWAT_NUM_PAD,
                    bool = SWAT_CODE_PAD,
                    SWAT_STR_PAD)
    }
    swat_string_pad(tolower(display_name), pad = pad, align = align)
  }, character(1))
  writeLines(paste0(header_parts, collapse = ""), f)

  # Data rows
  for (i in seq_len(nrow(data))) {
    row_parts <- vapply(cols, function(cn) {
      # Use sequential row number for id column (matching Python bug fix)
      val <- if (cn == "id") i else data[[cn]][i]
      align <- if (cn %in% names(col_aligns)) col_aligns[[cn]] else {
        if (cn == "name" || cn == "file_name") "left" else "right"
      }
      pad <- if (cn %in% names(col_pads)) col_pads[[cn]] else NULL

      ctype <- if (cn %in% names(col_types)) col_types[[cn]] else NULL
      if (is.null(ctype)) {
        if (is.integer(val)) ctype <- "int"
        else if (is.numeric(val)) ctype <- "num"
        else if (is.logical(val)) ctype <- "bool"
        else ctype <- "str"
      }

      if (cn == "description" || cn == "desc") {
        return(if (is.na(val) || is.null(val)) "" else as.character(val))
      }

      use_nzm <- cn %in% non_zero_min_cols

      if (is.null(pad)) {
        pad <- switch(ctype,
                      int = SWAT_INT_PAD,
                      num = SWAT_NUM_PAD,
                      bool = SWAT_CODE_PAD,
                      SWAT_STR_PAD)
      }

      switch(ctype,
             int = swat_int_pad(val, pad = pad, align = align),
             num = swat_num_pad(val, pad = pad, align = align, use_non_zero_min = use_nzm),
             bool = swat_bool_pad(val, pad = pad, align = align),
             swat_string_pad(val, pad = pad, align = align))
    }, character(1))
    writeLines(paste0(row_parts, collapse = ""), f)
  }

  invisible(NULL)
}

#' Get file names from file_cio for a classification section
#' @param con DBI connection.
#' @param section Character. Classification name (e.g. "simulation").
#' @return Character vector of file names.
#' @keywords internal
get_cio_file_names <- function(con, section) {
  # 1. Fixed the column name to default_file_name
  sql <- "SELECT f.default_file_name FROM file_cio f
          JOIN file_cio_classification c ON f.classification_id = c.id
          WHERE c.name = ? ORDER BY f.order_in_class"
  
  result <- tryCatch(
    query_db(con, sql, params = list(section)),
    error = function(e) {
      message("Database error: ", e$message) # Added message to see if it's actually crashing
      return(data.frame(default_file_name = character(0)))
    }
  )
  
  if (is.null(result) || nrow(result) == 0) return(character(0))
  
  # 2. Match the column name here as well
  result$default_file_name
}

#' Get count of rows in a table (safely)
#' @param con DBI connection.
#' @param table_name Table name.
#' @return Integer count, 0 if table doesn't exist.
#' @keywords internal
safe_count <- function(con, table_name) {
  tryCatch({
    result <- query_db(con, paste0("SELECT COUNT(*) as cnt FROM ", table_name))
    result$cnt[1]
  }, error = function(e) 0L)
}

#' Safely check if a table exists and has data
#' @param con DBI connection.
#' @param table_name Table name.
#' @return Logical.
#' @keywords internal
has_data <- function(con, table_name) {
  safe_count(con, table_name) > 0
}
