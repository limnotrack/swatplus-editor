#' Read SWAT+ simulation output files into SQLite
#'
#' Mirrors the functionality of \code{src/api/actions/read_output.py}.
#' Reads the SWAT+ output CSV files produced by a simulation run and stores
#' them in the output SQLite database.
#'
#' @keywords internal
NULL

# ===========================================================================
# Main entry point
# ===========================================================================

#' Import SWAT+ simulation output into the output database
#'
#' Reads \code{files_out.out} from the SWAT+ text output directory to
#' discover output CSV files, then imports each into the output database.
#'
#' Mirrors \code{ReadOutput} in \code{src/api/actions/read_output.py}.
#'
#' @param project_db   Path to the project \code{.sqlite} database.
#' @param output_db    Path to the output \code{.sqlite} database (will be
#'   created if absent).
#' @param txt_dir      Directory containing the SWAT+ output text files
#'   (including \code{files_out.out}).  If \code{NULL} attempts to read from
#'   the \code{input_files_dir} stored in \code{project_config}.
#' @param batch_size   Rows per batch insert (default 100,000).
#' @param verbose      Print progress messages.  Default \code{TRUE}.
#' @return Invisibly, a named list of tables and row counts imported.
#' @export
read_output <- function(project_db,
                        output_db  = NULL,
                        txt_dir    = NULL,
                        batch_size = 100000L,
                        verbose    = TRUE) {

  if (!file.exists(project_db))
    stop("Project database not found: ", project_db)

  # Resolve output directory from project_config if not specified
  if (is.null(txt_dir)) {
    proj_con <- swat_open_db(project_db)
    cfg      <- DBI::dbGetQuery(proj_con,
      "SELECT input_files_dir FROM project_config LIMIT 1")
    swat_close_db(proj_con)
    if (nrow(cfg) == 0L || is.na(cfg$input_files_dir))
      stop("txt_dir not provided and not found in project_config.")
    txt_dir <- full_path(project_db, cfg$input_files_dir)
  }

  files_out_path <- file.path(txt_dir, "files_out.out")
  if (!file.exists(files_out_path))
    stop("'files_out.out' not found in: ", txt_dir)

  # Determine output database path
  if (is.null(output_db)) {
    output_db <- file.path(dirname(project_db), "swatplus_output.sqlite")
  }

  # Create or reset output database
  if (!file.exists(output_db)) {
    create_output_db(output_db)
  }

  out_con  <- swat_open_db(output_db)
  on.exit(swat_close_db(out_con), add = TRUE)

  # Copy project_config reference into output DB
  proj_con <- swat_open_db(project_db)
  cfg_row  <- DBI::dbGetQuery(proj_con,
    "SELECT * FROM project_config LIMIT 1")
  swat_close_db(proj_con)

  if (nrow(cfg_row) > 0L) {
    DBI::dbExecute(out_con, "DELETE FROM project_config")
    DBI::dbExecute(out_con,
      "INSERT INTO project_config (project_name, project_db, output_db)
       VALUES (?, ?, ?)",
      params = list(cfg_row$project_name, project_db, output_db))
  }

  # Parse files_out.out
  csv_files <- .parse_files_out(files_out_path)
  if (length(csv_files) == 0L) {
    if (verbose) message("No output CSV files listed in files_out.out")
    return(invisible(list()))
  }

  counts <- list()
  for (csv_rel in csv_files) {
    csv_path <- file.path(txt_dir, csv_rel)
    if (!file.exists(csv_path)) {
      if (verbose) message("Output file not found, skipping: ", csv_path)
      next
    }

    table_name <- .csv_to_table_name(csv_rel)
    if (verbose) emit_progress(-1L, paste0("Importing: ", csv_rel))

    n <- tryCatch(
      .import_csv_to_db(out_con, csv_path, table_name, batch_size),
      error = function(e) {
        warning("Error importing ", csv_rel, ": ", conditionMessage(e))
        -1L
      }
    )
    if (n >= 0L) counts[[table_name]] <- n
  }

  # Update project_config output timestamp
  proj_con2 <- swat_open_db(project_db)
  DBI::dbExecute(proj_con2,
    "UPDATE project_config SET output_last_imported = datetime('now')")
  swat_close_db(proj_con2)

  if (verbose)
    message("Output import complete. ",
            length(counts), " table(s) imported.")
  invisible(counts)
}

# ===========================================================================
# Internal helpers
# ===========================================================================

.parse_files_out <- function(path) {
  lines <- readLines(path, warn = FALSE)
  # Skip header lines; find lines that look like filenames
  csv_lines <- grep("\\.csv$", lines, value = TRUE, ignore.case = TRUE)
  trimws(csv_lines)
}

.csv_to_table_name <- function(csv_rel) {
  # "hru_wb_aa.csv" -> "hru_wb_aa"
  tbl <- tools::file_path_sans_ext(basename(csv_rel))
  clean_column_name(tbl)
}

#' Clean a string for use as an SQL identifier
#'
#' @param name Character string.
#' @return Cleaned character string suitable for use as a column/table name.
#' @export
clean_column_name <- function(name) {
  name <- trimws(name)
  name <- gsub("[^A-Za-z0-9_]", "_", name)
  if (grepl("^[0-9]", name)) name <- paste0("x", name)
  name
}

.import_csv_to_db <- function(con, csv_path, table_name, batch_size) {
  # Read CSV with flexible delimiter detection
  first_line <- readLines(csv_path, n = 2L, warn = FALSE)
  if (length(first_line) < 2L) return(0L)

  delim <- if (grepl("\t", first_line[[2L]])) "\t" else
            if (grepl(",",  first_line[[2L]])) "," else " "

  data <- tryCatch(
    utils::read.table(csv_path,
                      header    = TRUE,
                      sep       = delim,
                      comment.char = "",
                      stringsAsFactors = FALSE,
                      check.names = FALSE,
                      fill      = TRUE),
    error = function(e) NULL
  )
  if (is.null(data) || nrow(data) == 0L) return(0L)

  # Sanitise column names
  names(data) <- vapply(names(data), clean_column_name, character(1L))

  # Drop existing table and recreate
  if (swat_exists_table(con, table_name)) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", table_name))
  }

  # Type inference: try to keep numeric columns numeric
  data <- .infer_column_types(data)

  # Fix gis_id = 0 by extracting from 'name' column
  if ("gis_id" %in% names(data) && "name" %in% names(data)) {
    zero_idx <- which(as.integer(data$gis_id) == 0L)
    if (length(zero_idx) > 0L) {
      extracted <- suppressWarnings(
        as.integer(gsub("[^0-9]", "", data$name[zero_idx]))
      )
      data$gis_id[zero_idx[!is.na(extracted)]] <- extracted[!is.na(extracted)]
    }
  }

  swat_bulk_insert(con, table_name, data, batch_size)
}

.infer_column_types <- function(df) {
  for (col in names(df)) {
    v <- df[[col]]
    if (is.character(v)) {
      num_try <- suppressWarnings(as.numeric(v))
      if (!any(is.na(num_try[!is.na(v)]))) {
        df[[col]] <- num_try
      }
    }
  }
  df
}

# ===========================================================================
# Reset output database
# ===========================================================================

#' Drop all imported output tables from the output database
#'
#' @param output_db Path to the output \code{.sqlite} database.
#' @export
reset_output_db <- function(output_db) {
  if (!file.exists(output_db)) return(invisible(NULL))
  con <- swat_open_db(output_db)
  on.exit(swat_close_db(con), add = TRUE)
  # Keep metadata tables, drop everything else
  keep <- c("project_config", "table_description", "column_description",
            "sqlite_sequence")
  all_tables <- swat_get_table_names(con)
  to_drop <- setdiff(all_tables, keep)
  for (tbl in to_drop) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  }
  invisible(length(to_drop))
}
