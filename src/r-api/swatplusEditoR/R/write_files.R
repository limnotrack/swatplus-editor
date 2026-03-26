# Write SWAT+ model configuration files
# Invokes the SWAT+ Editor Python API to write input files

#' Write SWAT+ model configuration files
#'
#' Writes all SWAT+ input files (TxtInOut) from the project database using
#' the SWAT+ Editor Python API. This is the main function to generate the
#' model configuration files required to run SWAT+.
#'
#' The function works by either:
#' \enumerate{
#'   \item Calling the Python API directly via system command if the
#'         \code{editor_exe} is provided
#'   \item Sending HTTP requests to a running SWAT+ Editor API server
#'   \item Writing files directly from database tables (for simple file types)
#' }
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param output_dir Character. Directory where the input files will be
#'   written. Defaults to "TxtInOut" in the project directory.
#' @param editor_exe Character. Path to the SWAT+ Editor executable/script.
#'   If NULL, attempts to use a running API server or falls back to direct
#'   database writing.
#' @param api_url Character. URL of a running SWAT+ Editor API server.
#'   Default is "http://localhost:5000".
#' @param swat_version Character. SWAT+ version string. Default "60".
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Using the Python API executable
#' write_config_files(project, editor_exe = "/path/to/swatplus_api.py")
#'
#' # Using a running API server
#' write_config_files(project, api_url = "http://localhost:5000")
#'
#' # Direct database write (limited file types)
#' write_config_files(project)
#' }
write_config_files <- function(project, output_dir = NULL,
                               editor_exe = NULL,
                               api_url = "http://localhost:5000",
                               swat_version = "60") {
  validate_project(project)

  if (is.null(output_dir)) {
    output_dir <- file.path(project$project_dir, "TxtInOut")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Update output dir in project config
  con <- open_project_db(project$db_file)
  if (table_exists(con, "project_config")) {
    # Store relative path
    norm_out <- normalizePath(output_dir, mustWork = FALSE)
    norm_proj <- normalizePath(project$project_dir, mustWork = FALSE)
    rel_path <- if (startsWith(norm_out, norm_proj)) {
      trimmed <- substring(norm_out, nchar(norm_proj) + 1)
      sub("^[/\\\\]+", "", trimmed)
    } else {
      output_dir
    }
    execute_db(con,
      "UPDATE project_config SET input_files_dir = ?",
      params = list(rel_path))
  }
  close_db(con)

  # Strategy 1: Use editor executable
  if (!is.null(editor_exe)) {
    return(write_via_exe(project, editor_exe, output_dir, swat_version))
  }

  # Strategy 2: Use running API server
  if (api_server_available(api_url)) {
    return(write_via_api(project, api_url))
  }

  # Strategy 3: Direct database write
  write_direct(project, output_dir)
}

#' Write configuration files via SWAT+ Editor executable
#'
#' @param project Project list object.
#' @param editor_exe Path to swatplus_api.py or compiled executable.
#' @param output_dir Output directory for files.
#' @param swat_version SWAT+ version string.
#' @return The project object (invisibly).
#' @keywords internal
write_via_exe <- function(project, editor_exe, output_dir, swat_version) {
  if (!file.exists(editor_exe)) {
    stop("SWAT+ Editor executable not found: ", editor_exe, call. = FALSE)
  }

  # Determine if it's a Python script or compiled exe
  is_python <- grepl("\\.py$", editor_exe)

  cmd <- if (is_python) {
    paste("python", shQuote(editor_exe))
  } else {
    shQuote(editor_exe)
  }

  args <- paste(
    "--action write",
    "--project_db_file", shQuote(project$db_file),
    "--swat_version", shQuote(swat_version)
  )

  message("Writing SWAT+ input files via editor...")
  result <- system(paste(cmd, args), intern = TRUE)

  # Update timestamp
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  if (table_exists(con, "project_config")) {
    execute_db(con,
      "UPDATE project_config SET input_files_last_written = datetime('now')")
  }

  message("SWAT+ input files written to: ", output_dir)
  invisible(project)
}

#' Check if SWAT+ Editor API server is available
#'
#' @param api_url API base URL.
#' @return Logical. TRUE if server responds.
#' @keywords internal
api_server_available <- function(api_url) {
  tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(paste0(api_url, "/setup/config"),
                        httr::timeout(2))
      return(httr::status_code(resp) < 500)
    }
    FALSE
  }, error = function(e) {
    FALSE
  })
}

#' Write configuration files via running API server
#'
#' @param project Project list object.
#' @param api_url API base URL.
#' @return The project object (invisibly).
#' @keywords internal
write_via_api <- function(project, api_url) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for API server communication. ",
         "Install with: install.packages('httr')", call. = FALSE)
  }

  message("Writing SWAT+ input files via API server...")

  # Get run settings first
  resp <- httr::GET(
    paste0(api_url, "/setup/run-settings"),
    httr::add_headers("Project-Db" = project$db_file)
  )

  if (httr::status_code(resp) != 200) {
    stop("Failed to get run settings from API: ",
         httr::content(resp, "text"), call. = FALSE)
  }

  # Trigger write
  resp <- httr::PUT(
    paste0(api_url, "/setup/run-settings"),
    httr::add_headers("Project-Db" = project$db_file),
    body = httr::content(resp, "parsed"),
    encode = "json"
  )

  if (httr::status_code(resp) != 200) {
    warning("API write returned status: ", httr::status_code(resp),
            call. = FALSE)
  }

  message("SWAT+ input files written via API")
  invisible(project)
}

#' Write basic configuration files directly from database
#'
#' Writes a subset of SWAT+ input files directly from the database without
#' requiring the Python API. Covers the most common file types.
#'
#' @param project Project list object.
#' @param output_dir Output directory.
#' @return The project object (invisibly).
#' @keywords internal
write_direct <- function(project, output_dir) {
  message("Writing SWAT+ input files directly from database...")
  message("Note: For complete file writing, use editor_exe or api_url.")

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  tables <- list_db_tables(con)

  # Write time.sim
  if ("time_sim" %in% tables) {
    write_time_sim(con, output_dir)
  }

  # Write weather station file
  if ("weather_sta_cli" %in% tables) {
    write_weather_sta(con, output_dir)
  }

  # Write file.cio (master config)
  write_file_cio(output_dir)

  # Update timestamp
  if ("project_config" %in% tables) {
    execute_db(con,
      "UPDATE project_config SET input_files_last_written = datetime('now')")
  }

  message("Basic SWAT+ input files written to: ", output_dir)
  invisible(project)
}

#' Write time.sim file
#'
#' @param con Database connection.
#' @param output_dir Output directory.
#' @keywords internal
write_time_sim <- function(con, output_dir) {
  data <- query_db(con, "SELECT * FROM time_sim LIMIT 1")
  if (nrow(data) == 0) return(invisible(NULL))

  lines <- c(
    "time.sim: simulation time settings",
    sprintf("  %d  %d  %d  %d  %d",
            data$day_start, data$yrc_start,
            data$day_end, data$yrc_end, data$step)
  )

  writeLines(lines, file.path(output_dir, "time.sim"))
}

#' Write weather station CLI file
#'
#' @param con Database connection.
#' @param output_dir Output directory.
#' @keywords internal
write_weather_sta <- function(con, output_dir) {
  stations <- query_db(con,
    "SELECT name, pcp, tmp, slr, hmd, wnd, pet, atmo_dep, lat, lon, elev
     FROM weather_sta_cli ORDER BY id")
  if (nrow(stations) == 0) return(invisible(NULL))

  lines <- c(
    "weather-sta.cli: weather station data",
    sprintf("  %d", nrow(stations))
  )
  for (i in seq_len(nrow(stations))) {
    s <- stations[i, ]
    lines <- c(lines, sprintf("%-20s %-20s %-20s %-20s %-20s %-20s %-20s %-20s %12.5f %12.5f %8.2f",
      if (is.na(s$name)) "null" else s$name,
      if (is.na(s$pcp)) "null" else s$pcp,
      if (is.na(s$tmp)) "null" else s$tmp,
      if (is.na(s$slr)) "null" else s$slr,
      if (is.na(s$hmd)) "null" else s$hmd,
      if (is.na(s$wnd)) "null" else s$wnd,
      if (is.na(s$pet)) "null" else s$pet,
      if (is.na(s$atmo_dep)) "null" else s$atmo_dep,
      if (is.na(s$lat)) 0 else s$lat,
      if (is.na(s$lon)) 0 else s$lon,
      if (is.na(s$elev)) 0 else s$elev
    ))
  }

  writeLines(lines, file.path(output_dir, "weather-sta.cli"))
}

#' Write file.cio (main SWAT+ configuration file)
#'
#' @param output_dir Output directory.
#' @keywords internal
write_file_cio <- function(output_dir) {
  lines <- c(
    "file.cio: file configuration for SWAT+",
    "  Master watershed file",
    "  simulation        sim",
    "  basin              basin",
    "  climate            climate",
    "  connect            connect",
    "  channel            channel",
    "  reservoir          reservoir",
    "  routing_unit       routing_unit",
    "  hru                hru",
    "  exco               exco",
    "  recall             recall",
    "  dr                 dr",
    "  aquifer            aquifer",
    "  hrd                hrd",
    "  water_rights       water_rights",
    "  link               link",
    "  basin_output       basin_output",
    "  hru_output         hru_output",
    "  basin_ls_output    basin_ls_output",
    "  lsreg_output       lsreg_output",
    "  basin_ch_output    basin_ch_output",
    "  basin_aqu_output   basin_aqu_output",
    "  basin_res_output   basin_res_output",
    "  basin_sd_output    basin_sd_output",
    "  basin_psc_output   basin_psc_output",
    "  region             region",
    "  ls_unit            ls_unit",
    "  soils              soils",
    "  decision_table     decision_table",
    "  init               init",
    "  lum                lum",
    "  chg                chg",
    "  ops                ops"
  )

  writeLines(lines, file.path(output_dir, "file.cio"))
}

#' Run the SWAT+ model executable
#'
#' Executes the SWAT+ model after configuration files have been written.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param swat_exe Character. Path to the SWAT+ executable. If NULL,
#'   looks for "swatplus" in the project's input files directory or PATH.
#' @param output_dir Character. Directory containing the input files.
#'   Defaults to "TxtInOut" in the project directory.
#' @param quiet Logical. If TRUE, suppress SWAT+ output. Default FALSE.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' project <- write_config_files(project)
#' run_swatplus(project, swat_exe = "/path/to/swatplus")
#' }
run_swatplus <- function(project, swat_exe = NULL, output_dir = NULL,
                         quiet = FALSE) {
  validate_project(project)

  if (is.null(output_dir)) {
    output_dir <- file.path(project$project_dir, "TxtInOut")
  }

  if (!dir.exists(output_dir)) {
    stop("Output directory does not exist: ", output_dir,
         ". Run write_config_files() first.", call. = FALSE)
  }

  if (is.null(swat_exe)) {
    # Try to find from config
    con <- open_project_db(project$db_file)
    cfg <- query_db(con, "SELECT swat_exe_filename FROM project_config LIMIT 1")
    close_db(con)

    if (nrow(cfg) > 0 && !is.na(cfg$swat_exe_filename)) {
      # Look in output_dir first
      exe_in_dir <- file.path(output_dir, cfg$swat_exe_filename)
      if (file.exists(exe_in_dir)) {
        swat_exe <- exe_in_dir
      } else {
        swat_exe <- cfg$swat_exe_filename
      }
    } else {
      swat_exe <- "swatplus"
    }
  }

  # Verify executable exists
  exe_found <- file.exists(swat_exe) || nchar(Sys.which(swat_exe)) > 0
  if (!exe_found) {
    stop("SWAT+ executable not found: ", swat_exe, call. = FALSE)
  }

  old_wd <- setwd(output_dir)
  on.exit(setwd(old_wd))

  message("Running SWAT+ model...")
  if (quiet) {
    system2(swat_exe, stdout = FALSE, stderr = FALSE)
  } else {
    system2(swat_exe)
  }

  # Update timestamp
  con <- open_project_db(project$db_file)
  on.exit(close_db(con), add = TRUE)
  if (table_exists(con, "project_config")) {
    execute_db(con,
      "UPDATE project_config SET swat_last_run = datetime('now')")
  }

  message("SWAT+ model run complete")
  invisible(project)
}
