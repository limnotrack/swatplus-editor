#' Run the complete SWAT+ workflow
#'
#' Mirrors the functionality of \code{src/api/actions/run_all.py}.
#' Orchestrates the full sequence: project setup → WGN import →
#' weather import → write input files → run SWAT+ model →
#' import output.
#'
#' @keywords internal
NULL

# ===========================================================================
# Main entry point
# ===========================================================================

#' Run the complete SWAT+ modelling workflow
#'
#' High-level function that calls each stage in order.  All stages are
#' optional; set their controlling argument to \code{FALSE} or \code{NULL}
#' to skip.
#'
#' Mirrors \code{RunAll} in \code{src/api/actions/run_all.py}.
#'
#' @param project_db      Path to the SWAT+ project \code{.sqlite} database.
#' @param datasets_db     Path to the SWAT+ datasets \code{.sqlite} database.
#' @param swat_exe        Path to the SWAT+ executable, or \code{NULL} to
#'   skip model execution.
#' @param output_dir      Directory for SWAT+ input/output files.
#' @param output_db       Path to the output \code{.sqlite} database.
#' @param editor_version  Version string (default \code{"2.3.0"}).
#' @param project_name    Project name.
#' @param is_lte          Use LTE variant.  Default \code{FALSE}.
#'
#' @param run_setup       Run \code{\link{setup_project}}.  Default \code{TRUE}.
#' @param run_gis_import  Run \code{\link{import_gis}}.  Default \code{TRUE}.
#'
#' @param wgn_db          Path to WGN database, or \code{NULL} to skip WGN
#'   import.
#' @param wgn_table       WGN table name (default \code{"weather_wgn_cli"}).
#'
#' @param weather_dir     Directory containing observed weather files, or
#'   \code{NULL} to skip weather import.
#' @param weather_format  \code{"observed"} or \code{"2012"}.
#'
#' @param yrc_start       Simulation start year.
#' @param day_start       Simulation start Julian day (0 = start of year).
#' @param yrc_end         Simulation end year.
#' @param day_end         Simulation end Julian day (0 = end of year).
#' @param step            Time step (0 = daily).
#'
#' @param run_write_files Run \code{\link{write_swatplus_files}}.
#'   Default \code{TRUE}.
#' @param run_model       Run the SWAT+ executable.  Default \code{TRUE}
#'   (only executed if \code{swat_exe} is not \code{NULL}).
#' @param run_read_output Run \code{\link{read_output}} after model execution.
#'   Default \code{TRUE}.
#'
#' @param verbose         Print progress messages.  Default \code{TRUE}.
#' @return Invisibly, a named list summarising what was done.
#' @export
run_all <- function(project_db,
                    datasets_db     = NULL,
                    swat_exe        = NULL,
                    output_dir      = NULL,
                    output_db       = NULL,
                    editor_version  = "2.3.0",
                    project_name    = NULL,
                    is_lte          = FALSE,

                    run_setup       = TRUE,
                    run_gis_import  = TRUE,

                    wgn_db          = NULL,
                    wgn_table       = "weather_wgn_cli",

                    weather_dir     = NULL,
                    weather_format  = "observed",

                    yrc_start       = 1980L,
                    day_start       = 0L,
                    yrc_end         = 1985L,
                    day_end         = 0L,
                    step            = 0L,

                    run_write_files = TRUE,
                    run_model       = TRUE,
                    run_read_output = TRUE,

                    verbose         = TRUE) {

  result <- list(
    setup = FALSE, gis = FALSE, wgn = FALSE, weather = FALSE,
    write = FALSE, model = FALSE, output = FALSE
  )

  # -------------------------------------------------------------------------
  # 1. Project setup
  # -------------------------------------------------------------------------
  if (run_setup) {
    if (verbose) emit_progress(5, "Setting up project database...")
    setup_project(
      project_db      = project_db,
      datasets_db     = datasets_db,
      editor_version  = editor_version,
      project_name    = project_name,
      is_lte          = is_lte,
      run_gis_import  = run_gis_import,
      verbose         = verbose
    )
    result$setup <- TRUE
    result$gis   <- run_gis_import
  }

  # Open project connection for later configuration updates
  proj_con <- swat_open_db(project_db)

  # -------------------------------------------------------------------------
  # 2. Update input_files_dir if provided
  # -------------------------------------------------------------------------
  if (!is.null(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    rel_out <- tryCatch(
      sub(paste0("^", dirname(project_db), .Platform$file.sep), "",
          normalizePath(output_dir, mustWork = FALSE)),
      error = function(e) output_dir
    )
    DBI::dbExecute(proj_con,
      "UPDATE project_config SET input_files_dir = ?",
      params = list(rel_out))
  }

  # -------------------------------------------------------------------------
  # 3. WGN import
  # -------------------------------------------------------------------------
  if (!is.null(wgn_db)) {
    if (verbose) emit_progress(15, "Importing WGN data...")
    DBI::dbExecute(proj_con,
      "UPDATE project_config SET wgn_db = ?, wgn_table_name = ?",
      params = list(wgn_db, wgn_table))
    swat_close_db(proj_con)
    import_wgn(project_db, wgn_db, wgn_table, verbose = verbose)
    proj_con <- swat_open_db(project_db)
    result$wgn <- TRUE
  }

  # -------------------------------------------------------------------------
  # 4. Weather import
  # -------------------------------------------------------------------------
  if (!is.null(weather_dir)) {
    if (verbose) emit_progress(25, "Importing weather data...")
    DBI::dbExecute(proj_con,
      "UPDATE project_config SET weather_data_dir = ?, weather_data_format = ?",
      params = list(weather_dir, weather_format))
    swat_close_db(proj_con)
    import_weather(project_db, weather_dir, format = weather_format,
                   verbose = verbose)
    match_weather_stations(project_db, verbose = verbose)
    proj_con <- swat_open_db(project_db)
    result$weather <- TRUE
  }

  # -------------------------------------------------------------------------
  # 5. Update simulation time
  # -------------------------------------------------------------------------
  existing_ts <- swat_count(proj_con, "time_sim")
  if (existing_ts == 0L) {
    DBI::dbExecute(proj_con,
      "INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
       VALUES (?, ?, ?, ?, ?)",
      params = list(day_start, yrc_start, day_end, yrc_end, step))
  } else {
    DBI::dbExecute(proj_con,
      "UPDATE time_sim SET day_start=?, yrc_start=?, day_end=?, yrc_end=?, step=?",
      params = list(day_start, yrc_start, day_end, yrc_end, step))
  }

  swat_close_db(proj_con)

  # -------------------------------------------------------------------------
  # 6. Write input files
  # -------------------------------------------------------------------------
  resolved_output_dir <- output_dir
  if (is.null(resolved_output_dir)) {
    proj_con2 <- swat_open_db(project_db)
    cfg <- DBI::dbGetQuery(proj_con2, "SELECT input_files_dir FROM project_config LIMIT 1")
    swat_close_db(proj_con2)
    if (nrow(cfg) > 0L && !is.na(cfg$input_files_dir)) {
      resolved_output_dir <- full_path(project_db, cfg$input_files_dir)
    }
  }

  if (run_write_files && !is.null(resolved_output_dir)) {
    if (verbose) emit_progress(45, "Writing SWAT+ input files...")
    write_swatplus_files(project_db, resolved_output_dir, verbose = verbose)
    result$write <- TRUE
  }

  # -------------------------------------------------------------------------
  # 7. Run SWAT+ model
  # -------------------------------------------------------------------------
  model_success <- FALSE
  if (run_model && !is.null(swat_exe) && !is.null(resolved_output_dir)) {
    if (!file.exists(swat_exe))
      warning("SWAT+ executable not found: ", swat_exe)
    else {
      if (verbose) emit_progress(60, "Running SWAT+ model...")
      ret <- tryCatch(
        run_swatplus(
          swat_exe    = swat_exe,
          working_dir = resolved_output_dir,
          verbose     = verbose
        ),
        error = function(e) { warning("Model execution failed: ", conditionMessage(e)); NULL }
      )
      if (!is.null(ret) && isTRUE(ret$success)) {
        model_success <- TRUE
        result$model <- TRUE
        # Update run timestamp
        proj_con3 <- swat_open_db(project_db)
        DBI::dbExecute(proj_con3,
          "UPDATE project_config SET swat_last_run = datetime('now')")
        swat_close_db(proj_con3)
      }
    }
  }

  # -------------------------------------------------------------------------
  # 8. Read simulation output
  # -------------------------------------------------------------------------
  if (run_read_output && (model_success || (!run_model && !is.null(resolved_output_dir)))) {
    if (verbose) emit_progress(80, "Importing SWAT+ simulation output...")
    counts <- tryCatch(
      read_output(project_db,
                  output_db = output_db,
                  txt_dir   = resolved_output_dir,
                  verbose   = verbose),
      error = function(e) {
        warning("Output import failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(counts)) result$output <- TRUE
  }

  if (verbose) emit_progress(100, "Workflow complete.")
  invisible(result)
}
