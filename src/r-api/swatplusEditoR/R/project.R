# Project management for swatplusEditoR
# Functions to initialize, load, and manage SWAT+ projects

#' Load a SWAT+ project from an R project object
#'
#' Takes a project list object (as produced by upstream GIS/delineation tools)
#' and validates that it has the required database file for SWAT+ Editor
#' operations.
#'
#' @param project List. A project object with at minimum \code{project_dir}
#'   and \code{db_file} fields. The \code{db_file} should point to the SQLite
#'   database containing gis_* tables.
#' @return The project object with a validated database connection confirmed.
#' @export
#' @examples
#' \dontrun{
#' project <- list(
#'   project_dir = "/path/to/project",
#'   db_file = "/path/to/project/project.sqlite"
#' )
#' project <- load_project(project)
#' }
load_project <- function(project) {
  if (!is.list(project)) {
    stop("project must be a list.", call. = FALSE)
  }

  validate_project(project)

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  tables <- list_db_tables(con)
  gis_tables <- c("gis_aquifers", "gis_channels", "gis_deep_aquifers",
                   "gis_hrus", "gis_lsus", "gis_points", "gis_routing",
                   "gis_subbasins", "gis_water")
  present <- gis_tables[gis_tables %in% tables]
  missing <- gis_tables[!gis_tables %in% tables]

  message("SWAT+ project loaded from: ", project$db_file)
  message("  GIS tables present: ", length(present), "/", length(gis_tables))
  if (length(missing) > 0) {
    message("  Missing GIS tables: ", paste(missing, collapse = ", "))
  }
  message("  Total tables in database: ", length(tables))

  project
}

#' Create a new SWAT+ project database
#'
#' Creates an empty SQLite database with the GIS tables populated from
#' a project object's spatial data. This initializes the database schema
#' matching the SWAT+ Editor format.
#'
#' @param project List. A project object.
#' @param db_path Character. Path where the SQLite database will be created.
#' @param overwrite Logical. If TRUE, overwrite an existing database.
#' @return The project object with \code{db_file} set to the new database path.
#' @export
#' @importFrom DBI dbWriteTable
#' @examples
#' \dontrun{
#' project <- list(project_dir = "/path/to/project")
#' project <- create_project_db(project, "/path/to/project/swatplus.sqlite")
#' }
create_project_db <- function(project, db_path, overwrite = FALSE) {
  if (!is.list(project)) {
    stop("project must be a list.", call. = FALSE)
  }

  if (file.exists(db_path) && !overwrite) {
    stop("Database already exists at: ", db_path,
         ". Use overwrite = TRUE to replace.", call. = FALSE)
  }

  if (file.exists(db_path) && overwrite) {
    file.remove(db_path)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(close_db(con))

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  # Create GIS tables matching SWAT+ Editor schema
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_subbasins (
      id INTEGER PRIMARY KEY,
      area REAL, slo1 REAL, len1 REAL, sll REAL,
      lat REAL, lon REAL, elev REAL, elevmin REAL, elevmax REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_channels (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER, areac REAL, strahler INTEGER,
      len2 REAL, slo2 REAL, wid2 REAL, dep2 REAL,
      elevmin REAL, elevmax REAL, midlat REAL, midlon REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_hrus (
      id INTEGER PRIMARY KEY,
      lsu INTEGER, arsub REAL, arlsu REAL,
      landuse TEXT, arland REAL, soil TEXT, arso REAL,
      slp TEXT, arslp REAL, slope REAL,
      lat REAL, lon REAL, elev REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_lsus (
      id INTEGER PRIMARY KEY,
      category INTEGER, channel INTEGER, area REAL, slope REAL,
      len1 REAL, csl REAL, wid1 REAL, dep1 REAL,
      lat REAL, lon REAL, elev REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_aquifers (
      id INTEGER PRIMARY KEY,
      category INTEGER, subbasin INTEGER, deep_aquifer INTEGER,
      area REAL, lat REAL, lon REAL, elev REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_deep_aquifers (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER, area REAL, lat REAL, lon REAL, elev REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_water (
      id INTEGER PRIMARY KEY,
      wtype TEXT, lsu INTEGER, subbasin INTEGER, area REAL,
      xpr REAL, ypr REAL, lat REAL, lon REAL, elev REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_points (
      id INTEGER PRIMARY KEY,
      subbasin INTEGER, ptype TEXT,
      xpr REAL, ypr REAL, lat REAL, lon REAL, elev REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS gis_routing (
      sourceid INTEGER PRIMARY KEY,
      sourcecat TEXT, hyd_typ TEXT,
      sinkid INTEGER, sinkcat TEXT, percent REAL
    )")

  # Create project config table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS project_config (
      id INTEGER PRIMARY KEY,
      project_name TEXT,
      editor_version TEXT,
      reference_db TEXT,
      wgn_db TEXT,
      weather_data_dir TEXT,
      weather_data_format TEXT DEFAULT 'observed',
      input_files_dir TEXT DEFAULT 'TxtInOut',
      input_files_last_written TEXT,
      swat_last_run TEXT,
      output_last_imported TEXT,
      swat_exe_filename TEXT DEFAULT 'swatplus',
      gis_version TEXT,
      gis_type TEXT,
      is_lte INTEGER DEFAULT 0,
      use_gwflow INTEGER DEFAULT 0,
      netcdf_data_file TEXT
    )")

  # Create weather tables
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS weather_sta_cli (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      lat REAL, lon REAL, elev REAL,
      pcp TEXT, tmp TEXT, slr TEXT, hmd TEXT, wnd TEXT, pet TEXT,
      atmo_dep TEXT,
      wgn_id INTEGER
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS weather_wgn_cli (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      lat REAL, lon REAL, elev REAL, rain_yrs REAL
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS weather_wgn_cli_mon (
      id INTEGER PRIMARY KEY,
      weather_wgn_cli_id INTEGER NOT NULL,
      month INTEGER NOT NULL,
      tmp_max_ave REAL, tmp_min_ave REAL, tmp_max_sd REAL, tmp_min_sd REAL,
      pcp_ave REAL, pcp_sd REAL, pcp_skew REAL,
      wet_dry REAL, wet_wet REAL, pcp_days REAL, pcp_hhr REAL,
      slr_ave REAL, dew_ave REAL, wnd_ave REAL,
      FOREIGN KEY (weather_wgn_cli_id) REFERENCES weather_wgn_cli(id)
    )")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS weather_file (
      id INTEGER PRIMARY KEY,
      filename TEXT, type TEXT,
      lat REAL, lon REAL
    )")

  # Populate project config
  project_name <- basename(project$project_dir)
  DBI::dbExecute(con, "
    INSERT INTO project_config (project_name, editor_version, input_files_dir)
    VALUES (?, '3.2.0', 'TxtInOut')",
    params = list(project_name))

  # Populate GIS tables from project data if available
  if (!is.null(project$basin_data) && is.data.frame(project$basin_data)) {
    populate_gis_from_project(con, project)
  }

  project$db_file <- normalizePath(db_path, mustWork = TRUE)
  message("Created SWAT+ project database: ", project$db_file)
  project
}

#' Populate GIS tables from project basin/HRU data
#'
#' @param con DBI connection.
#' @param project Project list object.
#' @keywords internal
populate_gis_from_project <- function(con, project) {
  if (!is.null(project$basin_data) && is.data.frame(project$basin_data)) {
    basin <- project$basin_data
    gis_cols <- c("id", "area", "slo1", "len1", "sll", "lat", "lon",
                  "elev", "elevmin", "elevmax")
    avail_cols <- intersect(gis_cols, names(basin))
    if (length(avail_cols) > 0) {
      DBI::dbWriteTable(con, "gis_subbasins", basin[, avail_cols, drop = FALSE],
                        append = TRUE)
    }
  }

  if (!is.null(project$hru_data) && is.data.frame(project$hru_data)) {
    hru <- project$hru_data
    gis_cols <- c("id", "lsu", "arsub", "arlsu", "landuse", "arland",
                  "soil", "arso", "slp", "arslp", "slope", "lat", "lon",
                  "elev")
    avail_cols <- intersect(gis_cols, names(hru))
    if (length(avail_cols) > 0) {
      DBI::dbWriteTable(con, "gis_hrus", hru[, avail_cols, drop = FALSE],
                        append = TRUE)
    }
  }
}

#' Get project configuration from the database
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A list with project configuration values.
#' @export
#' @examples
#' \dontrun{
#' config <- get_project_config(project)
#' }
get_project_config <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "project_config")) {
    stop("No project_config table found in database.", call. = FALSE)
  }

  result <- query_db(con, "SELECT * FROM project_config LIMIT 1")
  if (nrow(result) == 0) {
    stop("No project configuration found.", call. = FALSE)
  }
  as.list(result[1, ])
}

#' Get project information summary
#'
#' Returns a summary of the project including GIS table row counts,
#' weather station status, and configuration.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A list with project summary information.
#' @export
#' @examples
#' \dontrun{
#' info <- get_project_info(project)
#' }
get_project_info <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  tables <- list_db_tables(con)

  # Count rows in GIS tables
  gis_tables <- c("gis_aquifers", "gis_channels", "gis_deep_aquifers",
                   "gis_hrus", "gis_lsus", "gis_points", "gis_routing",
                   "gis_subbasins", "gis_water")
  gis_counts <- vapply(gis_tables, function(tbl) {
    if (tbl %in% tables) {
      query_db(con, paste("SELECT COUNT(*) as n FROM", tbl))$n
    } else {
      NA_integer_
    }
  }, integer(1))
  names(gis_counts) <- gis_tables

  # Check weather status
  weather_stations <- 0L
  weather_generators <- 0L
  if ("weather_sta_cli" %in% tables) {
    weather_stations <- query_db(con,
      "SELECT COUNT(*) as n FROM weather_sta_cli")$n
  }
  if ("weather_wgn_cli" %in% tables) {
    weather_generators <- query_db(con,
      "SELECT COUNT(*) as n FROM weather_wgn_cli")$n
  }

  # Get config
  config <- NULL
  if ("project_config" %in% tables) {
    cfg <- query_db(con, "SELECT * FROM project_config LIMIT 1")
    if (nrow(cfg) > 0) config <- as.list(cfg[1, ])
  }

  # Check gwflow
  use_gwflow <- FALSE
  if (!is.null(config) && !is.null(config$use_gwflow)) {
    use_gwflow <- as.logical(config$use_gwflow)
  }

  list(
    name = if (!is.null(config)) config$project_name else basename(project$project_dir),
    db_file = project$db_file,
    total_tables = length(tables),
    gis_counts = gis_counts,
    status = list(
      imported_weather = weather_stations > 0 && weather_generators > 0,
      weather_stations = weather_stations,
      weather_generators = weather_generators,
      use_gwflow = use_gwflow,
      wrote_inputs = !is.null(config$input_files_last_written),
      ran_swat = !is.null(config$swat_last_run)
    ),
    config = config
  )
}
