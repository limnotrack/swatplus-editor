# Groundwater flow (GWFLOW) configuration for swatplusEditoR
# Enable, configure, and manage GWFLOW settings

#' Get GWFLOW status
#'
#' Checks whether GWFLOW is enabled and available in the project.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A list with \code{use_gwflow} (logical) and \code{can_enable}
#'   (logical).
#' @export
#' @examples
#' \dontrun{
#' status <- get_gwflow_status(project)
#' status$use_gwflow
#' }
get_gwflow_status <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  tables <- list_db_tables(con)

  use_gwflow <- FALSE
  can_enable <- FALSE

  if ("project_config" %in% tables) {
    cfg <- query_db(con, "SELECT use_gwflow FROM project_config LIMIT 1")
    if (nrow(cfg) > 0 && !is.na(cfg$use_gwflow)) {
      use_gwflow <- as.logical(cfg$use_gwflow)
    }
  }

  # GWFLOW can be enabled if grid tables exist
  can_enable <- "gwflow_grid" %in% tables &&
    query_db(con, "SELECT COUNT(*) as n FROM gwflow_grid")$n > 0

  list(use_gwflow = use_gwflow, can_enable = can_enable)
}

#' Initialize GWFLOW in the project
#'
#' Enables the groundwater flow module and creates the necessary database
#' tables if they don't exist.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param cell_size Numeric. Grid cell size in map units.
#' @param row_count Integer. Number of grid rows.
#' @param col_count Integer. Number of grid columns.
#' @param boundary_conditions Integer. Boundary condition type.
#' @param recharge Integer. Recharge option (1 = soil water, 2 = specific rate).
#' @param soil_transfer Integer. Soil transfer option.
#' @param saturation_excess Integer. Saturation excess routing.
#' @param external_pumping Integer. External pumping flag.
#' @param tile_drainage Integer. Tile drainage flag.
#' @param reservoir_exchange Integer. Reservoir exchange flag.
#' @param wetland_exchange Integer. Wetland exchange flag.
#' @param floodplain_exchange Integer. Floodplain exchange flag.
#' @param canal_seepage Integer. Canal seepage flag.
#' @param solute_transport Integer. Solute transport flag.
#' @param daily_output Integer. Daily output flag.
#' @param annual_output Integer. Annual output flag.
#' @param aa_output Integer. Average annual output flag.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' init_gwflow(project, cell_size = 200, row_count = 100,
#'             col_count = 150)
#' }
init_gwflow <- function(project, cell_size, row_count, col_count,
                        boundary_conditions = 1, recharge = 1,
                        soil_transfer = 0, saturation_excess = 0,
                        external_pumping = 0, tile_drainage = 0,
                        reservoir_exchange = 0, wetland_exchange = 0,
                        floodplain_exchange = 0, canal_seepage = 0,
                        solute_transport = 0, daily_output = 0,
                        annual_output = 0, aa_output = 0) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  
  gw_flow_ini_file <- system.file("extdata", "gwflow.ini",
                                  package = "swatplusEditoR")

  # Create gwflow tables if needed
  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_base (
      cell_size REAL, row_count INTEGER, col_count INTEGER,
      boundary_conditions INTEGER DEFAULT 1,
      recharge INTEGER DEFAULT 1,
      soil_transfer INTEGER DEFAULT 0,
      saturation_excess INTEGER DEFAULT 0,
      external_pumping INTEGER DEFAULT 0,
      tile_drainage INTEGER DEFAULT 0,
      reservoir_exchange INTEGER DEFAULT 0,
      wetland_exchange INTEGER DEFAULT 0,
      floodplain_exchange INTEGER DEFAULT 0,
      canal_seepage INTEGER DEFAULT 0,
      solute_transport INTEGER DEFAULT 0,
      daily_output INTEGER DEFAULT 0,
      annual_output INTEGER DEFAULT 0,
      aa_output INTEGER DEFAULT 0
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_grid (
      cell_id INTEGER PRIMARY KEY,
      status INTEGER DEFAULT 1,
      row INTEGER, col INTEGER,
      lat REAL, lon REAL, elev REAL,
      zone INTEGER DEFAULT 1
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_zone (
      zone_id INTEGER PRIMARY KEY,
      aquifer_k REAL DEFAULT 10.0,
      specific_yield REAL DEFAULT 0.1,
      streambed_k REAL DEFAULT 1.0,
      streambed_thickness REAL DEFAULT 1.0
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_hrucell (
      id INTEGER PRIMARY KEY,
      cell_id INTEGER,
      hru_id INTEGER,
      FOREIGN KEY (cell_id) REFERENCES gwflow_grid(cell_id)
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_fpcell (
      cell_id INTEGER PRIMARY KEY,
      channel INTEGER,
      area_m2 REAL,
      conductivity REAL DEFAULT 1.0,
      FOREIGN KEY (cell_id) REFERENCES gwflow_grid(cell_id)
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_rivcell (
      cell_id INTEGER PRIMARY KEY,
      channel INTEGER,
      FOREIGN KEY (cell_id) REFERENCES gwflow_grid(cell_id)
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_lsucell (
      id INTEGER PRIMARY KEY,
      cell_id INTEGER,
      lsu_id INTEGER,
      FOREIGN KEY (cell_id) REFERENCES gwflow_grid(cell_id)
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_rescell (
      cell_id INTEGER PRIMARY KEY,
      res INTEGER,
      res_stage REAL DEFAULT 0.0,
      FOREIGN KEY (cell_id) REFERENCES gwflow_grid(cell_id)
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_wetland (
      wet_id INTEGER PRIMARY KEY,
      thickness REAL DEFAULT 1.0
    )")

  execute_db(con, "
    CREATE TABLE IF NOT EXISTS gwflow_solutes (
      solute_name TEXT,
      sorption REAL DEFAULT 0.0,
      rate_const REAL DEFAULT 0.0,
      canal_irr REAL DEFAULT 0.0,
      init_data INTEGER DEFAULT 0,
      init_conc REAL DEFAULT 0.0
    )")

  # Insert or update gwflow_base
  result <- query_db(con, "SELECT COUNT(*) as n FROM gwflow_base")
  if (result$n == 0) {
    execute_db(con, "
      INSERT INTO gwflow_base
        (cell_size, row_count, col_count, boundary_conditions, recharge,
         soil_transfer, saturation_excess, external_pumping, tile_drainage,
         reservoir_exchange, wetland_exchange, floodplain_exchange,
         canal_seepage, solute_transport, daily_output, annual_output,
         aa_output)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(cell_size, row_count, col_count,
                    boundary_conditions, recharge, soil_transfer,
                    saturation_excess, external_pumping, tile_drainage,
                    reservoir_exchange, wetland_exchange,
                    floodplain_exchange, canal_seepage, solute_transport,
                    daily_output, annual_output, aa_output))
  } else {
    execute_db(con, "
      UPDATE gwflow_base SET cell_size = ?, row_count = ?, col_count = ?,
        boundary_conditions = ?, recharge = ?, soil_transfer = ?,
        saturation_excess = ?, external_pumping = ?, tile_drainage = ?,
        reservoir_exchange = ?, wetland_exchange = ?,
        floodplain_exchange = ?, canal_seepage = ?,
        solute_transport = ?, daily_output = ?, annual_output = ?,
        aa_output = ?",
      params = list(cell_size, row_count, col_count,
                    boundary_conditions, recharge, soil_transfer,
                    saturation_excess, external_pumping, tile_drainage,
                    reservoir_exchange, wetland_exchange,
                    floodplain_exchange, canal_seepage, solute_transport,
                    daily_output, annual_output, aa_output))
  }

  # Enable gwflow in project config
  if (table_exists(con, "project_config")) {
    execute_db(con, "UPDATE project_config SET use_gwflow = 1")
  }

  # Create default zone if none exists
  zone_count <- query_db(con, "SELECT COUNT(*) as n FROM gwflow_zone")$n
  if (zone_count == 0) {
    execute_db(con, "
      INSERT INTO gwflow_zone (zone_id, aquifer_k, specific_yield,
        streambed_k, streambed_thickness)
      VALUES (1, 10.0, 0.1, 1.0, 1.0)")
  }

  message("Initialized GWFLOW: ", row_count, " x ", col_count,
          " grid (cell size = ", cell_size, ")")
  invisible(project)
}

#' Get GWFLOW base configuration
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A list with GWFLOW base settings.
#' @export
get_gwflow_base <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "gwflow_base")) {
    stop("gwflow_base table not found. Initialize GWFLOW first with ",
         "init_gwflow().", call. = FALSE)
  }

  result <- query_db(con, "SELECT * FROM gwflow_base")
  if (nrow(result) == 0) {
    stop("No GWFLOW base configuration found.", call. = FALSE)
  }
  as.list(result[1, ])
}

#' Update GWFLOW base configuration
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param ... Named arguments for GWFLOW base settings to update (e.g.,
#'   cell_size, recharge, solute_transport).
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' update_gwflow_base(project, recharge = 2, daily_output = 1)
#' }
update_gwflow_base <- function(project, ...) {
  validate_project(project)
  updates <- list(...)

  if (length(updates) == 0) {
    stop("No update fields provided.", call. = FALSE)
  }

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "gwflow_base")) {
    stop("gwflow_base table not found.", call. = FALSE)
  }

  sql <- build_update_sql("gwflow_base", updates, "1 = 1")
  execute_db(con, sql)

  message("Updated GWFLOW base settings")
  invisible(project)
}

#' Get GWFLOW zones
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with zone parameters.
#' @export
get_gwflow_zones <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "gwflow_zone")) {
    stop("gwflow_zone table not found.", call. = FALSE)
  }

  query_db(con, "SELECT * FROM gwflow_zone ORDER BY zone_id")
}

#' Update GWFLOW zone parameters
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param zone_id Integer. ID of the zone to update.
#' @param aquifer_k Numeric. Hydraulic conductivity (m/day).
#' @param specific_yield Numeric. Specific yield (dimensionless).
#' @param streambed_k Numeric. Streambed conductivity (m/day).
#' @param streambed_thickness Numeric. Streambed thickness (m).
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' update_gwflow_zones(project, zone_id = 1, aquifer_k = 15.0,
#'                     specific_yield = 0.15)
#' }
update_gwflow_zones <- function(project, zone_id, aquifer_k = NULL,
                                specific_yield = NULL, streambed_k = NULL,
                                streambed_thickness = NULL) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "gwflow_zone")) {
    stop("gwflow_zone table not found.", call. = FALSE)
  }

  updates <- list()
  if (!is.null(aquifer_k))           updates$aquifer_k <- aquifer_k
  if (!is.null(specific_yield))      updates$specific_yield <- specific_yield
  if (!is.null(streambed_k))         updates$streambed_k <- streambed_k
  if (!is.null(streambed_thickness)) updates$streambed_thickness <- streambed_thickness

  if (length(updates) == 0) {
    stop("No update fields provided.", call. = FALSE)
  }

  sql <- build_update_sql("gwflow_zone", updates,
                          paste0("zone_id = ", as.integer(zone_id)))
  n <- execute_db(con, sql)
  if (n == 0) {
    warning("No zone found with zone_id = ", zone_id, call. = FALSE)
  } else {
    message("Updated GWFLOW zone ", zone_id)
  }

  invisible(project)
}
