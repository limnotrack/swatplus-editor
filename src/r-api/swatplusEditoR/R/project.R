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

#' Ensure all required SWAT+ tables exist before writing files
#'
#' Creates any missing tables that \code{\link{write_config_files}} needs and
#' populates mandatory tables with sensible defaults (mirroring the Python
#' SWAT+ Editor \code{setup.py} initialisation).  Tables that already exist
#' are left untouched.
#'
#' @param con DBI connection to the project database.
#' @return Invisible \code{NULL}.
#' @keywords internal
ensure_write_tables <- function(con) {

  # ---- helper: create a table only if it does not exist ----
  create_if_missing <- function(sql) {
    DBI::dbExecute(con, sql)
  }

  # ---- helper: insert a default row when a table is empty ----
  insert_if_empty <- function(tbl, sql) {
    n <- tryCatch(
      DBI::dbGetQuery(con,
        paste0("SELECT COUNT(*) AS n FROM ", tbl))$n,
      error = function(e) 0L)
    if (n == 0L) DBI::dbExecute(con, sql)
  }

  existing <- DBI::dbListTables(con)

  # ==================================================================
  # 1. Simulation tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS time_sim (
      id INTEGER PRIMARY KEY,
      day_start INTEGER, yrc_start INTEGER,
      day_end   INTEGER, yrc_end   INTEGER,
      step INTEGER DEFAULT 0
    )")
  insert_if_empty("time_sim",
    "INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
     VALUES (0, 1980, 0, 1985, 0)")

  create_if_missing("
    CREATE TABLE IF NOT EXISTS print_prt (
      id INTEGER PRIMARY KEY,
      nyskip INTEGER, day_start INTEGER, yrc_start INTEGER,
      day_end INTEGER, yrc_end INTEGER, interval INTEGER,
      csvout INTEGER, dbout INTEGER, cdfout INTEGER,
      crop_yld TEXT DEFAULT 'b', mgtout INTEGER, hydcon INTEGER,
      fdcout INTEGER
    )")
  insert_if_empty("print_prt",
    "INSERT INTO print_prt
       (nyskip, day_start, yrc_start, day_end, yrc_end, interval,
        csvout, dbout, cdfout, crop_yld, mgtout, hydcon, fdcout)
     VALUES (1, 0, 0, 0, 0, 1, 0, 0, 0, 'b', 0, 0, 0)")

  create_if_missing("
    CREATE TABLE IF NOT EXISTS print_prt_object (
      id INTEGER PRIMARY KEY,
      print_prt_id INTEGER,
      name TEXT, daily INTEGER, monthly INTEGER,
      yearly INTEGER, avann INTEGER,
      FOREIGN KEY (print_prt_id) REFERENCES print_prt(id)
    )")

  # Default print objects (52 rows) --------------------------------
  prt_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM print_prt ORDER BY id LIMIT 1")$id,
    error = function(e) 1L)

  if (safe_count(con, "print_prt_object") == 0L) {
    # Names where yearly=1, avann=1
    active <- c(
      "basin_wb", "basin_nb", "basin_ls", "basin_pw", "basin_aqu",
      "basin_res", "basin_cha", "basin_sd_cha", "basin_psc",
      "region_wb", "region_nb", "region_ls", "region_pw", "region_aqu",
      "region_res", "region_sd_cha", "region_psc", "water_allo",
      "lsunit_wb", "lsunit_nb", "lsunit_ls", "lsunit_pw",
      "hru_wb", "hru_nb", "hru_ls", "hru_pw",
      "hru-lte_wb", "hru-lte_nb", "hru-lte_ls", "hru-lte_pw",
      "channel", "channel_sd", "aquifer", "reservoir", "recall",
      "hyd", "ru", "pest")
    # Names where all flags = 0
    inactive <- c(
      "basin_salt", "hru_salt", "ru_salt", "aqu_salt",
      "channel_salt", "res_salt", "wetland_salt",
      "basin_cs", "hru_cs", "ru_cs", "aqu_cs",
      "channel_cs", "res_cs", "wetland_cs")

    rows <- data.frame(
      print_prt_id = prt_id,
      name     = c(active, inactive),
      daily    = 0L,
      monthly  = 0L,
      yearly   = c(rep(1L, length(active)),  rep(0L, length(inactive))),
      avann    = c(rep(1L, length(active)),  rep(0L, length(inactive))),
      stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "print_prt_object", rows, append = TRUE)
  }

  create_if_missing("
    CREATE TABLE IF NOT EXISTS object_prt (
      id INTEGER PRIMARY KEY,
      ob_typ TEXT, ob_typ_no INTEGER, hyd_typ TEXT, filename TEXT
    )")

  create_if_missing("
    CREATE TABLE IF NOT EXISTS object_cnt (
      id INTEGER PRIMARY KEY,
      name TEXT,
      obj INTEGER DEFAULT 0, hru INTEGER DEFAULT 0,
      lhru INTEGER DEFAULT 0, rtu INTEGER DEFAULT 0,
      mfl INTEGER DEFAULT 0, aqu INTEGER DEFAULT 0,
      cha INTEGER DEFAULT 0, res INTEGER DEFAULT 0,
      rec INTEGER DEFAULT 0, exco INTEGER DEFAULT 0,
      dlr INTEGER DEFAULT 0, can INTEGER DEFAULT 0,
      pmp INTEGER DEFAULT 0, out INTEGER DEFAULT 0,
      lcha INTEGER DEFAULT 0, aqu2d INTEGER DEFAULT 0,
      hrd INTEGER DEFAULT 0, wro INTEGER DEFAULT 0
    )")

  cfg_name <- tryCatch(
    DBI::dbGetQuery(con, "SELECT project_name FROM project_config LIMIT 1")$project_name,
    error = function(e) "default")
  if (is.null(cfg_name) || is.na(cfg_name) || cfg_name == "") cfg_name <- "default"
  insert_if_empty("object_cnt", paste0(
    "INSERT INTO object_cnt (name) VALUES ('", cfg_name, "')"))

  create_if_missing("
    CREATE TABLE IF NOT EXISTS constituents_cs (
      id INTEGER PRIMARY KEY,
      name TEXT, pest_coms TEXT, path_coms TEXT,
      hmet_coms TEXT, salt_coms TEXT
    )")

  # ==================================================================
  # 2. Basin tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS codes_bsn (
      id INTEGER PRIMARY KEY,
      pet_file TEXT, wq_file TEXT,
      pet INTEGER, event INTEGER, crack INTEGER, swift_out INTEGER,
      sed_det INTEGER, rte_cha INTEGER, deg_cha INTEGER, wq_cha INTEGER,
      nostress INTEGER, cn INTEGER, c_fact INTEGER, carbon INTEGER,
      lapse INTEGER, uhyd INTEGER, sed_cha INTEGER, tiledrain INTEGER,
      wtable INTEGER, soil_p INTEGER, gampt INTEGER,
      atmo_dep TEXT, stor_max INTEGER, i_fpwet INTEGER,
      gwflow INTEGER DEFAULT 0
    )")
  insert_if_empty("codes_bsn",
    "INSERT INTO codes_bsn
       (pet_file, wq_file,
        pet, event, crack, swift_out, sed_det, rte_cha, deg_cha, wq_cha,
        nostress, cn, c_fact, carbon, lapse, uhyd, sed_cha, tiledrain,
        wtable, soil_p, gampt, atmo_dep, stor_max, i_fpwet, gwflow)
     VALUES (NULL, NULL,
             1, 0, 0, 1, 0, 0, 0, 1,
             0, 0, 0, 0, 0, 1, 0, 0,
             0, 0, 0, 'a', 0, 1, 0)")

  create_if_missing("
    CREATE TABLE IF NOT EXISTS parameters_bsn (
      id INTEGER PRIMARY KEY,
      lai_noevap REAL, sw_init REAL, surq_lag REAL,
      adj_pkrt REAL, adj_pkrt_sed REAL, lin_sed REAL, exp_sed REAL,
      orgn_min REAL, n_uptake REAL, p_uptake REAL,
      n_perc REAL, p_perc REAL, p_soil REAL, p_avail REAL,
      rsd_decomp REAL, pest_perc REAL,
      msk_co1 REAL, msk_co2 REAL, msk_x REAL,
      nperco_lchtile REAL, evap_adj REAL, scoef REAL,
      denit_exp REAL, denit_frac REAL, man_bact REAL,
      adj_uhyd REAL, cn_froz REAL, dorm_hr REAL,
      plaps REAL, tlaps REAL, n_fix_max REAL,
      rsd_decay REAL, rsd_cover REAL, urb_init_abst REAL,
      petco_pmpt REAL, uhyd_alpha REAL,
      splash REAL, rill REAL, surq_exp REAL, cov_mgt REAL,
      cha_d50 REAL, co2 REAL, day_lag_max REAL,
      igen INTEGER
    )")
  insert_if_empty("parameters_bsn",
    "INSERT INTO parameters_bsn
       (lai_noevap, sw_init, surq_lag, adj_pkrt, adj_pkrt_sed,
        lin_sed, exp_sed, orgn_min, n_uptake, p_uptake,
        n_perc, p_perc, p_soil, p_avail, rsd_decomp, pest_perc,
        msk_co1, msk_co2, msk_x, nperco_lchtile, evap_adj, scoef,
        denit_exp, denit_frac, man_bact, adj_uhyd, cn_froz, dorm_hr,
        plaps, tlaps, n_fix_max, rsd_decay, rsd_cover, urb_init_abst,
        petco_pmpt, uhyd_alpha, splash, rill, surq_exp, cov_mgt,
        cha_d50, co2, day_lag_max, igen)
     VALUES (3.0, 0.0, 4.0, 1.0, 484.0,
             0.0, 0.0, 0.0, 20.0, 20.0,
             0.1, 10.0, 175.0, 0.4, 0.05, 0.5,
             0.75, 0.25, 0.2, 0.5, 0.6, 1.0,
             1.4, 1.3, 0.15, 0.0, 0.0, 0.0,
             0.0, 0.0, 20.0, 0.01, 0.3, 1.0,
             1.0, 5.0, 1.0, 0.7, 1.2, 0.03,
             50.0, 400.0, 0.0, 0)")

  # ==================================================================
  # 3. Climate tables (weather_sta_cli / weather_wgn_cli already in
  #    create_project_db; just make sure they exist)
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_sta_cli (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      lat REAL, lon REAL, elev REAL,
      pcp TEXT, tmp TEXT, slr TEXT, hmd TEXT, wnd TEXT, pet TEXT,
      atmo_dep TEXT, wgn_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_wgn_cli (
      id INTEGER PRIMARY KEY,
      name TEXT UNIQUE NOT NULL,
      lat REAL, lon REAL, elev REAL, rain_yrs REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_wgn_cli_mon (
      id INTEGER PRIMARY KEY,
      weather_wgn_cli_id INTEGER NOT NULL,
      month INTEGER NOT NULL,
      tmp_max_ave REAL, tmp_min_ave REAL,
      tmp_max_sd REAL, tmp_min_sd REAL,
      pcp_ave REAL, pcp_sd REAL, pcp_skew REAL,
      wet_dry REAL, wet_wet REAL, pcp_days REAL, pcp_hhr REAL,
      slr_ave REAL, dew_ave REAL, wnd_ave REAL,
      FOREIGN KEY (weather_wgn_cli_id) REFERENCES weather_wgn_cli(id)
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weather_file (
      id INTEGER PRIMARY KEY,
      filename TEXT, type TEXT, lat REAL, lon REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS atmo_cli (
      id INTEGER PRIMARY KEY,
      filename TEXT, sim_sta_id INTEGER
    )")

  # ==================================================================
  # 4. Connection tables (13 types + their _con_out partners)
  # ==================================================================
  con_types <- c("hru", "hru_lte", "rout_unit", "modflow",
                 "aquifer", "aquifer2d", "channel", "reservoir",
                 "recall", "exco", "delratio", "outlet", "chandeg")
  for (ct in con_types) {
    tbl <- paste0(ct, "_con")
    create_if_missing(paste0("
      CREATE TABLE IF NOT EXISTS ", tbl, " (
        id INTEGER PRIMARY KEY,
        name TEXT, gis_id INTEGER, area REAL, lat REAL, lon REAL,
        elev REAL, wst_id INTEGER, cst_id INTEGER,
        ovfl INTEGER DEFAULT 0, rule INTEGER DEFAULT 0,
        ob_typ TEXT, obj_id INTEGER
      )"))
    create_if_missing(paste0("
      CREATE TABLE IF NOT EXISTS ", tbl, "_out (
        id INTEGER PRIMARY KEY,
        ", tbl, "_id INTEGER,
        order_id INTEGER, obj_typ TEXT, obj_id INTEGER,
        hyd_typ TEXT, frac REAL,
        FOREIGN KEY (", tbl, "_id) REFERENCES ", tbl, "(id)
      )"))
  }

  # ==================================================================
  # 5. Channel tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS initial_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      org_min_id INTEGER, pest_id INTEGER, path_id INTEGER,
      hmet_id INTEGER, salt_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS channel_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      wd REAL, dp REAL, slp REAL, len REAL, mann REAL,
      k REAL, erod_fact REAL, cov_fact REAL,
      hc_cov REAL, eq_slp REAL, d50 REAL,
      clay REAL, carbon REAL, dry_bd REAL,
      side_slp REAL, bed_load REAL,
      fps REAL, fpn REAL, n_conc REAL, p_conc REAL, p_bio REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS sediment_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      cha_cov INTEGER, bed_load REAL,
      bank_erode REAL, channel_erode REAL,
      shear_bank REAL, shear_bed REAL,
      hc_erod REAL, hc_ht REAL, hc_len REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS nutrients_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      ptl_n REAL, ptl_p REAL, algae REAL,
      bod REAL, cbod REAL, dis_ox REAL,
      nh3_n REAL, no2_n REAL, no3_n REAL,
      sol_p REAL, ch_onp REAL, ch_onn REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS channel_lte_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      hyd_id INTEGER, init_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hyd_sed_lte_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      wd REAL, dp REAL, slp REAL, len REAL, mann REAL,
      k REAL, cov_fact REAL, wd_rto REAL, eq_slp REAL, d50 REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS temperature_cha (
      id INTEGER PRIMARY KEY, name TEXT,
      lat REAL, lon REAL, elev REAL
    )")

  # ==================================================================
  # 6. Reservoir tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS initial_res (
      id INTEGER PRIMARY KEY, name TEXT,
      org_min_id INTEGER, pest_id INTEGER, path_id INTEGER,
      hmet_id INTEGER, salt_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS reservoir_res (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_res (
      id INTEGER PRIMARY KEY, name TEXT,
      yr_op REAL, mon_op REAL, area_ps REAL,
      vol_ps REAL, area_es REAL, vol_es REAL,
      k REAL, evap_co REAL, shp_co1 REAL, shp_co2 REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS sediment_res (
      id INTEGER PRIMARY KEY, name TEXT,
      sed_stl REAL, velsetl_d50 REAL, velsetl_stl REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS nutrients_res (
      id INTEGER PRIMARY KEY, name TEXT,
      mid_start INTEGER, mid_end INTEGER,
      mid_n_stl REAL, n_stl REAL, mid_p_stl REAL, p_stl REAL,
      chla_co REAL, secchi_co REAL, theta_n REAL,
      theta_p REAL, n_min_stl REAL, p_min_stl REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS weir_res (
      id INTEGER PRIMARY KEY, name TEXT,
      num_steps INTEGER, hyd_flo TEXT, hyd_flo2 TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS wetland_wet (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_wet (
      id INTEGER PRIMARY KEY, name TEXT,
      psa REAL, pdep REAL, esa REAL, edep REAL,
      k REAL, evap REAL
    )")

  # ==================================================================
  # 7. Routing unit tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_def_con (
      id INTEGER PRIMARY KEY, name TEXT, rtu_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_ele (
      id INTEGER PRIMARY KEY, name TEXT,
      rtu_id INTEGER, obj_id INTEGER, obj_typ TEXT, frac REAL,
      dlr_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_rtu (
      id INTEGER PRIMARY KEY, name TEXT,
      topo_id INTEGER, field_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS rout_unit_dr (
      id INTEGER PRIMARY KEY, name TEXT, dr_id INTEGER
    )")

  # ==================================================================
  # 8. HRU tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hru_data_hru (
      id INTEGER PRIMARY KEY, name TEXT,
      topo_id INTEGER, hydro_id INTEGER, soil_id INTEGER,
      lu_mgt_id INTEGER, soil_plant_ini_id INTEGER,
      surf_stor TEXT, snow_id INTEGER, field_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hru_lte_hru (
      id INTEGER PRIMARY KEY, name TEXT,
      cn2 REAL, usle_k REAL, usle_ls REAL, usle_p REAL,
      ovn REAL, elev REAL, slope REAL, slope_len REAL,
      lat REAL, perco REAL, eta REAL, pet REAL,
      strsol REAL, strtmp REAL, sw REAL, awc REAL
    )")

  # ==================================================================
  # 9. DR (delivery ratio) tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS delratio_del (
    id INTEGER PRIMARY KEY, name TEXT, om_id INTEGER)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_om_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_pest_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_path_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_hmet_del (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS dr_salt_del (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 10. Aquifer tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS initial_aqu (
      id INTEGER PRIMARY KEY, name TEXT,
      org_min_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS aquifer_aqu (
      id INTEGER PRIMARY KEY, name TEXT,
      init_id INTEGER, gw_flo REAL, dep_bot REAL,
      dep_wt REAL, no3_n REAL, sol_p REAL,
      ptl_n REAL, ptl_p REAL,
      bf_max REAL, alpha_bf REAL, revap REAL,
      rchg_dp REAL, spec_yld REAL, hl_no3n REAL,
      flo_min REAL, revap_min REAL
    )")

  # ==================================================================
  # 11. Water rights
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS water_allocation_wro (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS element_wro (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS define_wro (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 12. Link tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS chan_surf_lin (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS chan_aqu_lin (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 13. Basin tables (already created above)
  # ==================================================================

  # ==================================================================
  # 14. Hydrology tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS hydrology_hyd (
      id INTEGER PRIMARY KEY, name TEXT,
      lat_ttime REAL, lat_sed REAL, can_max REAL,
      esco REAL, epco REAL, orgn_enrich REAL,
      orgp_enrich REAL, cn3_swf REAL,
      bio_mix REAL, perco REAL, lat_orgn REAL,
      lat_orgp REAL, harg_pet REAL,
      latq_co REAL, cn2 REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS topography_hyd (
      id INTEGER PRIMARY KEY, name TEXT,
      slp REAL, slp_len REAL, lat_len REAL,
      dist_cha REAL, depos REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS field_fld (
      id INTEGER PRIMARY KEY, name TEXT,
      len REAL, wd REAL, ang REAL
    )")

  # ==================================================================
  # 15. EXCO tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_om_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_pest_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_path_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_hmet_exc (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS exco_salt_exc (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 16. Recall tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS recall_rec (
    id INTEGER PRIMARY KEY, name TEXT, rec_typ INTEGER)")
  create_if_missing("CREATE TABLE IF NOT EXISTS recall_dat (
    id INTEGER PRIMARY KEY, recall_rec_id INTEGER,
    yr INTEGER, t_step INTEGER,
    flo REAL, sed REAL, orgn REAL, sedp REAL,
    no3 REAL, solp REAL, chla REAL, nh3 REAL,
    no2 REAL, cbod REAL, dox REAL, sand REAL,
    silt REAL, clay REAL, sag REAL, lag REAL,
    gravel REAL, tmp REAL)")

  # ==================================================================
  # 17. Structural tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS tiledrain_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS septic_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS filterstrip_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS grassedww_str (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS bmpuser_str (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 18. HRU parameter database tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS plants_plt (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS fertilizer_frt (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS tillage_til (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pesticide_pst (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pathogens_pth (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS metals_mtl (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS salts_slt (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS urban_urb (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS septic_sep (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS snow_sno (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 19. Operations tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS harv_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS graze_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS irr_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS chem_app_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS fire_ops (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS sweep_ops (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 20. Land use management tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS landuse_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS management_sch (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS cntable_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS cons_prac_lum (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ovn_table_lum (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 21. Calibration / change tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS cal_parms_cal (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS calibration_cal (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS codes_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS wb_parms_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS water_balance_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ch_sed_budget_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ch_sed_parms_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS plant_parms_sft (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS plant_gro_sft (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 22. Initial condition tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS plant_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS soil_plant_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS om_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pest_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS pest_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS path_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS path_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS hmet_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS hmet_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS salt_hru_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS salt_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 23. Soils tables
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS soils_sol (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS nutrients_sol (
    id INTEGER PRIMARY KEY, name TEXT, exp_co REAL)")
  create_if_missing("CREATE TABLE IF NOT EXISTS soils_lte_sol (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 24. Decision table tables
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl (
      id INTEGER PRIMARY KEY, name TEXT, file_name TEXT,
      description TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl_cond (
      id INTEGER PRIMARY KEY, d_table_dtl_id INTEGER,
      var TEXT, obj TEXT, obj_num INTEGER,
      lim_var TEXT, lim_op TEXT, lim_const REAL,
      alt TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS d_table_dtl_act (
      id INTEGER PRIMARY KEY, d_table_dtl_id INTEGER,
      act_typ TEXT, obj TEXT, obj_num INTEGER,
      name TEXT, option TEXT, const1 REAL,
      const2 REAL, fp TEXT
    )")

  # ==================================================================
  # 25. Region tables
  # ==================================================================
  region_tbls <- c("ls_unit_ele", "ls_unit_def", "ls_reg_ele", "ls_reg_def",
                   "ch_catunit_ele", "ch_catunit_def", "ch_reg_def",
                   "aqu_catunit_ele", "aqu_catunit_def", "aqu_reg_def",
                   "res_catunit_ele", "res_catunit_def", "res_reg_def",
                   "rec_catunit_ele", "rec_catunit_def", "rec_reg_def")
  for (rt in region_tbls) {
    create_if_missing(paste0(
      "CREATE TABLE IF NOT EXISTS ", rt, " (
         id INTEGER PRIMARY KEY, name TEXT
       )"))
  }

  # ==================================================================
  # 26. Herd tables (stub - matching Python write_herd() which is pass)
  # ==================================================================
  create_if_missing("CREATE TABLE IF NOT EXISTS animal_hrd (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS herd_hrd (
    id INTEGER PRIMARY KEY, name TEXT)")
  create_if_missing("CREATE TABLE IF NOT EXISTS ranch_hrd (
    id INTEGER PRIMARY KEY, name TEXT)")

  # ==================================================================
  # 27. gwflow tables (groundwater flow module)
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_base (
      id INTEGER PRIMARY KEY,
      cell_size REAL, row_count INTEGER, col_count INTEGER,
      boundary_conditions INTEGER DEFAULT 2,
      recharge INTEGER DEFAULT 1, soil_transfer INTEGER DEFAULT 0,
      saturation_excess INTEGER DEFAULT 0, external_pumping INTEGER DEFAULT 0,
      tile_drainage INTEGER DEFAULT 0, reservoir_exchange INTEGER DEFAULT 0,
      wetland_exchange INTEGER DEFAULT 0, floodplain_exchange INTEGER DEFAULT 0,
      canal_seepage INTEGER DEFAULT 0, solute_transport INTEGER DEFAULT 0,
      timestep_balance REAL DEFAULT 1.0,
      daily_output INTEGER DEFAULT 0, annual_output INTEGER DEFAULT 0,
      aa_output INTEGER DEFAULT 0,
      recharge_delay REAL DEFAULT 0,
      river_depth REAL DEFAULT 0,
      daily_output_row INTEGER DEFAULT 0, daily_output_col INTEGER DEFAULT 0,
      resbed_thickness REAL DEFAULT 0, resbed_k REAL DEFAULT 0,
      wet_thickness REAL DEFAULT 0,
      tile_depth REAL DEFAULT 0, tile_area REAL DEFAULT 0,
      tile_k REAL DEFAULT 0, tile_groups INTEGER DEFAULT 0,
      transport_steps INTEGER DEFAULT 1, disp_coef REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_zone (
      id INTEGER PRIMARY KEY, zone_id INTEGER,
      aquifer_k REAL, specific_yield REAL,
      streambed_k REAL, streambed_thickness REAL
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_grid (
      id INTEGER PRIMARY KEY, cell_id INTEGER,
      status INTEGER DEFAULT 0, elevation REAL DEFAULT 0,
      aquifer_thickness REAL DEFAULT 0, zone INTEGER DEFAULT 0,
      extinction_depth REAL DEFAULT 0, initial_head REAL DEFAULT 0,
      tile INTEGER DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_out_days (
      id INTEGER PRIMARY KEY, year INTEGER, jday INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_obs_locs (
      id INTEGER PRIMARY KEY, cell_id INTEGER
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_solutes (
      id INTEGER PRIMARY KEY, solute_name TEXT,
      sorption REAL DEFAULT 0, rate_const REAL DEFAULT 0,
      canal_irr REAL DEFAULT 0,
      init_data TEXT DEFAULT 'single', init_conc REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_init_conc (
      id INTEGER PRIMARY KEY, cell_id INTEGER,
      init_no3 REAL DEFAULT 0, init_p REAL DEFAULT 0,
      init_so4 REAL DEFAULT 0, init_ca REAL DEFAULT 0,
      init_mg REAL DEFAULT 0, init_na REAL DEFAULT 0,
      init_k REAL DEFAULT 0, init_cl REAL DEFAULT 0,
      init_co3 REAL DEFAULT 0, init_hco3 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_hrucell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, hru INTEGER,
      area_m2 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_fpcell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, channel_id INTEGER,
      area_m2 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_rivcell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, channel INTEGER,
      length_m REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_lsucell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, lsu INTEGER,
      area_m2 REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_rescell (
      id INTEGER PRIMARY KEY, cell_id INTEGER, res_id INTEGER,
      res_stage REAL DEFAULT 0
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS gwflow_wetland (
      id INTEGER PRIMARY KEY, wet_id INTEGER, thickness REAL DEFAULT 0
    )")

  # ==================================================================
  # 26. file_cio_classification + file_cio
  # ==================================================================
  create_if_missing("
    CREATE TABLE IF NOT EXISTS file_cio_classification (
      id INTEGER PRIMARY KEY, name TEXT
    )")
  create_if_missing("
    CREATE TABLE IF NOT EXISTS file_cio (
      id INTEGER PRIMARY KEY,
      classification_id INTEGER,
      order_in_class INTEGER,
      file_name TEXT,
      FOREIGN KEY (classification_id)
        REFERENCES file_cio_classification(id)
    )")

  if (safe_count(con, "file_cio_classification") == 0L) {
    cls_names <- c(
      "simulation", "basin", "climate", "connect", "channel",
      "reservoir", "routing_unit", "hru", "exco", "recall",
      "dr", "aquifer", "herd", "water_rights", "link",
      "hydrology", "structural", "hru_parm_db", "ops", "lum",
      "chg", "init", "soils", "decision_table", "regions",
      "pcp_path", "tmp_path", "slr_path", "hmd_path", "wnd_path",
      "out_path")

    cls_df <- data.frame(name = cls_names, stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "file_cio_classification", cls_df, append = TRUE)

    # Build file_cio records  (classification_id, order_in_class, file_name)
    # ------ classification_id values map to cls_names insertion order ------
    cls_lookup <- DBI::dbGetQuery(con,
      "SELECT id, name FROM file_cio_classification ORDER BY id")
    cid <- function(name) cls_lookup$id[cls_lookup$name == name]

    file_rows <- data.frame(
      classification_id = integer(0),
      order_in_class    = integer(0),
      file_name         = character(0),
      stringsAsFactors  = FALSE)

    add_files <- function(section, files) {
      for (i in seq_along(files)) {
        file_rows[nrow(file_rows) + 1L, ] <<- list(cid(section), i, files[i])
      }
    }

    add_files("simulation", c("time.sim", "print.prt", "object.prt",
                               "object.cnt", "constituents.cs"))
    add_files("basin", c("codes.bsn", "parameters.bsn"))
    add_files("climate", c("weather-sta.cli", "weather-wgn.cli",
                            "pet.cli", "pcp.cli", "tmp.cli",
                            "slr.cli", "hmd.cli", "wnd.cli", "atmodep.cli"))
    add_files("connect", c("hru.con", "hru-lte.con", "rout_unit.con",
                            "gwflow.con", "aquifer.con", "aquifer2d.con",
                            "channel.con", "reservoir.con", "recall.con",
                            "exco.con", "delratio.con", "outlet.con",
                            "chandeg.con"))
    add_files("channel", c("initial.cha", "channel.cha", "hydrology.cha",
                            "sediment.cha", "nutrients.cha",
                            "channel-lte.cha", "hyd-sed-lte.cha",
                            "temperature.cha"))
    add_files("reservoir", c("initial.res", "reservoir.res", "hydrology.res",
                              "sediment.res", "nutrients.res", "weir.res",
                              "wetland.wet", "hydrology.wet"))
    add_files("routing_unit", c("rout_unit.def", "rout_unit.ele",
                                 "rout_unit.rtu", "rout_unit.dr"))
    add_files("hru", c("hru-data.hru", "hru-lte.hru"))
    add_files("exco", c("exco.exc", "exco_om.exc", "exco_pest.exc",
                          "exco_path.exc", "exco_hmet.exc", "exco_salt.exc"))
    add_files("recall", "recall.rec")
    add_files("dr", c("delratio.del", "dr_om.del", "dr_pest.del",
                        "dr_path.del", "dr_hmet.del", "dr_salt.del"))
    add_files("aquifer", c("initial.aqu", "aquifer.aqu"))
    add_files("herd", c("animal.hrd", "herd.hrd", "ranch.hrd"))
    add_files("water_rights", c("water_allocation.wro", "element.wro",
                                 "define.wro"))
    add_files("link", c("chan-surf.lin", "chan-aqu.lin"))
    add_files("hydrology", c("hydrology.hyd", "topography.hyd", "field.fld"))
    add_files("structural", c("tiledrain.str", "septic.str",
                                "filterstrip.str", "grassedww.str",
                                "bmpuser.str"))
    add_files("hru_parm_db", c("plants.plt", "fertilizer.frt", "tillage.til",
                                 "pesticide.pst", "pathogens.pth",
                                 "metals.mtl", "salts.slt", "urban.urb",
                                 "septic.sep", "snow.sno"))
    add_files("ops", c("harv.ops", "graze.ops", "irr.ops",
                         "chem_app.ops", "fire.ops", "sweep.ops"))
    add_files("lum", c("landuse.lum", "management.sch", "cntable.lum",
                         "cons_prac.lum", "ovn_table.lum"))
    add_files("chg", c("cal_parms.cal", "calibration.cal", "codes.sft",
                         "wb_parms.sft", "water_balance.sft",
                         "ch_sed_budget.sft", "ch_sed_parms.sft",
                         "plant_parms.sft", "plant_gro.sft"))
    add_files("init", c("plant.ini", "soil_plant.ini", "om_water.ini",
                          "pest_hru.ini", "pest_water.ini",
                          "path_hru.ini", "path_water.ini",
                          "hmet_hru.ini", "hmet_water.ini",
                          "salt_hru.ini", "salt_water.ini"))
    add_files("soils", c("soils.sol", "nutrients.sol", "soils_lte.sol"))
    add_files("decision_table", c("lum.dtl", "res_rel.dtl",
                                    "scen_lu.dtl", "flo_con.dtl"))
    add_files("regions", c(
      "ls_unit.ele", "ls_unit.def", "ls_reg.ele", "ls_reg.def",
      "ls_cal.reg",
      "ch_catunit.ele", "ch_catunit.def", "ch_reg.def",
      "aqu_catunit.ele", "aqu_catunit.def", "aqu_reg.def",
      "res_catunit.ele", "res_catunit.def", "res_reg.def",
      "rec_catunit.ele", "rec_catunit.def", "rec_reg.def"))

    DBI::dbWriteTable(con, "file_cio", file_rows, append = TRUE)
  }

  invisible(NULL)
}
