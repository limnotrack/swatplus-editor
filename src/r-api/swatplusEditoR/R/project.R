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
#' @param overwrite Logical. If TRUE, overwrite an existing database.
#' @return The project object with \code{db_file} set to the new database path.
#' @export
#' @importFrom DBI dbWriteTable
#' @importFrom rQSWATPlus qswat_write_database
#' @examples
#' \dontrun{
#' project <- list(project_dir = "/path/to/project")
#' project <- create_project_db(project, "/path/to/project/swatplus.sqlite")
#' }
create_project_db <- function(project, overwrite = FALSE) {
  if (!is.list(project)) {
    stop("project must be a list.", call. = FALSE)
  }

  db_file <- project$db_file
  if (file.exists(db_file) && !overwrite) {
    stop("Database already exists at: ", db_file,
         ". Use overwrite = TRUE to replace.", call. = FALSE)
  }

  if (file.exists(db_file) && overwrite) {
    file.remove(db_file)
  }
  
  rQSWATPlus::qswat_write_database(project = project, db_file = db_file, 
                                   overwrite = overwrite)


  project$db_file <- normalizePath(db_file, mustWork = TRUE)
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
#' Delegates to the \code{ensure_write_tables()} function in \pkg{rQSWATPlus},
#' which creates any missing tables that \code{\link{write_config_files}} needs
#' and populates mandatory tables with sensible defaults (mirroring the Python
#' SWAT+ Editor \code{setup.py} initialisation).  Tables that already exist
#' are left untouched.
#'
#' @param con DBI connection to the project database.
#' @return Invisible \code{NULL}.
#' @keywords internal
ensure_write_tables <- function(con) {
  fn <- tryCatch(
    get("ensure_write_tables", envir = asNamespace("rQSWATPlus")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    message("Note: rQSWATPlus::ensure_write_tables() not available; ",
            "table initialization skipped.")
    return(invisible(NULL))
  }
  fn(con)
  invisible(NULL)
}

#' Populate reference/parameter tables from the SWAT+ datasets databases
#'
#' Delegates to the \code{populate_from_datasets()} function in
#' \pkg{rQSWATPlus}, which copies reference data (plants, fertilizers,
#' operations, land use, calibration parameters, etc.) from bundled databases
#' into the project database.  Only empty or missing tables are populated;
#' tables with existing data are left untouched.
#'
#' @param con DBI connection to the project database.
#' @return Invisible \code{NULL}.
#' @keywords internal
populate_from_datasets <- function(con) {
  fn <- tryCatch(
    get("populate_from_datasets", envir = asNamespace("rQSWATPlus")),
    error = function(e) NULL
  )
  if (is.null(fn)) {
    return(invisible(NULL))
  }
  fn(con)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Internal helper: safe row count (returns 0 on error or missing table)
# --------------------------------------------------------------------------
.gis_count <- function(con, tbl) {
  tryCatch(
    DBI::dbGetQuery(con,
      paste0("SELECT COUNT(*) AS n FROM ", tbl))$n[1L],
    error = function(e) 0L
  )
}

# --------------------------------------------------------------------------
# Internal helper: safe table write (ignore errors from schema mismatches)
# --------------------------------------------------------------------------
.gis_write <- function(con, tbl, df) {
  if (nrow(df) == 0L) return(invisible(NULL))
  tryCatch(
    DBI::dbWriteTable(con, tbl, df, append = TRUE, row.names = FALSE),
    error = function(e) NULL)
  invisible(NULL)
}

# Like .gis_write but silently drops data-frame columns that do not exist in
# the target DB table.  This allows callers to pass a "full-defaults" frame
# while remaining compatible with minimal test schemas.
.gis_write_safe <- function(con, tbl, df) {
  if (nrow(df) == 0L) return(invisible(NULL))
  db_cols <- tryCatch(DBI::dbListFields(con, tbl),
                      error = function(e) names(df))
  common  <- intersect(names(df), db_cols)
  if (length(common) == 0L) return(invisible(NULL))
  .gis_write(con, tbl, df[, common, drop = FALSE])
}

# --------------------------------------------------------------------------
# Internal helper: safe single SQL execute
# --------------------------------------------------------------------------
.gis_exec <- function(con, sql) {
  tryCatch(DBI::dbExecute(con, sql), error = function(e) NULL)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Internal helper: zero-padded name generation matching Python get_name()
# --------------------------------------------------------------------------
.gis_name <- function(prefix, id, cnt) {
  if (cnt < 10L)        sprintf("%s%d",   prefix, id)
  else if (cnt < 100L)  sprintf("%s%02d", prefix, id)
  else if (cnt < 1000L) sprintf("%s%03d", prefix, id)
  else                  sprintf("%s%04d", prefix, id)
}

# --------------------------------------------------------------------------
# SWAT+ × 10 naming: id multiplied by 10, minimum 4-digit zero-padding.
# Used for routing-unit level objects (rtu, toportu, fld) to leave room
# for inserting intermediate IDs between units (standard QSWAT+ convention).
# --------------------------------------------------------------------------
.gis_name_x10 <- function(id, prefix) {
  sprintf("%s%04d", prefix, id * 10L)
}

# --------------------------------------------------------------------------
# Internal helper: slope pct -> slope length (Python get_slope_len())
# --------------------------------------------------------------------------
.slope_len <- function(slope_pct) {
  s <- slope_pct / 100
  if (is.na(s) || s <= 0) return(50.0)
  min(150.0, 4.570 * (s ^ -0.6142))
}

#' Populate SWAT+ model tables from GIS data
#'
#' Reads the \code{gis_*} tables written by
#' \code{rQSWATPlus::qswat_write_database()} and creates the SWAT+ model
#' objects (channels, routing units, HRUs, aquifers, connections, etc.)
#' that \code{\link{write_config_files}} needs to produce a complete set of
#' SWAT+ input files.
#'
#' This mirrors the Python SWAT+ Editor \file{actions/import_gis.py}
#' \code{insert_default()} method. Only creates rows in tables that are
#' currently empty; existing data is left untouched.
#'
#' @param con DBI connection to the project database (must already have
#'   \code{gis_*} tables populated by
#'   \code{rQSWATPlus::qswat_write_database()}).
#' @return Invisible \code{NULL}.
#' @keywords internal
populate_from_gis <- function(con) {

  # Only run when gis tables are present
  if (.gis_count(con, "gis_lsus") == 0L &&
      .gis_count(con, "gis_hrus") == 0L) {
    return(invisible(NULL))
  }

  is_lte <- tryCatch({
    cfg <- DBI::dbGetQuery(con,
      "SELECT is_lte FROM project_config LIMIT 1")
    isTRUE(cfg$is_lte[1L] == 1L)
  }, error = function(e) FALSE)

  if (is_lte) {
    .populate_gis_lte(con)
  } else {
    .populate_gis_standard(con)
  }

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Standard (non-LTE) GIS import — mirrors import_gis.py insert_default()
# --------------------------------------------------------------------------
.populate_gis_standard <- function(con) {
  .gis_insert_routing_units(con)
  .gis_insert_om_water(con)
  .gis_insert_channels(con)
  .gis_insert_reservoirs(con)
  .gis_insert_recall(con)
  .gis_insert_hrus(con)
  .gis_insert_aquifers(con)
  .gis_insert_connections(con)
  .gis_insert_lsus(con)
  .gis_update_object_cnt(con)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# LTE GIS import
# --------------------------------------------------------------------------
.populate_gis_lte <- function(con) {
  .gis_insert_om_water(con)
  .gis_insert_channels_lte(con)
  .gis_insert_lsus_lte(con)
  .gis_insert_hru_ltes(con)
  .gis_insert_connections_lte(con)
  .gis_update_object_cnt(con)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 1: Routing units from gis_lsus
# Schema (rQSWATPlus ensure_write_tables):
#   topography_hyd: (id, name, slp, slp_len, lat_len, dist_cha, depos)
#   field_fld:      (id, name, len, wd, ang)
#   rout_unit_rtu:  (id, name, topo_id, field_id)
#   rout_unit_con:  (id, name, gis_id, area, lat, lon, elev, ovfl, rule)
#   rout_unit_def_con: (id, name, rtu_id)
# --------------------------------------------------------------------------
.gis_insert_routing_units <- function(con) {
  if (.gis_count(con, "rout_unit_rtu") > 0L) return(invisible(NULL))
  lsus <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_lsus ORDER BY id"),
    error = function(e) NULL)
  if (is.null(lsus) || nrow(lsus) == 0L) return(invisible(NULL))

  n   <- nrow(lsus)
  cnt <- max(lsus$id)
  idx <- seq_len(n)

  topos <- data.frame(
    id       = idx,
    name     = sapply(idx, .gis_name_x10, prefix = "toportu"),
    slp      = pmax(lsus$slope / 100, 0.001),
    slp_len  = sapply(lsus$slope, .slope_len),
    lat_len  = sapply(lsus$slope, .slope_len),
    dist_cha = 121.0,
    depos    = 0.0,
    type     = "sub",
    stringsAsFactors = FALSE)

  fields <- data.frame(
    id   = idx,
    name = sapply(idx, .gis_name_x10, prefix = "fld"),
    len  = 500.0,
    wd   = 100.0,
    ang  = 30.0,
    stringsAsFactors = FALSE)

  rtus <- data.frame(
    id       = idx,
    name     = sapply(idx, .gis_name_x10, prefix = "rtu"),
    topo_id  = idx,
    field_id = idx,
    stringsAsFactors = FALSE)

  rtu_cons <- data.frame(
    id     = idx,
    name   = sapply(idx, .gis_name_x10, prefix = "rtu"),
    gis_id = lsus$id,
    lat    = lsus$lat,
    lon    = lsus$lon,
    elev   = lsus$elev,
    area   = lsus$area,
    ovfl   = 0L,
    rule   = 0L,
    stringsAsFactors = FALSE)

  # rout_unit_def_con mirrors rout_unit_con (acts as a "definition" flag table)
  rtu_def_cons <- data.frame(
    id     = idx,
    name   = sapply(idx, .gis_name_x10, prefix = "rtu"),
    rtu_id = idx,
    stringsAsFactors = FALSE)

  .gis_write_safe(con, "topography_hyd", topos)
  .gis_write(con, "field_fld",      fields)
  .gis_write(con, "rout_unit_rtu",  rtus)
  .gis_write(con, "rout_unit_con",  rtu_cons)
  .gis_write(con, "rout_unit_def_con", rtu_def_cons)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 2: om_water_ini default singleton
# --------------------------------------------------------------------------
.gis_insert_om_water <- function(con) {
  if (.gis_count(con, "om_water_ini") > 0L) return(invisible(NULL))
  .gis_exec(con, "INSERT INTO om_water_ini (id, name) VALUES (1, 'omwat1')")
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 3a: Standard channels from gis_channels
# Schema:
#   initial_cha:    (id, name, org_min_id)
#   hydrology_cha:  (id, name, wd, dp, slp, len, mann, k)
#   sediment_cha:   (id, name)
#   nutrients_cha:  (id, name)
#   channel_cha:    (id, name, init_id, hyd_id, sed_id, nut_id)
#   chandeg_con:    (id, name, gis_id, area, lat, lon, elev, ovfl, rule[, wst_id])
# --------------------------------------------------------------------------
.gis_insert_channels <- function(con) {
  if (.gis_count(con, "channel_cha") > 0L) return(invisible(NULL))
  chas <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_channels ORDER BY id"),
    error = function(e) NULL)
  if (is.null(chas) || nrow(chas) == 0L) return(invisible(NULL))

  # Ensure om_water_ini exists
  .gis_insert_om_water(con)
  om_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM om_water_ini LIMIT 1")$id[1L],
    error = function(e) 1L)

  # One default initial_cha row
  if (.gis_count(con, "initial_cha") == 0L) {
    .gis_exec(con, paste0(
      "INSERT INTO initial_cha (id, name, org_min_id) VALUES (1, 'initcha1', ",
      om_id, ")"))
  }
  init_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM initial_cha LIMIT 1")$id[1L],
    error = function(e) 1L)

  # One default nutrients_cha row with physics-based defaults (mirrors Python model).
  # .gis_write_safe() silently drops columns absent from the DB schema (e.g. tests).
  if (.gis_count(con, "nutrients_cha") == 0L) {
    nuts_def <- data.frame(
      id          = 1L,
      name        = "nutcha1",
      plt_n       = 0,
      ptl_p       = 0,
      alg_stl     = 1,
      ben_disp    = 0.05,
      ben_nh3n    = 0.5,
      ptln_stl    = 0.05,
      ptlp_stl    = 0.05,
      cst_stl     = 2.5,
      ben_cst     = 2.5,
      cbn_bod_co  = 1.71,
      air_rt      = 50,
      cbn_bod_stl = 0.36,
      ben_bod     = 2,
      bact_die    = 2,
      cst_decay   = 1.71,
      nh3n_no2n   = 0.55,
      no2n_no3n   = 1.1,
      ptln_nh3n   = 0.21,
      ptlp_solp   = 0.35,
      q2e_lt      = 2L,
      q2e_alg     = 2L,
      chla_alg    = 50,
      alg_n       = 0.08,
      alg_p       = 0.015,
      alg_o2_prod = 1.6,
      alg_o2_resp = 2,
      o2_nh3n     = 3.5,
      o2_no2n     = 1.07,
      alg_grow    = 2,
      alg_resp    = 2.5,
      slr_act     = 0.3,
      lt_co       = 0.75,
      const_n     = 0.02,
      const_p     = 0.025,
      lt_nonalg   = 1,
      alg_shd_l   = 0.03,
      alg_shd_nl  = 0.054,
      nh3_pref    = 0.5,
      stringsAsFactors = FALSE)
    .gis_write_safe(con, "nutrients_cha", nuts_def)
  }
  nut_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM nutrients_cha LIMIT 1")$id[1L],
    error = function(e) 1L)

  n   <- nrow(chas)
  cnt <- max(chas$id)
  idx <- seq_len(n)

  # Standard hydrology_cha: per-channel geometry-based parameters.
  # Extra columns (beyond wd/dp/slp/len/mann/k) use .gis_write_safe() so they
  # are silently dropped when the DB schema does not have them (e.g. in tests).
  hyds <- data.frame(
    id        = idx,
    name      = mapply(.gis_name, "hyd", chas$id, cnt),
    wd        = pmax(chas$wid2, 0.1),
    dp        = pmax(chas$dep2, 0.1),
    slp       = pmax(chas$slo2, 0.0001),
    len       = pmax(chas$len2, 0.001),
    mann      = 0.05,
    k         = 1.0,
    erod_fact = 0.02,
    cov_fact  = 0.0,
    hc_cov    = 0.0,
    eq_slp    = 0.0,
    d50       = 0.1,
    clay      = 0.1,
    carbon    = 0.01,
    dry_bd    = 1.2,
    side_slp  = 2.0,
    bed_load  = 0.5,
    fps       = 0.0,
    fpn       = 0.0,
    n_conc    = 0.0,
    p_conc    = 0.0,
    p_bio     = 0.0,
    stringsAsFactors = FALSE)

  # One sediment_cha row per channel (not a single shared row).
  seds <- data.frame(
    id   = idx,
    name = mapply(.gis_name, "sed", chas$id, cnt),
    stringsAsFactors = FALSE)

  # Standard channel_cha: references hydrology, sediment, nutrients, initial
  chan_chas <- data.frame(
    id      = idx,
    name    = mapply(.gis_name, "cha", chas$id, cnt),
    init_id = init_id,
    hyd_id  = idx,
    sed_id  = idx,
    nut_id  = nut_id,
    stringsAsFactors = FALSE)

  # chandeg_con: channel connections (same structure for standard and LTE)
  # Fall back to subbasin lat/lon/elev when channel midlat/midlon are 0 or NA.
  subs <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, lat, lon, elev FROM gis_subbasins ORDER BY id"),
    error = function(e) data.frame(id = integer(0), lat = numeric(0),
                                   lon = numeric(0), elev = numeric(0)))
  sub_lat  <- if (nrow(subs) > 0L) setNames(subs$lat,  as.character(subs$id)) else c()
  sub_lon  <- if (nrow(subs) > 0L) setNames(subs$lon,  as.character(subs$id)) else c()
  sub_elev <- if (nrow(subs) > 0L) setNames(subs$elev, as.character(subs$id)) else c()

  lat  <- ifelse(!is.na(chas$midlat)  & chas$midlat  != 0,
                 chas$midlat, sub_lat[as.character(chas$subbasin)])
  lon  <- ifelse(!is.na(chas$midlon)  & chas$midlon  != 0,
                 chas$midlon, sub_lon[as.character(chas$subbasin)])
  elev <- ifelse(!is.na(chas$elevmin) & chas$elevmin != 0,
                 chas$elevmin, sub_elev[as.character(chas$subbasin)])

  chan_cons <- data.frame(
    id     = idx,
    name   = mapply(.gis_name, "cha", chas$id, cnt),
    gis_id = chas$id,
    lat    = lat,
    lon    = lon,
    elev   = elev,
    area   = chas$areac,
    ovfl   = 0L,
    rule   = 0L,
    stringsAsFactors = FALSE)

  # Assign nearest weather station to each channel
  wst_rows <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, lat, lon FROM weather_sta_cli"),
    error = function(e) data.frame(id = integer(0), lat = numeric(0),
                                   lon = numeric(0)))
  if (nrow(wst_rows) > 0L) {
    chan_cons$wst_id <- vapply(seq_len(nrow(chan_cons)), function(i) {
      d2 <- (wst_rows$lat - chan_cons$lat[i])^2 +
            (wst_rows$lon - chan_cons$lon[i])^2
      wst_rows$id[which.min(d2)]
    }, integer(1L))
  }

  .gis_write_safe(con, "hydrology_cha",  hyds)
  .gis_write_safe(con, "sediment_cha",   seds)
  .gis_write(con, "channel_cha",    chan_chas)
  .gis_write_safe(con, "chandeg_con",    chan_cons)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 3b: LTE channels from gis_channels (used for LTE mode only)
# Schema:
#   initial_cha:    (id, name, org_min_id)
#   hyd_sed_lte_cha:(id, name, wd, dp, slp, len, mann, k, cov_fact, wd_rto,
#                    eq_slp, d50)
#   channel_lte_cha:(id, name, hyd_id, init_id)
#   chandeg_con:    (id, name, gis_id, area, lat, lon, elev, ovfl, rule)
# --------------------------------------------------------------------------
.gis_insert_channels_lte <- function(con) {
  if (.gis_count(con, "channel_lte_cha") > 0L) return(invisible(NULL))
  chas <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_channels ORDER BY id"),
    error = function(e) NULL)
  if (is.null(chas) || nrow(chas) == 0L) return(invisible(NULL))

  # Ensure om_water_ini exists
  .gis_insert_om_water(con)
  om_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM om_water_ini LIMIT 1")$id[1L],
    error = function(e) 1L)

  # One default initial_cha row
  if (.gis_count(con, "initial_cha") == 0L) {
    .gis_exec(con, paste0(
      "INSERT INTO initial_cha (id, name, org_min_id) VALUES (1, 'initcha1', ",
      om_id, ")"))
  }
  init_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM initial_cha LIMIT 1")$id[1L],
    error = function(e) 1L)

  n   <- nrow(chas)
  cnt <- max(chas$id)
  idx <- seq_len(n)

  hyds <- data.frame(
    id       = idx,
    name     = mapply(.gis_name, "hyd", chas$id, cnt),
    wd       = pmax(chas$wid2, 0.1),
    dp       = pmax(chas$dep2, 0.1),
    slp      = pmax(chas$slo2 / 100, 0.0001),
    len      = pmax(chas$len2 / 1000, 0.001),
    mann     = 0.05,
    k        = 1.0,
    cov_fact = 0.005,
    wd_rto   = 10.0,
    eq_slp   = 0.001,
    d50      = 12.0,
    stringsAsFactors = FALSE)

  chan_ltes <- data.frame(
    id      = idx,
    name    = mapply(.gis_name, "cha", chas$id, cnt),
    hyd_id  = idx,
    init_id = init_id,
    stringsAsFactors = FALSE)

  # Fall back to subbasin lat/lon/elev when channel midlat/midlon are 0 or NA.
  subs <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, lat, lon, elev FROM gis_subbasins ORDER BY id"),
    error = function(e) data.frame(id = integer(0), lat = numeric(0),
                                   lon = numeric(0), elev = numeric(0)))
  sub_lat  <- if (nrow(subs) > 0L) setNames(subs$lat,  as.character(subs$id)) else c()
  sub_lon  <- if (nrow(subs) > 0L) setNames(subs$lon,  as.character(subs$id)) else c()
  sub_elev <- if (nrow(subs) > 0L) setNames(subs$elev, as.character(subs$id)) else c()

  lat  <- ifelse(!is.na(chas$midlat)  & chas$midlat  != 0,
                 chas$midlat, sub_lat[as.character(chas$subbasin)])
  lon  <- ifelse(!is.na(chas$midlon)  & chas$midlon  != 0,
                 chas$midlon, sub_lon[as.character(chas$subbasin)])
  elev <- ifelse(!is.na(chas$elevmin) & chas$elevmin != 0,
                 chas$elevmin, sub_elev[as.character(chas$subbasin)])

  chan_cons <- data.frame(
    id     = idx,
    name   = mapply(.gis_name, "cha", chas$id, cnt),
    gis_id = chas$id,
    lat    = lat,
    lon    = lon,
    elev   = elev,
    area   = chas$areac,
    ovfl   = 0L,
    rule   = 0L,
    stringsAsFactors = FALSE)

  # Assign nearest weather station to each channel
  wst_rows <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, lat, lon FROM weather_sta_cli"),
    error = function(e) data.frame(id = integer(0), lat = numeric(0),
                                   lon = numeric(0)))
  if (nrow(wst_rows) > 0L) {
    chan_cons$wst_id <- vapply(seq_len(nrow(chan_cons)), function(i) {
      d2 <- (wst_rows$lat - chan_cons$lat[i])^2 +
            (wst_rows$lon - chan_cons$lon[i])^2
      wst_rows$id[which.min(d2)]
    }, integer(1L))
  }

  .gis_write(con, "hyd_sed_lte_cha",  hyds)
  .gis_write(con, "channel_lte_cha",  chan_ltes)
  .gis_write_safe(con, "chandeg_con",      chan_cons)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 4: Reservoirs from gis_water
# --------------------------------------------------------------------------
.gis_insert_reservoirs <- function(con) {
  if (.gis_count(con, "reservoir_res") > 0L) return(invisible(NULL))
  waters <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_water ORDER BY id"),
    error = function(e) NULL)
  if (is.null(waters) || nrow(waters) == 0L) return(invisible(NULL))

  .gis_insert_om_water(con)
  om_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM om_water_ini LIMIT 1")$id[1L],
    error = function(e) 1L)

  if (.gis_count(con, "initial_res") == 0L) {
    .gis_exec(con, paste0(
      "INSERT INTO initial_res (id, name, org_min_id) VALUES (1,'initres1',",
      om_id, ")"))
  }
  init_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM initial_res LIMIT 1")$id[1L],
    error = function(e) 1L)

  if (.gis_count(con, "sediment_res") == 0L) {
    .gis_exec(con, "
      INSERT INTO sediment_res (id, name, sed_stl, velsetl_d50, velsetl_stl)
      VALUES (1, 'sedres1', 1, 10, 1)")
  }
  sed_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM sediment_res LIMIT 1")$id[1L],
    error = function(e) 1L)

  if (.gis_count(con, "nutrients_res") == 0L) {
    .gis_exec(con, "
      INSERT INTO nutrients_res
        (id,name,mid_start,mid_end,mid_n_stl,n_stl,mid_p_stl,p_stl,
         chla_co,secchi_co,theta_n,theta_p,n_min_stl,p_min_stl)
      VALUES (1,'nutres1',5,10,5.5,5.5,10,10,1,1,1,1,0.1,0.01)")
  }
  nut_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM nutrients_res LIMIT 1")$id[1L],
    error = function(e) 1L)

  n   <- nrow(waters)
  cnt <- max(waters$id)
  idx <- seq_len(n)

  res_names <- mapply(.gis_name, tolower(waters$wtype), waters$id, cnt)

  hyd_res <- data.frame(
    id      = idx,
    name    = res_names,
    yr_op   = 1.0,
    mon_op  = 1.0,
    area_ps = waters$area,
    vol_ps  = waters$area * 10,
    area_es = waters$area * 1.15,
    vol_es  = waters$area * 1.15 * 10,
    k       = 0.0,
    evap_co = 0.6,
    shp_co1 = 0.0,
    shp_co2 = 0.0,
    stringsAsFactors = FALSE)

  res_objs <- data.frame(
    id      = idx,
    name    = res_names,
    init_id = init_id,
    hyd_id  = idx,
    sed_id  = sed_id,
    nut_id  = nut_id,
    stringsAsFactors = FALSE)

  res_cons <- data.frame(
    id     = idx,
    name   = res_names,
    gis_id = waters$id,
    lat    = waters$lat,
    lon    = waters$lon,
    elev   = waters$elev,
    area   = waters$area,
    ovfl   = 0L,
    rule   = 0L,
    stringsAsFactors = FALSE)

  .gis_write(con, "hydrology_res", hyd_res)
  .gis_write(con, "reservoir_res", res_objs)
  .gis_write(con, "reservoir_con", res_cons)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 5: Recall (point sources) from gis_points
# --------------------------------------------------------------------------
.gis_insert_recall <- function(con) {
  if (.gis_count(con, "recall_rec") > 0L) return(invisible(NULL))
  pts <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT * FROM gis_points WHERE ptype IN ('P','I') ORDER BY id"),
    error = function(e) NULL)
  if (is.null(pts) || nrow(pts) == 0L) return(invisible(NULL))

  cnt <- max(pts$id)
  idx <- seq_len(nrow(pts))

  recs <- data.frame(
    id      = idx,
    name    = mapply(.gis_name, "pt", pts$id, cnt),
    rec_typ = 4L,
    stringsAsFactors = FALSE)
  rec_cons <- data.frame(
    id     = idx,
    name   = mapply(.gis_name, "pt", pts$id, cnt),
    gis_id = pts$id,
    lat    = pts$lat,
    lon    = pts$lon,
    elev   = pts$elev,
    area   = 0.0,
    ovfl   = 0L,
    rule   = 0L,
    stringsAsFactors = FALSE)

  .gis_write(con, "recall_rec", recs)
  .gis_write(con, "recall_con", rec_cons)
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 6: HRUs from gis_hrus (standard projects)
# Schema (rQSWATPlus):
#   hydrology_hyd:  (id,name,lat_ttime,lat_sed,can_max,esco,epco,
#                    orgn_enrich,orgp_enrich,cn3_swf,bio_mix,perco,
#                    lat_orgn,lat_orgp,harg_pet,latq_co,cn2)
#   topography_hyd: (id,name,slp,slp_len,lat_len,dist_cha,depos)
#                   NOTE: appended rows, ids continue from routing-unit topos
#   hru_data_hru:   (id,name,topo_id,hydro_id,soil_id,lu_mgt_id,
#                    soil_plant_ini_id,surf_stor,snow_id,field_id)
#   hru_con:        (id,name,gis_id,area,lat,lon,elev,ovfl,rule)
#   rout_unit_ele:  (id,name,rtu_id,obj_id,obj_typ,frac,dlr_id)
#   ls_unit_ele:    (id,name)  [minimal]
# --------------------------------------------------------------------------
.gis_insert_hrus <- function(con) {
  # When hru_data_hru is already populated (e.g. by rQSWATPlus), we still need
  # to create the related tables (hydrology_hyd, hru_con, rout_unit_ele,
  # ls_unit_ele, topography_hyd for HRUs) that depend on it.  Only skip
  # individual tables that are already populated.
  hru_data_exists <- .gis_count(con, "hru_data_hru") > 0L

  hrus <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_hrus ORDER BY id"),
    error = function(e) NULL)
  if (is.null(hrus) || nrow(hrus) == 0L) return(invisible(NULL))

  # Ensure soils_sol has a row for every unique soil name
  .gis_ensure_soils(con, unique(hrus$soil))

  # Ensure nutrients_sol default row
  nut_sol_id <- .gis_ensure_nutrients_sol(con)

  # Ensure soil_plant_ini default row
  sp_id <- .gis_ensure_soil_plant_ini(con, nut_sol_id)

  # Ensure landuse_lum has rows for each unique land use
  lum_dict <- .gis_ensure_landuse_lum(con, unique(hrus$landuse))

  # Create management schedules for annual crop land uses (mirrors Python import_gis.py)
  .gis_ensure_management_sch(con, unique(hrus$landuse))

  # Get snow_sno id (should already exist from populate_from_datasets)
  snow_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM snow_sno LIMIT 1")$id[1L],
    error = function(e) NA_integer_)

  n   <- nrow(hrus)
  cnt <- max(hrus$id)
  idx <- seq_len(n)

  # Soil ids (from soils_sol by name)
  soils_map <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, name, hyd_grp FROM soils_sol"),
    error = function(e) data.frame(id = integer(0), name = character(0),
                                   hyd_grp = character(0)))
  soils_id <- soils_map$id[match(hrus$soil, soils_map$name)]
  soils_id[is.na(soils_id)] <- if (nrow(soils_map) > 0L) soils_map$id[1L] else 1L

  # Get hyd_grp per HRU for hydrology parameter computation
  hyd_grp_for_hru <- if ("hyd_grp" %in% names(soils_map)) {
    soils_map$hyd_grp[match(hrus$soil, soils_map$name)]
  } else {
    rep(NA_character_, n)
  }

  # Compute perco, cn3_swf, latq_co from soil hydrological group and slope
  # Mirrors Python Hydrology_hyd.get_perco_cn3_swf_latq_co()
  hyd_params <- .get_perco_cn3_swf_latq_co(hyd_grp_for_hru, hrus$slope / 100)

  # Landuse lum ids
  lu_ids <- unname(lum_dict[tolower(hrus$landuse)])
  lu_ids[is.na(lu_ids)] <- if (length(lum_dict) > 0L) lum_dict[[1L]] else 1L

  # field_id: link to field_fld via rout_unit (1 field per LSU/routing unit)
  rtu_map <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, gis_id FROM rout_unit_con"),
    error = function(e) data.frame(id = integer(0), gis_id = integer(0)))
  rtu_id_for_hru <- rtu_map$id[match(hrus$lsu, rtu_map$gis_id)]
  rtu_id_for_hru[is.na(rtu_id_for_hru)] <- 1L
  # field_id matches the field created for each routing unit (same id)
  field_id_for_hru <- rtu_id_for_hru

  # Topography_hyd: one row per HRU (Python inserts these starting after RTU topos)
  # topo_id starts after the existing topography_hyd entries (RTU level)
  existing_topo_count <- .gis_count(con, "topography_hyd")
  topo_start_id <- existing_topo_count + 1L
  topo_ids_for_hru <- seq(topo_start_id, length.out = n)

  # hydrology_hyd: one row per HRU (mirrors Python)
  if (.gis_count(con, "hydrology_hyd") == 0L) {
    hyds <- data.frame(
      id          = idx,
      name        = mapply(.gis_name, "hyd", hrus$id, cnt),
      lat_ttime   = 0.0,
      lat_sed     = 0.0,
      can_max     = 1.0,
      esco        = 0.95,
      epco        = 0.50,
      orgn_enrich = 0.0,
      orgp_enrich = 0.0,
      cn3_swf     = hyd_params$cn3_swf,
      bio_mix     = 0.20,
      perco       = hyd_params$perco,
      lat_orgn    = 0.0,
      lat_orgp    = 0.0,
      pet_co      = 1.0,
      latq_co     = hyd_params$latq_co,
      stringsAsFactors = FALSE)
    .gis_write_safe(con, "hydrology_hyd", hyds)
  }

  # topography_hyd: one row per HRU (mirrors Python)
  # Only insert if no HRU topography entries exist yet
  hru_topo_count <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT COUNT(*) AS n FROM topography_hyd WHERE type = 'hru'")$n[1L],
    error = function(e) 0L)
  if (is.na(hru_topo_count) || hru_topo_count == 0L) {
    hru_topos <- data.frame(
      id       = topo_ids_for_hru,
      name     = mapply(.gis_name, "topohru", hrus$id, cnt),
      slp      = pmax(hrus$slope / 100, 0.001),
      slp_len  = sapply(hrus$slope, .slope_len),
      lat_len  = sapply(hrus$slope, .slope_len),
      dist_cha = 121.0,
      depos    = 0.0,
      type     = "hru",
      stringsAsFactors = FALSE)
    .gis_write_safe(con, "topography_hyd", hru_topos)
  }

  # hru_data_hru: only create if not already present
  if (!hru_data_exists) {
    hru_objs <- data.frame(
      id                 = idx,
      name               = mapply(.gis_name, "hru", hrus$id, cnt),
      topo_id            = topo_ids_for_hru,
      hydro_id           = idx,
      soil_id            = soils_id,
      lu_mgt_id          = lu_ids,
      soil_plant_init_id = sp_id,
      surf_stor_id       = NA_integer_,
      snow_id            = if (!is.na(snow_id)) snow_id else NA_integer_,
      field_id           = NA_integer_,
      description        = NA_character_,
      stringsAsFactors   = FALSE)
    .gis_write_safe(con, "hru_data_hru", hru_objs)
  }

  # hru_con: includes wst_id and hru_id (FK to hru_data_hru)
  if (.gis_count(con, "hru_con") == 0L) {
    hru_cons <- data.frame(
      id     = idx,
      name   = mapply(.gis_name, "hru", hrus$id, cnt),
      gis_id = hrus$id,
      area   = hrus$arslp,
      lat    = hrus$lat,
      lon    = hrus$lon,
      elev   = hrus$elev,
      wst_id = NA_integer_,
      cst_id = NA_integer_,
      ovfl   = 0L,
      rule   = 0L,
      hru_id = idx,
      stringsAsFactors = FALSE)
    .gis_write_safe(con, "hru_con", hru_cons)
  }

  # rout_unit_ele: links HRUs to their routing units
  if (.gis_count(con, "rout_unit_ele") == 0L) {
    lsu_area_map <- tryCatch(
      DBI::dbGetQuery(con, "SELECT id, area FROM gis_lsus"),
      error = function(e) data.frame(id = integer(0), area = numeric(0)))
    arlsu_for_hru <- lsu_area_map$area[match(hrus$lsu, lsu_area_map$id)]
    arlsu_for_hru[is.na(arlsu_for_hru) | arlsu_for_hru <= 0] <- 1.0
    
    # frac should be arslp / sum(arslp) within each rtu_id
    # i.e. each HRU's fraction of its routing unit's total slope area
    
    rtu_arslp_totals <- tapply(hrus$arslp, rtu_id_for_hru, sum)
    rtu_arslp_for_hru <- rtu_arslp_totals[as.character(rtu_id_for_hru)]
    
    rtu_eles <- data.frame(
      id      = idx,
      name    = mapply(.gis_name, "hru", hrus$id, cnt),
      rtu_id  = rtu_id_for_hru,
      obj_typ = "hru",
      obj_id  = idx,
      frac    = hrus$arslp / rtu_arslp_for_hru,  # fraction within RTU
      dlr_id  = NA_integer_,
      stringsAsFactors = FALSE)
    # rtu_eles <- data.frame(
    #   id      = idx,
    #   name    = mapply(.gis_name, "hru", hrus$id, cnt),
    #   rtu_id  = rtu_id_for_hru,
    #   obj_typ = "hru",
    #   obj_id  = idx,
    #   frac    = pmin(1.0, hrus$arslp / arlsu_for_hru),
    #   dlr_id  = NA_integer_,
    #   stringsAsFactors = FALSE)
    .gis_write_safe(con, "rout_unit_ele", rtu_eles)
  }

  # ls_unit_ele: full columns matching Python (obj_typ, obj_typ_no, bsn_frac,
  # sub_frac, reg_frac, ls_unit_def_id)
  if (.gis_count(con, "ls_unit_ele") == 0L) {
    # bsn_area = total basin area (sum of all subbasin areas)
    bsn_area <- tryCatch(
      DBI::dbGetQuery(con, "SELECT SUM(area) AS tot FROM gis_subbasins")$tot[1L],
      error = function(e) NA_real_)
    if (is.na(bsn_area) || bsn_area <= 0) bsn_area <- sum(hrus$arslp, na.rm = TRUE)
    if (bsn_area <= 0) bsn_area <- 1.0

    lsu_area_map2 <- tryCatch(
      DBI::dbGetQuery(con, "SELECT id, area FROM gis_lsus"),
      error = function(e) data.frame(id = integer(0), area = numeric(0)))
    arlsu2 <- lsu_area_map2$area[match(hrus$lsu, lsu_area_map2$id)]
    arlsu2[is.na(arlsu2) | arlsu2 <= 0] <- 1.0

    ls_eles <- data.frame(
      id             = idx,
      name           = mapply(.gis_name, "hru", hrus$id, cnt),
      obj_typ        = "hru",
      obj_typ_no     = idx,
      bsn_frac       = hrus$arslp / bsn_area,
      sub_frac       = hrus$arslp / arlsu2,
      reg_frac       = 0.0,
      ls_unit_def_id = rtu_id_for_hru,
      stringsAsFactors = FALSE)
    .gis_write_safe(con, "ls_unit_ele", ls_eles)
  }

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Internal helper: compute perco, cn3_swf, latq_co from soil hydrological
# group and slope (decimal).  Mirrors Python
# Hydrology_hyd.get_perco_cn3_swf_latq_co()
# --------------------------------------------------------------------------
.get_perco_cn3_swf_latq_co <- function(hyd_grp, slope) {
  n <- length(hyd_grp)
  perco   <- rep(0.05, n)
  cn3_swf <- rep(0.95, n)
  latq_co <- rep(0.01, n)

  for (i in seq_len(n)) {
    grp <- toupper(trimws(as.character(hyd_grp[i])))
    s   <- if (is.na(slope[i])) 0 else slope[i]

    leach_pot  <- "low"
    runoff_pot <- "low"

    # Thresholds match Python exactly (slope is in decimal form = percent/100)
    if (grp == "A") {
      leach_pot  <- "high"
      runoff_pot <- if (s < 6) "low" else if (s <= 12) "mod" else "high"
    } else if (grp == "B") {
      leach_pot  <- if (s < 6) "high" else "mod"
      runoff_pot <- if (s < 4) "low" else if (s <= 6) "mod" else "high"
    } else if (grp == "C") {
      leach_pot  <- if (s < 12) "mod" else "low"
      runoff_pot <- if (s < 2) "low" else if (s <= 6) "mod" else "high"
    } else if (grp == "D") {
      leach_pot  <- "low"
      runoff_pot <- if (s < 2) "low" else if (s <= 4) "mod" else "high"
    }
    # else: defaults (low leach, low runoff) when hyd_grp is NA/unknown

    perco[i]   <- if (leach_pot == "high") 0.9 else if (leach_pot == "mod") 0.5 else 0.05
    cn3_swf[i] <- if (runoff_pot == "high") 0 else if (runoff_pot == "mod") 0.3 else 0.95
    latq_co[i] <- if (runoff_pot == "high") 0.9 else if (runoff_pot == "mod") 0.2 else 0.01
  }

  list(perco = perco, cn3_swf = cn3_swf, latq_co = latq_co)
}

# Helper: ensure soils_sol has a row for every unique soil name (id, name)
.gis_ensure_soils <- function(con, soil_names) {
  existing <- tryCatch(
    DBI::dbGetQuery(con, "SELECT name FROM soils_sol")$name,
    error = function(e) character(0))
  missing_soils <- setdiff(soil_names, existing)
  if (length(missing_soils) == 0L) return(invisible(NULL))

  next_id <- max(c(0L, tryCatch(
    DBI::dbGetQuery(con, "SELECT MAX(id) AS m FROM soils_sol")$m[1L],
    error = function(e) 0L)), na.rm = TRUE) + 1L

  for (sname in missing_soils) {
    .gis_exec(con, paste0(
      "INSERT OR IGNORE INTO soils_sol (id, name) VALUES (",
      next_id, ", '", sname, "')"))
    next_id <- next_id + 1L
  }
  invisible(NULL)
}

# Helper: ensure nutrients_sol has a default row; return its id
.gis_ensure_nutrients_sol <- function(con) {
  n <- .gis_count(con, "nutrients_sol")
  if (n > 0L) {
    return(tryCatch(
      DBI::dbGetQuery(con, "SELECT id FROM nutrients_sol LIMIT 1")$id[1L],
      error = function(e) 1L))
  }
  .gis_exec(con,
    "INSERT INTO nutrients_sol (id, name, exp_co) VALUES (1,'soilnut1',0.0005)")
  1L
}

# Helper: ensure soil_plant_ini has a default row; return its id
.gis_ensure_soil_plant_ini <- function(con, nut_id) {
  n <- .gis_count(con, "soil_plant_ini")
  if (n > 0L) {
    return(tryCatch(
      DBI::dbGetQuery(con, "SELECT id FROM soil_plant_ini LIMIT 1")$id[1L],
      error = function(e) 1L))
  }
  .gis_exec(con, "INSERT INTO soil_plant_ini (id, name) VALUES (1,'soilplant1')")
  1L
}

# Helper: ensure landuse_lum has rows for each land use; return name->id map.
# For each new land use code, populates FK columns by looking up:
#   - plants_plt for plant-based land uses (sets plnt_com_id via plant_ini)
#   - urban_urb for urban land uses (sets urban_id)
#   - default cn2_id (id=5), cons_prac_id (id=1), ov_mann_id (id=2) from datasets
# Mirrors Python import_gis.py insert_landuse().
.gis_ensure_landuse_lum <- function(con, landuse_codes) {
  existing <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, name FROM landuse_lum"),
    error = function(e) data.frame(id = integer(0), name = character(0)))
  lum_map <- setNames(existing$id, tolower(existing$name))

  missing_codes <- setdiff(tolower(landuse_codes), names(lum_map))
  if (length(missing_codes) == 0L) return(lum_map)

  # Default FK ids (Python defaults: cn2=5, cons_prac=1, ov_mann=2)
  default_cn2 <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM cntable_lum WHERE id = 5 LIMIT 1")$id[1L],
    error = function(e) NA_integer_)
  if (is.na(default_cn2)) default_cn2 <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM cntable_lum ORDER BY id LIMIT 1 OFFSET 4")$id[1L],
    error = function(e) NA_integer_)

  default_cons_prac <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM cons_prac_lum ORDER BY id LIMIT 1")$id[1L],
    error = function(e) NA_integer_)

  default_ov_mann <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM ovn_table_lum WHERE id = 2 LIMIT 1")$id[1L],
    error = function(e) NA_integer_)
  if (is.na(default_ov_mann)) default_ov_mann <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM ovn_table_lum ORDER BY id LIMIT 1 OFFSET 1")$id[1L],
    error = function(e) NA_integer_)

  # Urban cn2 and ov_mann (Python defaults: cn2=49, ov_mann=18)
  urban_cn2 <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM cntable_lum WHERE id = 49 LIMIT 1")$id[1L],
    error = function(e) NA_integer_)
  if (is.na(urban_cn2)) urban_cn2 <- default_cn2

  urban_ov_mann <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM ovn_table_lum WHERE id = 18 LIMIT 1")$id[1L],
    error = function(e) NA_integer_)
  if (is.na(urban_ov_mann)) urban_ov_mann <- default_ov_mann

  next_id <- max(c(0L, existing$id), na.rm = TRUE) + 1L
  next_pi_id <- .gis_count(con, "plant_ini") + 1L

  for (code in missing_codes) {
    # Try plant type first
    plant_row <- tryCatch(
      DBI::dbGetQuery(con, paste0(
        "SELECT id, name FROM plants_plt WHERE LOWER(name) = '", code, "' LIMIT 1")),
      error = function(e) data.frame())

    if (nrow(plant_row) > 0L) {
      # Create or reuse a plant_ini entry named '{code}_comm'
      comm_name <- paste0(code, "_comm")
      pi_id <- tryCatch(
        DBI::dbGetQuery(con, paste0(
          "SELECT id FROM plant_ini WHERE name = '", comm_name, "' LIMIT 1"))$id[1L],
        error = function(e) NA_integer_)
      if (is.na(pi_id)) {
        # Get rot_yr_ini from datasets plant_ini if available
        ds_rot <- tryCatch(
          DBI::dbGetQuery(con, paste0(
            "SELECT rot_yr_ini FROM plant_ini WHERE name = '", comm_name, "' LIMIT 1"))$rot_yr_ini[1L],
          error = function(e) 1L)
        rot_yr <- if (!is.na(ds_rot)) ds_rot else 1L
        .gis_exec(con, paste0(
          "INSERT OR IGNORE INTO plant_ini (id, name, rot_yr_ini) VALUES (",
          next_pi_id, ", '", comm_name, "', ", rot_yr, ")"))
        pi_id_check <- tryCatch(
          DBI::dbGetQuery(con, paste0(
            "SELECT id FROM plant_ini WHERE name = '", comm_name, "' LIMIT 1"))$id[1L],
          error = function(e) NA_integer_)
        if (!is.na(pi_id_check)) {
          pi_id <- pi_id_check
          next_pi_id <- next_pi_id + 1L

          # Populate plant_ini_item: add one default item using the plant itself
          p_id <- plant_row$id[1L]
          has_item <- tryCatch(
            DBI::dbGetQuery(con, paste0(
              "SELECT COUNT(*) AS n FROM plant_ini_item WHERE plant_ini_id = ", pi_id))$n[1L],
            error = function(e) 0L)
          if (is.na(has_item) || has_item == 0L) {
            nxt_item_id <- tryCatch(
              DBI::dbGetQuery(con, "SELECT COALESCE(MAX(id),0)+1 AS n FROM plant_ini_item")$n[1L],
              error = function(e) 1L)
            .gis_exec(con, paste0(
              "INSERT OR IGNORE INTO plant_ini_item ",
              "(id, plant_ini_id, plnt_name_id, lc_status, lai_init, bm_init, ",
              "phu_init, plnt_pop, yrs_init, rsd_init) VALUES (",
              nxt_item_id, ", ", pi_id, ", ", p_id, ", 0, 0.0, 0.0, 0.0, 0.0, 0.0, 10000.0)"))
          }
        }
      }

      cn2_val     <- if (!is.na(default_cn2)) default_cn2 else "NULL"
      cons_val    <- if (!is.na(default_cons_prac)) default_cons_prac else "NULL"
      ovmann_val  <- if (!is.na(default_ov_mann)) default_ov_mann else "NULL"
      pi_val      <- if (!is.na(pi_id)) pi_id else "NULL"

      .gis_exec(con, paste0(
        "INSERT OR IGNORE INTO landuse_lum (id, name, plnt_com_id, cn2_id, cons_prac_id, ov_mann_id) VALUES (",
        next_id, ", '", code, "', ", pi_val, ", ", cn2_val, ", ", cons_val, ", ", ovmann_val, ")"))

    } else {
      # Try urban type
      urban_row <- tryCatch(
        DBI::dbGetQuery(con, paste0(
          "SELECT id FROM urban_urb WHERE LOWER(name) = '", code, "' LIMIT 1")),
        error = function(e) data.frame())

      if (nrow(urban_row) > 0L) {
        u_id       <- urban_row$id[1L]
        cn2_val    <- if (!is.na(urban_cn2)) urban_cn2 else "NULL"
        cons_val   <- if (!is.na(default_cons_prac)) default_cons_prac else "NULL"
        ovmann_val <- if (!is.na(urban_ov_mann)) urban_ov_mann else "NULL"

        .gis_exec(con, paste0(
          "INSERT OR IGNORE INTO landuse_lum ",
          "(id, name, urban_id, urb_ro, cn2_id, cons_prac_id, ov_mann_id) VALUES (",
          next_id, ", '", code, "', ", u_id, ", 'buildup_washoff', ",
          cn2_val, ", ", cons_val, ", ", ovmann_val, ")"))
      } else {
        # Unknown type: insert minimal row
        .gis_exec(con, paste0(
          "INSERT OR IGNORE INTO landuse_lum (id, name) VALUES (", next_id, ", '", code, "')"))
      }
    }

    lum_map[[code]] <- next_id
    next_id <- next_id + 1L
  }
  lum_map
}

# Helper: create management_sch auto-op schedules for annual crop land uses.
# Mirrors Python import_gis.py logic: for each unique landuse that matches a
# warm_annual or cold_annual plant in plants_plt, create a "{plant}_rot"
# management schedule with an auto-op referencing the pl_hv_summer1 (warm) or
# pl_hv_winter1 (cold) decision table.  Returns a name->id map of schedules.
.gis_ensure_management_sch <- function(con, landuse_codes) {
  # Skip if management_sch already has data
  if (.gis_count(con, "management_sch") > 0L) {
    existing <- tryCatch(
      DBI::dbGetQuery(con, "SELECT id, name FROM management_sch"),
      error = function(e) data.frame(id = integer(0), name = character(0)))
    return(setNames(existing$id, existing$name))
  }

  # Look up plant types for the landuse codes
  plants <- tryCatch(
    DBI::dbGetQuery(con, paste0(
      "SELECT name, plnt_typ FROM plants_plt WHERE name IN (",
      paste0("'", unique(tolower(landuse_codes)), "'", collapse = ","), ")")),
    error = function(e) data.frame(name = character(0), plnt_typ = character(0)))

  if (nrow(plants) == 0L) return(invisible(setNames(integer(0), character(0))))

  annual_plants <- plants[grepl("^(warm_annual|cold_annual)", plants$plnt_typ), , drop = FALSE]
  if (nrow(annual_plants) == 0L) return(invisible(setNames(integer(0), character(0))))

  # Get the decision table IDs for summer and winter schedules
  summer_dt <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM d_table_dtl WHERE name = 'pl_hv_summer1' LIMIT 1")$id[1L],
    error = function(e) NA_integer_)
  winter_dt <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM d_table_dtl WHERE name = 'pl_hv_winter1' LIMIT 1")$id[1L],
    error = function(e) NA_integer_)

  if (is.na(summer_dt) && is.na(winter_dt)) {
    return(invisible(setNames(integer(0), character(0))))
  }

  next_mgt_id <- .gis_count(con, "management_sch") + 1L
  next_auto_id <- .gis_count(con, "management_sch_auto") + 1L
  mgt_map <- setNames(integer(0), character(0))

  for (i in seq_len(nrow(annual_plants))) {
    plant_name <- annual_plants$name[i]
    plnt_typ   <- annual_plants$plnt_typ[i]
    mgt_name   <- paste0(plant_name, "_rot")

    dt_id <- if (grepl("^warm_annual", plnt_typ)) summer_dt else winter_dt
    if (is.na(dt_id)) next

    .gis_exec(con, paste0(
      "INSERT OR IGNORE INTO management_sch (id, name) VALUES (", next_mgt_id,
      ", '", mgt_name, "')"))
    .gis_exec(con, paste0(
      "INSERT INTO management_sch_auto (id, management_sch_id, d_table_id, plant1) VALUES (",
      next_auto_id, ", ", next_mgt_id, ", ", dt_id, ", '", plant_name, "')"))

    mgt_map[mgt_name] <- next_mgt_id
    next_mgt_id  <- next_mgt_id  + 1L
    next_auto_id <- next_auto_id + 1L
  }

  mgt_map
}

# --------------------------------------------------------------------------
# Step 7: Aquifers from gis_aquifers
# Schema (rQSWATPlus):
#   initial_aqu: (id, name, org_min_id)
#   aquifer_aqu: (id, name, init_id, gw_flo, dep_bot, dep_wt, no3_n,
#                 sol_p, ptl_n, ptl_p, bf_max, alpha_bf, revap,
#                 rchg_dp, spec_yld, hl_no3n, flo_min, revap_min)
#   aquifer_con: (id, name, gis_id, area, lat, lon, elev, ovfl, rule)
# --------------------------------------------------------------------------
.gis_insert_aquifers <- function(con) {
  if (.gis_count(con, "aquifer_aqu") > 0L) return(invisible(NULL))

  aqfs  <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_aquifers ORDER BY id"),
    error = function(e) NULL)
  deep  <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_deep_aquifers ORDER BY id"),
    error = function(e) NULL)

  total <- 0L
  if (!is.null(aqfs))  total <- total + nrow(aqfs)
  if (!is.null(deep))  total <- total + nrow(deep)
  if (total == 0L) return(invisible(NULL))

  # One default initial_aqu row
  .gis_insert_om_water(con)
  om_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM om_water_ini LIMIT 1")$id[1L],
    error = function(e) 1L)
  if (.gis_count(con, "initial_aqu") == 0L) {
    .gis_exec(con, paste0(
      "INSERT INTO initial_aqu (id, name, org_min_id) VALUES (1,'initaqu1',",
      om_id, ")"))
  }
  init_id <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id FROM initial_aqu LIMIT 1")$id[1L],
    error = function(e) 1L)

  aqu_rows <- list()
  con_rows <- list()
  i <- 1L

  # Shallow aquifers
  if (!is.null(aqfs) && nrow(aqfs) > 0L) {
    cnt <- max(aqfs$id)
    for (j in seq_len(nrow(aqfs))) {
      row <- aqfs[j, ]
      nm <- .gis_name("aqu", row$id, cnt)
      aqu_rows[[i]] <- data.frame(
        id        = i, name = nm, init_id   = init_id,
        gw_flo    = 0.05,  dep_bot   = 10.0,   dep_wt    = 2000.0,
        no3_n     = 0.0,   sol_p     = 0.0,    ptl_n     = 0.0,
        ptl_p     = 0.0,   bf_max    = 1.0,    alpha_bf  = 0.048,
        revap     = 0.02,  rchg_dp   = 0.05,   spec_yld  = 0.05,
        hl_no3n   = 0.0,   flo_min   = 0.0,    revap_min = 0.0,
        stringsAsFactors = FALSE)
      con_rows[[i]] <- data.frame(
        id = i, name = nm, gis_id = row$id,
        lat = row$lat, lon = row$lon, area = row$area, elev = row$elev,
        ovfl = 0L, rule = 0L,
        stringsAsFactors = FALSE)
      i <- i + 1L
    }
  }

  # Deep aquifers
  if (!is.null(deep) && nrow(deep) > 0L) {
    cnt_d <- max(deep$id)
    for (j in seq_len(nrow(deep))) {
      row <- deep[j, ]
      nm <- .gis_name("aqu_deep", row$id, cnt_d)
      aqu_rows[[i]] <- data.frame(
        id = i, name = nm, init_id = init_id,
        gw_flo = 0.05, dep_bot = 10.0, dep_wt = 2000.0,
        no3_n = 0.0, sol_p = 0.0, ptl_n = 0.0, ptl_p = 0.0,
        bf_max = 1.0, alpha_bf = 0.048, revap = 0.02,
        rchg_dp = 0.05, spec_yld = 0.05, hl_no3n = 0.0,
        flo_min = 0.0, revap_min = 0.0,
        stringsAsFactors = FALSE)
      con_rows[[i]] <- data.frame(
        id = i, name = nm, gis_id = row$id,
        lat = row$lat, lon = row$lon, area = row$area, elev = row$elev,
        ovfl = 0L, rule = 0L,
        stringsAsFactors = FALSE)
      i <- i + 1L
    }
  }

  if (length(aqu_rows) > 0L) {
    .gis_write(con, "aquifer_aqu", do.call(rbind, aqu_rows))
    .gis_write(con, "aquifer_con", do.call(rbind, con_rows))
  }
  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 8: Connections from gis_routing
# Schema (_con_out): (id, {tbl}_id, order_id, obj_typ, obj_id, hyd_typ, frac)
# --------------------------------------------------------------------------
.gis_insert_connections <- function(con) {
  # Build category -> {gis_id -> con_id} lookup maps.
  # These are needed for both routing-based and synthetic fallback logic, so
  # they are built before the routing early-return check.
  .make_map <- function(con_tbl) {
    tryCatch(
      DBI::dbGetQuery(con, paste0("SELECT id, gis_id FROM ", con_tbl)),
      error = function(e) data.frame(id = integer(0), gis_id = integer(0)))
  }
  rtu_map <- .make_map("rout_unit_con")
  cha_map <- .make_map("chandeg_con")
  aqu_map <- .make_map("aquifer_con")
  res_map <- .make_map("reservoir_con")
  hru_map <- .make_map("hru_con")

  routing <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_routing"),
    error = function(e) NULL)

  if (!is.null(routing) && nrow(routing) > 0L) {
    cat_to_map <- list(
      lsu = rtu_map, sub = rtu_map,
      ch  = cha_map, sdc = cha_map,
      aqu = aqu_map,
      wtr = res_map, res = res_map, pnd = res_map,
      hru = hru_map)
    cat_to_typ <- list(
      lsu = "ru",  sub = "ru",
      ch  = "sdc", sdc = "sdc",
      aqu = "aqu",
      wtr = "res", res = "res", pnd = "res",
      hru = "hru")

    .lookup_id <- function(cat, gis_id) {
      m <- cat_to_map[[tolower(cat)]]
      if (is.null(m) || nrow(m) == 0L) return(NA_integer_)
      m$id[match(gis_id, m$gis_id)]
    }

    # Build PT pass-through node lookup: sourceid -> routing row.
    # Mirrors Python pt_source_row_dict in import_gis.py get_connections().
    pt_rows <- routing[tolower(routing$sourcecat) == "pt", , drop = FALSE]
    pt_dict <- if (nrow(pt_rows) > 0L) {
      setNames(
        lapply(seq_len(nrow(pt_rows)), function(i) pt_rows[i, , drop = FALSE]),
        as.character(pt_rows$sourceid))
    } else list()

    # Follow a PT chain from a routing row to its first non-PT destination.
    # Mirrors Python while-loop in get_connections():
    #   while con_row.sinkcat == RouteCat.PT: con_row = pt_source_row_dict[con_row.sinkid]
    .follow_pt <- function(row, max_iter = 100L) {
      iter <- 0L
      while (!is.null(row) && tolower(row$sinkcat) == "pt" && iter < max_iter) {
        row  <- pt_dict[[as.character(row$sinkid)]]
        iter <- iter + 1L
      }
      if (!is.null(row) && tolower(row$sinkcat) != "pt") row else NULL
    }

    # All rows with non-zero percent, excluding explicit outlets.
    # RouteCat.OUTLET = "X" in Python; PT-sink rows are kept for chain following.
    supported <- c("lsu","sub","ch","sdc","aqu","wtr","res","pnd")
    route_src <- routing[routing$percent > 0 &
                         !tolower(routing$sinkcat) %in% "x", , drop = FALSE]

    # Fallback: if gis_routing has no explicit CH/SDC source rows, derive channel
    # routing synthetically from the subbasin topology (sub→sub or sub→ch rows) +
    # gis_channels.subbasin. This handles QSWAT+ projects that store only
    # subbasin-level routing without explicit channel-to-channel rows.
    has_ch_src <- any(tolower(routing$sourcecat) %in% c("ch","sdc"))
    if (!has_ch_src && nrow(cha_map) > 0L) {
      gis_cha <- tryCatch(
        DBI::dbGetQuery(con, "SELECT id AS cha_gis_id, subbasin FROM gis_channels"),
        error = function(e) data.frame(cha_gis_id = integer(0), subbasin = integer(0)))
      if (nrow(gis_cha) > 0L) {
        sub_to_cha <- setNames(gis_cha$cha_gis_id, as.character(gis_cha$subbasin))
        sub_rows <- routing[tolower(routing$sourcecat) %in% c("sub","lsu") &
                            routing$percent > 0 &
                            !tolower(routing$sinkcat) %in% "x", , drop = FALSE]
        synth <- list()
        for (k in seq_len(nrow(sub_rows))) {
          r <- sub_rows[k, ]
          src_cha <- sub_to_cha[as.character(r$sourceid)]
          if (is.na(src_cha)) next
          snk_cha <- NA_integer_
          sk <- tolower(r$sinkcat)
          if (sk %in% c("sub","lsu")) {
            snk_cha <- sub_to_cha[as.character(r$sinkid)]
          } else if (sk %in% c("ch","sdc")) {
            snk_cha <- r$sinkid
          } else if (sk == "pt") {
            # follow PT chain; if it resolves to sub/ch, map to channel
            pt_row <- .follow_pt(r)
            if (!is.null(pt_row)) {
              fsk <- tolower(pt_row$sinkcat)
              if (fsk %in% c("sub","lsu")) snk_cha <- sub_to_cha[as.character(pt_row$sinkid)]
              else if (fsk %in% c("ch","sdc")) snk_cha <- pt_row$sinkid
            }
          }
          if (is.na(snk_cha)) next
          hyd <- if (!is.null(r$hyd_typ) && !is.na(r$hyd_typ) && nzchar(r$hyd_typ))
                   r$hyd_typ else "tot"
          synth[[length(synth) + 1L]] <- data.frame(
            sourceid  = as.integer(src_cha),
            sourcecat = "ch",
            hyd_typ   = hyd,
            sinkid    = as.integer(snk_cha),
            sinkcat   = "ch",
            percent   = r$percent,
            stringsAsFactors = FALSE)
        }
        if (length(synth) > 0L) {
          synth_df <- do.call(rbind, synth)
          synth_df <- synth_df[!duplicated(synth_df[, c("sourceid","sinkid")]), ]
          routing   <- rbind(routing, synth_df)
          route_src <- rbind(route_src, synth_df)
        }
      }
    }

    .build_con_out <- function(src_cats, id_col, src_map) {
      rows_sub <- route_src[tolower(route_src$sourcecat) %in% src_cats, , drop = FALSE]
      if (nrow(rows_sub) == 0L || nrow(src_map) == 0L) return(NULL)

      result <- list()
      orders <- list()
      for (k in seq_len(nrow(rows_sub))) {
        r <- rows_sub[k, ]
        # Follow PT chain when the direct sink is a pass-through node.
        con_row <- if (tolower(r$sinkcat) == "pt") .follow_pt(r) else r
        if (is.null(con_row)) next
        if (!tolower(con_row$sinkcat) %in% supported) next

        src_id <- .lookup_id(r$sourcecat,       r$sourceid)
        snk_id <- .lookup_id(con_row$sinkcat,   con_row$sinkid)
        typ    <- cat_to_typ[[tolower(con_row$sinkcat)]]
        if (is.na(src_id) || is.na(snk_id) || is.null(typ)) next
        key <- as.character(src_id)
        orders[[key]] <- if (!is.null(orders[[key]])) orders[[key]] + 1L else 1L
        hyd <- if (!is.null(con_row$hyd_typ) && !is.na(con_row$hyd_typ) &&
                   nzchar(con_row$hyd_typ)) con_row$hyd_typ else "tot"
        row_df <- data.frame(
          src_id, orders[[key]], typ, snk_id, hyd, r$percent / 100,
          stringsAsFactors = FALSE)
        names(row_df) <- c(id_col, "order_id", "obj_typ", "obj_id",
                           "hyd_typ", "frac")
        result[[length(result) + 1L]] <- row_df
      }
      if (length(result) > 0L) do.call(rbind, result) else NULL
    }

    if (.gis_count(con, "rout_unit_con_out") == 0L) {
      df <- .build_con_out(c("lsu","sub"), "rout_unit_con_id", rtu_map)
      if (!is.null(df)) .gis_write(con, "rout_unit_con_out", df)
    }

    if (.gis_count(con, "chandeg_con_out") == 0L) {
      df <- .build_con_out(c("ch","sdc"), "chandeg_con_id", cha_map)
      if (!is.null(df)) .gis_write(con, "chandeg_con_out", df)
    }

    if (.gis_count(con, "aquifer_con_out") == 0L) {
      df <- .build_con_out("aqu", "aquifer_con_id", aqu_map)
      if (!is.null(df)) .gis_write(con, "aquifer_con_out", df)
    }

    if (.gis_count(con, "reservoir_con_out") == 0L && nrow(res_map) > 0L) {
      df <- .build_con_out(c("wtr","pnd","res"), "reservoir_con_id", res_map)
      if (!is.null(df)) .gis_write(con, "reservoir_con_out", df)
    }

    if (.gis_count(con, "hru_con_out") == 0L && nrow(hru_map) > 0L) {
      df <- .build_con_out("hru", "hru_con_id", hru_map)
      if (!is.null(df)) .gis_write(con, "hru_con_out", df)
    }
  }

  # Synthetic fallback for aquifer_con_out when gis_routing is absent or has no
  # AQU source rows.  Each shallow aquifer gets up to two outflow connections
  # derived from the gis_aquifers topology:
  #   1. sdc -> corresponding channel in the same subbasin (hyd_typ = "tot")
  #   2. aqu -> corresponding deep aquifer via gis_aquifers.deep_aquifer (hyd_typ = "rhg")
  # Mirrors the Python behaviour where aquifer outflows are built from gis_routing
  # AQU rows; this fallback covers projects where those rows are absent.
  if (.gis_count(con, "aquifer_con_out") == 0L && nrow(aqu_map) > 0L) {
    gis_aqf <- tryCatch(
      DBI::dbGetQuery(con,
        "SELECT id, subbasin, deep_aquifer FROM gis_aquifers ORDER BY id"),
      error = function(e) data.frame(id = integer(0), subbasin = integer(0),
                                     deep_aquifer = integer(0)))

    if (nrow(gis_aqf) > 0L) {
      # Build subbasin -> channel gis_id lookup
      gis_cha_sub <- tryCatch(
        DBI::dbGetQuery(con, "SELECT id AS cha_gis_id, subbasin FROM gis_channels"),
        error = function(e) data.frame(cha_gis_id = integer(0),
                                       subbasin = integer(0)))
      sub_to_cha_gis <- if (nrow(gis_cha_sub) > 0L)
        setNames(gis_cha_sub$cha_gis_id, as.character(gis_cha_sub$subbasin))
      else c()

      # Separate shallow and deep aquifer_con rows by name prefix so that
      # overlapping gis_ids between gis_aquifers and gis_deep_aquifers are
      # resolved correctly.
      aqu_all <- tryCatch(
        DBI::dbGetQuery(con, "SELECT id, gis_id, name FROM aquifer_con ORDER BY id"),
        error = function(e) data.frame(id = integer(0), gis_id = integer(0),
                                       name = character(0)))
      shallow_df <- aqu_all[!grepl("^aqu_deep", aqu_all$name), , drop = FALSE]
      deep_df    <- aqu_all[ grepl("^aqu_deep", aqu_all$name), , drop = FALSE]
      shallow_map <- if (nrow(shallow_df) > 0L)
        setNames(shallow_df$id, as.character(shallow_df$gis_id)) else c()
      deep_map    <- if (nrow(deep_df) > 0L)
        setNames(deep_df$id, as.character(deep_df$gis_id)) else c()

      aqu_outs <- list()
      for (k in seq_len(nrow(gis_aqf))) {
        row    <- gis_aqf[k, ]
        aqu_id <- shallow_map[as.character(row$id)]
        if (is.na(aqu_id)) next

        order_id <- 0L

        # Connection 1: sdc -> channel in same subbasin (total flow)
        cha_gis_id <- sub_to_cha_gis[as.character(row$subbasin)]
        if (!is.na(cha_gis_id)) {
          cha_con_id <- cha_map$id[match(cha_gis_id, cha_map$gis_id)]
          if (!is.na(cha_con_id)) {
            order_id <- order_id + 1L
            aqu_outs[[length(aqu_outs) + 1L]] <- data.frame(
              aquifer_con_id = aqu_id, order_id = order_id,
              obj_typ = "sdc", obj_id = cha_con_id,
              hyd_typ = "tot", frac = 1.0,
              stringsAsFactors = FALSE)
          }
        }

        # Connection 2: aqu -> deep aquifer (return flow, rhg)
        if (!is.na(row$deep_aquifer) && row$deep_aquifer > 0L) {
          deep_con_id <- deep_map[as.character(row$deep_aquifer)]
          if (!is.na(deep_con_id)) {
            order_id <- order_id + 1L
            aqu_outs[[length(aqu_outs) + 1L]] <- data.frame(
              aquifer_con_id = aqu_id, order_id = order_id,
              obj_typ = "aqu", obj_id = deep_con_id,
              hyd_typ = "rhg", frac = 1.0,
              stringsAsFactors = FALSE)
          }
        }
      }
      if (length(aqu_outs) > 0L)
        .gis_write(con, "aquifer_con_out", do.call(rbind, aqu_outs))
    }
  }

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Step 9: ls_unit_def from rout_unit_con (minimal: id, name)
# --------------------------------------------------------------------------
.gis_insert_lsus <- function(con) {
  if (.gis_count(con, "ls_unit_def") > 0L) return(invisible(NULL))
  rtu_cons <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, name FROM rout_unit_con"),
    error = function(e) NULL)
  ele_rtus <- tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT rtu_id FROM rout_unit_ele"),
    error = function(e) NULL)
  if (is.null(rtu_cons) || nrow(rtu_cons) == 0L) return(invisible(NULL))

  if (!is.null(ele_rtus) && nrow(ele_rtus) > 0L)
    rtu_cons <- rtu_cons[rtu_cons$id %in% ele_rtus$rtu_id, , drop = FALSE]
  if (nrow(rtu_cons) == 0L) return(invisible(NULL))

  .gis_write(con, "ls_unit_def",
    data.frame(id = rtu_cons$id, name = rtu_cons$name,
               stringsAsFactors = FALSE))
  invisible(NULL)
}

# --------------------------------------------------------------------------
# LTE helpers
# --------------------------------------------------------------------------
.gis_insert_hru_ltes <- function(con) {
  if (.gis_count(con, "hru_lte_hru") > 0L) return(invisible(NULL))
  hrus <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_hrus ORDER BY id"),
    error = function(e) NULL)
  if (is.null(hrus) || nrow(hrus) == 0L) return(invisible(NULL))

  cnt <- max(hrus$id)
  idx <- seq_len(nrow(hrus))
  lum_dict <- .gis_ensure_landuse_lum(con, unique(hrus$landuse))

  # hru_lte_hru schema from rQSWATPlus is different - has many more fields
  # Use minimal defaults that SWAT+ LTE can work with
  hru_ltes <- data.frame(
    id       = idx,
    name     = mapply(.gis_name, "hru", hrus$id, cnt),
    cn2      = 65.0,
    usle_k   = 0.3,
    usle_ls  = 0.5,
    usle_p   = 1.0,
    ovn      = 0.014,
    elev     = hrus$elev,
    slope    = pmax(hrus$slope / 100, 0.001),
    slope_len = sapply(hrus$slope, .slope_len),
    lat      = hrus$lat,
    perco    = 0.5,
    eta      = 1.0,
    pet      = 1.0,
    strsol   = 0.0,
    strtmp   = 0.0,
    sw       = 0.5,
    awc      = 0.3,
    stringsAsFactors = FALSE)

  hru_cons <- data.frame(
    id     = idx,
    name   = mapply(.gis_name, "hru", hrus$id, cnt),
    gis_id = hrus$id,
    lat    = hrus$lat,
    lon    = hrus$lon,
    elev   = hrus$elev,
    area   = hrus$arslp,
    ovfl   = 0L,
    rule   = 0L,
    stringsAsFactors = FALSE)

  .gis_write(con, "hru_lte_hru", hru_ltes)
  .gis_write(con, "hru_lte_con", hru_cons)
  invisible(NULL)
}

.gis_insert_lsus_lte <- function(con) {
  if (.gis_count(con, "ls_unit_def") > 0L) return(invisible(NULL))
  lsus <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_lsus ORDER BY id"),
    error = function(e) NULL)
  hru_lsu_ids <- tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT lsu FROM gis_hrus")$lsu,
    error = function(e) integer(0))
  if (is.null(lsus) || nrow(lsus) == 0L) return(invisible(NULL))

  lsus <- lsus[lsus$id %in% hru_lsu_ids, , drop = FALSE]
  if (nrow(lsus) == 0L) return(invisible(NULL))

  cnt <- max(lsus$id)
  .gis_write(con, "ls_unit_def",
    data.frame(id   = seq_len(nrow(lsus)),
               name = mapply(.gis_name, "lsu", lsus$id, cnt),
               stringsAsFactors = FALSE))
  invisible(NULL)
}

.gis_insert_connections_lte <- function(con) {
  routing <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM gis_routing"),
    error = function(e) NULL)
  if (is.null(routing) || nrow(routing) == 0L) return(invisible(NULL))

  cha_map <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, gis_id FROM chandeg_con"),
    error = function(e) data.frame(id = integer(0), gis_id = integer(0)))

  if (.gis_count(con, "chandeg_con_out") == 0L && nrow(cha_map) > 0L) {
    # Build PT pass-through node lookup for chain following.
    pt_rows <- routing[tolower(routing$sourcecat) == "pt", , drop = FALSE]
    pt_dict <- if (nrow(pt_rows) > 0L) {
      setNames(
        lapply(seq_len(nrow(pt_rows)), function(i) pt_rows[i, , drop = FALSE]),
        as.character(pt_rows$sourceid))
    } else list()

    .follow_pt_lte <- function(row, max_iter = 100L) {
      iter <- 0L
      while (!is.null(row) && tolower(row$sinkcat) == "pt" && iter < max_iter) {
        row  <- pt_dict[[as.character(row$sinkid)]]
        iter <- iter + 1L
      }
      if (!is.null(row) && tolower(row$sinkcat) != "pt") row else NULL
    }

    # RouteCat.OUTLET = "X" in Python; fall back to sub topology when no CH rows.
    has_ch_src_lte <- any(tolower(routing$sourcecat) %in% c("ch","sdc"))
    ch_routing_lte <- routing
    if (!has_ch_src_lte) {
      gis_cha_lte <- tryCatch(
        DBI::dbGetQuery(con, "SELECT id AS cha_gis_id, subbasin FROM gis_channels"),
        error = function(e) data.frame(cha_gis_id = integer(0), subbasin = integer(0)))
      if (nrow(gis_cha_lte) > 0L) {
        sub_to_cha_lte <- setNames(gis_cha_lte$cha_gis_id, as.character(gis_cha_lte$subbasin))
        sub_rows_lte <- routing[tolower(routing$sourcecat) %in% c("sub","lsu") &
                                routing$percent > 0 &
                                !tolower(routing$sinkcat) %in% "x", , drop = FALSE]
        synth_lte <- list()
        for (k in seq_len(nrow(sub_rows_lte))) {
          r <- sub_rows_lte[k, ]
          src_cha <- sub_to_cha_lte[as.character(r$sourceid)]
          if (is.na(src_cha)) next
          snk_cha <- NA_integer_
          sk <- tolower(r$sinkcat)
          if (sk %in% c("sub","lsu")) snk_cha <- sub_to_cha_lte[as.character(r$sinkid)]
          else if (sk %in% c("ch","sdc")) snk_cha <- r$sinkid
          else if (sk == "pt") {
            pt_row <- .follow_pt_lte(r)
            if (!is.null(pt_row)) {
              fsk <- tolower(pt_row$sinkcat)
              if (fsk %in% c("sub","lsu")) snk_cha <- sub_to_cha_lte[as.character(pt_row$sinkid)]
              else if (fsk %in% c("ch","sdc")) snk_cha <- pt_row$sinkid
            }
          }
          if (is.na(snk_cha)) next
          hyd <- if (!is.null(r$hyd_typ) && !is.na(r$hyd_typ) && nzchar(r$hyd_typ)) r$hyd_typ else "tot"
          synth_lte[[length(synth_lte) + 1L]] <- data.frame(
            sourceid = as.integer(src_cha), sourcecat = "ch", hyd_typ = hyd,
            sinkid = as.integer(snk_cha), sinkcat = "ch", percent = r$percent,
            stringsAsFactors = FALSE)
        }
        if (length(synth_lte) > 0L) {
          s <- do.call(rbind, synth_lte)
          s <- s[!duplicated(s[, c("sourceid","sinkid")]), ]
          ch_routing_lte <- rbind(routing, s)
        }
      }
    }

    ch_rows <- ch_routing_lte[
      tolower(ch_routing_lte$sourcecat) %in% c("ch","sdc") &
      ch_routing_lte$percent > 0 &
      !tolower(ch_routing_lte$sinkcat) %in% "x", , drop = FALSE]
    con_outs <- list()
    orders   <- list()
    for (k in seq_len(nrow(ch_rows))) {
      r       <- ch_rows[k, ]
      con_row <- if (tolower(r$sinkcat) == "pt") .follow_pt_lte(r) else r
      if (is.null(con_row)) next
      if (!tolower(con_row$sinkcat) %in% c("ch","sdc")) next
      src_id <- cha_map$id[match(r$sourceid,       cha_map$gis_id)]
      snk_id <- cha_map$id[match(con_row$sinkid,   cha_map$gis_id)]
      if (is.na(src_id) || is.na(snk_id)) next
      key <- as.character(src_id)
      orders[[key]] <- if (!is.null(orders[[key]])) orders[[key]] + 1L else 1L
      hyd <- if (!is.null(con_row$hyd_typ) && !is.na(con_row$hyd_typ) &&
                 nzchar(con_row$hyd_typ)) con_row$hyd_typ else "tot"
      con_outs[[length(con_outs) + 1L]] <- data.frame(
        chandeg_con_id = src_id, order_id = orders[[key]],
        obj_typ = "sdc", obj_id = snk_id, hyd_typ = hyd,
        frac = r$percent / 100,
        stringsAsFactors = FALSE)
    }
    if (length(con_outs) > 0L)
      .gis_write(con, "chandeg_con_out", do.call(rbind, con_outs))
  }

  # Build hru_lte_con_out: Each HRU routes 100% to its channel (obj_typ='sdc').
  # Mirrors Python insert_connections_lte() which processes HRU→CH gis_routing rows.
  hru_lte_map <- tryCatch(
    DBI::dbGetQuery(con, "SELECT id, gis_id FROM hru_lte_con"),
    error = function(e) data.frame(id = integer(0), gis_id = integer(0)))

  if (.gis_count(con, "hru_lte_con_out") == 0L && nrow(hru_lte_map) > 0L &&
      nrow(cha_map) > 0L) {
    hru_rows <- routing[tolower(routing$sourcecat) == "hru" &
                        routing$percent > 0 &
                        tolower(routing$sinkcat) == "ch", , drop = FALSE]
    hru_rows <- hru_rows[order(hru_rows$sourceid), , drop = FALSE]
    hru_outs <- list()
    for (k in seq_len(nrow(hru_rows))) {
      r <- hru_rows[k, ]
      src_id <- hru_lte_map$id[match(r$sourceid, hru_lte_map$gis_id)]
      snk_id <- cha_map$id[match(r$sinkid, cha_map$gis_id)]
      if (is.na(src_id) || is.na(snk_id)) next
      hru_outs[[length(hru_outs) + 1L]] <- data.frame(
        hru_lte_con_id = src_id, order_id = 1L,
        obj_typ = "sdc", obj_id = snk_id,
        hyd_typ = "tot", frac = 1.0,
        stringsAsFactors = FALSE)
    }
    if (length(hru_outs) > 0L)
      .gis_write(con, "hru_lte_con_out", do.call(rbind, hru_outs))
  }

  invisible(NULL)
}

# --------------------------------------------------------------------------
# Update object_cnt based on actual row counts
# --------------------------------------------------------------------------
.gis_update_object_cnt <- function(con) {
  hru_n  <- .gis_count(con, "hru_con")
  lhru_n <- .gis_count(con, "hru_lte_con")
  rtu_n  <- .gis_count(con, "rout_unit_con")
  aqu_n  <- .gis_count(con, "aquifer_con")
  cha_n  <- .gis_count(con, "channel_con")
  res_n  <- .gis_count(con, "reservoir_con")
  rec_n  <- .gis_count(con, "recall_con")
  lcha_n <- .gis_count(con, "chandeg_con")
  obj_n  <- hru_n + lhru_n + rtu_n + aqu_n + cha_n + res_n + rec_n + lcha_n
  .gis_exec(con, paste0("
    UPDATE object_cnt SET
      obj  = ", obj_n,  ",
      hru  = ", hru_n,  ",
      lhru = ", lhru_n, ",
      rtu  = ", rtu_n,  ",
      aqu  = ", aqu_n,  ",
      cha  = ", cha_n,  ",
      res  = ", res_n,  ",
      rec  = ", rec_n,  ",
      lcha = ", lcha_n, "
    WHERE id = (SELECT id FROM object_cnt LIMIT 1)"))
  invisible(NULL)
}
