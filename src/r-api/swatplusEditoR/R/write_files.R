# Write SWAT+ model configuration files
# Complete R-native implementation guided by Python fileio modules

#' Write SWAT+ model configuration files
#'
#' Writes all SWAT+ input files (TxtInOut) from the project database entirely
#' within R, without requiring an external Python executable or API server.
#' The writing logic mirrors the Python \code{fileio} modules in the SWAT+
#' Editor repository.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param output_dir Character. Directory where the input files will be
#'   written. Defaults to \code{"TxtInOut"} in the project directory.
#' @param swat_version Character. SWAT+ version string. Default \code{"60"}.
#' @param weather_dir Character. Path to weather data files. If provided,
#'   weather \code{.cli} files are copied from this directory to output_dir.
#' @param editor_exe Character. Optional path to the SWAT+ Editor
#'   executable/script. If provided, delegates to the exe instead of writing
#'   natively. Default \code{NULL} (write natively in R).
#' @param api_url Character. Optional URL of a running SWAT+ Editor API
#'   server. Default \code{NULL} (write natively in R).
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' # Write all SWAT+ input files natively in R (recommended)
#' write_config_files(project)
#'
#' # Specify a custom output directory
#' write_config_files(project, output_dir = "/path/to/TxtInOut")
#'
#' # Copy weather data files from another directory
#' write_config_files(project, weather_dir = "/path/to/weather")
#' }
write_config_files <- function(project, output_dir = NULL,
                               swat_version = "60",
                               weather_dir = NULL,
                               editor_exe = NULL,
                               api_url = NULL) {
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

  # Optional fallbacks to external tools
  if (!is.null(editor_exe)) {
    return(write_via_exe(project, editor_exe, output_dir, swat_version))
  }
  if (!is.null(api_url) && api_server_available(api_url)) {
    return(write_via_api(project, api_url))
  }

  # Primary path: write all files natively in R
  write_direct(project, output_dir, swat_version, weather_dir)
}

#' Write configuration files via SWAT+ Editor executable
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
  is_python <- grepl("\\.py$", editor_exe)
  cmd <- if (is_python) paste("python", shQuote(editor_exe)) else shQuote(editor_exe)
  args <- paste("--action write",
                "--project_db_file", shQuote(project$db_file),
                "--swat_version", shQuote(swat_version))
  message("Writing SWAT+ input files via editor...")
  system(paste(cmd, args), intern = TRUE)

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  if (table_exists(con, "project_config")) {
    execute_db(con, "UPDATE project_config SET input_files_last_written = datetime('now')")
  }
  message("SWAT+ input files written to: ", output_dir)
  invisible(project)
}

#' Check if SWAT+ Editor API server is available
#' @param api_url API base URL.
#' @return Logical. TRUE if server responds.
#' @keywords internal
api_server_available <- function(api_url) {
  tryCatch({
    if (requireNamespace("httr", quietly = TRUE)) {
      resp <- httr::GET(paste0(api_url, "/setup/config"), httr::timeout(2))
      return(httr::status_code(resp) < 500)
    }
    FALSE
  }, error = function(e) FALSE)
}

#' Write configuration files via running API server
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
  resp <- httr::GET(paste0(api_url, "/setup/run-settings"),
                    httr::add_headers("Project-Db" = project$db_file))
  if (httr::status_code(resp) != 200) {
    stop("Failed to get run settings from API: ",
         httr::content(resp, "text"), call. = FALSE)
  }
  resp <- httr::PUT(paste0(api_url, "/setup/run-settings"),
                    httr::add_headers("Project-Db" = project$db_file),
                    body = httr::content(resp, "parsed"), encode = "json")
  if (httr::status_code(resp) != 200) {
    warning("API write returned status: ", httr::status_code(resp), call. = FALSE)
  }
  message("SWAT+ input files written via API")
  invisible(project)
}

# ===================================================================
# Primary R-native file writing
# ===================================================================

#' Write all SWAT+ configuration files directly from the database
#'
#' This is the main R-native writer that replaces the Python executable.
#' It writes all SWAT+ input text files by reading from the SQLite project
#' database, following the same logic as the Python fileio modules.
#'
#' @param project Project list object.
#' @param output_dir Output directory.
#' @param swat_version SWAT+ version string.
#' @param weather_dir Optional weather data directory to copy files from.
#' @return The project object (invisibly).
#' @keywords internal
write_direct <- function(project, output_dir, swat_version = "60",
                         weather_dir = NULL) {
  message("Writing SWAT+ input files from database...")

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  # Ensure all required tables exist with sensible defaults
  ensure_write_tables(con)

  tables <- list_db_tables(con)

  # Read project config
  is_lte <- FALSE
  weather_data_format <- "observed"
  version <- NULL
  if ("project_config" %in% tables) {
    cfg <- tryCatch(query_db(con, "SELECT * FROM project_config LIMIT 1"),
                    error = function(e) data.frame())
    if (nrow(cfg) > 0) {
      is_lte <- isTRUE(cfg$is_lte == 1)
      if ("weather_data_format" %in% names(cfg))
        weather_data_format <- cfg$weather_data_format
      if ("editor_version" %in% names(cfg))
        version <- cfg$editor_version
      if (is.null(weather_dir) && "weather_data_dir" %in% names(cfg) &&
          !is.null(cfg$weather_data_dir) && !is.na(cfg$weather_data_dir) &&
          nchar(cfg$weather_data_dir) > 0) {
        wd <- cfg$weather_data_dir
        if (!file.exists(wd)) {
          wd <- file.path(dirname(project$db_file), wd)
        }
        if (dir.exists(wd)) weather_dir <- wd
      }
    }
  }

  v <- version
  sv <- swat_version

  # Helper to get file names from file_cio table
  get_files <- function(section, n) {
    fnames <- get_cio_file_names(con, section)
    if (length(fnames) < n) {
      # Pad with nulls
      fnames <- c(fnames, rep("null", n - length(fnames)))
    }
    fnames
  }

  # Determine if file_cio table exists (full SWAT+ Editor database)
  has_cio <- "file_cio" %in% tables && "file_cio_classification" %in% tables

  # ---- SIMULATION section ----
  message("  Writing simulation files...")
  if (has_cio) {
    files <- get_files("simulation", 5)
    write_section_file(con, files[1], output_dir, v, sv,
                       writer = write_time_sim)
    write_section_file(con, files[2], output_dir, v, sv,
                       writer = write_print_prt)
    write_section_file(con, files[3], output_dir, v, sv,
                       writer = write_object_prt)
    write_section_file(con, files[4], output_dir, v, sv,
                       writer = write_object_cnt)
    write_section_file(con, files[5], output_dir, v, sv,
                       writer = write_constituents_cs)
  } else {
    if ("time_sim" %in% tables) write_time_sim(con, output_dir, v, sv)
    if ("print_prt" %in% tables) write_print_prt(con, output_dir, v, sv)
    if ("object_prt" %in% tables) write_object_prt(con, output_dir, v, sv)
    if ("object_cnt" %in% tables) write_object_cnt(con, output_dir, v, sv)
    if ("constituents_cs" %in% tables) write_constituents_cs(con, output_dir, v, sv)
  }

  # ---- CLIMATE section ----
  message("  Writing climate files...")
  if (has_cio) {
    files <- get_files("climate", 9)
    if (weather_data_format == "netcdf") {
      write_section_file(con, "netcdf.ncw", output_dir, v, sv,
                         writer = write_weather_sta_cli)
    } else {
      write_section_file(con, files[1], output_dir, v, sv,
                         writer = write_weather_sta_cli)
    }
    write_section_file(con, files[2], output_dir, v, sv,
                       writer = write_weather_wgn)
    write_section_file(con, files[9], output_dir, v, sv,
                       writer = write_atmo_cli)
  } else {
    if ("weather_sta_cli" %in% tables) write_weather_sta_cli(con, output_dir, v, sv)
    if ("weather_wgn_cli" %in% tables) write_weather_wgn(con, output_dir, v, sv)
    if ("atmo_cli" %in% tables) write_atmo_cli(con, output_dir, v, sv)
  }

  # Copy weather files
  copy_weather_files(con, output_dir, weather_dir, weather_data_format)

  # ---- CONNECT section (13 files) ----
  message("  Writing connect files...")
  write_connect_section(con, output_dir, v, sv, has_cio)

  # ---- CHANNEL section ----
  message("  Writing channel files...")
  write_table_section(con, output_dir, v, sv, has_cio, "channel", list(
    list(tbl = "initial_cha", file = "initial.cha"),
    list(tbl = "channel_cha", file = "channel-lte.cha",
         query = "SELECT c.id, c.name,
                    COALESCE(i.name, 'null') as init,
                    COALESCE(h.name, 'null') as hyd,
                    COALESCE(s.name, 'null') as sed,
                    COALESCE(n.name, 'null') as nut
                  FROM channel_cha c
                  LEFT JOIN initial_cha i ON c.init_id = i.id
                  LEFT JOIN hydrology_cha h ON c.hyd_id = h.id
                  LEFT JOIN sediment_cha s ON c.sed_id = s.id
                  LEFT JOIN nutrients_cha n ON c.nut_id = n.id
                  ORDER BY c.id"),
    list(tbl = "hydrology_cha", file = "hydrology.cha",
         non_zero_min = c("wd", "dp", "slp", "len", "fps")),
    list(tbl = "sediment_cha", file = "sediment.cha"),
    list(tbl = "nutrients_cha", file = "nutrients.cha"),
    list(tbl = "channel_lte_cha", file = "channel-lte.cha"),
    list(tbl = "hyd_sed_lte_cha", file = "hyd-sed-lte.cha")
  ))

  # ---- RESERVOIR section ----
  message("  Writing reservoir files...")
  write_table_section(con, output_dir, v, sv, has_cio, "reservoir", list(
    list(tbl = "initial_res", file = "initial.res"),
    list(tbl = "reservoir_res", file = "reservoir.res"),
    list(tbl = "hydrology_res", file = "hydrology.res", ignore_id = TRUE),
    list(tbl = "sediment_res", file = "sediment.res"),
    list(tbl = "nutrients_res", file = "nutrients.res"),
    list(tbl = "weir_res", file = "weir.res"),
    list(tbl = "wetland_wet", file = "wetland.wet"),
    list(tbl = "hydrology_wet", file = "hydrology.wet")
  ))

  # ---- ROUTING UNIT section ----
  message("  Writing routing unit files...")
  write_table_section(con, output_dir, v, sv, has_cio, "routing_unit", list(
    list(tbl = "rout_unit_def_con", file = "rout_unit.def",
         query_tbl = "rout_unit_con"),
    list(tbl = "rout_unit_ele", file = "rout_unit.ele"),
    list(tbl = "rout_unit_rtu", file = "rout_unit.rtu"),
    list(tbl = "rout_unit_dr", file = "rout_unit.dr")
  ))

  # ---- HRU section ----
  message("  Writing HRU files...")
  write_table_section(con, output_dir, v, sv, has_cio, "hru", list(
    list(tbl = "hru_data_hru", file = "hru-data.hru"),
    list(tbl = "hru_lte_hru", file = "hru-lte.hru")
  ))

  # ---- DR section ----
  write_table_section(con, output_dir, v, sv, has_cio, "dr", list(
    list(tbl = "delratio_del", file = "delratio.del"),
    list(tbl = "dr_om_del", file = "dr.del")
  ))

  # ---- AQUIFER section ----
  message("  Writing aquifer files...")
  write_table_section(con, output_dir, v, sv, has_cio, "aquifer", list(
    list(tbl = "initial_aqu", file = "initial.aqu"),
    list(tbl = "aquifer_aqu", file = "aquifer.aqu")
  ))

  # ---- WATER RIGHTS section ----
  write_table_section(con, output_dir, v, sv, has_cio, "water_rights", list(
    list(tbl = "water_allocation_wro", file = "water_allocation.wro")
  ))

  # ---- BASIN section ----
  message("  Writing basin files...")
  write_table_section(con, output_dir, v, sv, has_cio, "basin", list(
    list(tbl = "codes_bsn", file = "codes.bsn", ignore_id = TRUE),
    list(tbl = "parameters_bsn", file = "parameters.bsn", ignore_id = TRUE)
  ))

  # ---- HYDROLOGY section ----
  message("  Writing hydrology files...")
  write_table_section(con, output_dir, v, sv, has_cio, "hydrology", list(
    list(tbl = "hydrology_hyd", file = "hydrology.hyd"),
    list(tbl = "topography_hyd", file = "topography.hyd"),
    list(tbl = "field_fld", file = "field.fld", ignore_id = TRUE)
  ))

  # ---- EXCO section ----
  write_table_section(con, output_dir, v, sv, has_cio, "exco", list(
    list(tbl = "exco_exc", file = "exco.exc"),
    list(tbl = "exco_om_exc", file = "exco_om.exc"),
    list(tbl = "exco_pest_exc", file = "exco_pest.exc"),
    list(tbl = "exco_path_exc", file = "exco_path.exc"),
    list(tbl = "exco_hmet_exc", file = "exco_hmet.exc"),
    list(tbl = "exco_salt_exc", file = "exco_salt.exc")
  ))

  # ---- RECALL section ----
  message("  Writing recall files...")
  if (has_data(con, "recall_rec")) {
    write_recall_rec(con, output_dir, v, sv)
  }

  # ---- STRUCTURAL section ----
  write_table_section(con, output_dir, v, sv, has_cio, "structural", list(
    list(tbl = "tiledrain_str", file = "tiledrain.str"),
    list(tbl = "septic_str", file = "septic.str"),
    list(tbl = "filterstrip_str", file = "filterstrip.str"),
    list(tbl = "grassedww_str", file = "grassedww.str"),
    list(tbl = "bmpuser_str", file = "bmpuser.str")
  ))

  # ---- HRU PARM DB section ----
  message("  Writing parameter database files...")
  write_table_section(con, output_dir, v, sv, has_cio, "hru_parm_db", list(
    list(tbl = "plants_plt", file = "plants.plt"),
    list(tbl = "fertilizer_frt", file = "fertilizer.frt"),
    list(tbl = "tillage_til", file = "tillage.til"),
    list(tbl = "pesticide_pst", file = "pesticide.pst"),
    list(tbl = "pathogens_pth", file = "pathogens.pth"),
    list(tbl = "metals_mtl", file = "metals.mtl"),
    list(tbl = "salts_slt", file = "salts.slt"),
    list(tbl = "urban_urb", file = "urban.urb"),
    list(tbl = "septic_sep", file = "septic.sep"),
    list(tbl = "snow_sno", file = "snow.sno")
  ))

  # ---- OPS section ----
  write_table_section(con, output_dir, v, sv, has_cio, "ops", list(
    list(tbl = "harv_ops", file = "harv.ops"),
    list(tbl = "graze_ops", file = "graze.ops"),
    list(tbl = "irr_ops", file = "irr.ops"),
    list(tbl = "chem_app_ops", file = "chem_app.ops"),
    list(tbl = "fire_ops", file = "fire.ops"),
    list(tbl = "sweep_ops", file = "sweep.ops")
  ))

  # ---- LUM section ----
  message("  Writing land use management files...")
  write_table_section(con, output_dir, v, sv, has_cio, "lum", list(
    list(tbl = "landuse_lum", file = "landuse.lum"),
    list(tbl = "management_sch", file = "management.sch"),
    list(tbl = "cntable_lum", file = "cntable.lum"),
    list(tbl = "cons_prac_lum", file = "cons_prac.lum"),
    list(tbl = "ovn_table_lum", file = "ovn_table.lum")
  ))

  # ---- CHG section ----
  write_table_section(con, output_dir, v, sv, has_cio, "chg", list(
    list(tbl = "cal_parms_cal", file = "cal_parms.cal", write_count = TRUE),
    list(tbl = "calibration_cal", file = "calibration.cal"),
    list(tbl = "codes_sft", file = "codes.sft"),
    list(tbl = "wb_parms_sft", file = "wb_parms.sft", write_count = TRUE),
    list(tbl = "water_balance_sft", file = "water_balance.sft"),
    list(tbl = "ch_sed_budget_sft", file = "ch_sed_budget.sft"),
    list(tbl = "ch_sed_parms_sft", file = "ch_sed_parms.sft", write_count = TRUE),
    list(tbl = "plant_parms_sft", file = "plant_parms.sft"),
    list(tbl = "plant_gro_sft", file = "plant_gro.sft")
  ))

  # ---- INIT section ----
  message("  Writing initial condition files...")
  write_table_section(con, output_dir, v, sv, has_cio, "init", list(
    list(tbl = "plant_ini", file = "plant.ini"),
    list(tbl = "soil_plant_ini", file = "soil_plant.ini"),
    list(tbl = "om_water_ini", file = "om_water.ini", ignore_id = TRUE),
    list(tbl = "pest_hru_ini", file = "pest_hru.ini"),
    list(tbl = "pest_water_ini", file = "pest_water.ini"),
    list(tbl = "path_hru_ini", file = "path_hru.ini"),
    list(tbl = "path_water_ini", file = "path_water.ini")
  ))

  # ---- SOILS section ----
  message("  Writing soils files...")
  write_table_section(con, output_dir, v, sv, has_cio, "soils", list(
    list(tbl = "soils_sol", file = "soils.sol"),
    list(tbl = "nutrients_sol", file = "nutrients.sol",
         non_zero_min = c("exp_co")),
    list(tbl = "soils_lte_sol", file = "soils_lte.sol", ignore_id = TRUE)
  ))

  # ---- DECISION TABLE section ----
  message("  Writing decision table files...")
  if (has_data(con, "d_table_dtl")) {
    write_decision_tables(con, output_dir, v, sv)
  }

  # ---- REGIONS section ----
  message("  Writing region files...")
  write_table_section(con, output_dir, v, sv, has_cio, "regions", list(
    list(tbl = "ls_unit_ele", file = "ls_unit.ele"),
    list(tbl = "ls_unit_def", file = "ls_unit.def"),
    list(tbl = "ls_reg_ele", file = "ls_reg.ele"),
    list(tbl = "ls_reg_def", file = "ls_reg.def"),
    list(tbl = NULL, file = NULL),
    list(tbl = "ch_catunit_ele", file = "ch_catunit.ele"),
    list(tbl = "ch_catunit_def", file = "ch_catunit.def"),
    list(tbl = "ch_reg_def", file = "ch_reg.def"),
    list(tbl = "aqu_catunit_ele", file = "aqu_catunit.ele"),
    list(tbl = "aqu_catunit_def", file = "aqu_catunit.def"),
    list(tbl = "aqu_reg_def", file = "aqu_reg.def"),
    list(tbl = "res_catunit_ele", file = "res_catunit.ele"),
    list(tbl = "res_catunit_def", file = "res_catunit.def"),
    list(tbl = "res_reg_def", file = "res_reg.def"),
    list(tbl = "rec_catunit_ele", file = "rec_catunit.ele"),
    list(tbl = "rec_catunit_def", file = "rec_catunit.def"),
    list(tbl = "rec_reg_def", file = "rec_reg.def")
  ))

  # ---- file.cio (master index) ----
  message("  Writing file.cio...")
  write_file_cio(con, output_dir, v, sv, is_lte, weather_data_format)

  # Update timestamp
  if ("project_config" %in% tables) {
    execute_db(con, paste0(
      "UPDATE project_config SET input_files_last_written = datetime('now'),",
      " swat_last_run = NULL, output_last_imported = NULL"))
  }

  message("SWAT+ input files written to: ", output_dir)
  invisible(project)
}

# ===================================================================
# Section-level helpers
# ===================================================================

#' Write a file if file_name is not "null"
#' @keywords internal
write_section_file <- function(con, file_name, output_dir, v, sv,
                               writer = NULL) {
  if (is.null(file_name) || trimws(file_name) == "null" ||
      trimws(file_name) == "") return(invisible(NULL))
  file_name <- trimws(file_name)
  writer(con, output_dir, v, sv, file_name = file_name)
}

#' Write a batch of simple table-based files for a section
#' @keywords internal
write_table_section <- function(con, output_dir, v, sv, has_cio,
                                section, specs) {
  cio_files <- if (has_cio) get_cio_file_names(con, section) else character(0)

  for (idx in seq_along(specs)) {
    spec <- specs[[idx]]
    tbl <- spec$tbl
    if (is.null(tbl)) next

    # Determine output file name
    fname <- if (idx <= length(cio_files) && cio_files[idx] != "null") {
      trimws(cio_files[idx])
    } else {
      spec$file
    }
    if (is.null(fname) || fname == "null" || fname == "") next

    # Check table exists and has data
    if (!has_data(con, tbl)) next

    query_tbl <- if (!is.null(spec$query_tbl)) spec$query_tbl else NULL
    custom_query <- if (!is.null(spec$query)) spec$query else NULL
    ignore_id <- isTRUE(spec$ignore_id)
    write_cnt <- isTRUE(spec$write_count)
    nzm <- if (!is.null(spec$non_zero_min)) spec$non_zero_min else character(0)

    # Use custom query or default table
    actual_tbl <- if (!is.null(query_tbl)) query_tbl else tbl
    swat_write_table(con, actual_tbl,
                     file.path(output_dir, fname),
                     version = v, swat_version = sv,
                     ignore_id = ignore_id,
                     query = custom_query,
                     write_count = write_cnt,
                     non_zero_min_cols = nzm)
  }
}

# ===================================================================
# Individual file writers
# ===================================================================

#' Write time.sim file
#' @param con Database connection.
#' @param output_dir Output directory.
#' @param version Editor version.
#' @param swat_version SWAT+ version.
#' @param file_name Output file name.
#' @keywords internal
write_time_sim <- function(con, output_dir, version = NULL,
                           swat_version = NULL, file_name = "time.sim") {
  swat_write_table(con, "time_sim",
                   file.path(output_dir, file_name),
                   version = version, swat_version = swat_version,
                   ignore_id = TRUE)
}

#' Write print.prt file (print output settings)
#' @keywords internal
write_print_prt <- function(con, output_dir, version = NULL,
                            swat_version = NULL, file_name = "print.prt") {
  if (!has_data(con, "print_prt")) return(invisible(NULL))

  row <- query_db(con, "SELECT * FROM print_prt LIMIT 1")
  fp <- file.path(output_dir, file_name)
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  # Time settings row
  writeLines(paste0(
    swat_int_pad(row$nyskip, pad = 10),
    swat_int_pad(row$day_start),
    swat_int_pad(row$yrc_start),
    swat_int_pad(row$day_end),
    swat_int_pad(row$yrc_end),
    swat_int_pad(row$interval)
  ), f)

  # AA intervals
  aa_ints <- if (!is.null(row$aa_ints) && !is.na(row$aa_ints) &&
                 nchar(row$aa_ints) > 0) {
    as.integer(strsplit(as.character(row$aa_ints), ",")[[1]])
  } else integer(0)

  writeLines(paste0(swat_int_pad("aa_int_cnt", pad = 10)), f)
  aa_line <- swat_int_pad(length(aa_ints), pad = 10)
  for (ai in aa_ints) aa_line <- paste0(aa_line, swat_int_pad(ai))
  writeLines(aa_line, f)

  # Output format
  writeLines(paste0(
    swat_bool_pad(row$csvout),
    swat_bool_pad(row$dbout),
    swat_bool_pad(row$cdfout)
  ), f)

  # Extra options
  crop_yld <- if (!is.null(row$crop_yld)) row$crop_yld else "n"
  writeLines(paste0(
    swat_string_pad(crop_yld, pad = SWAT_CODE_PAD),
    swat_bool_pad(row$mgtout),
    swat_bool_pad(row$hydcon),
    swat_bool_pad(row$fdcout)
  ), f)

  # Print objects
  if (has_data(con, "print_prt_object")) {
    objects <- query_db(con, "SELECT * FROM print_prt_object ORDER BY id")
    writeLines(paste0(
      swat_string_pad("objects", pad = SWAT_STR_PAD, align = "left"),
      swat_string_pad("daily"),
      swat_string_pad("monthly"),
      swat_string_pad("yearly"),
      swat_string_pad("avann")
    ), f)
    seen <- character(0)
    for (i in seq_len(nrow(objects))) {
      obj <- objects[i, ]
      if (obj$name %in% seen) next
      seen <- c(seen, obj$name)
      writeLines(paste0(
        swat_string_pad(obj$name, align = "left"),
        swat_bool_pad(obj$daily),
        swat_bool_pad(obj$monthly),
        swat_bool_pad(obj$yearly),
        swat_bool_pad(obj$avann)
      ), f)
    }
  }
}

#' Write object.prt file
#' @keywords internal
write_object_prt <- function(con, output_dir, version = NULL,
                             swat_version = NULL, file_name = "object.prt") {
  swat_write_table(con, "object_prt",
                   file.path(output_dir, file_name),
                   version = version, swat_version = swat_version)
}

#' Write object.cnt file (object counts)
#' @keywords internal
write_object_cnt <- function(con, output_dir, version = NULL,
                             swat_version = NULL, file_name = "object.cnt") {
  if (!has_data(con, "object_cnt")) return(invisible(NULL))

  row <- query_db(con, "SELECT * FROM object_cnt LIMIT 1")
  fp <- file.path(output_dir, file_name)
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  # Header
  hdr <- paste0(
    swat_string_pad("name", align = "left"),
    swat_num_pad("ls_area"), swat_num_pad("tot_area"),
    swat_int_pad("obj"), swat_int_pad("hru"), swat_int_pad("lhru"),
    swat_int_pad("rtu"), swat_int_pad("gwfl"), swat_int_pad("aqu"),
    swat_int_pad("cha"), swat_int_pad("res"), swat_int_pad("rec"),
    swat_int_pad("exco"), swat_int_pad("dlr"), swat_int_pad("can"),
    swat_int_pad("pmp"), swat_int_pad("out"), swat_int_pad("lcha"),
    swat_int_pad("aqu2d"), swat_int_pad("hrd"), swat_int_pad("wro")
  )
  writeLines(hdr, f)

  # Dynamic counts from con tables
  hru_cnt  <- safe_count(con, "hru_con")
  lhru_cnt <- safe_count(con, "hru_lte_con")
  rtu_cnt  <- safe_count(con, "rout_unit_con")
  mfl_cnt  <- safe_count(con, "modflow_con")
  aqu_cnt  <- safe_count(con, "aquifer_con")
  cha_cnt  <- safe_count(con, "channel_con")
  res_cnt  <- safe_count(con, "reservoir_con")
  rec_cnt  <- tryCatch(
    query_db(con, "SELECT COUNT(*) as cnt FROM recall_rec WHERE rec_typ != 4")$cnt[1],
    error = function(e) safe_count(con, "recall_con"))
  exco_cnt <- tryCatch(
    query_db(con, "SELECT COUNT(*) as cnt FROM recall_dat d
                    JOIN recall_rec r ON d.recall_rec_id = r.id
                    WHERE r.rec_typ = 4 AND d.flo != 0")$cnt[1],
    error = function(e) safe_count(con, "exco_con"))
  dlr_cnt  <- safe_count(con, "delratio_con")
  out_cnt  <- safe_count(con, "outlet_con")
  lcha_cnt <- safe_count(con, "chandeg_con")
  aqu2d_cnt <- safe_count(con, "aquifer2d_con")

  ls_area <- tryCatch(
    query_db(con, "SELECT COALESCE(SUM(area), 0) as s FROM ls_unit_def")$s[1],
    error = function(e) 0)
  tot_area <- tryCatch(
    query_db(con, "SELECT COALESCE(SUM(area), 0) as s FROM rout_unit_con")$s[1],
    error = function(e) 0)

  name_val <- gsub("\\W", "", row$name)
  can_v <- if (!is.null(row$can)) row$can else 0L
  pmp_v <- if (!is.null(row$pmp)) row$pmp else 0L
  hrd_v <- if (!is.null(row$hrd)) row$hrd else 0L
  wro_v <- if (!is.null(row$wro)) row$wro else 0L

  obj_tot <- hru_cnt + lhru_cnt + rtu_cnt + mfl_cnt + aqu_cnt + cha_cnt +
    res_cnt + rec_cnt + exco_cnt + dlr_cnt + out_cnt + lcha_cnt + aqu2d_cnt +
    can_v + pmp_v + hrd_v + wro_v

  writeLines(paste0(
    swat_string_pad(name_val, align = "left"),
    swat_num_pad(ls_area), swat_num_pad(tot_area),
    swat_int_pad(obj_tot),
    swat_int_pad(hru_cnt), swat_int_pad(lhru_cnt),
    swat_int_pad(rtu_cnt), swat_int_pad(mfl_cnt),
    swat_int_pad(aqu_cnt), swat_int_pad(cha_cnt),
    swat_int_pad(res_cnt), swat_int_pad(rec_cnt),
    swat_int_pad(exco_cnt), swat_int_pad(dlr_cnt),
    swat_int_pad(can_v), swat_int_pad(pmp_v),
    swat_int_pad(out_cnt), swat_int_pad(lcha_cnt),
    swat_int_pad(aqu2d_cnt),
    swat_int_pad(hrd_v), swat_int_pad(wro_v)
  ), f)
}

#' Write constituents.cs file
#' @keywords internal
write_constituents_cs <- function(con, output_dir, version = NULL,
                                  swat_version = NULL,
                                  file_name = "constituents.cs") {
  if (!has_data(con, "constituents_cs")) return(invisible(NULL))

  row <- query_db(con, "SELECT * FROM constituents_cs LIMIT 1")
  pest <- if (!is.null(row$pest_coms) && !is.na(row$pest_coms) &&
              nchar(row$pest_coms) > 0) sort(strsplit(row$pest_coms, ",")[[1]]) else character(0)
  path <- if (!is.null(row$path_coms) && !is.na(row$path_coms) &&
              nchar(row$path_coms) > 0) sort(strsplit(row$path_coms, ",")[[1]]) else character(0)
  hmet <- if (!is.null(row$hmet_coms) && !is.na(row$hmet_coms) &&
              nchar(row$hmet_coms) > 0) sort(strsplit(row$hmet_coms, ",")[[1]]) else character(0)
  salt <- if (!is.null(row$salt_coms) && !is.na(row$salt_coms) &&
              nchar(row$salt_coms) > 0) sort(strsplit(row$salt_coms, ",")[[1]]) else character(0)

  if (length(pest) == 0 && length(path) == 0 &&
      length(hmet) == 0 && length(salt) == 0) return(invisible(NULL))

  fp <- file.path(output_dir, file_name)
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  write_constit <- function(items, label) {
    cnt_str <- sprintf("%6d", length(items))
    writeLines(paste0(cnt_str, "              ", sprintf("%-16s  ", paste0("!", label))), f)
    writeLines(paste0("        ", paste(items, collapse = " ")), f)
  }

  write_constit(pest, "pesticides")
  write_constit(path, "pathogens")
  write_constit(hmet, "metals")
  write_constit(salt, "salts")
}

#' Write weather station CLI file
#' @keywords internal
write_weather_sta_cli <- function(con, output_dir, version = NULL,
                                  swat_version = NULL,
                                  file_name = "weather-sta.cli") {
  if (!has_data(con, "weather_sta_cli")) return(invisible(NULL))

  # Join with wgn to get wgn name
  sql <- "SELECT s.name, COALESCE(w.name, 'null') as wgn,
                 s.pcp, s.tmp, s.slr, s.hmd, s.wnd, s.pet, s.atmo_dep
          FROM weather_sta_cli s
          LEFT JOIN weather_wgn_cli w ON s.wgn_id = w.id
          ORDER BY s.id"
  stations <- tryCatch(query_db(con, sql), error = function(e) {
    # Fallback without join if wgn table doesn't exist
    query_db(con, "SELECT name, 'null' as wgn, pcp, tmp, slr, hmd, wnd, pet, atmo_dep
                   FROM weather_sta_cli ORDER BY id")
  })
  if (nrow(stations) == 0) return(invisible(NULL))

  fp <- file.path(output_dir, file_name)
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  # Header
  writeLines(paste0(
    swat_string_pad("name", align = "left"),
    swat_string_pad("wgn"),
    swat_string_pad("pcp"),
    swat_string_pad("tmp"),
    swat_string_pad("slr"),
    swat_string_pad("hmd"),
    swat_string_pad("wnd"),
    swat_string_pad("pet"),
    swat_string_pad("atmo_dep")
  ), f)

  for (i in seq_len(nrow(stations))) {
    s <- stations[i, ]
    na_to_null <- function(x) if (is.na(x) || is.null(x)) SWAT_NULL_STR else x
    writeLines(paste0(
      swat_string_pad(na_to_null(s$name), align = "left"),
      swat_string_pad(na_to_null(s$wgn)),
      swat_string_pad(na_to_null(s$pcp)),
      swat_string_pad(na_to_null(s$tmp)),
      swat_string_pad(na_to_null(s$slr)),
      swat_string_pad(na_to_null(s$hmd)),
      swat_string_pad(na_to_null(s$wnd)),
      swat_string_pad(na_to_null(s$pet)),
      swat_string_pad(na_to_null(s$atmo_dep))
    ), f)
  }
}

#' Write weather generator file (weather-wgn.cli)
#' @keywords internal
write_weather_wgn <- function(con, output_dir, version = NULL,
                              swat_version = NULL,
                              file_name = "weather-wgn.cli") {
  if (!has_data(con, "weather_wgn_cli")) return(invisible(NULL))

  stations <- query_db(con, "SELECT * FROM weather_wgn_cli ORDER BY id")
  fp <- file.path(output_dir, file_name)
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  mon_cols <- c("tmp_max_ave", "tmp_min_ave", "tmp_max_sd", "tmp_min_sd",
                "pcp_ave", "pcp_sd", "pcp_skew", "wet_dry", "wet_wet",
                "pcp_days", "pcp_hhr", "slr_ave", "dew_ave", "wnd_ave")

  for (i in seq_len(nrow(stations))) {
    sta <- stations[i, ]

    # Station header
    writeLines(paste0(
      swat_string_pad("name", align = "left"),
      swat_num_pad("lat"), swat_num_pad("lon"),
      swat_num_pad("elev"), swat_num_pad("rain_yrs")
    ), f)
    writeLines(paste0(
      swat_string_pad(sta$name, align = "left"),
      swat_num_pad(sta$lat), swat_num_pad(sta$lon),
      swat_num_pad(sta$elev), swat_num_pad(sta$rain_yrs)
    ), f)

    # Monthly data header
    hdr <- paste0(swat_int_pad("month"))
    for (mc in mon_cols) hdr <- paste0(hdr, swat_num_pad(mc))
    writeLines(hdr, f)

    # Monthly data
    mon_data <- tryCatch(
      query_db(con, "SELECT * FROM weather_wgn_cli_mon
                     WHERE weather_wgn_cli_id = ? ORDER BY month",
               params = list(sta$id)),
      error = function(e) data.frame())

    if (nrow(mon_data) > 0) {
      for (m in seq_len(nrow(mon_data))) {
        md <- mon_data[m, ]
        line <- swat_int_pad(md$month)
        for (mc in mon_cols) {
          val <- if (mc %in% names(md)) md[[mc]] else 0
          line <- paste0(line, swat_num_pad(val))
        }
        writeLines(line, f)
      }
    }
  }
}

#' Write atmospheric deposition file (atmo.cli)
#' @keywords internal
write_atmo_cli <- function(con, output_dir, version = NULL,
                           swat_version = NULL, file_name = "atmo.cli") {
  if (!has_data(con, "atmo_cli")) return(invisible(NULL))

  fp <- file.path(output_dir, file_name)
  swat_write_table(con, "atmo_cli", fp,
                   version = version, swat_version = swat_version)
}

# ===================================================================
# Connect file writers
# ===================================================================

#' Write all connect section files
#' @keywords internal
write_connect_section <- function(con, output_dir, v, sv, has_cio) {
  # Map connect tables to their element tables and names
  con_specs <- list(
    list(con_tbl = "hru_con",        con_out_tbl = "hru_con_out",
         elem_name = "hru",  file = "hru.con"),
    list(con_tbl = "hru_lte_con",    con_out_tbl = "hru_lte_con_out",
         elem_name = "lhru", file = "hru-lte.con"),
    list(con_tbl = "rout_unit_con",  con_out_tbl = "rout_unit_con_out",
         elem_name = "rtu",  file = "rout_unit.con"),
    list(con_tbl = "aquifer_con",    con_out_tbl = "aquifer_con_out",
         elem_name = "aqu",  file = "aquifer.con"),
    list(con_tbl = "channel_con",    con_out_tbl = "channel_con_out",
         elem_name = "cha",  file = "channel-lte.con"),
    list(con_tbl = "reservoir_con",  con_out_tbl = "reservoir_con_out",
         elem_name = "res",  file = "reservoir.con"),
    list(con_tbl = "recall_con",     con_out_tbl = "recall_con_out",
         elem_name = "rec",  file = "recall.con"),
    list(con_tbl = "exco_con",       con_out_tbl = "exco_con_out",
         elem_name = "exco", file = "exco.con"),
    list(con_tbl = "delratio_con",   con_out_tbl = "delratio_con_out",
         elem_name = "dlr",  file = "delratio.con"),
    list(con_tbl = "chandeg_con",    con_out_tbl = "chandeg_con_out",
         elem_name = "lcha", file = "chandeg.con")
  )

  cio_files <- if (has_cio) get_cio_file_names(con, "connect") else character(0)

  for (idx in seq_along(con_specs)) {
    spec <- con_specs[[idx]]
    fname <- if (idx <= length(cio_files) && cio_files[idx] != "null") {
      trimws(cio_files[idx])
    } else {
      spec$file
    }
    if (is.null(fname) || fname == "null") next
    if (!has_data(con, spec$con_tbl)) next

    write_connect_file(con, spec$con_tbl, spec$con_out_tbl,
                       spec$elem_name,
                       file.path(output_dir, fname), v, sv)
  }
}

#' Write a single connect file (e.g. hru.con, channel.con)
#' @keywords internal
write_connect_file <- function(con, con_tbl, con_out_tbl, elem_name,
                               file_path, version, swat_version) {
  # Check if con_out table exists
  has_con_out <- has_data(con, con_out_tbl)

  # Get connection data with weather station name
  sql <- paste0(
    "SELECT c.id, c.name, c.gis_id, c.area, c.lat, c.lon, c.elev, ",
    "COALESCE(w.name, 'null') as wst ",
    "FROM ", con_tbl, " c ",
    "LEFT JOIN weather_sta_cli w ON c.wst_id = w.id ",
    "ORDER BY c.id")
  cons <- tryCatch(query_db(con, sql), error = function(e) {
    # Fallback without weather join
    tryCatch(query_db(con, paste0("SELECT * FROM ", con_tbl, " ORDER BY id")),
             error = function(e2) data.frame())
  })
  if (nrow(cons) == 0) return(invisible(NULL))

  f <- file(file_path, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(file_path, version, swat_version), f)

  # Header
  hdr <- paste0(
    swat_int_pad("id"),
    swat_string_pad("name", align = "left"),
    swat_int_pad("gis_id"),
    swat_num_pad("area"),
    swat_num_pad("lat"),
    swat_num_pad("lon"),
    swat_num_pad("elev"),
    swat_int_pad(elem_name),
    swat_string_pad("wst"),
    swat_int_pad("cst"),
    swat_int_pad("ovfl"),
    swat_int_pad("rule"),
    swat_int_pad("out_tot")
  )
  if (has_con_out) {
    hdr <- paste0(hdr,
      swat_string_pad("obj_typ", pad = SWAT_CODE_PAD),
      swat_int_pad("obj_id"),
      swat_string_pad("hyd_typ", pad = SWAT_CODE_PAD),
      swat_num_pad("frac")
    )
  }
  writeLines(hdr, f)

  # Get element ID column name
  elem_id_col <- paste0(elem_name, "_id")

  # Data rows
  for (i in seq_len(nrow(cons))) {
    c_row <- cons[i, ]
    elem_id <- if (elem_id_col %in% names(c_row)) c_row[[elem_id_col]] else i
    if (is.null(elem_id) || is.na(elem_id)) elem_id <- i

    wst <- if ("wst" %in% names(c_row)) c_row$wst else SWAT_NULL_STR
    cst_id <- if ("cst_id" %in% names(c_row)) c_row$cst_id else 0L
    ovfl <- if ("ovfl" %in% names(c_row)) c_row$ovfl else 0L
    rule <- if ("rule" %in% names(c_row)) c_row$rule else 0L

    # Get outflows for this connection
    outs <- if (has_con_out) {
      tryCatch(
        query_db(con, paste0("SELECT * FROM ", con_out_tbl,
                             " WHERE ", gsub("_out$", "_id", con_out_tbl),
                             " = ? ORDER BY id"),
                 params = list(c_row$id)),
        error = function(e) data.frame())
    } else data.frame()

    out_tot <- nrow(outs)

    line <- paste0(
      swat_int_pad(i),
      swat_string_pad(c_row$name, align = "left"),
      swat_int_pad(if (is.null(c_row$gis_id) || is.na(c_row$gis_id)) 0L else c_row$gis_id),
      swat_num_pad(c_row$area, use_non_zero_min = TRUE),
      swat_num_pad(c_row$lat),
      swat_num_pad(c_row$lon),
      swat_num_pad(c_row$elev),
      swat_int_pad(elem_id),
      swat_string_pad(wst),
      swat_int_pad(cst_id),
      swat_int_pad(ovfl),
      swat_int_pad(rule),
      swat_int_pad(out_tot)
    )

    if (out_tot > 0) {
      for (j in seq_len(out_tot)) {
        o <- outs[j, ]
        line <- paste0(line,
          swat_string_pad(o$obj_typ, pad = SWAT_CODE_PAD),
          swat_int_pad(o$obj_id),
          swat_string_pad(o$hyd_typ, pad = SWAT_CODE_PAD),
          swat_num_pad(o$frac)
        )
      }
    }
    writeLines(line, f)
  }
}

# ===================================================================
# Recall file writer
# ===================================================================

#' Write recall.rec file (includes nested recall data)
#' @keywords internal
write_recall_rec <- function(con, output_dir, version = NULL,
                             swat_version = NULL) {
  recs <- query_db(con, "SELECT * FROM recall_rec ORDER BY id")
  if (nrow(recs) == 0) return(invisible(NULL))

  fp <- file.path(output_dir, "recall.rec")
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  for (i in seq_len(nrow(recs))) {
    rec <- recs[i, ]

    # Get data for this recall
    dat <- tryCatch(
      query_db(con, "SELECT * FROM recall_dat WHERE recall_rec_id = ? ORDER BY id",
               params = list(rec$id)),
      error = function(e) data.frame())

    writeLines(paste0(
      swat_int_pad(i),
      swat_string_pad(rec$name, align = "left"),
      swat_int_pad(rec$rec_typ)
    ), f)

    if (nrow(dat) > 0) {
      # Write data rows
      for (d in seq_len(nrow(dat))) {
        dr <- dat[d, ]
        line <- ""
        # Write numeric columns (skip id and recall_rec_id)
        dat_cols <- setdiff(names(dr), c("id", "recall_rec_id"))
        for (dc in dat_cols) {
          val <- dr[[dc]]
          if (is.numeric(val)) {
            line <- paste0(line, swat_num_pad(val))
          } else {
            line <- paste0(line, swat_string_pad(val))
          }
        }
        writeLines(line, f)
      }
    }
  }
}

# ===================================================================
# Decision table writer
# ===================================================================

#' Write decision table files
#' @keywords internal
write_decision_tables <- function(con, output_dir, version, swat_version) {
  # Decision tables are grouped by file_name
  file_names <- tryCatch(
    query_db(con, "SELECT DISTINCT file_name FROM d_table_dtl ORDER BY file_name"),
    error = function(e) data.frame(file_name = character(0)))

  for (fn in file_names$file_name) {
    if (is.na(fn) || fn == "" || fn == "null") next

    tables <- query_db(con,
      "SELECT * FROM d_table_dtl WHERE file_name = ? ORDER BY id",
      params = list(fn))

    fp <- file.path(output_dir, fn)
    f <- file(fp, "w")
    on.exit(close(f), add = TRUE)

    writeLines(swat_meta_line(fp, version, swat_version), f)

    for (t in seq_len(nrow(tables))) {
      tbl <- tables[t, ]

      # Get conditions and actions
      conds <- tryCatch(
        query_db(con, "SELECT * FROM d_table_dtl_cond WHERE d_table_dtl_id = ? ORDER BY id",
                 params = list(tbl$id)),
        error = function(e) data.frame())
      acts <- tryCatch(
        query_db(con, "SELECT * FROM d_table_dtl_act WHERE d_table_dtl_id = ? ORDER BY id",
                 params = list(tbl$id)),
        error = function(e) data.frame())

      # Table header
      writeLines(paste0(
        swat_string_pad(tbl$name, align = "left"),
        swat_int_pad(nrow(conds)),
        swat_int_pad(0), # alts placeholder
        swat_int_pad(nrow(acts))
      ), f)

      # Conditions
      if (nrow(conds) > 0) {
        for (ci in seq_len(nrow(conds))) {
          cd <- conds[ci, ]
          writeLines(paste0(
            swat_string_pad(if(is.na(cd$var)) "null" else cd$var, align = "left"),
            swat_string_pad(if(is.na(cd$obj)) "null" else cd$obj),
            swat_int_pad(if(is.na(cd$obj_num)) 0L else cd$obj_num),
            swat_string_pad(if(is.na(cd$lim_var)) "null" else cd$lim_var),
            swat_string_pad(if(is.na(cd$lim_op)) "null" else cd$lim_op),
            swat_num_pad(if(is.na(cd$lim_const)) 0 else cd$lim_const)
          ), f)
        }
      }

      # Actions
      if (nrow(acts) > 0) {
        for (ai in seq_len(nrow(acts))) {
          ac <- acts[ai, ]
          writeLines(paste0(
            swat_string_pad(if(is.na(ac$act_typ)) "null" else ac$act_typ, align = "left"),
            swat_string_pad(if(is.na(ac$obj)) "null" else ac$obj),
            swat_int_pad(if(is.na(ac$obj_num)) 0L else ac$obj_num),
            swat_string_pad(if(is.na(ac$name)) "null" else ac$name),
            swat_string_pad(if(is.na(ac$option)) "null" else ac$option),
            swat_num_pad(if(is.na(ac$const)) 0 else ac$const),
            swat_num_pad(if(is.na(ac$const2)) 0 else ac$const2)
          ), f)
        }
      }
    }
  }
}

# ===================================================================
# Weather file copying
# ===================================================================

#' Copy weather data files from weather_dir to output_dir
#' @keywords internal
copy_weather_files <- function(con, output_dir, weather_dir,
                               weather_data_format = "observed") {
  if (is.null(weather_dir) || !dir.exists(weather_dir)) return(invisible(NULL))
  if (normalizePath(weather_dir, mustWork = FALSE) ==
      normalizePath(output_dir, mustWork = FALSE)) return(invisible(NULL))

  if (weather_data_format == "netcdf") {
    message("  Skipping weather file copy (using NetCDF format)")
    return(invisible(NULL))
  }

  message("  Copying weather files from: ", weather_dir)

  # Copy standard cli files
  for (ext in c("hmd.cli", "pcp.cli", "slr.cli", "tmp.cli", "wnd.cli")) {
    src <- file.path(weather_dir, ext)
    if (file.exists(src)) {
      file.copy(src, file.path(output_dir, ext), overwrite = TRUE)
    }
  }

  # Copy individual weather files listed in weather_file table
  if (has_data(con, "weather_file")) {
    wfiles <- query_db(con, "SELECT filename FROM weather_file")
    for (wf in wfiles$filename) {
      src <- file.path(weather_dir, wf)
      if (file.exists(src)) {
        file.copy(src, file.path(output_dir, wf), overwrite = TRUE)
      }
    }
  }
}

# ===================================================================
# file.cio writer (database-driven)
# ===================================================================

#' Write file.cio (main SWAT+ configuration file)
#'
#' Database-driven version that reads the file_cio and
#' file_cio_classification tables to determine which files to include.
#' Falls back to a static template if these tables don't exist.
#'
#' @param con Database connection.
#' @param output_dir Output directory.
#' @param version Editor version.
#' @param swat_version SWAT+ version.
#' @param is_lte Logical. Is this an LTE project.
#' @param weather_data_format Character. Weather data format.
#' @keywords internal
write_file_cio <- function(con, output_dir, version = NULL,
                           swat_version = NULL, is_lte = FALSE,
                           weather_data_format = "observed") {
  fp <- file.path(output_dir, "file.cio")

  # Try database-driven approach
  if (has_data(con, "file_cio_classification") && has_data(con, "file_cio")) {
    write_file_cio_from_db(con, fp, version, swat_version,
                           is_lte, weather_data_format)
  } else {
    write_file_cio_static(fp, version, swat_version)
  }
}

#' Write file.cio from database tables
#' @keywords internal
write_file_cio_from_db <- function(con, file_path, version, swat_version,
                                   is_lte, weather_data_format) {
  is_netcdf <- identical(weather_data_format, "netcdf")
  classifications <- get_file_cio_conditions(con, is_lte, is_netcdf)

  classes <- query_db(con,
    "SELECT * FROM file_cio_classification ORDER BY id")
  files <- query_db(con,
    "SELECT * FROM file_cio ORDER BY order_in_class")

  f <- file(file_path, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(file_path, version, swat_version), f)

  for (ci in seq_len(nrow(classes))) {
    cls <- classes[ci, ]
    cls_name <- cls$name
    conditions <- classifications[[cls_name]]

    line <- swat_string_pad(cls_name, align = "left")

    # Get files for this classification
    cls_files <- files[files$classification_id == cls$id, ]

    if (nrow(cls_files) == 0) {
      line <- paste0(line, swat_string_pad(SWAT_NULL_STR, align = "left"))
    } else {
      for (fi in seq_len(nrow(cls_files))) {
        cf <- cls_files[fi, ]
        order_idx <- cf$order_in_class
        cond_met <- if (!is.null(conditions) && order_idx %in% names(conditions)) {
          conditions[[as.character(order_idx)]]
        } else if (!is.null(conditions) && order_idx <= length(conditions)) {
          conditions[[order_idx]]
        } else {
          FALSE
        }

        fname <- if (isTRUE(cond_met)) {
          fn <- cf$file_name
          if (is.null(fn) || is.na(fn) || fn == "") SWAT_NULL_STR else fn
        } else {
          SWAT_NULL_STR
        }

        # NetCDF: replace weather-sta.cli with netcdf.ncw
        if (is_netcdf && cls_name == "climate" && order_idx == 1) {
          fname <- "netcdf.ncw"
        }

        line <- paste0(line, swat_string_pad(fname, align = "left"))
      }
    }

    writeLines(line, f)
  }
}

#' Get file.cio condition flags for each classification
#' @keywords internal
get_file_cio_conditions <- function(con, is_lte = FALSE, is_netcdf = FALSE) {
  # Helper for safe count
  sc <- function(tbl) safe_count(con, tbl)

  gwflow_on <- tryCatch({
    cfg <- query_db(con, "SELECT * FROM codes_bsn LIMIT 1")
    isTRUE(cfg$gwflow == 1)
  }, error = function(e) FALSE)

  list(
    simulation = list(TRUE, TRUE, sc("object_prt") > 0, TRUE,
                      sc("constituents_cs") > 0),
    basin = list(sc("codes_bsn") > 0, sc("parameters_bsn") > 0),
    climate = if (is_netcdf) {
      list(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
           sc("atmo_cli") > 0)
    } else {
      list(TRUE, TRUE,
           sc("weather_file") > 0, sc("weather_file") > 0,
           sc("weather_file") > 0, sc("weather_file") > 0,
           sc("weather_file") > 0, sc("weather_file") > 0,
           sc("atmo_cli") > 0)
    },
    connect = list(
      sc("hru_con") > 0, sc("hru_lte_con") > 0,
      sc("rout_unit_con") > 0, gwflow_on,
      !gwflow_on && sc("aquifer_con") > 0,
      sc("aquifer2d_con") > 0, sc("channel_con") > 0,
      sc("reservoir_con") > 0, sc("recall_con") > 0,
      sc("exco_con") > 0, sc("delratio_con") > 0,
      sc("outlet_con") > 0, sc("chandeg_con") > 0),
    channel = list(
      sc("initial_cha") > 0, sc("channel_cha") > 0,
      sc("hydrology_cha") > 0, sc("sediment_cha") > 0,
      sc("nutrients_cha") > 0, sc("channel_lte_cha") > 0,
      sc("hyd_sed_lte_cha") > 0, FALSE),
    reservoir = list(
      sc("initial_res") > 0, sc("reservoir_res") > 0,
      sc("hydrology_res") > 0, sc("sediment_res") > 0,
      sc("nutrients_res") > 0, sc("weir_res") > 0,
      sc("wetland_wet") > 0, sc("hydrology_wet") > 0),
    routing_unit = list(
      sc("rout_unit_ele") > 0, sc("rout_unit_ele") > 0,
      sc("rout_unit_rtu") > 0, sc("rout_unit_dr") > 0),
    hru = list(sc("hru_data_hru") > 0, sc("hru_lte_hru") > 0),
    exco = list(
      sc("exco_exc") > 0, sc("exco_om_exc") > 0,
      sc("exco_pest_exc") > 0, sc("exco_path_exc") > 0,
      sc("exco_hmet_exc") > 0, sc("exco_salt_exc") > 0),
    recall = list(sc("recall_rec") > 0),
    dr = list(
      sc("delratio_del") > 0, sc("dr_om_del") > 0,
      sc("dr_pest_del") > 0, sc("dr_path_del") > 0,
      sc("dr_hmet_del") > 0, sc("dr_salt_del") > 0),
    aquifer = list(sc("initial_aqu") > 0, sc("aquifer_aqu") > 0),
    water_rights = list(sc("water_allocation_wro") > 0, FALSE, FALSE),
    link = list(sc("chan_surf_lin") > 0, FALSE),
    hydrology = list(
      sc("hydrology_hyd") > 0, sc("topography_hyd") > 0,
      sc("field_fld") > 0),
    structural = list(
      sc("tiledrain_str") > 0, sc("septic_str") > 0,
      sc("filterstrip_str") > 0, sc("grassedww_str") > 0,
      sc("bmpuser_str") > 0),
    hru_parm_db = list(
      sc("plants_plt") > 0, sc("fertilizer_frt") > 0,
      sc("tillage_til") > 0, sc("pesticide_pst") > 0,
      sc("pathogens_pth") > 0, sc("metals_mtl") > 0,
      sc("salts_slt") > 0, sc("urban_urb") > 0,
      sc("septic_sep") > 0, sc("snow_sno") > 0),
    ops = list(
      sc("harv_ops") > 0, sc("graze_ops") > 0, sc("irr_ops") > 0,
      sc("chem_app_ops") > 0, sc("fire_ops") > 0, sc("sweep_ops") > 0),
    lum = list(
      sc("landuse_lum") > 0, sc("management_sch") > 0,
      sc("cntable_lum") > 0, sc("cons_prac_lum") > 0,
      sc("ovn_table_lum") > 0),
    chg = list(
      sc("cal_parms_cal") > 0, sc("calibration_cal") > 0,
      sc("codes_sft") > 0, sc("wb_parms_sft") > 0,
      sc("water_balance_sft") > 0, sc("ch_sed_budget_sft") > 0,
      sc("ch_sed_parms_sft") > 0, sc("plant_parms_sft") > 0,
      sc("plant_gro_sft") > 0),
    init = list(
      sc("plant_ini") > 0, sc("soil_plant_ini") > 0,
      sc("om_water_ini") > 0, sc("pest_hru_ini") > 0,
      sc("pest_water_ini") > 0, sc("path_hru_ini") > 0,
      sc("path_water_ini") > 0, FALSE, FALSE, FALSE, FALSE),
    soils = list(
      !is_lte && sc("soils_sol") > 0,
      sc("nutrients_sol") > 0,
      is_lte && sc("soils_lte_sol") > 0),
    decision_table = list(
      sc("d_table_dtl") > 0, sc("d_table_dtl") > 0,
      sc("d_table_dtl") > 0, sc("d_table_dtl") > 0),
    regions = list(
      sc("ls_unit_ele") > 0, sc("ls_unit_def") > 0,
      sc("ls_reg_ele") > 0, sc("ls_reg_def") > 0, FALSE,
      sc("ch_catunit_ele") > 0, sc("ch_catunit_def") > 0,
      sc("ch_reg_def") > 0, sc("aquifer_con") > 0,
      sc("aqu_catunit_def") > 0, sc("aqu_reg_def") > 0,
      sc("res_catunit_ele") > 0, sc("res_catunit_def") > 0,
      sc("res_reg_def") > 0, sc("rec_catunit_ele") > 0,
      sc("rec_catunit_def") > 0, sc("rec_reg_def") > 0),
    pcp_path = list(TRUE),
    tmp_path = list(TRUE),
    slr_path = list(TRUE),
    hmd_path = list(TRUE),
    wnd_path = list(TRUE),
    out_path = list(TRUE)
  )
}

#' Write static file.cio as fallback
#' @keywords internal
write_file_cio_static <- function(file_path, version = NULL,
                                  swat_version = NULL) {
  f <- file(file_path, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(file_path, version, swat_version), f)

  sections <- c(
    "simulation", "basin", "climate", "connect", "channel", "reservoir",
    "routing_unit", "hru", "exco", "recall", "dr", "aquifer", "hrd",
    "water_rights", "link", "hydrology", "structural", "hru_parm_db",
    "ops", "lum", "chg", "init", "soils", "decision_table", "regions"
  )

  for (sec in sections) {
    writeLines(paste0(swat_string_pad(sec, align = "left"),
                      swat_string_pad(SWAT_NULL_STR, align = "left")), f)
  }
}
