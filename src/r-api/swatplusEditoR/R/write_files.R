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
                               weather_dir = NULL) {
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
  
  # Primary path: write all files natively in R
  write_direct(project, output_dir, swat_version, weather_dir)
}

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

  # Ensure all required tables exist and are populated with reference data.
  ensure_write_tables(con)

  # Populate SWAT+ model tables from GIS data (channels, HRUs, routing units,
  # aquifers, etc.) if they are empty.  This mirrors the Python SWAT+ Editor
  # import_gis.py::insert_default() step.
  populate_from_gis(con)

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
        if (!is.na(cfg$weather_data_format)) {
          weather_data_format <- cfg$weather_data_format
        }
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
  
  # Update codes_bsn gwflow flag (matches Python gwflow_writer.update_codes_bsn())
  update_gwflow_codes_bsn(con)
  
  # Helper to get file names from file_cio table
  get_files <- function(con, section, n) {
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
    files <- get_files(con = con, section = "simulation", 5)
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
    files <- get_files(con = con, section = "climate", n = 9)
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
  write_table_section(con, output_dir, v, sv, has_cio, section = "channel",
                      specs =  list(
                        list(tbl = "initial_cha", file = "initial.cha",
                             ignore_id = TRUE,
                             query = "SELECT i.id, i.name,
                    COALESCE(o.name, 'null') as org_min,
                    'null' as pest, 'null' as path,
                    'null' as hmet, 'null' as salt
                  FROM initial_cha i
                  LEFT JOIN om_water_ini o ON i.org_min_id = o.id
                  ORDER BY i.id"),
                        list(tbl = "channel_cha", file = "channel.cha",
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
                        list(tbl = "channel_lte_cha", file = "channel-lte.cha",
                             query = "SELECT c.id, c.name,
                    COALESCE(i.name, 'null') as cha_ini,
                    COALESCE(h.name, 'null') as cha_hyd,
                    'null' as cha_sed, 'null' as cha_nut
                  FROM channel_lte_cha c
                  LEFT JOIN initial_cha i ON c.init_id = i.id
                  LEFT JOIN hyd_sed_lte_cha h ON c.hyd_id = h.id
                  ORDER BY c.id"),
                        list(tbl = "hyd_sed_lte_cha", file = "hyd-sed-lte.cha",
                             ignore_id = TRUE,
                             non_zero_min = c("wd", "dp", "slp", "len")),
                        list(tbl = "temperature_cha", file = "temperature.cha")
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
  if (has_data(con, "rout_unit_con")) {
    write_rout_unit_def(con, output_dir, v, sv)
  }
  write_table_section(con, output_dir, v, sv, has_cio, "routing_unit", list(
    list(tbl = NULL, file = NULL),  # rout_unit.def written above
    list(tbl = "rout_unit_ele", file = "rout_unit.ele",
         query = "SELECT e.id, e.name, e.obj_typ, e.obj_id, e.frac,
           COALESCE(d.name, '0') as dlr
         FROM rout_unit_ele e
         LEFT JOIN delratio_del d ON e.dlr_id = d.id
         ORDER BY e.id"),
    list(tbl = "rout_unit_rtu", file = "rout_unit.rtu",
         query = "SELECT r.id, r.name, r.name as define,
           'null' as dlr,
           COALESCE(t.name, 'null') as topo,
           COALESCE(f.name, 'null') as field
         FROM rout_unit_rtu r
         LEFT JOIN topography_hyd t ON r.topo_id = t.id
         LEFT JOIN field_fld f ON r.field_id = f.id
         ORDER BY r.id"),
    list(tbl = "rout_unit_dr", file = "rout_unit.dr")
  ))

  # ---- HRU section ----
  message("  Writing HRU files...")
  write_table_section(con, output_dir, v, sv, has_cio, "hru", list(
    list(tbl = "hru_data_hru", file = "hru-data.hru",
         query = "SELECT h.id, h.name,
           COALESCE(t.name, 'null') as topo,
           COALESCE(hy.name, 'null') as hydro,
           COALESCE(s.name, 'null') as soil,
           COALESCE(l.name, 'null') as lu_mgt,
           COALESCE(sp.name, 'null') as soil_plant_init,
           COALESCE(h.surf_stor, 'null') as surf_stor,
           COALESCE(sn.name, 'null') as snow,
           COALESCE(f.name, '0') as field
         FROM hru_data_hru h
         LEFT JOIN topography_hyd t ON h.topo_id = t.id
         LEFT JOIN hydrology_hyd hy ON h.hydro_id = hy.id
         LEFT JOIN soils_sol s ON h.soil_id = s.id
         LEFT JOIN landuse_lum l ON h.lu_mgt_id = l.id
         LEFT JOIN soil_plant_ini sp ON h.soil_plant_ini_id = sp.id
         LEFT JOIN snow_sno sn ON h.snow_id = sn.id
         LEFT JOIN field_fld f ON h.field_id = f.id
         ORDER BY h.id"),
    list(tbl = "hru_lte_hru", file = "hru-lte.hru")
  ))
  
  # ---- DR section ----
  write_table_section(con, output_dir, v, sv, has_cio, "dr", list(
    list(tbl = "delratio_del", file = "delratio.del"),
    list(tbl = "dr_om_del", file = "dr.del"),
    list(tbl = "dr_pest_del", file = "dr_pest.del"),
    list(tbl = "dr_path_del", file = "dr_path.del"),
    list(tbl = "dr_hmet_del", file = "dr_hmet.del"),
    list(tbl = "dr_salt_del", file = "dr_salt.del")
  ))
  
  # ---- AQUIFER section ----
  message("  Writing aquifer files...")
  write_table_section(con, output_dir, v, sv, has_cio, "aquifer", list(
    list(tbl = "initial_aqu", file = "initial.aqu",
         ignore_id = TRUE,
         query = "SELECT i.id, i.name,
           COALESCE(o.name, 'null') as org_min,
           'null' as pest, 'null' as path,
           'null' as hmet, 'null' as salt
         FROM initial_aqu i
         LEFT JOIN om_water_ini o ON i.org_min_id = o.id
         ORDER BY i.id"),
    list(tbl = "aquifer_aqu", file = "aquifer.aqu",
         query = "SELECT a.id, a.name,
           COALESCE(i.name, 'null') as init,
           a.gw_flo, a.dep_bot, a.dep_wt, a.no3_n, a.sol_p,
           0.0 as carbon, 50.0 as flo_dist,
           a.bf_max, a.alpha_bf, a.revap, a.rchg_dp,
           a.spec_yld, a.hl_no3n, a.flo_min, a.revap_min
         FROM aquifer_aqu a
         LEFT JOIN initial_aqu i ON a.init_id = i.id
         ORDER BY a.id")
  ))
  
  # ---- HERD section ----
  # Stub matching Python write_herd() which is also a pass/stub.
  # The file.cio section exists (animal.hrd, herd.hrd, ranch.hrd)
  # but no writer classes exist in the Python codebase either.
  write_table_section(con, output_dir, v, sv, has_cio, "herd", list(
    list(tbl = "animal_hrd", file = "animal.hrd"),
    list(tbl = "herd_hrd", file = "herd.hrd"),
    list(tbl = "ranch_hrd", file = "ranch.hrd")
  ))
  
  # ---- WATER RIGHTS section ----
  write_table_section(con, output_dir, v, sv, has_cio, "water_rights", list(
    list(tbl = "water_allocation_wro", file = "water_allocation.wro"),
    list(tbl = "element_wro", file = "element.wro"),
    list(tbl = "define_wro", file = "define.wro")
  ))
  
  # ---- LINK section ----
  write_table_section(con, output_dir, v, sv, has_cio, "link", list(
    list(tbl = "chan_surf_lin", file = "chan-surf.lin"),
    list(tbl = "chan_aqu_lin", file = "chan-aqu.lin")
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
    list(tbl = "topography_hyd", file = "topography.hyd", ignore_id = TRUE),
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
  if (has_data(con, "management_sch")) {
    write_management_sch(con, output_dir, v, sv)
  }
  write_table_section(con, output_dir, v, sv, has_cio, "lum", list(
    list(tbl = "landuse_lum", file = "landuse.lum",
         query = "SELECT l.name, l.cal_group,
           COALESCE(pi.name, 'null') AS plnt_com,
           COALESCE(m.name,  'null') AS mgt,
           COALESCE(c.name,  'null') AS cn2,
           COALESCE(cp.name, 'null') AS cons_prac,
           COALESCE(u.name,  'null') AS urban,
           COALESCE(l.urb_ro,'null') AS urb_ro,
           COALESCE(o.name,  'null') AS ov_mann,
           COALESCE(ti.name, 'null') AS tile,
           COALESCE(se.name, 'null') AS sep,
           COALESCE(fs.name, 'null') AS vfs,
           COALESCE(gw.name, 'null') AS grww,
           COALESCE(bm.name, 'null') AS bmp
         FROM landuse_lum l
         LEFT JOIN plant_ini       pi ON l.plnt_com_id  = pi.id
         LEFT JOIN management_sch   m ON l.mgt_id        = m.id
         LEFT JOIN cntable_lum      c ON l.cn2_id         = c.id
         LEFT JOIN cons_prac_lum   cp ON l.cons_prac_id   = cp.id
         LEFT JOIN urban_urb        u ON l.urban_id        = u.id
         LEFT JOIN ovn_table_lum    o ON l.ov_mann_id      = o.id
         LEFT JOIN tiledrain_str   ti ON l.tile_id          = ti.id
         LEFT JOIN septic_str      se ON l.sep_id            = se.id
         LEFT JOIN filterstrip_str fs ON l.vfs_id            = fs.id
         LEFT JOIN grassedww_str   gw ON l.grww_id           = gw.id
         LEFT JOIN bmpuser_str     bm ON l.bmp_id             = bm.id
         ORDER BY l.id"),
    list(tbl = NULL, file = NULL),  # management.sch written above
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
  if (has_data(con, "plant_ini")) {
    write_plant_ini(con, output_dir, v, sv)
  }
  write_table_section(con, output_dir, v, sv, has_cio, "init", list(
    list(tbl = NULL, file = NULL),  # plant.ini written above
    list(tbl = "soil_plant_ini", file = "soil_plant.ini"),
    list(tbl = "om_water_ini", file = "om_water.ini", ignore_id = TRUE),
    list(tbl = "pest_hru_ini", file = "pest_hru.ini"),
    list(tbl = "pest_water_ini", file = "pest_water.ini"),
    list(tbl = "path_hru_ini", file = "path_hru.ini"),
    list(tbl = "path_water_ini", file = "path_water.ini"),
    list(tbl = "hmet_hru_ini", file = "hmet_hru.ini"),
    list(tbl = "hmet_water_ini", file = "hmet_water.ini"),
    list(tbl = "salt_hru_ini", file = "salt_hru.ini"),
    list(tbl = "salt_water_ini", file = "salt_water.ini")
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
    list(tbl = "aquifer_con", file = "aqu_catunit.ele",
         query = paste(
           "SELECT ac.id, ac.name, 'aqu' as obj_typ, ac.id as obj_typ_no,",
           "CASE WHEN (SELECT COALESCE(SUM(area), 0) FROM rout_unit_con) > 0",
           "     THEN ac.area / (SELECT SUM(area) FROM rout_unit_con)",
           "     ELSE 0.0 END as bsn_frac,",
           "0.0 as sub_frac, 0.0 as reg_frac",
           "FROM aquifer_con ac ORDER BY ac.id")),
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
  
  # ---- GWFLOW files (if gwflow module is active) ----
  if (gwflow_exists(con)) {
    message("  Writing gwflow files...")
    write_gwflow_files(con, output_dir, v, sv)
  }
  
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
    if (!has_data(con, tbl)) {
      cli::cli_alert_warning("  - Skipping {fname} (table '{tbl}' is empty or missing)")
      next
    }
    
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

#' Write rout_unit.def in SWAT+ format
#' Each routing unit block: id / name / elem_tot / element IDs.
#' Iterates over rout_unit_con (one row per RTU) and for each RTU writes
#' the sequential IDs of all associated rout_unit_ele rows using range
#' notation (negative = "through").
#' Mirrors Python Rout_unit_def.write() in fileio/routing_unit.py.
#' @keywords internal
write_rout_unit_def <- function(con, output_dir, version = NULL,
                                swat_version = NULL,
                                file_name = "rout_unit.def") {
  rtus <- tryCatch(
    query_db(con, "SELECT id, name FROM rout_unit_con ORDER BY id"),
    error = function(e) NULL)
  if (is.null(rtus) || nrow(rtus) == 0L) return(invisible(NULL))

  # All rout_unit_ele rows ordered by id — used to derive sequential position
  eles <- tryCatch(
    query_db(con, "SELECT id, rtu_id, obj_id FROM rout_unit_ele ORDER BY id"),
    error = function(e) NULL)
  if (is.null(eles) || nrow(eles) == 0L) return(invisible(NULL))

  # Build ordered list of obj_ids (used by Python write_ele_ids2 to find position)
  all_obj_ids <- eles$obj_id

  fp <- file.path(output_dir, file_name)
  f  <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  writeLines(paste0(
    swat_int_pad("id"),
    swat_string_pad("name"),
    swat_int_pad("elem_tot"),
    swat_int_pad("elements")
  ), f)

  for (i in seq_len(nrow(rtus))) {
    rtu <- rtus[i, ]
    rtu_eles <- eles[eles$rtu_id == rtu$id, , drop = FALSE]

    # Convert obj_ids to their sequential position in all_obj_ids
    seq_ids <- match(rtu_eles$obj_id, all_obj_ids)
    seq_ids <- seq_ids[!is.na(seq_ids)]

    # Build range notation: positive = start, negative = end-of-range
    ele_parts <- character(0)
    if (length(seq_ids) > 0L) {
      last_id        <- 0L
      last_appended  <- 0L
      just_wrote     <- FALSE
      for (sid in seq_ids) {
        if (last_id == 0L) {
          ele_parts  <- c(ele_parts, swat_int_pad(sid))
          just_wrote <- TRUE
          last_appended <- sid
        } else if (sid > last_id + 1L) {
          if (last_appended != last_id)
            ele_parts <- c(ele_parts, swat_int_pad(-last_id))
          ele_parts   <- c(ele_parts, swat_int_pad(sid))
          last_appended <- sid
          just_wrote  <- TRUE
        } else {
          just_wrote  <- FALSE
        }
        last_id <- sid
      }
      if (!just_wrote && length(seq_ids) > 0L)
        ele_parts <- c(ele_parts, swat_int_pad(-last_id))
    }

    writeLines(paste0(
      swat_int_pad(i),
      swat_string_pad(rtu$name),
      swat_int_pad(length(ele_parts)),
      paste(ele_parts, collapse = "")
    ), f)
  }

  invisible(NULL)
}


#' Write plant.ini in SWAT+ hierarchical format
#' Each plant community block: name + plt_cnt + rot_yr_ini, then one sub-row
#' per plant (indented).  Only writes communities referenced from
#' landuse_lum.plnt_com_id (i.e. those actually used in the watershed).
#' Mirrors Python Plant_ini.write() in fileio/init.py.
#' @keywords internal
write_plant_ini <- function(con, output_dir, version = NULL,
                            swat_version = NULL,
                            file_name = "plant.ini") {
  if (!has_data(con, "plant_ini")) return(invisible(NULL))

  # Only communities actually referenced from landuse_lum
  used_sql <- paste0(
    "SELECT pi.id, pi.name, pi.rot_yr_ini ",
    "FROM plant_ini pi ",
    "WHERE pi.id IN (SELECT DISTINCT plnt_com_id FROM landuse_lum WHERE plnt_com_id IS NOT NULL) ",
    "ORDER BY pi.id")
  comms <- tryCatch(query_db(con, used_sql), error = function(e) NULL)
  if (is.null(comms) || nrow(comms) == 0L) return(invisible(NULL))

  # Plant sub-rows: join plant_ini_item with plants_plt to get plant name
  items_sql <- paste0(
    "SELECT pii.plant_ini_id, p.name AS plt_name, ",
    "pii.lc_status, pii.lai_init, pii.bm_init, pii.phu_init, ",
    "pii.plnt_pop, pii.yrs_init, pii.rsd_init ",
    "FROM plant_ini_item pii ",
    "LEFT JOIN plants_plt p ON pii.plnt_name_id = p.id ",
    "WHERE pii.plant_ini_id IN (",
    paste(comms$id, collapse = ","), ") ",
    "ORDER BY pii.plant_ini_id, pii.id")
  items <- tryCatch(query_db(con, items_sql), error = function(e) data.frame())

  fp <- file.path(output_dir, file_name)
  f  <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  # Header — community-level columns then plant-level columns
  # Python: string_pad(left) + int_pad + int_pad + key_name_pad + code_pad + num_pad*6
  name_pad <- SWAT_STR_PAD
  writeLines(paste0(
    swat_string_pad("pcom_name", pad = name_pad, align = "left"),
    swat_int_pad("plt_cnt"),
    swat_int_pad("rot_yr_ini"),
    swat_string_pad("plt_name", pad = SWAT_KEY_PAD, align = "right"),
    swat_string_pad("lc_status", pad = SWAT_CODE_PAD, align = "right"),
    swat_num_pad("lai_init"),
    swat_num_pad("bm_init"),
    swat_num_pad("phu_init"),
    swat_num_pad("plnt_pop"),
    swat_num_pad("yrs_init"),
    swat_num_pad("rsd_init")
  ), f)
  writeLines("", f)

  blank_name <- swat_string_pad("", pad = name_pad, align = "left", null_text = "")
  blank_int  <- swat_int_pad(0)
  # Override blank_int to be empty (matching Python blank padding)
  blank_int  <- formatC("", width = SWAT_INT_PAD + 2L, flag = "-")

  for (i in seq_len(nrow(comms))) {
    comm  <- comms[i, ]
    c_items <- if (nrow(items) > 0 && "plant_ini_id" %in% names(items))
      items[items$plant_ini_id == comm$id, , drop = FALSE]
    else data.frame()
    plt_cnt <- nrow(c_items)

    # Community header row
    writeLines(paste0(
      swat_string_pad(comm$name, pad = name_pad, align = "left"),
      swat_int_pad(plt_cnt),
      swat_int_pad(comm$rot_yr_ini)
    ), f)

    # Plant sub-rows
    for (j in seq_len(nrow(c_items))) {
      item <- c_items[j, ]
      writeLines(paste0(
        blank_name,
        blank_int,
        swat_string_pad(item$plt_name, pad = SWAT_KEY_PAD, align = "right"),
        swat_bool_pad(item$lc_status),
        swat_num_pad(item$lai_init),
        swat_num_pad(item$bm_init),
        swat_num_pad(item$phu_init),
        swat_num_pad(item$plnt_pop),
        swat_num_pad(item$yrs_init),
        swat_num_pad(item$rsd_init)
      ), f)
    }
  }

  invisible(NULL)
}


#' Writes the SWAT+ management schedule file, including auto-operations
#' (decision-table-driven) and manual operations for each schedule.
#' Mirrors Python Management_sch.write() in fileio/lum.py.
#' @keywords internal
write_management_sch <- function(con, output_dir, version = NULL,
                                 swat_version = NULL,
                                 file_name = "management.sch") {
  if (!has_data(con, "management_sch")) return(invisible(NULL))

  scheds <- tryCatch(
    query_db(con, "SELECT id, name FROM management_sch ORDER BY id"),
    error = function(e) NULL)
  if (is.null(scheds) || nrow(scheds) == 0L) return(invisible(NULL))

  auto_ops <- tryCatch(
    query_db(con, paste0(
      "SELECT a.management_sch_id, d.name as d_table, a.plant1, a.plant2 ",
      "FROM management_sch_auto a ",
      "LEFT JOIN d_table_dtl d ON a.d_table_id = d.id ",
      "ORDER BY a.management_sch_id, a.id")),
    error = function(e) data.frame())

  reg_ops <- tryCatch(
    query_db(con, paste0(
      "SELECT management_sch_id, op_typ, mon, day, hu_sch, ",
      "op_data1, op_data2, op_data3 ",
      "FROM management_sch_op ",
      "ORDER BY management_sch_id, id")),
    error = function(e) data.frame())

  fp <- file.path(output_dir, file_name)
  f <- file(fp, "w")
  on.exit(close(f))

  writeLines(swat_meta_line(fp, version, swat_version), f)

  # Header line
  name_pad <- 41L
  writeLines(paste0(
    swat_string_pad("name", align = "left", pad = name_pad),
    swat_string_pad("numb_ops",  align = "right", pad = SWAT_INT_PAD),
    swat_string_pad("numb_auto", align = "right", pad = 9L),
    swat_string_pad("op_typ",    align = "right"),
    swat_string_pad("mon",       align = "right"),
    swat_string_pad("day",       align = "right"),
    swat_string_pad("hu_sch",    align = "right"),
    swat_string_pad("op_data1",  align = "right"),
    swat_string_pad("op_data2",  align = "right"),
    swat_string_pad("op_data3",  align = "right")
  ), f)
  writeLines("", f)

  row_pad <- 46L  # padding before auto-op / reg-op entries

  for (i in seq_len(nrow(scheds))) {
    sch_id   <- scheds$id[i]
    sch_name <- scheds$name[i]

    aops <- if (nrow(auto_ops) > 0 && "management_sch_id" %in% names(auto_ops))
      auto_ops[auto_ops$management_sch_id == sch_id, , drop = FALSE] else data.frame()
    rops <- if (nrow(reg_ops) > 0 && "management_sch_id" %in% names(reg_ops))
      reg_ops[reg_ops$management_sch_id == sch_id, , drop = FALSE] else data.frame()

    n_ops  <- nrow(rops)
    n_auto <- nrow(aops)

    writeLines(paste0(
      swat_string_pad(sch_name, align = "left", pad = 25L),
      swat_int_pad(n_ops),
      swat_string_pad(as.character(n_auto), align = "right", pad = 9L)
    ), f)
    writeLines("", f)

    # Auto-op rows
    special_tables <- c("pl_hv_summer1", "pl_hv_summer2", "pl_hv_winter1")
    for (j in seq_len(nrow(aops))) {
      aop <- aops[j, ]
      line <- paste0(swat_string_pad(" ", pad = row_pad),
                     swat_string_pad(aop$d_table, align = "left", pad = 16L))
      if (!is.na(aop$d_table) && aop$d_table %in% special_tables) {
        line <- paste0(line, swat_string_pad(
          if (!is.na(aop$plant1) && aop$plant1 != "") aop$plant1 else "null",
          align = "left", pad = 5L))
        if (!is.na(aop$d_table) && aop$d_table == "pl_hv_summer2" &&
            !is.na(aop$plant2) && aop$plant2 != "") {
          line <- paste0(line, swat_string_pad(aop$plant2, align = "left", pad = 5L))
        }
      }
      writeLines(line, f)
    }

    # Regular operation rows
    for (j in seq_len(nrow(rops))) {
      rop <- rops[j, ]
      skip_val <- if (!is.na(rop$op_typ) && rop$op_typ == "skip") "skip" else ""
      if (skip_val == "skip") {
        writeLines(paste0(swat_string_pad(" ", pad = row_pad),
                          swat_string_pad("skip", align = "left")), f)
      } else {
        writeLines(paste0(
          swat_string_pad(" ",           pad = row_pad),
          swat_string_pad(rop$op_typ,    align = "left"),
          swat_int_pad(rop$mon),
          swat_int_pad(rop$day),
          swat_num_pad(rop$hu_sch),
          swat_string_pad(rop$op_data1, align = "left"),
          swat_string_pad(rop$op_data2, align = "left"),
          swat_string_pad(rop$op_data3, align = "left")
        ), f)
      }
    }
  }

  invisible(NULL)
}

#' Write weather station CLI file
#' @keywords internal
write_weather_sta_cli <- function(con, output_dir, version = NULL,
                                  swat_version = NULL,
                                  file_name = "weather-sta.cli") {
  if (!has_data(con, "weather_sta_cli")) return(invisible(NULL))
  
  # Join with wgn to get wgn name
  sql <- "SELECT s.name, w.name as wgn,
               s.pcp, s.tmp, s.slr, s.hmd, s.wnd, s.pet, s.atmo_dep
        FROM weather_sta_cli s
        INNER JOIN weather_wgn_cli w ON s.wgn_id = w.name
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
  # Map connect tables to file.cio positions (1-based).
  # The file.cio connect section has 13 entries in fixed order.
  # Positions 4 (gwflow.con), 6 (aquifer2d.con), and 12 (outlet.con)
  # are handled by file.cio conditions but not explicitly written here
  # (matching Python behaviour where these positions are skipped).
  con_specs <- list(
    list(cio_pos = 1,  con_tbl = "hru_con",        con_out_tbl = "hru_con_out",
         elem_name = "hru",  file = "hru.con"),
    list(cio_pos = 2,  con_tbl = "hru_lte_con",    con_out_tbl = "hru_lte_con_out",
         elem_name = "lhru", file = "hru-lte.con"),
    list(cio_pos = 3,  con_tbl = "rout_unit_con",  con_out_tbl = "rout_unit_con_out",
         elem_name = "rtu",  file = "rout_unit.con"),
    list(cio_pos = 5,  con_tbl = "aquifer_con",    con_out_tbl = "aquifer_con_out",
         elem_name = "aqu",  file = "aquifer.con"),
    list(cio_pos = 7,  con_tbl = "channel_con",    con_out_tbl = "channel_con_out",
         elem_name = "cha",  file = "channel-lte.con"),
    list(cio_pos = 8,  con_tbl = "reservoir_con",  con_out_tbl = "reservoir_con_out",
         elem_name = "res",  file = "reservoir.con"),
    list(cio_pos = 9,  con_tbl = "recall_con",     con_out_tbl = "recall_con_out",
         elem_name = "rec",  file = "recall.con"),
    list(cio_pos = 10, con_tbl = "exco_con",       con_out_tbl = "exco_con_out",
         elem_name = "exco", file = "exco.con"),
    list(cio_pos = 11, con_tbl = "delratio_con",   con_out_tbl = "delratio_con_out",
         elem_name = "dlr",  file = "delratio.con"),
    list(cio_pos = 12, con_tbl = "outlet_con",     con_out_tbl = "outlet_con_out",
         elem_name = "out",  file = "outlet.con"),
    list(cio_pos = 13, con_tbl = "chandeg_con",    con_out_tbl = "chandeg_con_out",
         elem_name = "lcha", file = "chandeg.con")
  )
  
  cio_files <- if (has_cio) get_cio_file_names(con, "connect") else character(0)
  
  for (spec in con_specs) {
    pos <- spec$cio_pos
    fname <- if (pos <= length(cio_files) && cio_files[pos] != "null") {
      trimws(cio_files[pos])
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
    # on.exit(close(f), add = TRUE)
    
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
    close(f)
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
          fn <- cf$default_file_name
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

  # Ensure path entries appear even if missing from file_cio_classification in DB
  # (rQSWATPlus may use an older database without these entries)
  db_class_names <- classes$name
  expected_paths <- c("pcp_path", "tmp_path", "slr_path", "hmd_path", "wnd_path", "out_path")
  for (pn in expected_paths) {
    if (!pn %in% db_class_names) {
      writeLines(paste0(swat_string_pad(pn, align = "left"),
                        swat_string_pad(SWAT_NULL_STR, align = "left")), f)
    }
  }
}

#' Get file.cio condition flags for each classification
#' @keywords internal
get_file_cio_conditions <- function(con, is_lte = FALSE, is_netcdf = FALSE) {
  # Helper for safe count
  sc <- function(tbl) safe_count(con, tbl)

  # Helper: count rows in weather_sta_cli where a column has a non-null file ref
  sc_wsta <- function(col) {
    tryCatch({
      r <- query_db(con, sprintf(
        "SELECT COUNT(*) as cnt FROM weather_sta_cli WHERE %s IS NOT NULL AND %s NOT IN ('null', '')",
        col, col))
      r$cnt[1]
    }, error = function(e) 0L)
  }

  # Per-file counts for decision tables
  dtl_files <- tryCatch(
    query_db(con, "SELECT DISTINCT file_name FROM d_table_dtl WHERE file_name IS NOT NULL")$file_name,
    error = function(e) character(0))

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
           sc_wsta("pet") > 0,  # pet.cli
           sc_wsta("pcp") > 0,  # pcp.cli
           sc_wsta("tmp") > 0,  # tmp.cli
           sc_wsta("slr") > 0,  # slr.cli
           sc_wsta("hmd") > 0,  # hmd.cli
           sc_wsta("wnd") > 0,  # wnd.cli
           sc("atmo_cli") > 0)  # atmodep.cli
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
      sc("initial_cha") > 0,
      !is_lte && sc("channel_cha") > 0,       # standard only
      !is_lte && sc("hydrology_cha") > 0,     # standard only
      !is_lte && sc("sediment_cha") > 0,      # standard only
      sc("nutrients_cha") > 0,
      is_lte && sc("channel_lte_cha") > 0,   # lte only
      is_lte && sc("hyd_sed_lte_cha") > 0,   # lte only
      sc("temperature_cha") > 0),
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
    herd = list(sc("animal_hrd") > 0, sc("herd_hrd") > 0, sc("ranch_hrd") > 0),
    water_rights = list(sc("water_allocation_wro") > 0,
                        sc("element_wro") > 0, sc("define_wro") > 0),
    link = list(sc("chan_surf_lin") > 0, sc("chan_aqu_lin") > 0),
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
      sc("path_water_ini") > 0, sc("hmet_hru_ini") > 0,
      sc("hmet_water_ini") > 0, sc("salt_hru_ini") > 0,
      sc("salt_water_ini") > 0),
    soils = list(
      !is_lte && sc("soils_sol") > 0,
      sc("nutrients_sol") > 0,
      is_lte && sc("soils_lte_sol") > 0),
    decision_table = list(
      "lum.dtl"     %in% dtl_files,   # lum.dtl
      "res_rel.dtl" %in% dtl_files,   # res_rel.dtl
      "scen_lu.dtl" %in% dtl_files,   # scen_lu.dtl
      "flo_con.dtl" %in% dtl_files),  # flo_con.dtl
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

# ===================================================================
# gwflow module support
# ===================================================================

#' Check if gwflow module is active
#' @param con Database connection.
#' @return Logical. TRUE if gwflow_base table exists and has data,
#'   AND project_config.use_gwflow is enabled.
#' @keywords internal
gwflow_exists <- function(con) {
  use_gw <- tryCatch({
    cfg <- query_db(con, "SELECT use_gwflow FROM project_config LIMIT 1")
    isTRUE(cfg$use_gwflow == 1)
  }, error = function(e) FALSE)
  
  if (!use_gw) return(FALSE)
  has_data(con, "gwflow_base")
}

#' Update codes_bsn gwflow flag
#'
#' Sets codes_bsn.gwflow = 1 if gwflow module is active, 0 otherwise.
#' Matches Python gwflow_writer.update_codes_bsn().
#' @param con Database connection.
#' @keywords internal
update_gwflow_codes_bsn <- function(con) {
  if (!has_data(con, "codes_bsn")) return(invisible(NULL))
  
  gw_flag <- if (gwflow_exists(con)) 1L else 0L
  tryCatch(
    execute_db(con, "UPDATE codes_bsn SET gwflow = ?", params = list(gw_flag)),
    error = function(e) invisible(NULL)
  )
}

#' Write all gwflow module files
#'
#' Writes the gwflow input files when gwflow_base exists. This mirrors
#' the Python \code{Gwflow_files.write()} method.
#'
#' @param con Database connection.
#' @param output_dir Output directory.
#' @param version Editor version string.
#' @param swat_version SWAT+ version string.
#' @keywords internal
write_gwflow_files <- function(con, output_dir, version = NULL,
                               swat_version = NULL) {
  write_gwflow_input(con, output_dir, version, swat_version)
  write_gwflow_chancells(con, output_dir, version, swat_version)
  write_gwflow_hrucell(con, output_dir, version, swat_version)
  write_gwflow_lsucell(con, output_dir, version, swat_version)
  write_gwflow_rescells(con, output_dir, version, swat_version)
  write_gwflow_floodplain(con, output_dir, version, swat_version)
  write_gwflow_wetland(con, output_dir, version, swat_version)
  write_gwflow_tiles(con, output_dir, version, swat_version)
  write_gwflow_solutes(con, output_dir, version, swat_version)
}

#' Build grid index mapping cell_id -> cell data
#' @keywords internal
gwflow_grid_index <- function(con) {
  base <- query_db(con, "SELECT * FROM gwflow_base LIMIT 1")
  total_cells <- base$row_count * base$col_count
  
  grid <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_grid ORDER BY cell_id"),
    error = function(e) data.frame()
  )
  
  # Build lookup: cell_id -> row (or NULL)
  grid_map <- list()
  if (nrow(grid) > 0) {
    for (i in seq_len(nrow(grid))) {
      grid_map[[as.character(grid$cell_id[i])]] <- grid[i, ]
    }
  }
  
  list(base = base, total_cells = total_cells, grid_map = grid_map)
}

#' Write gwflow.input file
#' @keywords internal
write_gwflow_input <- function(con, output_dir, version, swat_version) {
  if (!has_data(con, "gwflow_base")) return(invisible(NULL))
  
  gw <- gwflow_grid_index(con)
  base <- gw$base
  fp <- file.path(output_dir, "gwflow.input")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0(" INPUT FOR GWFLOW MODULE ",
                    swat_meta_line(fp, version, swat_version)), f)
  
  writeLines(" Basic information", f)
  writeLines(" structured", f)
  writeLines(paste0(" ", formatC(base$cell_size, format = "f", digits = 1, width = 12),
                    " cell size (m)"), f)
  writeLines(paste0(" ", base$row_count, " ", base$col_count,
                    " number of rows, number of columns"), f)
  writeLines(paste0(" ", formatC(base$boundary_conditions, format = "f", digits = 0, width = 12),
                    " boundary condition type (1=constant head; 2=no-flow)"), f)
  writeLines(paste0(" ", formatC(base$recharge, format = "f", digits = 0, width = 12),
                    " recharge connection type (1=HRU-cell; 2=LSU-cell)"), f)
  writeLines(paste0(" ", formatC(base$soil_transfer, format = "f", digits = 0, width = 12),
                    " groundwater-->soil transfer (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$saturation_excess, format = "f", digits = 0, width = 12),
                    " groundwater saturation excess flow (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$external_pumping, format = "f", digits = 0, width = 12),
                    " external groundwater pumping (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$tile_drainage, format = "f", digits = 0, width = 12),
                    " groundwater tile drainage (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$reservoir_exchange, format = "f", digits = 0, width = 12),
                    " groundwater-reservoir exchange (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$wetland_exchange, format = "f", digits = 0, width = 12),
                    " groundwater-wetland exchange (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$floodplain_exchange, format = "f", digits = 0, width = 12),
                    " groundwater-floodplain exchange (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$canal_seepage, format = "f", digits = 0, width = 12),
                    " canal seepage to groundwater (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$solute_transport, format = "f", digits = 0, width = 12),
                    " groundwater solute transport (0=off; 1=on)"), f)
  writeLines(paste0(" ", formatC(base$timestep_balance, format = "f", digits = 2, width = 12),
                    " time step (days)"), f)
  writeLines(paste0(" ", base$daily_output, " ", base$annual_output, " ", base$aa_output,
                    " write flags (daily, annual, avg. annual)"), f)
  writeLines(paste0(" ", formatC(1, format = "f", digits = 0, width = 12),
                    " number of columns in output files"), f)
  
  # Aquifer zones
  zones <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_zone ORDER BY zone_id"),
    error = function(e) data.frame()
  )
  zone_cnt <- nrow(zones)
  zone_id_index <- list()
  if (zone_cnt > 0) {
    for (i in seq_len(zone_cnt)) {
      zone_id_index[[as.character(zones$zone_id[i])]] <- i
    }
  }
  
  writeLines(" Aquifer and Streambed Parameter Zones", f)
  
  writeLines(" Aquifer Hydraulic Conductivity (m/day) Zones", f)
  writeLines(paste0(" ", zone_cnt), f)
  for (i in seq_len(zone_cnt)) {
    writeLines(paste0(i, "\t", format(zones$aquifer_k[i], nsmall = 4)), f)
  }
  
  writeLines(" Aquifer Specific Yield Zones", f)
  writeLines(paste0(" ", zone_cnt), f)
  for (i in seq_len(zone_cnt)) {
    writeLines(paste0(i, "\t", format(zones$specific_yield[i], nsmall = 4)), f)
  }
  
  writeLines(" Streambed Hydraulic Conductivity (m/day) Zones", f)
  writeLines(paste0(" ", zone_cnt), f)
  for (i in seq_len(zone_cnt)) {
    writeLines(paste0(i, "\t", format(zones$streambed_k[i], nsmall = 4)), f)
  }
  
  writeLines(" Streambed Thickness (m) Zones", f)
  writeLines(paste0(" ", zone_cnt), f)
  for (i in seq_len(zone_cnt)) {
    writeLines(paste0(i, "\t", format(zones$streambed_thickness[i], nsmall = 4)), f)
  }
  
  # Grid cell information
  writeLines(" Grid Cell Information", f)
  
  status_lines <- "Cell Status (0=inactive; 1=active; 2=boundary)\n"
  elevation_lines <- "Ground Surface Elevation (m)\n"
  thickness_lines <- "Aquifer Thickness(m)\n"
  zone_k_lines <- "Hydraulic conductivity zone\n"
  zone_yld_lines <- "Specific yield zone\n"
  recharge_lines <- "Recharge delay(Days)\n"
  et_lines <- "Groundwater ET Extinction Depth (m)\t\n"
  init_head_lines <- "Initial Groundwater Head (m)\n"
  
  col_count <- base$col_count
  col <- 1
  for (cell_id in seq_len(gw$total_cells)) {
    if (col == col_count + 1) {
      status_lines <- paste0(status_lines, "\n")
      elevation_lines <- paste0(elevation_lines, "\n")
      thickness_lines <- paste0(thickness_lines, "\n")
      zone_k_lines <- paste0(zone_k_lines, "\n")
      zone_yld_lines <- paste0(zone_yld_lines, "\n")
      recharge_lines <- paste0(recharge_lines, "\n")
      et_lines <- paste0(et_lines, "\n")
      init_head_lines <- paste0(init_head_lines, "\n")
      col <- 1
    }
    
    cell <- gw$grid_map[[as.character(cell_id)]]
    if (is.null(cell)) {
      status_lines <- paste0(status_lines, "0\t")
      elevation_lines <- paste0(elevation_lines, "0.00\t")
      thickness_lines <- paste0(thickness_lines, "0.00\t")
      zone_k_lines <- paste0(zone_k_lines, "0\t")
      zone_yld_lines <- paste0(zone_yld_lines, "0\t")
      recharge_lines <- paste0(recharge_lines,
                               formatC(base$recharge_delay, format = "f", digits = 2), "\t")
      et_lines <- paste0(et_lines, "0.00\t")
      init_head_lines <- paste0(init_head_lines, "0.00\t")
    } else {
      zone_idx <- zone_id_index[[as.character(cell$zone)]]
      if (is.null(zone_idx)) zone_idx <- 0L
      status_lines <- paste0(status_lines, cell$status, "\t")
      elevation_lines <- paste0(elevation_lines,
                                formatC(cell$elevation, format = "f", digits = 2), "\t")
      thickness_lines <- paste0(thickness_lines,
                                formatC(cell$aquifer_thickness, format = "f", digits = 2), "\t")
      zone_k_lines <- paste0(zone_k_lines, zone_idx, "\t")
      zone_yld_lines <- paste0(zone_yld_lines, zone_idx, "\t")
      recharge_lines <- paste0(recharge_lines,
                               formatC(base$recharge_delay, format = "f", digits = 2), "\t")
      et_lines <- paste0(et_lines,
                         formatC(cell$extinction_depth, format = "f", digits = 2), "\t")
      init_head_lines <- paste0(init_head_lines,
                                formatC(cell$initial_head, format = "f", digits = 2), "\t")
    }
    col <- col + 1
  }
  if (!endsWith(status_lines, "\n")) {
    status_lines <- paste0(status_lines, "\n")
    elevation_lines <- paste0(elevation_lines, "\n")
    thickness_lines <- paste0(thickness_lines, "\n")
    zone_k_lines <- paste0(zone_k_lines, "\n")
    zone_yld_lines <- paste0(zone_yld_lines, "\n")
    recharge_lines <- paste0(recharge_lines, "\n")
    et_lines <- paste0(et_lines, "\n")
    init_head_lines <- paste0(init_head_lines, "\n")
  }
  
  cat(status_lines, file = f)
  cat(elevation_lines, file = f)
  cat(thickness_lines, file = f)
  cat(zone_k_lines, file = f)
  cat(zone_yld_lines, file = f)
  cat(recharge_lines, file = f)
  cat(et_lines, file = f)
  cat(init_head_lines, file = f)
  
  # Output days
  writeLines(" Times for Groundwater Head Output", f)
  out_days <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_out_days ORDER BY year, jday"),
    error = function(e) data.frame()
  )
  writeLines(paste0("\t\t", nrow(out_days)), f)
  if (nrow(out_days) > 0) {
    for (i in seq_len(nrow(out_days))) {
      writeLines(paste0(swat_int_pad(out_days$year[i]), " ",
                        swat_int_pad(out_days$jday[i])), f)
    }
  }
  
  # Observation locations
  writeLines(" Groundwater Observation Locations", f)
  obs_locs <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_obs_locs"),
    error = function(e) data.frame()
  )
  writeLines(paste0("\t\t", nrow(obs_locs)), f)
  if (nrow(obs_locs) > 0) {
    for (i in seq_len(nrow(obs_locs))) {
      writeLines(as.character(obs_locs$cell_id[i]), f)
    }
  }
  
  # Daily output cell
  writeLines(" Cell for detailed daily sources/sink output", f)
  row_det <- base$daily_output_row
  col_det <- base$daily_output_col
  cell_det <- 0L
  if (!is.null(row_det) && !is.null(col_det) && row_det > 0 && col_det > 0) {
    cell_det <- (row_det - 1L) * col_count + col_det
  }
  writeLines(paste0(" ", cell_det), f)
  
  # River cell information
  writeLines(" River Cell Information", f)
  writeLines(paste0(" ", formatC(base$river_depth, format = "f", digits = 2)), f)
}

#' Write gwflow.chancells file
#' @keywords internal
write_gwflow_chancells <- function(con, output_dir, version, swat_version) {
  if (!has_data(con, "gwflow_rivcell")) return(invisible(NULL))
  
  fp <- file.path(output_dir, "gwflow.chancells")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0(" Cell-Channel Connection Information ",
                    swat_meta_line(fp, version, swat_version)), f)
  writeLines("", f)
  
  # Header
  writeLines(paste0(
    swat_int_pad("ID"),
    swat_num_pad("elev_m"),
    swat_int_pad("channel"),
    swat_num_pad("riv_length_m"),
    swat_int_pad("zone")
  ), f)
  writeLines("", f)
  
  # Build channel GIS-to-con index
  chan_idx <- gwflow_gis_to_con_index(con, "chandeg_con")
  
  cells <- tryCatch(
    query_db(con,
             "SELECT r.cell_id, g.elevation, r.channel, r.length_m, g.zone
       FROM gwflow_rivcell r
       JOIN gwflow_grid g ON r.cell_id = g.cell_id
       ORDER BY r.cell_id"),
    error = function(e) data.frame()
  )
  for (i in seq_len(nrow(cells))) {
    row <- cells[i, ]
    chan_con <- chan_idx[[as.character(row$channel)]]
    if (is.null(chan_con)) chan_con <- 0L
    writeLines(paste0(
      swat_int_pad(row$cell_id),
      swat_num_pad(row$elevation, decimals = 2),
      swat_int_pad(chan_con),
      swat_num_pad(row$length_m, decimals = 2),
      swat_int_pad(row$zone)
    ), f)
  }
}

#' Write gwflow.hrucell and gwflow.cellhru files
#' @keywords internal
write_gwflow_hrucell <- function(con, output_dir, version, swat_version) {
  if (!has_data(con, "gwflow_hrucell")) return(invisible(NULL))
  
  base <- query_db(con, "SELECT * FROM gwflow_base LIMIT 1")
  recharge_type <- base$recharge
  if (!(recharge_type %in% c(1, 3))) return(invisible(NULL))
  
  hru_idx <- gwflow_gis_to_con_index(con, "hru_con")
  
  cells <- tryCatch(
    query_db(con,
             "SELECT h.cell_id, h.hru, h.area_m2,
              g.elevation, COALESCE(gis.arslp, 0) as arslp
       FROM gwflow_hrucell h
       JOIN gwflow_grid g ON h.cell_id = g.cell_id
       LEFT JOIN gis_hrus gis ON h.hru = gis.id
       ORDER BY h.hru, h.cell_id"),
    error = function(e) data.frame()
  )
  if (nrow(cells) == 0) return(invisible(NULL))
  
  # gwflow.hrucell
  fp1 <- file.path(output_dir, "gwflow.hrucell")
  f1 <- file(fp1, "w")
  on.exit(close(f1), add = TRUE)
  
  writeLines(paste0(" HRU-Cell Connection Information ",
                    swat_meta_line(fp1, version, swat_version)), f1)
  writeLines("", f1)
  writeLines(" HRUs that are connected to cells", f1)
  
  hru_ids <- sort(unique(cells$hru))
  writeLines(as.character(length(hru_ids)), f1)
  for (hid in hru_ids) {
    hcon <- hru_idx[[as.character(hid)]]
    if (is.null(hcon)) hcon <- 0L
    writeLines(as.character(hcon), f1)
  }
  writeLines("", f1)
  
  writeLines(paste0(
    swat_int_pad("hru"),
    swat_num_pad("area_m2"),
    swat_int_pad("cell_id"),
    swat_num_pad("overlap_m2")
  ), f1)
  writeLines("", f1)
  
  for (i in seq_len(nrow(cells))) {
    row <- cells[i, ]
    hcon <- hru_idx[[as.character(row$hru)]]
    if (is.null(hcon)) hcon <- 0L
    writeLines(paste0(
      swat_int_pad(hcon),
      swat_num_pad(row$arslp * 10000, decimals = 2),
      swat_int_pad(row$cell_id),
      swat_num_pad(row$area_m2, decimals = 2)
    ), f1)
  }
  
  # gwflow.cellhru
  fp2 <- file.path(output_dir, "gwflow.cellhru")
  f2 <- file(fp2, "w")
  on.exit(close(f2), add = TRUE)
  
  writeLines(paste0(" Cell-HRU Connection Information ",
                    swat_meta_line(fp2, version, swat_version)), f2)
  writeLines("", f2)
  
  num_intersect <- length(unique(cells$cell_id))
  writeLines(paste0(num_intersect, "\t\t\tNumber of cells that intersect HRUs"), f2)
  
  writeLines(paste0(
    swat_int_pad("cell_id"),
    swat_int_pad("hru"),
    swat_num_pad("cell_area"),
    swat_num_pad("overlap_m2")
  ), f2)
  writeLines("", f2)
  
  cell_area <- base$cell_size * base$cell_size
  cells_by_cell <- cells[order(cells$cell_id), ]
  for (i in seq_len(nrow(cells_by_cell))) {
    row <- cells_by_cell[i, ]
    hcon <- hru_idx[[as.character(row$hru)]]
    if (is.null(hcon)) hcon <- 0L
    writeLines(paste0(
      swat_int_pad(row$cell_id),
      swat_int_pad(hcon),
      swat_num_pad(cell_area, decimals = 2),
      swat_num_pad(row$area_m2, decimals = 2)
    ), f2)
  }
}

#' Write gwflow.lsucell file
#' @keywords internal
write_gwflow_lsucell <- function(con, output_dir, version, swat_version) {
  if (!has_data(con, "gwflow_lsucell")) return(invisible(NULL))
  
  base <- query_db(con, "SELECT * FROM gwflow_base LIMIT 1")
  if (!(base$recharge %in% c(2, 3))) return(invisible(NULL))
  
  lsu_idx <- gwflow_gis_to_con_index(con, "rout_unit_con")
  
  cells <- tryCatch(
    query_db(con,
             "SELECT l.cell_id, l.lsu, l.area_m2,
              COALESCE(gis.area, 0) as lsu_area
       FROM gwflow_lsucell l
       LEFT JOIN gis_lsus gis ON l.lsu = gis.id
       ORDER BY l.cell_id"),
    error = function(e) data.frame()
  )
  if (nrow(cells) == 0) return(invisible(NULL))
  
  fp <- file.path(output_dir, "gwflow.lsucell")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0(" LSU (landscape unit) - Cell Connection Information ",
                    swat_meta_line(fp, version, swat_version)), f)
  
  total_lsu <- safe_count(con, "gis_lsus")
  unique_lsu <- sort(unique(cells$lsu))
  writeLines(paste0(total_lsu, "\t\t total number of landscape units in model"), f)
  writeLines(paste0(length(unique_lsu),
                    "\t\t number of landscape units connected to grid cells"), f)
  for (lid in unique_lsu) {
    lcon <- lsu_idx[[as.character(lid)]]
    if (is.null(lcon)) lcon <- 0L
    writeLines(as.character(lcon), f)
  }
  writeLines("", f)
  
  writeLines("connection information between landscape units and grid cells", f)
  writeLines(paste0(
    swat_int_pad("lsu_id"),
    swat_num_pad("lsu_area_m2"),
    swat_int_pad("cell_id"),
    swat_num_pad("area_m2")
  ), f)
  writeLines("", f)
  
  for (i in seq_len(nrow(cells))) {
    row <- cells[i, ]
    lcon <- lsu_idx[[as.character(row$lsu)]]
    if (is.null(lcon)) lcon <- 0L
    writeLines(paste0(
      swat_int_pad(lcon),
      swat_num_pad(row$lsu_area * 10000, decimals = 2),
      swat_int_pad(row$cell_id),
      swat_num_pad(row$area_m2, decimals = 2)
    ), f)
  }
}

#' Write gwflow.rescells file
#' @keywords internal
write_gwflow_rescells <- function(con, output_dir, version, swat_version) {
  if (!has_data(con, "gwflow_rescell")) return(invisible(NULL))
  
  base <- query_db(con, "SELECT * FROM gwflow_base LIMIT 1")
  if (!isTRUE(base$reservoir_exchange == 1)) return(invisible(NULL))
  
  res_idx <- gwflow_gis_to_con_index(con, "reservoir_con")
  
  cells <- tryCatch(
    query_db(con,
             "SELECT r.cell_id, r.res_id, r.res_stage
       FROM gwflow_rescell r
       JOIN gwflow_grid g ON r.cell_id = g.cell_id
       ORDER BY r.cell_id"),
    error = function(e) data.frame()
  )
  if (nrow(cells) == 0) return(invisible(NULL))
  
  fp <- file.path(output_dir, "gwflow.rescells")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0(" Cell-Reservoir Connection Information ",
                    swat_meta_line(fp, version, swat_version)), f)
  writeLines(" Reservoir bed parameters", f)
  writeLines(paste0(" ", base$resbed_thickness, "\t\t bed thickness (m)"), f)
  writeLines(paste0(" ", base$resbed_k, "\t\t bed conductivity (m/day)"), f)
  writeLines(paste0(" ", nrow(cells),
                    "\t\t number of cells connected to reservoirs"), f)
  
  writeLines(paste0(
    swat_int_pad("cell_id"),
    swat_int_pad("res_id"),
    swat_num_pad("res_stage_m")
  ), f)
  writeLines("", f)
  
  for (i in seq_len(nrow(cells))) {
    row <- cells[i, ]
    rcon <- res_idx[[as.character(row$res_id)]]
    if (is.null(rcon)) rcon <- 0L
    writeLines(paste0(
      swat_int_pad(row$cell_id),
      swat_int_pad(rcon),
      swat_num_pad(row$res_stage, decimals = 2)
    ), f)
  }
}

#' Write gwflow.floodplain file
#' @keywords internal
write_gwflow_floodplain <- function(con, output_dir, version, swat_version) {
  if (!has_data(con, "gwflow_fpcell")) return(invisible(NULL))
  
  base <- query_db(con, "SELECT * FROM gwflow_base LIMIT 1")
  if (!isTRUE(base$floodplain_exchange == 1)) return(invisible(NULL))
  
  chan_idx <- gwflow_gis_to_con_index(con, "chandeg_con")
  
  cells <- tryCatch(
    query_db(con,
             "SELECT f.cell_id, f.channel_id, f.area_m2
       FROM gwflow_fpcell f
       JOIN gwflow_grid g ON f.cell_id = g.cell_id
       ORDER BY f.cell_id"),
    error = function(e) data.frame()
  )
  if (nrow(cells) == 0) return(invisible(NULL))
  
  fp <- file.path(output_dir, "gwflow.floodplain")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0(
    "gwflow floodplain cells (optional file; list cells that interact with channels, ",
    "when channel water is in the floodplain) ",
    swat_meta_line(fp, version, swat_version)), f)
  
  # Filter to valid channel connections
  valid_lines <- character(0)
  for (i in seq_len(nrow(cells))) {
    row <- cells[i, ]
    ccon <- chan_idx[[as.character(row$channel_id)]]
    if (is.null(ccon)) next
    valid_lines <- c(valid_lines, paste0(
      swat_int_pad(row$cell_id),
      swat_int_pad(ccon),
      swat_num_pad(0, decimals = 4),
      swat_num_pad(row$area_m2, decimals = 2)
    ))
  }
  
  writeLines(paste0(length(valid_lines),
                    "\t\t\t\t\tNumber of floodplain cells"), f)
  writeLines(paste0(
    swat_int_pad("cell_id"),
    swat_int_pad("chan_id"),
    swat_num_pad("fp_K"),
    swat_num_pad("area_m2")
  ), f)
  writeLines("", f)
  for (line in valid_lines) writeLines(line, f)
}

#' Write gwflow.wetland file
#' @keywords internal
write_gwflow_wetland <- function(con, output_dir, version, swat_version) {
  base <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_base LIMIT 1"),
    error = function(e) data.frame()
  )
  if (nrow(base) == 0 || !isTRUE(base$wetland_exchange == 1))
    return(invisible(NULL))
  
  fp <- file.path(output_dir, "gwflow.wetland")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0("gwflow.wetland: parameters for groundwater-wetland interactions ",
                    swat_meta_line(fp, version, swat_version)), f)
  writeLines("wet_id = same as id listed in wetland.wet", f)
  writeLines("thick_m = thickness (in meters) of wetland bottom material", f)
  writeLines("(hydraulic conductivity is listed in hydrology.wet)", f)
  
  writeLines(paste0(swat_int_pad("wet_id"), swat_num_pad("thick_m")), f)
  writeLines("", f)
  
  wet_thick <- list()
  if (has_data(con, "gwflow_wetland")) {
    wt <- query_db(con, "SELECT wet_id, thickness FROM gwflow_wetland ORDER BY wet_id")
    for (i in seq_len(nrow(wt))) {
      wet_thick[[as.character(wt$wet_id[i])]] <- wt$thickness[i]
    }
  }
  
  if (has_data(con, "wetland_wet")) {
    wets <- query_db(con, "SELECT id FROM wetland_wet ORDER BY id")
    default_thick <- base$wet_thickness
    for (i in seq_len(nrow(wets))) {
      wid <- wets$id[i]
      thick <- wet_thick[[as.character(wid)]]
      if (is.null(thick)) thick <- default_thick
      writeLines(paste0(
        swat_int_pad(wid),
        swat_num_pad(thick, decimals = 2)
      ), f)
    }
  }
}

#' Write gwflow.tiles file
#' @keywords internal
write_gwflow_tiles <- function(con, output_dir, version, swat_version) {
  base <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_base LIMIT 1"),
    error = function(e) data.frame()
  )
  if (nrow(base) == 0 || !isTRUE(base$tile_drainage == 1))
    return(invisible(NULL))
  
  gw <- gwflow_grid_index(con)
  fp <- file.path(output_dir, "gwflow.tiles")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0("gwflow tile drain information ",
                    swat_meta_line(fp, version, swat_version)), f)
  writeLines(paste0(" ", swat_num_pad(base$tile_depth, decimals = 5),
                    "\t\t Depth (m) of tiles below ground surface"), f)
  writeLines(paste0(" ", swat_num_pad(base$tile_area, decimals = 5),
                    "\t\t Area (m2) of groundwater inflow * flow length"), f)
  writeLines(paste0(" ", swat_num_pad(base$tile_k, decimals = 5),
                    "\t\t Hydraulic conductivity (m/day) of the drain perimeter"), f)
  tile_groups <- if (!is.null(base$tile_groups)) base$tile_groups else 0L
  writeLines(paste0(" ", swat_int_pad(tile_groups, pad = SWAT_NUM_PAD),
                    "\t\t Tile cell groups (flag: 0=no; 1=yes)"), f)
  
  status_lines <- "gwflow tile cells (0=no tile; 1=tiles are present)\n"
  col_count <- base$col_count
  col <- 1
  for (cell_id in seq_len(gw$total_cells)) {
    if (col == col_count + 1) {
      status_lines <- paste0(status_lines, "\n")
      col <- 1
    }
    cell <- gw$grid_map[[as.character(cell_id)]]
    tile_val <- if (is.null(cell) || is.null(cell$tile)) 0L else cell$tile
    status_lines <- paste0(status_lines, tile_val, "\t")
    col <- col + 1
  }
  if (!endsWith(status_lines, "\n")) {
    status_lines <- paste0(status_lines, "\n")
  }
  cat(status_lines, file = f)
}

#' Write gwflow.solutes file
#' @keywords internal
write_gwflow_solutes <- function(con, output_dir, version, swat_version) {
  base <- tryCatch(
    query_db(con, "SELECT * FROM gwflow_base LIMIT 1"),
    error = function(e) data.frame()
  )
  if (nrow(base) == 0 || !isTRUE(base$solute_transport == 1))
    return(invisible(NULL))
  
  if (!has_data(con, "gwflow_solutes")) return(invisible(NULL))
  
  fp <- file.path(output_dir, "gwflow.solutes")
  f <- file(fp, "w")
  on.exit(close(f))
  
  writeLines(paste0("solute parameters and initial concentrations ",
                    swat_meta_line(fp, version, swat_version)), f)
  writeLines("general parameters", f)
  writeLines(paste0(" ", base$transport_steps,
                    "\t\t number of transport time steps for flow time step"), f)
  writeLines(paste0(" ", base$disp_coef,
                    "\t\t dispersion coefficient (m2/day)"), f)
  
  writeLines("solute parameters: name,sorption,rate constant,canal_irrig (one row per active solute)", f)
  
  solutes <- query_db(con, "SELECT * FROM gwflow_solutes")
  for (i in seq_len(nrow(solutes))) {
    sol <- solutes[i, ]
    writeLines(paste0(
      swat_string_pad(sol$solute_name, pad = 8, align = "left"),
      swat_num_pad(sol$sorption, decimals = 2),
      swat_num_pad(sol$rate_const, decimals = 4),
      swat_num_pad(sol$canal_irr, decimals = 2)
    ), f)
  }
  
  writeLines("initial concentrations (g/m3)", f)
  for (i in seq_len(nrow(solutes))) {
    sol <- solutes[i, ]
    writeLines(sol$solute_name, f)
    writeLines(sol$init_data, f)
    if (identical(sol$init_data, "single")) {
      writeLines(formatC(sol$init_conc, format = "f", digits = 2), f)
    } else {
      gw <- gwflow_grid_index(con)
      solute_col <- paste0("init_",
                           if (sol$solute_name == "no3-n") "no3" else sol$solute_name)
      grid_text <- gwflow_write_grid_column(con, gw, solute_col)
      cat(grid_text, file = f)
    }
  }
}

#' Build GIS ID to connection index mapping
#' @keywords internal
gwflow_gis_to_con_index <- function(con, con_table) {
  idx <- list()
  rows <- tryCatch(
    query_db(con, paste0("SELECT id, gis_id FROM ", con_table, " ORDER BY id")),
    error = function(e) data.frame()
  )
  if (nrow(rows) > 0) {
    for (i in seq_len(nrow(rows))) {
      idx[[as.character(rows$gis_id[i])]] <- i
    }
  }
  idx
}

#' Write a grid column for gwflow_init_conc
#' @keywords internal
gwflow_write_grid_column <- function(con, gw, column_name) {
  conc_data <- tryCatch(
    query_db(con, paste0("SELECT cell_id, ", column_name,
                         " FROM gwflow_init_conc ORDER BY cell_id")),
    error = function(e) data.frame()
  )
  
  conc_map <- list()
  if (nrow(conc_data) > 0) {
    for (i in seq_len(nrow(conc_data))) {
      conc_map[[as.character(conc_data$cell_id[i])]] <- conc_data[[column_name]][i]
    }
  }
  
  result <- ""
  base <- gw$base
  col <- 1
  for (cell_id in seq_len(gw$total_cells)) {
    if (col == base$col_count + 1) {
      result <- paste0(result, "\n")
      col <- 1
    }
    val <- conc_map[[as.character(cell_id)]]
    if (is.null(val)) val <- 0
    result <- paste0(result, formatC(val, format = "f", digits = 2), "\t")
    col <- col + 1
  }
  if (!endsWith(result, "\n")) result <- paste0(result, "\n")
  result
}
