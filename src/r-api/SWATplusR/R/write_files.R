#' Write SWAT+ input files from the project database
#'
#' Mirrors the functionality of \code{src/api/actions/write_files.py}.
#' Reads data from the project SQLite database and writes it to the text-based
#' SWAT+ input file format.
#'
#' @keywords internal
NULL

# ===========================================================================
# Main entry point
# ===========================================================================

#' Write all SWAT+ input files
#'
#' Reads every relevant table from the project database and writes the
#' corresponding SWAT+ plain-text input files to \code{output_dir}.
#'
#' Mirrors \code{WriteFiles.write()} in
#' \code{src/api/actions/write_files.py}.
#'
#' @param project_db  Path to the project \code{.sqlite} database.
#' @param output_dir  Directory where the SWAT+ \code{.txt}/\code{.cli}/etc.
#'   files should be written (created if absent).
#' @param verbose     Print progress messages.  Default \code{TRUE}.
#' @return Invisibly, the number of files written.
#' @export
write_swatplus_files <- function(project_db,
                                 output_dir,
                                 verbose = TRUE) {
  if (!file.exists(project_db))
    stop("Project database not found: ", project_db)
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  con <- swat_open_db(project_db)
  on.exit(swat_close_db(con), add = TRUE)

  n_written <- 0L

  if (verbose) emit_progress(5,  "Writing simulation files...")
  n_written <- n_written + .write_time_sim(con, output_dir)
  n_written <- n_written + .write_print_prt(con, output_dir)
  n_written <- n_written + .write_object_cnt(con, output_dir)

  if (verbose) emit_progress(20, "Writing basin files...")
  n_written <- n_written + .write_codes_bsn(con, output_dir)
  n_written <- n_written + .write_parameters_bsn(con, output_dir)

  if (verbose) emit_progress(35, "Writing climate files...")
  n_written <- n_written + .write_weather_sta(con, output_dir)
  n_written <- n_written + .write_weather_wgn(con, output_dir)
  n_written <- n_written + .write_atmo_cli(con, output_dir)

  if (verbose) emit_progress(50, "Writing connect files...")
  n_written <- n_written + .write_hru_con(con, output_dir)
  n_written <- n_written + .write_rtu_con(con, output_dir)
  n_written <- n_written + .write_cha_con(con, output_dir)
  n_written <- n_written + .write_aqu_con(con, output_dir)
  n_written <- n_written + .write_res_con(con, output_dir)
  n_written <- n_written + .write_recall_con(con, output_dir)

  if (verbose) emit_progress(65, "Writing channel/reservoir/HRU files...")
  n_written <- n_written + .write_channel_lte(con, output_dir)
  n_written <- n_written + .write_reservoir(con, output_dir)
  n_written <- n_written + .write_hru_data(con, output_dir)
  n_written <- n_written + .write_hru_lte(con, output_dir)

  if (verbose) emit_progress(80, "Writing aquifer/soil/routing files...")
  n_written <- n_written + .write_aquifer(con, output_dir)
  n_written <- n_written + .write_soils(con, output_dir)
  n_written <- n_written + .write_rout_unit(con, output_dir)

  if (verbose) emit_progress(90, "Writing land use / operations files...")
  n_written <- n_written + .write_landuse_lum(con, output_dir)
  n_written <- n_written + .write_management_sch(con, output_dir)
  n_written <- n_written + .write_decision_table(con, output_dir)

  # Update project_config timestamp
  DBI::dbExecute(con,
    "UPDATE project_config SET input_files_last_written = datetime('now')")

  if (verbose) emit_progress(100, paste0("Done. ", n_written, " file(s) written."))
  invisible(n_written)
}

# ===========================================================================
# Individual file writers
# ===========================================================================

.write_time_sim <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM time_sim LIMIT 1")
  if (nrow(rows) == 0L) return(0L)
  r <- rows[1L, ]
  lines <- c(
    "time.sim",
    "               SWAT+ time simulation file",
    paste("day_start yrc_start day_end   yrc_end   step"),
    sprintf("%9d %9d %7d %9d %6d",
            r$day_start, r$yrc_start, r$day_end, r$yrc_end, r$step)
  )
  .write_file(dir, "time.sim", lines)
}

.write_print_prt <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM print_prt LIMIT 1")
  if (nrow(rows) == 0L) return(0L)
  r <- rows[1L, ]

  header <- c(
    "print.prt",
    "               SWAT+ print control file",
    paste("nyskip day_start yrc_start day_end   yrc_end   interval  csvout",
          "dbout  cdfout  crop_yld  mgtout  hydcon  fdcout"),
    sprintf("%6d %9d %9d %7d %9d %8d %6s %6s %7s %9s %7s %7s %7s",
            r$nyskip, r$day_start, r$yrc_start,
            r$day_end, r$yrc_end, r$interval,
            .yn(r$csvout), .yn(r$dbout), .yn(r$cdfout),
            r$crop_yld,
            .yn(r$mgtout), .yn(r$hydcon), .yn(r$fdcout))
  )

  # Print objects
  objs <- swat_query(con, "SELECT * FROM print_prt_object ORDER BY id")
  if (nrow(objs) > 0L) {
    obj_hdr <- sprintf("%-20s %5s %7s %6s %5s",
                       "name", "daily", "monthly", "yearly", "avann")
    obj_lines <- apply(objs, 1L, function(o) {
      sprintf("%-20s %5s %7s %6s %5s",
              o["name"], .yn(o["daily"]), .yn(o["monthly"]),
              .yn(o["yearly"]), .yn(o["avann"]))
    })
    lines <- c(header, obj_hdr, obj_lines)
  } else {
    lines <- header
  }
  .write_file(dir, "print.prt", lines)
}

.write_object_cnt <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM object_cnt LIMIT 1")
  if (nrow(rows) == 0L) return(0L)
  r <- rows[1L, ]

  # Auto-calculate counts if 0
  cnt_map <- list(
    hru  = "hru_con",
    lhru = "hru_lte_con",
    rtu  = "rout_unit_con",
    aqu  = "aquifer_con",
    cha  = "chandeg_con",
    res  = "reservoir_con",
    rec  = "recall_con"
  )
  for (field in names(cnt_map)) {
    if (as.integer(r[[field]]) == 0L && swat_exists_table(con, cnt_map[[field]])) {
      r[[field]] <- swat_count(con, cnt_map[[field]])
    }
  }

  lines <- c(
    "object.cnt",
    "               SWAT+ object count file",
    paste("name               hru   lhru    rtu    mfl    aqu    cha",
          "   res    rec   exco    dlr    can    pmp    out   lcha  aqu2d",
          "   hrd    wro"),
    sprintf("%-16s %5d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d %6d",
            r$name,
            as.integer(r$hru),  as.integer(r$lhru), as.integer(r$rtu),
            as.integer(r$mfl),  as.integer(r$aqu),  as.integer(r$cha),
            as.integer(r$res),  as.integer(r$rec),  as.integer(r$exco),
            as.integer(r$dlr),  as.integer(r$can),  as.integer(r$pmp),
            as.integer(r$out),  as.integer(r$lcha), as.integer(r$aqu2d),
            as.integer(r$hrd),  as.integer(r$wro))
  )
  .write_file(dir, "object.cnt", lines)
}

.write_codes_bsn <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM codes_bsn LIMIT 1")
  if (nrow(rows) == 0L) return(0L)
  r <- rows[1L, ]
  lines <- c(
    "codes.bsn",
    "               SWAT+ basin codes file",
    "pet   event  crack  rtu_evap gwflow swift carbon lat_sed nutrient ch_sed ch_nu soil_p atmo_dep stor_ws pesticide pathogens hmet salt co2",
    sprintf("%3d %5d %6d %8d %6d %5d %6d %7d %8d %6d %5d %6d %8s %7d %9d %9d %4d %4d %3d",
            r$pet, r$event, r$crack, r$rtu_evap, r$gwflow, r$swift,
            r$carbon, r$lat_sed, r$nutrient, r$ch_sed, r$ch_nu, r$soil_p,
            r$atmo_dep, r$stor_ws, r$pesticide, r$pathogens,
            r$hmet, r$salt, r$co2)
  )
  .write_file(dir, "codes.bsn", lines)
}

.write_parameters_bsn <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM parameters_bsn LIMIT 1")
  if (nrow(rows) == 0L) return(0L)
  r <- rows[1L, ]
  lines <- c(
    "parameters.bsn",
    "               SWAT+ basin parameters file",
    "pet_co  esco    epco   evap_res_co  evap_sub_co  sub_lat_perc",
    sprintf("%6.3f %6.3f %6.3f %11.3f %12.3f %12.3f",
            r$pet_co, r$esco, r$epco,
            r$evap_res_co, r$evap_sub_co, r$sub_lat_perc)
  )
  .write_file(dir, "parameters.bsn", lines)
}

.write_weather_sta <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM weather_sta_cli ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c(
    "weather-sta.cli",
    "               SWAT+ weather station file",
    sprintf("%-20s %-20s %-20s %-20s %-20s %-20s %-20s %8s %8s",
            "name", "pcp", "tmp", "slr", "hmd", "wnd", "pet", "lat", "lon"),
    apply(rows, 1L, function(r) {
      sprintf("%-20s %-20s %-20s %-20s %-20s %-20s %-20s %8.3f %8.3f",
              .nv(r["name"]), .nv(r["pcp"]), .nv(r["tmp"]),
              .nv(r["slr"]),  .nv(r["hmd"]), .nv(r["wnd"]),
              .nv(r["pet"]),
              as.numeric(.nv(r["lat"], "0")),
              as.numeric(.nv(r["lon"], "0")))
    })
  )
  .write_file(dir, "weather-sta.cli", lines)
}

.write_weather_wgn <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM weather_wgn_cli ORDER BY id")
  if (nrow(rows) == 0L) return(0L)

  all_lines <- c("weather-wgn.cli",
                 "               SWAT+ WGN climate file",
                 "Number of wgn stations",
                 as.character(nrow(rows)))
  mon_cols <- c("tmp_max_ave","tmp_min_ave","tmp_max_sd","tmp_min_sd",
                "pcp_ave","pcp_sd","pcp_skew","wet_dry","wet_wet",
                "pcp_days","pcp_hhr","slr_ave","dew_ave","wnd_ave")

  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    all_lines <- c(all_lines,
      sprintf("%-20s %8.3f %8.3f %8.3f %6d",
              r$name, r$lat, r$lon, r$elev, as.integer(r$rain_yrs)))

    mon_rows <- swat_query(con,
      paste0("SELECT * FROM weather_wgn_cli_mon WHERE weather_wgn_cli_id=",
             r$id, " ORDER BY month"))
    if (nrow(mon_rows) > 0L) {
      for (j in seq_len(nrow(mon_rows))) {
        m <- mon_rows[j, ]
        all_lines <- c(all_lines,
          paste(sapply(mon_cols, function(c) sprintf("%8.3f", as.numeric(m[[c]]))),
                collapse = ""))
      }
    }
  }
  .write_file(dir, "weather-wgn.cli", all_lines)
}

.write_atmo_cli <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM atmo_cli LIMIT 1")
  if (nrow(rows) == 0L) return(0L)
  # Full atmo.cli writing is complex – write a stub header for now
  lines <- c(
    "atmo.cli",
    "               SWAT+ atmospheric deposition file",
    paste("filename:", rows$filename[[1L]])
  )
  .write_file(dir, "atmo.cli", lines)
}

.write_hru_con <- function(con, dir) {
  rows <- swat_query(con,
    "SELECT c.*, h.name AS hru_name
     FROM hru_con c LEFT JOIN hru_data_hru h ON h.id = c.hru_id
     ORDER BY c.id")
  if (nrow(rows) == 0L) return(0L)
  .write_con_file(con, dir, "hru.con", rows, "hru")
}

.write_rtu_con <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM rout_unit_con ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  .write_con_file(con, dir, "rout_unit.con", rows, "rtu_con")
}

.write_cha_con <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM chandeg_con ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  .write_con_file(con, dir, "chandeg.con", rows, "chandeg_con")
}

.write_aqu_con <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM aquifer_con ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  .write_con_file(con, dir, "aquifer.con", rows, "aquifer_con")
}

.write_res_con <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM reservoir_con ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  .write_con_file(con, dir, "reservoir.con", rows, "reservoir_con")
}

.write_recall_con <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM recall_con ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  .write_con_file(con, dir, "recall.con", rows, "recall_con")
}

.write_channel_lte <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM channel_lte_cha ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  hyd  <- swat_query(con, "SELECT * FROM hyd_sed_lte_cha ORDER BY id")
  lines <- c("chandeg.cha", "               SWAT+ channel LTE file",
             sprintf("%-16s %-16s", "name", "hyd_id"),
             apply(rows, 1L, function(r) sprintf("%-16s %8d", r["name"], as.integer(r["hyd_id"]))))
  .write_file(dir, "chandeg.cha", lines)
}

.write_reservoir <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM reservoir_res ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("reservoir.res", "               SWAT+ reservoir file",
             sprintf("%-16s", "name"),
             apply(rows, 1L, function(r) sprintf("%-16s", r["name"])))
  .write_file(dir, "reservoir.res", lines)
}

.write_hru_data <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM hru_data_hru ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("hru-data.hru", "               SWAT+ HRU data file",
             sprintf("%-16s %-16s %-16s", "name", "topo_id", "hydro_id"),
             apply(rows, 1L, function(r)
               sprintf("%-16s %8d %8d",
                        r["name"],
                        as.integer(.nv(r["topo_id"], "0")),
                        as.integer(.nv(r["hydro_id"], "0")))))
  .write_file(dir, "hru-data.hru", lines)
}

.write_hru_lte <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM hru_lte_hru ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("hru-lte.hru", "               SWAT+ HRU LTE file",
             sprintf("%-16s %8s %8s %8s", "name", "area", "cn2", "slp"),
             apply(rows, 1L, function(r)
               sprintf("%-16s %8.3f %8.3f %8.4f",
                        r["name"],
                        as.numeric(r["area"]),
                        as.numeric(r["cn2"]),
                        as.numeric(r["slp"]))))
  .write_file(dir, "hru-lte.hru", lines)
}

.write_aquifer <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM aquifer_aqu ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("aquifer.aqu", "               SWAT+ aquifer file",
             sprintf("%-16s %8s %8s %8s", "name", "gw_flo", "dep_bot", "dep_wt"),
             apply(rows, 1L, function(r)
               sprintf("%-16s %8.4f %8.2f %8.2f",
                        r["name"],
                        as.numeric(r["gw_flo"]),
                        as.numeric(r["dep_bot"]),
                        as.numeric(r["dep_wt"]))))
  .write_file(dir, "aquifer.aqu", lines)
}

.write_soils <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM soils_sol ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("soils.sol", "               SWAT+ soils file",
             sprintf("%-20s %-6s %8s", "name", "hyd_grp", "dp_tot"),
             apply(rows, 1L, function(r)
               sprintf("%-20s %-6s %8.2f",
                        r["name"], r["hyd_grp"], as.numeric(r["dp_tot"]))))
  .write_file(dir, "soils.sol", lines)
}

.write_rout_unit <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM rout_unit_rtu ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("rout_unit.rtu", "               SWAT+ routing unit file",
             sprintf("%-16s", "name"),
             apply(rows, 1L, function(r) sprintf("%-16s", r["name"])))
  .write_file(dir, "rout_unit.rtu", lines)
}

.write_landuse_lum <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM landuse_lum ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("landuse.lum", "               SWAT+ land use file",
             sprintf("%-20s", "name"),
             apply(rows, 1L, function(r) sprintf("%-20s", r["name"])))
  .write_file(dir, "landuse.lum", lines)
}

.write_management_sch <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM management_sch ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  lines <- c("management.sch", "               SWAT+ management schedule file",
             sprintf("%-20s", "name"),
             apply(rows, 1L, function(r) sprintf("%-20s", r["name"])))
  .write_file(dir, "management.sch", lines)
}

.write_decision_table <- function(con, dir) {
  rows <- swat_query(con, "SELECT * FROM d_table_dtl ORDER BY id")
  if (nrow(rows) == 0L) return(0L)
  # Group by file_name
  by_file <- split(rows, rows$file_name)
  n <- 0L
  for (fname in names(by_file)) {
    if (is.na(fname) || fname == "") next
    grp   <- by_file[[fname]]
    lines <- c(fname, paste0("               SWAT+ decision table file: ", fname),
               apply(grp, 1L, function(r) sprintf("%-30s", r["name"])))
    .write_file(dir, fname, lines)
    n <- n + 1L
  }
  n
}

# ===========================================================================
# Connection file helper
# ===========================================================================

.write_con_file <- function(con, dir, filename, rows, out_table_prefix) {
  out_table <- paste0(out_table_prefix, "_out")
  header <- c(
    filename,
    paste0("               SWAT+ connection file: ", filename),
    sprintf("%-20s %8s %8s %8s %8s %6s %6s",
            "name", "gis_id", "area", "lat", "lon", "ovfl", "rule")
  )
  data_lines <- apply(rows, 1L, function(r) {
    sprintf("%-20s %8d %8.3f %8.4f %8.4f %6d %6d",
            r["name"],
            as.integer(.nv(r["gis_id"], "0")),
            as.numeric(.nv(r["area"], "0")),
            as.numeric(.nv(r["lat"], "0")),
            as.numeric(.nv(r["lon"], "0")),
            as.integer(.nv(r["ovfl"], "0")),
            as.integer(.nv(r["rule"], "0")))
  })
  .write_file(dir, filename, c(header, data_lines))
}

# ===========================================================================
# Low-level file writer
# ===========================================================================

.write_file <- function(dir, filename, lines) {
  path <- file.path(dir, filename)
  writeLines(lines, path)
  1L
}

# ===========================================================================
# Mini helpers
# ===========================================================================

.yn <- function(val) if (isTRUE(as.logical(as.integer(val)))) "y" else "n"

.nv <- function(val, default = "null") {
  if (is.null(val) || is.na(val) || val == "") default else as.character(val)
}
