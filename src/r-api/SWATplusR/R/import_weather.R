#' Import weather data into a SWAT+ project database
#'
#' Mirrors the functionality of \code{src/api/actions/import_weather.py}.
#' Supports importing observed weather files, WGN (Weather Generator) data,
#' SWAT 2012 formatted data, and atmospheric deposition data.
#'
#' @keywords internal
NULL

# ===========================================================================
# Weather station name generation
# ===========================================================================

#' Generate a SWAT+ weather station name from latitude and longitude
#'
#' Mirrors \code{weather_sta_name()} in
#' \code{src/api/actions/import_weather.py}.
#'
#' @param lat    Latitude (decimal degrees).
#' @param lon    Longitude (decimal degrees).
#' @param prefix Station name prefix (default \code{"s"}).
#' @param mult   Multiplier applied before rounding (default \code{1000}).
#' @return Character station name.
#' @export
weather_sta_name <- function(lat, lon, prefix = "s", mult = 1000) {
  latp <- if (lat >= 0) "n" else "s"
  lonp <- if (lon >= 0) "e" else "w"
  sprintf("%s%d%s%d%s", prefix,
          abs(round(lat * mult)), latp,
          abs(round(lon * mult)), lonp)
}

# ===========================================================================
# WGN import
# ===========================================================================

#' Import Weather Generator (WGN) data into the project database
#'
#' Reads WGN data from a source SQLite database (single-table format used
#' by the SWAT+ editor WGN database) and inserts it into the project database
#' \code{weather_wgn_cli} / \code{weather_wgn_cli_mon} tables.
#'
#' Mirrors \code{WgnImport} in \code{src/api/actions/import_weather.py}.
#'
#' @param project_db  Path to the project \code{.sqlite} database.
#' @param wgn_db      Path to the WGN source \code{.sqlite} database.
#' @param wgn_table   Name of the WGN table in \code{wgn_db}.
#' @param verbose     Print progress messages.
#' @return Invisibly, the number of stations imported.
#' @export
import_wgn <- function(project_db,
                       wgn_db,
                       wgn_table = "weather_wgn_cli",
                       verbose   = TRUE) {
  if (!file.exists(wgn_db))
    stop("WGN database not found: ", wgn_db)

  wgn_con  <- swat_open_db(wgn_db)
  proj_con <- swat_open_db(project_db)
  on.exit({ swat_close_db(wgn_con); swat_close_db(proj_con) }, add = TRUE)

  if (!swat_exists_table(wgn_con, wgn_table))
    stop("Table '", wgn_table, "' not found in ", wgn_db)

  wgn_data <- DBI::dbGetQuery(wgn_con,
    paste0("SELECT * FROM ", wgn_table, " ORDER BY id"))
  if (nrow(wgn_data) == 0L) {
    if (verbose) message("No WGN records found.")
    return(invisible(0L))
  }

  # Monthly columns expected in the WGN table
  month_cols <- c("tmp_max_ave", "tmp_min_ave", "tmp_max_sd", "tmp_min_sd",
                  "pcp_ave", "pcp_sd", "pcp_skew", "wet_dry", "wet_wet",
                  "pcp_days", "pcp_hhr", "slr_ave", "dew_ave", "wnd_ave")

  stations_inserted <- 0L
  DBI::dbWithTransaction(proj_con, {
    for (i in seq_len(nrow(wgn_data))) {
      row <- wgn_data[i, ]

      # Insert station header
      DBI::dbExecute(proj_con,
        "INSERT OR IGNORE INTO weather_wgn_cli (name, lat, lon, elev, rain_yrs)
         VALUES (?, ?, ?, ?, ?)",
        params = list(row$name,
                      if ("lat" %in% names(row)) row$lat else 0.0,
                      if ("lon" %in% names(row)) row$lon else 0.0,
                      if ("elev" %in% names(row)) row$elev else 0.0,
                      if ("rain_yrs" %in% names(row)) row$rain_yrs else 30L))

      sta_id <- DBI::dbGetQuery(proj_con,
        paste0("SELECT id FROM weather_wgn_cli WHERE name='", row$name, "'"))$id

      # Insert monthly values (12 rows per station in wide format)
      for (m in 1:12) {
        sfx <- paste0("_", m)
        mon_row <- list(weather_wgn_cli_id = sta_id, month = m)
        for (col in month_cols) {
          wide_col <- paste0(col, sfx)
          mon_row[[col]] <- if (wide_col %in% names(row)) row[[wide_col]] else 0.0
        }
        cnams <- names(mon_row)
        names(mon_row) <- NULL
        DBI::dbExecute(proj_con, paste0(
          "INSERT INTO weather_wgn_cli_mon (",
          paste(cnams, collapse = ", "), ") VALUES (",
          paste(rep("?", length(mon_row)), collapse = ", "), ")"),
          params = mon_row)
      }
      stations_inserted <- stations_inserted + 1L
    }
  })

  if (verbose)
    message(stations_inserted, " WGN station(s) imported.")
  invisible(stations_inserted)
}

# ===========================================================================
# Observed weather import
# ===========================================================================

#' Import observed weather data files into the project database
#'
#' Scans \code{weather_dir} for SWAT+ weather input files
#' (\code{*.pcp}, \code{*.tmp}, \code{*.slr}, \code{*.hmd}, \code{*.wnd},
#' \code{*.pet}) and records them in \code{weather_file} and
#' \code{weather_sta_cli}.
#'
#' Mirrors \code{WeatherImport} in \code{src/api/actions/import_weather.py}.
#'
#' @param project_db  Path to the project \code{.sqlite} database.
#' @param weather_dir Directory containing the weather data files.
#' @param format      Weather data format: \code{"observed"} (default) or
#'   \code{"2012"} (SWAT 2012 style).
#' @param verbose     Print progress messages.
#' @return Invisibly, the number of station files found.
#' @export
import_weather <- function(project_db,
                           weather_dir,
                           format  = "observed",
                           verbose = TRUE) {
  if (!dir.exists(weather_dir))
    stop("Weather directory not found: ", weather_dir)

  proj_con <- swat_open_db(project_db)
  on.exit(swat_close_db(proj_con), add = TRUE)

  ext_map <- list(
    pcp = "pcp", tmp = "tmp", slr = "slr",
    hmd = "hmd", wnd = "wnd", pet = "pet"
  )

  files_found <- list()
  for (type in names(ext_map)) {
    pattern <- paste0("\\.", type, "$")
    fls <- list.files(weather_dir, pattern = pattern,
                      full.names = FALSE, ignore.case = TRUE)
    files_found[[type]] <- fls
  }

  all_files <- unlist(files_found, use.names = FALSE)
  if (length(all_files) == 0L) {
    if (verbose) message("No weather files found in ", weather_dir)
    return(invisible(0L))
  }

  # Record all weather files
  DBI::dbExecute(proj_con, "DELETE FROM weather_file")
  wf_rows <- do.call(rbind, lapply(names(files_found), function(type) {
    fls <- files_found[[type]]
    if (length(fls) == 0L) return(NULL)
    data.frame(
      filename = fls,
      type     = type,
      lat      = 0.0,
      lon      = 0.0,
      stringsAsFactors = FALSE
    )
  }))
  if (!is.null(wf_rows) && nrow(wf_rows) > 0L)
    swat_bulk_insert(proj_con, "weather_file", wf_rows)

  # Create weather stations from pcp files (one station per pcp file)
  n_sta <- 0L
  for (pcp_file in files_found$pcp) {
    sta_name <- tools::file_path_sans_ext(pcp_file)
    existing <- DBI::dbGetQuery(proj_con,
      paste0("SELECT id FROM weather_sta_cli WHERE name='", sta_name, "'"))
    if (nrow(existing) > 0L) next

    DBI::dbExecute(proj_con,
      "INSERT INTO weather_sta_cli (name, pcp, tmp, slr, hmd, wnd, pet)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(
        sta_name,
        pcp_file,
        if (tools::file_ext(pcp_file) == "pcp")
          paste0(sta_name, ".tmp") else NULL,
        paste0(sta_name, ".slr"),
        paste0(sta_name, ".hmd"),
        paste0(sta_name, ".wnd"),
        paste0(sta_name, ".pet")
      ))
    n_sta <- n_sta + 1L
  }

  if (verbose)
    message(n_sta, " weather station(s) created; ",
            nrow(wf_rows), " file record(s) stored.")
  invisible(n_sta)
}

# ===========================================================================
# Match weather stations to spatial objects
# ===========================================================================

#' Match weather stations to SWAT+ spatial objects by closest lat/lon
#'
#' Updates the \code{wst_id} column in connection tables
#' (\code{hru_con}, \code{rout_unit_con}, etc.) to point to the nearest
#' \code{weather_sta_cli} record.
#'
#' Mirrors \code{WeatherImport.match_stations()} in
#' \code{src/api/actions/import_weather.py}.
#'
#' @param project_db  Path to the project \code{.sqlite} database.
#' @param verbose     Print progress messages.
#' @return Invisibly \code{NULL}.
#' @export
match_weather_stations <- function(project_db, verbose = TRUE) {
  proj_con <- swat_open_db(project_db)
  on.exit(swat_close_db(proj_con), add = TRUE)

  if (swat_count(proj_con, "weather_sta_cli") == 0L) {
    if (verbose) message("No weather stations to match.")
    return(invisible(NULL))
  }

  con_tables <- c("hru_con", "hru_lte_con", "rout_unit_con",
                  "aquifer_con", "chandeg_con", "reservoir_con")

  for (tbl in con_tables) {
    if (!swat_exists_table(proj_con, tbl)) next
    if (swat_count(proj_con, tbl) == 0L) next

    sql <- paste0("
      UPDATE ", tbl, " SET wst_id = (
        SELECT id FROM weather_sta_cli
        ORDER BY ((", tbl, ".lat - weather_sta_cli.lat) *
                  (", tbl, ".lat - weather_sta_cli.lat) +
                  (", tbl, ".lon - weather_sta_cli.lon) *
                  (", tbl, ".lon - weather_sta_cli.lon))
        LIMIT 1
      )")
    DBI::dbExecute(proj_con, sql)
  }

  if (verbose) message("Weather stations matched.")
  invisible(NULL)
}

# ===========================================================================
# Atmospheric deposition import
# ===========================================================================

#' Import atmospheric deposition data from a CSV file
#'
#' Reads a CSV with columns \code{year}, \code{nh4_wet}, \code{no3_wet},
#' \code{nh4_dry}, \code{no3_dry} (plus optional \code{name}/\code{station})
#' and inserts it into \code{atmo_cli} / \code{atmo_cli_sta} /
#' \code{atmo_cli_sta_value}.
#'
#' Mirrors \code{AtmoImport} in \code{src/api/actions/import_weather.py}.
#'
#' @param project_db  Path to the project \code{.sqlite} database.
#' @param csv_path    Path to the CSV file.
#' @param filename    Output filename to store in \code{atmo_cli}
#'   (default \code{"atmo.cli"}).
#' @param timestep    Time step type: \code{"aa"} (annual average, default).
#' @param verbose     Print progress messages.
#' @return Invisibly, the number of rows imported.
#' @export
import_atmo_dep <- function(project_db,
                            csv_path,
                            filename  = "atmo.cli",
                            timestep  = "aa",
                            verbose   = TRUE) {
  if (!file.exists(csv_path))
    stop("CSV file not found: ", csv_path)

  data <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
  if (nrow(data) == 0L) {
    if (verbose) message("Empty CSV file.")
    return(invisible(0L))
  }

  # Required columns
  req <- c("nh4_wet", "no3_wet", "nh4_dry", "no3_dry")
  missing_cols <- setdiff(req, tolower(names(data)))
  if (length(missing_cols) > 0L)
    stop("CSV missing required columns: ", paste(missing_cols, collapse = ", "))

  proj_con <- swat_open_db(project_db)
  on.exit(swat_close_db(proj_con), add = TRUE)

  yr_col   <- if ("year" %in% tolower(names(data))) "year" else NULL
  sta_col  <- if ("name" %in% tolower(names(data))) "name" else
              if ("station" %in% tolower(names(data))) "station" else NULL

  mo_init <- if (!is.null(yr_col)) min(data[[yr_col]], na.rm = TRUE) else 1L
  num_aa  <- nrow(data)

  DBI::dbExecute(proj_con,
    "INSERT INTO atmo_cli (filename, timestep, mo_init, yr_init, num_aa)
     VALUES (?, ?, ?, ?, ?)",
    params = list(filename, timestep, 1L, as.integer(mo_init), num_aa))
  atmo_id <- DBI::dbGetQuery(proj_con,
    "SELECT last_insert_rowid() AS id")$id

  sta_name <- if (!is.null(sta_col)) data[[sta_col]][[1L]] else "sta1"
  DBI::dbExecute(proj_con,
    "INSERT INTO atmo_cli_sta (atmo_cli_id, name) VALUES (?, ?)",
    params = list(atmo_id, sta_name))
  sta_id <- DBI::dbGetQuery(proj_con,
    "SELECT last_insert_rowid() AS id")$id

  vals <- data.frame(
    sta_id   = rep(sta_id, nrow(data)),
    timestep = seq_len(nrow(data)),
    nh4_wet  = data$nh4_wet,
    no3_wet  = data$no3_wet,
    nh4_dry  = data$nh4_dry,
    no3_dry  = data$no3_dry,
    stringsAsFactors = FALSE
  )
  swat_bulk_insert(proj_con, "atmo_cli_sta_value", vals)

  if (verbose)
    message(nrow(data), " atmospheric deposition record(s) imported.")
  invisible(nrow(data))
}
