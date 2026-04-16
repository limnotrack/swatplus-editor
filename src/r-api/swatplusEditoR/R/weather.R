# Weather station management for swatplusEditoR
# Add, update, list, and remove weather station data

#' Add weather stations to the project database
#'
#' Inserts weather station records into the \code{weather_sta_cli} table. Each
#' station includes geographic coordinates and file references for climate
#' variables (precipitation, temperature, solar radiation, humidity, wind, PET).
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param stations A data.frame with weather station data. Required columns:
#'   \code{name}, \code{lat}, \code{lon}. Optional columns: \code{elev},
#'   \code{pcp}, \code{tmp}, \code{slr}, \code{hmd}, \code{wnd}, \code{pet},
#'   \code{atmo_dep}, \code{wgn_id}.
#'   File columns (\code{pcp}, \code{tmp}, etc.) should be character strings
#'   with the weather data filename (e.g., "pcp1.cli"). Use \code{NULL} or
#'   \code{NA} for unavailable variables.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' stations <- data.frame(
#'   name = c("station1", "station2"),
#'   lat = c(-38.1, -38.2),
#'   lon = c(176.3, 176.4),
#'   pcp = c("pcp1.cli", "pcp2.cli"),
#'   tmp = c("tmp1.cli", "tmp2.cli"),
#'   slr = c("slr1.cli", "slr2.cli"),
#'   hmd = c("hmd1.cli", "hmd2.cli"),
#'   wnd = c("wnd1.cli", "wnd2.cli"),
#'   pet = c("pet1.cli", "pet2.cli"),
#'   atmo_dep = c("atmo1.cli", "atmo2.cli"),
#'   stringsAsFactors = FALSE
#' )
#' add_weather_stations(project, stations)
#' }
add_weather_stations <- function(project, stations) {
  validate_project(project)

  if (!is.data.frame(stations)) {
    stop("stations must be a data.frame.", call. = FALSE)
  }
  if (nrow(stations) == 0) {
    stop("stations data.frame is empty.", call. = FALSE)
  }

  required_cols <- c("name", "lat", "lon")
  missing_cols <- setdiff(required_cols, names(stations))
  if (length(missing_cols) > 0) {
    stop("stations is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Generate station names if missing
  if (any(is.na(stations$name) | stations$name == "")) {
    na_idx <- which(is.na(stations$name) | stations$name == "")
    stations$name[na_idx] <- vapply(na_idx, function(i) {
      weather_sta_name(stations$lat[i], stations$lon[i])
    }, character(1))
  }

  # Ensure optional columns exist with NULL defaults
  optional_cols <- c("pcp", "tmp", "slr", "hmd", "wnd", "pet",
                     "atmo_dep", "wgn_id")
  for (col in optional_cols) {
    if (!col %in% names(stations)) {
      stations[[col]] <- NA
    }
  }

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "weather_sta_cli")) {
    stop("weather_sta_cli table not found. Ensure the database schema is ",
         "properly initialized.", call. = FALSE)
  }

  # Insert stations
  n_inserted <- 0
  for (i in seq_len(nrow(stations))) {
    row <- stations[i, ]
    weather_sta_cli <- DBI::dbReadTable(con, "weather_sta_cli")
    tryCatch({
      execute_db(con,
        "INSERT INTO weather_sta_cli
         (name, wgn_id, pcp, tmp, slr, hmd, wnd, pet, atmo_dep, lat, lon)
         VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          as.character(row$name), as.integer(row$wgn_id), as.character(row$pcp),
          as.character(row$tmp), as.character(row$slr), as.character(row$hmd),
          as.character(row$wnd), as.character(row$pet),
          as.character(row$atmo_dep), as.numeric(row$lat),
          as.numeric(row$lon)
        ))
      n_inserted <- n_inserted + 1
    }, error = function(e) {
      warning("Failed to insert station '", row$name, "': ", e$message,
              call. = FALSE)
    })
  }

  message("Inserted ", n_inserted, " of ", nrow(stations), " weather stations")
  invisible(project)
}

#' List weather stations in the project database
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with all weather stations.
#' @export
#' @examples
#' \dontrun{
#' stations <- list_weather_stations(project)
#' }
list_weather_stations <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (!table_exists(con, "weather_sta_cli")) {
    stop("weather_sta_cli table not found in database.", call. = FALSE)
  }

  query_db(con, "SELECT * FROM weather_sta_cli ORDER BY id")
}

#' Update a weather station
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param station_id Integer. ID of the station to update.
#' @param ... Named arguments of fields to update (e.g., name, lat, lon,
#'   pcp, tmp, slr, hmd, wnd, pet, atmo_dep, wgn_id).
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' update_weather_station(project, station_id = 1, lat = -38.15, lon = 176.35)
#' }
update_weather_station <- function(project, station_id, ...) {
  validate_project(project)
  updates <- list(...)

  if (length(updates) == 0) {
    stop("No update fields provided.", call. = FALSE)
  }

  valid_fields <- c("name", "lat", "lon", "elev", "pcp", "tmp", "slr",
                    "hmd", "wnd", "pet", "atmo_dep", "wgn_id")
  invalid <- setdiff(names(updates), valid_fields)
  if (length(invalid) > 0) {
    stop("Invalid fields: ", paste(invalid, collapse = ", "),
         ". Valid fields: ", paste(valid_fields, collapse = ", "),
         call. = FALSE)
  }

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  sql <- build_update_sql("weather_sta_cli", updates,
                          paste0("id = ", as.integer(station_id)))
  n <- execute_db(con, sql)
  if (n == 0) {
    warning("No station found with id = ", station_id, call. = FALSE)
  } else {
    message("Updated station id = ", station_id)
  }

  invisible(project)
}

#' Remove weather stations from the project database
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param station_ids Integer vector. IDs of stations to remove.
#'   If NULL, removes all stations.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' remove_weather_stations(project, station_ids = c(1, 2))
#' remove_weather_stations(project)  # removes all
#' }
remove_weather_stations <- function(project, station_ids = NULL) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  if (is.null(station_ids)) {
    n <- execute_db(con, "DELETE FROM weather_sta_cli")
    message("Removed all ", n, " weather stations")
  } else {
    ids <- paste(as.integer(station_ids), collapse = ", ")
    n <- execute_db(con, paste0("DELETE FROM weather_sta_cli WHERE id IN (", ids, ")"))
    message("Removed ", n, " weather stations")
  }

  invisible(project)
}

#' Add weather generator (WGN) data
#'
#' Inserts weather generator station data into the \code{weather_wgn_cli}
#' table and optional monthly values into \code{weather_wgn_cli_mon}.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param wgn_data A data.frame with columns: \code{name}, \code{lat},
#'   \code{lon}, \code{elev}, \code{rain_yrs}.
#' @param monthly_data Optional data.frame with monthly WGN values. Must have
#'   columns: \code{wgn_name} (matching name in wgn_data), \code{month} (1-12),
#'   and the monthly parameters: \code{tmp_max_ave}, \code{tmp_min_ave},
#'   \code{tmp_max_sd}, \code{tmp_min_sd}, \code{pcp_ave}, \code{pcp_sd},
#'   \code{pcp_skew}, \code{wet_dry}, \code{wet_wet}, \code{pcp_days},
#'   \code{pcp_hhr}, \code{slr_ave}, \code{dew_ave}, \code{wnd_ave}.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' wgn <- data.frame(
#'   name = "wgn_station1",
#'   lat = -38.1, lon = 176.3, elev = 350, rain_yrs = 30
#' )
#' add_weather_generators(project, wgn)
#' }
add_weather_generators <- function(project, wgn_data, monthly_data = NULL) {
  validate_project(project)

  if (!is.data.frame(wgn_data) || nrow(wgn_data) == 0) {
    stop("wgn_data must be a non-empty data.frame.", call. = FALSE)
  }

  required_cols <- c("name", "lat", "lon", "elev", "rain_yrs")
  missing_cols <- setdiff(required_cols, names(wgn_data))
  if (length(missing_cols) > 0) {
    stop("wgn_data is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  n_inserted <- 0
  for (i in seq_len(nrow(wgn_data))) {
    row <- wgn_data[i, ]
    tryCatch({
      execute_db(con,
        "INSERT INTO weather_wgn_cli (name, lat, lon, elev, rain_yrs)
         VALUES (?, ?, ?, ?, ?)",
        params = list(row$name, row$lat, row$lon, row$elev, row$rain_yrs))
      n_inserted <- n_inserted + 1

      # Insert monthly data if provided
      if (!is.null(monthly_data) && is.data.frame(monthly_data)) {
        wgn_mon <- monthly_data[monthly_data$wgn_name == row$name, ]
        if (nrow(wgn_mon) > 0) {
          wgn_id <- query_db(con,
            "SELECT id FROM weather_wgn_cli WHERE name = ?",
            params = list(row$name))$id
          for (j in seq_len(nrow(wgn_mon))) {
            mon <- wgn_mon[j, ]
            execute_db(con,
              "INSERT INTO weather_wgn_cli_mon
               (weather_wgn_cli_id, month, tmp_max_ave, tmp_min_ave,
                tmp_max_sd, tmp_min_sd, pcp_ave, pcp_sd, pcp_skew,
                wet_dry, wet_wet, pcp_days, pcp_hhr, slr_ave, dew_ave,
                wnd_ave)
               VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(
                wgn_id, mon$month,
                mon$tmp_max_ave, mon$tmp_min_ave,
                mon$tmp_max_sd, mon$tmp_min_sd,
                mon$pcp_ave, mon$pcp_sd, mon$pcp_skew,
                mon$wet_dry, mon$wet_wet, mon$pcp_days, mon$pcp_hhr,
                mon$slr_ave, mon$dew_ave, mon$wnd_ave
              ))
          }
        }
      }
    }, error = function(e) {
      warning("Failed to insert WGN '", row$name, "': ", e$message,
              call. = FALSE)
    })
  }

  message("Inserted ", n_inserted, " weather generator stations")
  invisible(project)
}

#' Set the weather data directory in the project configuration
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param weather_dir Character. Path to the directory containing weather
#'   data files.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' set_weather_dir(project, "/path/to/weather/data")
#' }
set_weather_dir <- function(project, weather_dir) {
  validate_project(project)

  if (!dir.exists(weather_dir)) {
    stop("Weather data directory does not exist: ", weather_dir, call. = FALSE)
  }

  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  execute_db(con,
    "UPDATE project_config SET weather_data_dir = ?",
    params = list(normalizePath(weather_dir)))

  message("Set weather data directory: ", weather_dir)
  invisible(project)
}

#' Get WGN data from the CFSR world database for the nearest stations
#'
#' For each station provided, finds the nearest weather generator site in the
#' \code{wgn_cfsr_world} table of the SWAT+ WGN database and writes the matched
#' WGN data (station header plus 12 monthly rows) to the project's
#' \code{weather_wgn_cli} and \code{weather_wgn_cli_mon} tables.
#' Only unique matched WGN sites are written; duplicate matches are silently
#' skipped via \code{INSERT OR IGNORE}.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param stations A data.frame with at minimum \code{lat} and \code{lon}
#'   columns representing the stations for which the nearest WGN site is needed.
#' @param wgn_db Character. Path to the \code{swatplus_wgn.sqlite} database
#'   containing the \code{wgn_cfsr_world} and \code{wgn_cfsr_world_mon} tables.
#' @return The project object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' stations <- data.frame(
#'   name = c("station1", "station2"),
#'   lat  = c(-38.1, -38.2),
#'   lon  = c(176.3, 176.4)
#' )
#' project <- get_wgn_cfsr_world(project, stations,
#'                               wgn_db = "path/to/swatplus_wgn.sqlite")
#' }
get_wgn_cfsr_world <- function(project, stations, wgn_db) {
  validate_project(project)
  
  # Open project database connectio
  proj_con <- open_project_db(project$db_file)
  on.exit(close_db(proj_con), add = TRUE)
  
  # Get stations from database
  if (missing(stations)) {
    stations <- query_db(proj_con, "SELECT id, name, lat, lon FROM weather_sta_cli")
  }

  if (!is.data.frame(stations) || nrow(stations) == 0) {
    stop("stations must be a non-empty data.frame.", call. = FALSE)
  }
  required_cols <- c("lat", "lon")
  missing_cols <- setdiff(required_cols, names(stations))
  if (length(missing_cols) > 0) {
    stop("stations is missing required columns: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!file.exists(wgn_db)) {
    stop("WGN database file not found: ", wgn_db, call. = FALSE)
  }

  # Open WGN database and read wgn_cfsr_world
  wgn_con <- DBI::dbConnect(RSQLite::SQLite(), wgn_db)
  on.exit(close_db(wgn_con), add = TRUE)

  if (!table_exists(wgn_con, "wgn_cfsr_world")) {
    stop("Table 'wgn_cfsr_world' not found in WGN database: ", wgn_db,
         call. = FALSE)
  }
  if (!table_exists(wgn_con, "wgn_cfsr_world_mon")) {
    stop("Table 'wgn_cfsr_world_mon' not found in WGN database: ", wgn_db,
         call. = FALSE)
  }

  wgn_sites <- query_db(wgn_con,
    "SELECT id, name, lat, lon, elev, rain_yrs FROM wgn_cfsr_world")

  if (nrow(wgn_sites) == 0) {
    stop("No stations found in 'wgn_cfsr_world' table.", call. = FALSE)
  }

  # For each input station find the nearest WGN site
  matched_wgn_ids <- vapply(seq_len(nrow(stations)), function(i) {
    idx <- find_closest_station(wgn_sites, stations$lat[i], stations$lon[i])
    wgn_sites$id[idx]
  }, integer(1))

  unique_ids <- unique(matched_wgn_ids)
  id_list <- paste(unique_ids, collapse = ", ")

  # Fetch matched WGN site headers
  matched_wgn <- query_db(wgn_con,
    paste0("SELECT id, name, lat, lon, elev, rain_yrs FROM wgn_cfsr_world ",
           "WHERE id IN (", id_list, ")"))

  # Fetch monthly data for matched WGN sites
  matched_mon <- query_db(wgn_con,
    paste0("SELECT wgn_id, month, tmp_max_ave, tmp_min_ave, tmp_max_sd, ",
           "tmp_min_sd, pcp_ave, pcp_sd, pcp_skew, wet_dry, wet_wet, ",
           "pcp_days, pcp_hhr, slr_ave, dew_ave, wnd_ave ",
           "FROM wgn_cfsr_world_mon WHERE wgn_id IN (", id_list, ")"))

  # Write to project database
  if (!table_exists(proj_con, "weather_wgn_cli")) {
    stop("weather_wgn_cli table not found in project database.", call. = FALSE)
  }

  n_inserted <- 0
  for (i in seq_len(nrow(matched_wgn))) {
    w <- matched_wgn[i, ]
    tryCatch({
      execute_db(proj_con,
        "INSERT OR IGNORE INTO weather_wgn_cli (name, lat, lon, elev, rain_yrs)
         VALUES (?, ?, ?, ?, ?)",
        params = list(w$name, w$lat, w$lon, w$elev, w$rain_yrs))

      # Retrieve the id (existing or just-inserted)
      proj_wgn_id <- query_db(proj_con,
        "SELECT id FROM weather_wgn_cli WHERE name = ?",
        params = list(w$name))$id

      # Insert monthly data only when none exist yet for this WGN site
      existing_mon <- query_db(proj_con,
        "SELECT COUNT(*) as n FROM weather_wgn_cli_mon WHERE weather_wgn_cli_id = ?",
        params = list(proj_wgn_id))$n
      if (existing_mon == 0) {
        mon_rows <- matched_mon[matched_mon$wgn_id == w$id, ]
        for (j in seq_len(nrow(mon_rows))) {
          m <- mon_rows[j, ]
          execute_db(proj_con,
            "INSERT INTO weather_wgn_cli_mon
             (weather_wgn_cli_id, month, tmp_max_ave, tmp_min_ave, tmp_max_sd,
              tmp_min_sd, pcp_ave, pcp_sd, pcp_skew, wet_dry, wet_wet,
              pcp_days, pcp_hhr, slr_ave, dew_ave, wnd_ave)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(
              proj_wgn_id, m$month,
              m$tmp_max_ave, m$tmp_min_ave, m$tmp_max_sd, m$tmp_min_sd,
              m$pcp_ave, m$pcp_sd, m$pcp_skew,
              m$wet_dry, m$wet_wet, m$pcp_days, m$pcp_hhr,
              m$slr_ave, m$dew_ave, m$wnd_ave
            ))
        }
      }
      n_inserted <- n_inserted + 1
    }, error = function(e) {
      warning("Failed to insert WGN station '", w$name, "': ", e$message,
              call. = FALSE)
    })
  }
  
  # Update wgn column in weather_sta_cli with matched WGN station IDs
  for (i in seq_len(nrow(stations))) {
    wgn_id <- matched_wgn$name[i]
    execute_db(proj_con,
      "UPDATE weather_sta_cli SET wgn_id = ? WHERE id = ?",
      params = list(wgn_id, stations$id[i]))
  }

  message("Inserted ", n_inserted, " WGN station(s) from wgn_cfsr_world into ",
          "weather_wgn_cli")
  invisible(project)
}

#' Match weather stations to spatial objects by nearest distance
#'
#' Updates the \code{wst_id} foreign key on connection tables (e.g.,
#' \code{hru_con}, \code{aquifer_con}) to reference the closest weather
#' station by lat/lon coordinates. Also matches \code{wgn_id} on
#' \code{weather_sta_cli} to the nearest weather generator station.
#'
#' Called automatically by \code{\link{add_weather_stations}}.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @param tables Character vector. Connection tables to update.
#'   Defaults to common SWAT+ connection tables.
#' @return The project object (invisibly).
#' @export
match_weather_stations <- function(project,
                                   tables = c("aquifer_con", "channel_con",
                                              "chandeg_con", "rout_unit_con",
                                              "reservoir_con", "recall_con",
                                              "exco_con", "hru_con",
                                              "hru_lte_con")) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  match_weather_stations_db(con, tables)
  invisible(project)
}

#' Match weather stations to spatial objects (database-level)
#'
#' Internal workhorse called by \code{\link{match_weather_stations}} and by
#' \code{write_direct()} during file writing.  Operates on an already-open
#' database connection so it can be used inside \code{write_config_files()}
#' after \code{populate_from_gis()} has created all \code{_con} tables.
#'
#' @param con DBI database connection.
#' @param tables Character vector of connection tables to update.
#' @keywords internal
match_weather_stations_db <- function(con,
                                      tables = c("aquifer_con", "channel_con",
                                                  "chandeg_con", "rout_unit_con",
                                                  "reservoir_con", "recall_con",
                                                  "exco_con", "hru_con",
                                                  "hru_lte_con")) {
  if (!has_data(con, "weather_sta_cli")) return(invisible(NULL))

  stations <- query_db(con, "SELECT id, lat, lon FROM weather_sta_cli")
  if (nrow(stations) == 0) return(invisible(NULL))

  all_tables <- list_db_tables(con)
  n_updated <- 0

  for (tbl in tables) {
    if (!tbl %in% all_tables) next

    # Check if table has lat, lon, and wst_id columns
    cols <- names(query_db(con, paste("SELECT * FROM", tbl, "LIMIT 0")))
    if (!all(c("lat", "lon") %in% cols)) next
    wst_col <- if ("wst_id" %in% cols) "wst_id" else if ("wst" %in% cols) "wst" else next

    objects <- query_db(con, paste("SELECT id, lat, lon FROM", tbl))
    if (nrow(objects) == 0) next

    for (i in seq_len(nrow(objects))) {
      closest_idx <- find_closest_station(stations, objects$lat[i],
                                          objects$lon[i])
      if (!is.na(closest_idx)) {
        execute_db(con,
          paste0("UPDATE ", tbl, " SET ", wst_col, " = ? WHERE id = ?"),
          params = list(stations$id[closest_idx], objects$id[i]))
        n_updated <- n_updated + 1
      }
    }
  }

  # Match WGN to weather stations (mirrors Python import_weather.match_wgn)
  if ("weather_wgn_cli" %in% all_tables && "weather_sta_cli" %in% all_tables) {
    wgn_stations <- query_db(con, "SELECT id, lat, lon FROM weather_wgn_cli")
    if (nrow(wgn_stations) > 0) {
      for (i in seq_len(nrow(stations))) {
        closest_idx <- find_closest_station(wgn_stations, stations$lat[i],
                                            stations$lon[i])
        if (!is.na(closest_idx)) {
          execute_db(con,
            "UPDATE weather_sta_cli SET wgn_id = ? WHERE id = ?",
            params = list(wgn_stations$id[closest_idx], stations$id[i]))
          n_updated <- n_updated + 1
        }
      }
    }
  }

  message("Matched ", n_updated, " objects to nearest weather stations")
  invisible(NULL)
}
