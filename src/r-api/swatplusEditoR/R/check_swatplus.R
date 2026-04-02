#' Check SWAT+ input file consistency
#'
#' @inheritParams write_config_files
#'
#' @returns A list of issues found, or an empty list if no issues detected. Each issue is a character string describing the problem.
#' @export
#'

check_swatplus <- function(project, output_dir) {
  
  # Helper to read a SWAT+ text file (skip title + header)
  read_swat <- function(file, out_dir = output_dir, skip = 1, ...) {
    path <- file.path(out_dir, file)
    if (!file.exists(path)) {
      message("  [MISSING] ", file)
      return(NULL)
    }
    tryCatch(
      read.table(path, skip = skip, header = TRUE, fill = TRUE, ...),
      error = function(e) {
        message("  [READ ERROR] ", file, ": ", e$message)
        NULL
      }
    )
  }
  
  issues <- list()
  
  cat("=== SWAT+ Connectivity Check ===\n\n")
  
  # ── 1. Object counts ──────────────────────────────────────────────────────
  cat("-- 1. Object counts (object.cnt vs actual file rows) --\n")
  
  cnt <- read_swat("object.cnt")
  
  if (!is.null(cnt)) {
    checks <- list(
      hru     = list(file = "hru-data.hru", col = "hru"),
      rtu     = list(file = "rout_unit.ele", col = "hru"),
      aqu     = list(file = "aquifer.aqu",   col = "aqu"),
      cha     = list(file = "chandeg.con",   col = "lcha")
    )
    
    for (nm in names(checks)) {
      chk  <- checks[[nm]]
      dat  <- read_swat(chk$file, skip = 1)
      if (is.null(dat)) next
      
      actual    <- nrow(dat)
      declared  <- cnt[[chk$col]]
      
      if (length(declared) == 0) {
        cat(sprintf("  [WARN] Column '%s' not found in object.cnt\n", chk$col))
        issues[[length(issues) + 1]] <- sprintf("object.cnt missing column: %s", chk$col)
      } else if (actual != declared) {
        msg <- sprintf("  [MISMATCH] %s: object.cnt says %d, file has %d rows",
                       chk$file, declared, actual)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] %-20s %d rows\n", chk$file, actual))
      }
    }
  }
  
  # ── 2. HRU consistency ───────────────────────────────────────────────────
  cat("\n-- 2. HRU name consistency (hru.con vs hru-data.hru) --\n")
  
  hru_con  <- read_swat("hru.con")
  hru_data <- read_swat("hru-data.hru")
  
  if (!is.null(hru_con) && !is.null(hru_data)) {
    missing_hru <- setdiff(hru_con[[1]], hru_data[[1]])
    if (length(missing_hru) > 0) {
      msg <- sprintf("  [MISMATCH] %d HRU names in hru.con missing from hru-data.hru",
                     length(missing_hru))
      cat(msg, "\n")
      cat("  First 5:", paste(head(missing_hru, 5), collapse = ", "), "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] All %d HRU names match\n", nrow(hru_con)))
    }
  }
  
  # ── 3. Routing unit consistency ───────────────────────────────────────────
  cat("\n-- 3. Routing unit name consistency (rout_unit.con vs rout_unit.ele) --\n")
  
  rtu_con <- read_swat("rout_unit.con")
  rtu_ele <- read_swat("rout_unit.ele")
  
  if (!is.null(rtu_con) && !is.null(rtu_ele)) {
    missing_rtu <- setdiff(rtu_con[[1]], rtu_ele[[1]])
    if (length(missing_rtu) > 0) {
      msg <- sprintf("  [MISMATCH] %d routing unit names in rout_unit.con missing from rout_unit.ele",
                     length(missing_rtu))
      cat(msg, "\n")
      cat("  First 5:", paste(head(missing_rtu, 5), collapse = ", "), "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] All %d routing unit names match\n", nrow(rtu_con)))
    }
  }
  
  # ── 4. Outflow targets in rout_unit.con ───────────────────────────────────
  cat("\n-- 4. Outflow targets in rout_unit.con --\n")
  
  cha_con <- read_swat("chandeg.con")
  aqu_con <- read_swat("aquifer.con")
  
  if (!is.null(rtu_con) && !is.null(cha_con) && !is.null(aqu_con)) {
    valid_targets <- c(
      cha_con[[1]],
      aqu_con[[1]],
      rtu_con[[1]],
      "null"
    )
    
    # Outflow object name columns — typically col 7 onwards (obj_typ + obj_name pairs)
    # rout_unit.con structure: name, area, lat, lon, elev, wst, obj_typ, obj_name, ...
    outflow_cols <- rtu_con[, 8, drop = TRUE]  # obj_name column
    
    bad_targets <- setdiff(
      unique(outflow_cols[!is.na(outflow_cols)]),
      valid_targets
    )
    
    if (length(bad_targets) > 0) {
      msg <- sprintf("  [MISMATCH] %d outflow targets not found in chandeg.con / aquifer.con",
                     length(bad_targets))
      cat(msg, "\n")
      cat("  Bad targets:", paste(head(bad_targets, 5), collapse = ", "), "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat("  [OK] All outflow targets are valid\n")
    }
  }
  
  # ── 5. Weather station name consistency ───────────────────────────────────
  cat("\n-- 5. Weather station names (weather-sta.cli vs weather-wgn.cli) --\n")
  
  sta <- read_swat("weather-sta.cli")
  wgn <- read_wgn_cli(output_dir = output_dir)
  
  if (!is.null(sta) && !is.null(wgn)) {
    missing_wgn <- setdiff(sta[[2]], wgn$summary$name)
    if (length(missing_wgn) > 0) {
      msg <- sprintf("  [MISMATCH] %d station names in weather-sta.cli missing from weather-wgn.cli",
                     length(missing_wgn))
      cat(msg, "\n")
      cat("  Missing:", paste(head(missing_wgn, 5), collapse = ", "), "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] All %d weather station names match\n", nrow(sta)))
    }
  }
  
  # ── 6. Weather station count matches project HRU subbasin count ───────────
  cat("\n-- 6. Weather station count vs subbasin count --\n")
  
  n_subbasins <- length(unique(project$hru_data$subbasin))
  n_stations  <- if (!is.null(sta)) nrow(sta) else NA
  
  if (!is.na(n_stations)) {
    if (n_stations != n_subbasins) {
      msg <- sprintf("  [WARN] %d weather stations but %d subbasins in project",
                     n_stations, n_subbasins)
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] %d stations match %d subbasins\n", n_stations, n_subbasins))
    }
  }
  
  # ── 7. Channel count matches project stream topology ──────────────────────
  cat("\n-- 7. Channel count vs project stream topology --\n")
  
  n_channels_project <- nrow(project$channel_topology)
  n_channels_file    <- if (!is.null(cha_con)) nrow(cha_con) else NA
  
  if (!is.na(n_channels_file)) {
    if (n_channels_file != n_channels_project) {
      msg <- sprintf("  [WARN] %d channels in chandeg.con but %d in project$channel_topology",
                     n_channels_file, n_channels_project)
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] %d channels match project topology\n", n_channels_file))
    }
  }
  
  # ── 8. HRU count matches project hru_data ────────────────────────────────
  cat("\n-- 8. HRU count vs project$hru_data --\n")
  
  n_hrus_project <- nrow(project$hru_data)
  n_hrus_file    <- if (!is.null(hru_data)) nrow(hru_data) else NA
  
  if (!is.na(n_hrus_file)) {
    if (n_hrus_file != n_hrus_project) {
      msg <- sprintf("  [WARN] %d HRUs in hru-data.hru but %d in project$hru_data",
                     n_hrus_file, n_hrus_project)
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] %d HRUs match project$hru_data\n", n_hrus_file))
    }
  }
  
  # ── 9. file.cio validation ───────────────────────────────────────────────
  cat("\n-- 9. file.cio validation --\n")
  
  cio_path <- file.path(output_dir, "file.cio")
  
  if (!file.exists(cio_path)) {
    msg <- "  [MISSING] file.cio not found"
    cat(msg, "\n")
    issues[[length(issues) + 1]] <- msg
  } else {
    
    cio_lines <- readLines(cio_path)
    
    # Check editor version resolved (vNA indicates a write failure)
    if (grepl("vNA", cio_lines[1])) {
      msg <- "  [WARN] file.cio header shows 'vNA' - editor version not resolved during write"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    }
    
    # Parse entries: split each line into category + file entries
    # Skip title line (line 1)
    cio_entries <- lapply(cio_lines[-1], function(l) {
      parts <- strsplit(trimws(l), "\\s+")[[1]]
      list(category = parts[1], files = parts[-1])
    })
    
    # Count how many categories are all-null
    all_null <- sapply(cio_entries, function(e) all(e$files == "null"))
    n_all_null   <- sum(all_null)
    n_categories <- length(cio_entries)
    
    if (n_all_null == n_categories) {
      msg <- "  [ERROR] file.cio has all entries set to null - no input files will be read"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else if (n_all_null > 0) {
      cat(sprintf("  [WARN] %d of %d categories are fully null\n", n_all_null, n_categories))
    }
    
    # Check that files referenced in file.cio actually exist in output_dir
    existing_files <- list.files(output_dir)
    referenced     <- unique(unlist(lapply(cio_entries, `[[`, "files")))
    referenced     <- referenced[referenced != "null"]
    
    missing_refs <- setdiff(referenced, existing_files)
    if (length(missing_refs) > 0) {
      msg <- sprintf("  [MISSING] %d file(s) referenced in file.cio not found on disk",
                     length(missing_refs))
      cat(msg, "\n")
      cat("  Missing:", paste(head(missing_refs, 5), collapse = ", "), "\n")
      issues[[length(issues) + 1]] <- msg
    } else if (length(referenced) > 0) {
      cat(sprintf("  [OK] All %d files referenced in file.cio exist on disk\n",
                  length(referenced)))
    }
    
    # Check key categories have non-null entries
    key_categories <- c("simulation", "basin", "climate", "connect",
                        "hru", "aquifer", "soils", "hydrology")
    
    category_names <- sapply(cio_entries, `[[`, "category")
    
    for (cat_name in key_categories) {
      idx <- which(category_names == cat_name)
      if (length(idx) == 0) {
        msg <- sprintf("  [WARN] Category '%s' not found in file.cio", cat_name)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat_files <- cio_entries[[idx]]$files
        if (all(cat_files == "null")) {
          msg <- sprintf("  [ERROR] Category '%s' is all null in file.cio", cat_name)
          cat(msg, "\n")
          issues[[length(issues) + 1]] <- msg
        } else {
          cat(sprintf("  [OK] %-20s %s\n", cat_name,
                      paste(cat_files[cat_files != "null"], collapse = ", ")))
        }
      }
    }
  }
  
  # ── 10. Plant community checks ───────────────────────────────────────────
  cat("\n-- 10. Plant community checks --\n")
  
  plt <- read_swat("plants.plt")
  lum <- read_swat("landuse.lum")
  
  if (!is.null(hru_data) && !is.null(lum)) {
    # Check lu_mgt pointers in hru-data.hru exist in landuse.lum
    missing_lum <- setdiff(hru_data$lu_mgt, lum$name)
    if (length(missing_lum) > 0) {
      msg <- sprintf("  [ERROR] %d lu_mgt value(s) in hru-data.hru missing from landuse.lum: %s",
                     length(missing_lum),
                     paste(head(missing_lum, 5), collapse = ", "))
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] All %d hru-data.hru lu_mgt names found in landuse.lum\n",
                  length(unique(hru_data$lu_mgt))))
    }
  }
  
  if (!is.null(lum)) {
    
    # Detect whether plnt_com is integer IDs or names — IDs indicate a writer bug
    plnt_col <- if ("plnt_com" %in% names(lum)) {
      "plnt_com"
    } else if ("plnt_com_id" %in% names(lum)) {
      "plnt_com_id"
    } else {
      NULL
    }
    
    if (is.null(plnt_col)) {
      msg <- "  [WARN] Neither 'plnt_com' nor 'plnt_com_id' column found in landuse.lum"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else if (plnt_col == "plnt_com_id" || is.numeric(lum[[plnt_col]])) {
      msg <- sprintf(
        "  [ERROR] landuse.lum column '%s' contains integer IDs instead of plant community names - writer must resolve IDs to names before writing",
        plnt_col
      )
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat("  [OK] landuse.lum plnt_com column contains names (not integer IDs)\n")
    }
  }
  
  if (!is.null(plt)) {
    # Check for duplicate plant codes
    dupes <- plt$name[duplicated(plt$name)]
    if (length(dupes) > 0) {
      msg <- sprintf("  [ERROR] %d duplicate plant codes in plants.plt: %s",
                     length(dupes), paste(head(dupes, 5), collapse = ", "))
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] plants.plt: %d unique plant codes, no duplicates\n", nrow(plt)))
    }
    
    # Info only - show available urban/water codes
    urban_water_codes <- plt$name[grepl("^ur|^wat", plt$name, ignore.case = TRUE)]
    cat(sprintf("  [INFO] Available urban/water codes in plants.plt: %s\n",
                paste(urban_water_codes, collapse = ", ")))
  }
  
  # ── Summary ───────────────────────────────────────────────────────────────
  cat("\n=== Summary ===\n")
  if (length(issues) == 0) {
    cat("No issues found. If SWAT+ still crashes, check diagnostics.out for runtime errors.\n")
  } else {
    cat(sprintf("%d issue(s) found:\n", length(issues)))
    for (i in seq_along(issues)) {
      cat(sprintf("  %d. %s\n", i, issues[[i]]))
    }
  }
  
  invisible(issues)
}

read_wgn_cli <- function(output_dir) {
  
  path <- file.path(output_dir, "weather-wgn.cli")
  
  if (!file.exists(path)) {
    stop("weather-wgn.cli not found in: ", output_dir)
  }
  
  lines <- readLines(path)
  
  # Skip the first title line
  lines <- lines[-1]
  
  # Monthly variable names come from the header row of the first station
  # Header rows start with whitespace + "tmp_max_ave..."
  # Station rows contain the station name (no leading space) + coords + n_months
  
  # Identify line types
  is_header  <- grepl("^\\s+tmp_max_ave", lines)
  is_station <- !is_header & nchar(trimws(lines)) > 0
  is_monthly <- !is_header & !is_station & nchar(trimws(lines)) > 0
  
  # Better approach: parse block by block
  # Station line pattern: name, lat, lon, elev, n_months (no leading whitespace on name)
  is_station_line <- grepl("^\\S+\\s+-?\\d+\\.\\d+\\s+-?\\d+\\.\\d+", lines)
  is_header_line  <- grepl("^\\s+tmp_max_ave", lines)
  is_data_line    <- !is_station_line & !is_header_line & nchar(trimws(lines)) > 0
  
  station_idx <- which(is_station_line)
  n_stations  <- length(station_idx)
  
  # Parse monthly variable names from first header line
  first_header <- lines[which(is_header_line)[1]]
  mon_vars <- strsplit(trimws(first_header), "\\s+")[[1]]
  
  # Build results
  stations <- vector("list", n_stations)
  
  for (i in seq_len(n_stations)) {
    
    # Parse station metadata line
    sta_line <- trimws(lines[station_idx[i]])
    sta_parts <- strsplit(sta_line, "\\s+")[[1]]
    
    name      <- sta_parts[1]
    lat       <- as.numeric(sta_parts[2])
    lon       <- as.numeric(sta_parts[3])
    elev      <- as.numeric(sta_parts[4])
    n_months  <- as.integer(sta_parts[5])
    
    # Data lines follow the header line after the station line
    # Structure per station: station line, header line, 12 data lines
    data_start <- station_idx[i] + 2  # skip station + header lines
    data_end   <- data_start + n_months - 1
    
    monthly_lines <- lines[data_start:data_end]
    
    monthly_vals <- lapply(monthly_lines, function(l) {
      as.numeric(strsplit(trimws(l), "\\s+")[[1]])
    })
    
    monthly_df <- as.data.frame(do.call(rbind, monthly_vals))
    names(monthly_df) <- mon_vars
    monthly_df$month  <- seq_len(n_months)
    monthly_df$name   <- name
    
    stations[[i]] <- list(
      name     = name,
      lat      = lat,
      lon      = lon,
      elev     = elev,
      n_months = n_months,
      monthly  = monthly_df
    )
  }
  
  # Also return a flat summary table (one row per station)
  summary_tbl <- data.frame(
    name     = sapply(stations, `[[`, "name"),
    lat      = sapply(stations, `[[`, "lat"),
    lon      = sapply(stations, `[[`, "lon"),
    elev     = sapply(stations, `[[`, "elev"),
    n_months = sapply(stations, `[[`, "n_months")
  )
  
  list(
    stations    = stations,
    summary     = summary_tbl,
    n_stations  = n_stations
  )
}
