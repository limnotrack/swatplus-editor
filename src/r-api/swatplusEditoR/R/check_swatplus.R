#' Check SWAT+ input file consistency
#'
#' @inheritParams write_config_files
#'
#' @returns A list of issues found, or an empty list if no issues detected. Each issue is a character string describing the problem.
#' @export
#'

check_swatplus <- function(project, output_dir) {
  
  issues <- list()
  
  cat("=== SWAT+ Connectivity Check ===\n\n")
  
  # ── 1. Object counts ──────────────────────────────────────────────────────
  cat("-- 1. Object counts (object.cnt vs actual file rows) --\n")
  
  cnt <- read_swat("object.cnt", output_dir)
  
  if (!is.null(cnt)) {
    checks <- list(
      hru     = list(file = "hru-data.hru", col = "hru"),
      rtu     = list(file = "rout_unit.ele", col = "hru"),
      aqu     = list(file = "aquifer.aqu",   col = "aqu"),
      cha     = list(file = "chandeg.con",   col = "lcha")
    )
    
    for (nm in names(checks)) {
      chk  <- checks[[nm]]
      dat  <- read_swat(chk$file, output_dir, skip = 1)
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
  
  hru_con  <- read_swat("hru.con", output_dir)
  hru_data <- read_swat("hru-data.hru", output_dir)
  
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
  
  rtu_con <- read_swat("rout_unit.con", output_dir)
  rtu_ele <- read_swat("rout_unit.ele", output_dir)
  
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
  
  cha_con <- read_swat("chandeg.con", output_dir)
  aqu_con <- read_swat("aquifer.con", output_dir)
  
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
  
  sta <- read_swat("weather-sta.cli", output_dir)
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
  
  plt <- read_swat("plants.plt", output_dir)
  lum <- read_swat("landuse.lum", output_dir)
  
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
  
  # ── 11. HRU routing element checks ───────────────────────────────────────
  cat("\n-- 11. HRU routing element checks (rout_unit.ele) --\n")
  
  if (!is.null(rtu_ele) && !is.null(hru_data)) {
    # rout_unit.ele columns: id, name, obj_typ, obj_id, frac, dlr
    hru_eles <- rtu_ele[rtu_ele$obj_typ == "hru", , drop = FALSE]
    
    if (nrow(hru_eles) == 0) {
      msg <- "  [WARN] No HRU elements found in rout_unit.ele"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      # Check all obj_id values reference valid HRU IDs (sequential 1..n)
      max_hru_id <- nrow(hru_data)
      bad_ids <- hru_eles$obj_id[hru_eles$obj_id < 1 | hru_eles$obj_id > max_hru_id]
      if (length(bad_ids) > 0) {
        msg <- sprintf(
          "  [ERROR] %d rout_unit.ele HRU elements have obj_id outside valid range [1, %d]",
          length(bad_ids), max_hru_id)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] All %d HRU element obj_ids are valid\n", nrow(hru_eles)))
      }
    }
  }
  
  # ── 12. Every HRU appears in rout_unit.ele ───────────────────────────────
  cat("\n-- 12. Every HRU accounted for in routing elements --\n")
  
  if (!is.null(rtu_ele) && !is.null(hru_con)) {
    hru_eles <- rtu_ele[rtu_ele$obj_typ == "hru", , drop = FALSE]
    hru_names_in_ele <- hru_eles$name
    hru_names_in_con <- hru_con[[1]]
    
    missing_from_ele <- setdiff(hru_names_in_con, hru_names_in_ele)
    if (length(missing_from_ele) > 0) {
      msg <- sprintf("  [ERROR] %d HRU(s) in hru.con not found in rout_unit.ele",
                     length(missing_from_ele))
      cat(msg, "\n")
      cat("  First 5:", paste(head(missing_from_ele, 5), collapse = ", "), "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] All %d HRUs from hru.con appear in rout_unit.ele\n",
                  length(hru_names_in_con)))
    }
  }
  
  # ── 13. rout_unit.ele fraction sums per RTU ──────────────────────────────
  cat("\n-- 13. Routing unit element fraction sums --\n")
  
  rtu_def <- read_swat("rout_unit.def", output_dir)
  
  if (!is.null(rtu_ele) && !is.null(rtu_def)) {
    # rtu_ele has: id, name, obj_typ, obj_id, frac, dlr
    # Group fractions and check they sum to ~1.0 (within tolerance)
    # Since rout_unit.ele doesn't directly have a rtu_id, 
    # use the rout_unit.def to map element ranges back to RTUs
    # Simpler: sum all fractions across all elements
    frac_sum <- sum(rtu_ele$frac, na.rm = TRUE)
    n_rtus <- nrow(rtu_def)
    
    if (n_rtus > 0 && abs(frac_sum - n_rtus) > 0.01 * n_rtus) {
      msg <- sprintf(
        "  [WARN] Total fraction sum across all rout_unit.ele = %.4f, expected ~%d (one per RTU)",
        frac_sum, n_rtus)
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else if (n_rtus > 0) {
      cat(sprintf("  [OK] Total fraction sum = %.4f across %d RTUs (avg %.4f per RTU)\n",
                  frac_sum, n_rtus, frac_sum / n_rtus))
    }
  }
  
  # ── 14. Outflow connectivity in connection files ─────────────────────────
  cat("\n-- 14. Connection outflow validation --\n")
  
  # Parse connection files to check outflow targets
  # Connection files have variable-width rows: base columns + repeated outflow groups
  # Base: id, name, gis_id, area, lat, lon, elev, elem, wst, cst, ovfl, rule, out_tot
  # Then: [obj_typ, obj_id, hyd_typ, frac] repeated out_tot times
  
  .parse_con_outflows <- function(con_file) {
    dat <- read_swat(con_file, output_dir)
    if (is.null(dat)) return(NULL)
    # The out_tot column tells how many outflow groups follow
    if (!"out_tot" %in% names(dat)) return(NULL)
    
    # Connection files are wide-format; outflow columns appear after out_tot
    # Column positions: 1=id, 2=name, ..., 13=out_tot, 14=obj_typ, 15=obj_id, ...
    outs <- list()
    out_tot_col <- which(names(dat) == "out_tot")
    if (length(out_tot_col) == 0) return(NULL)
    
    # Check if there are columns after out_tot (outflow data)
    remaining_cols <- ncol(dat) - out_tot_col
    if (remaining_cols < 4) return(NULL)
    
    # Extract obj_typ and obj_id from the first outflow group
    obj_typ_col <- out_tot_col + 1
    obj_id_col  <- out_tot_col + 2
    
    data.frame(
      con_name = dat[[2]],
      out_tot  = dat$out_tot,
      obj_typ  = dat[[obj_typ_col]],
      obj_id   = dat[[obj_id_col]],
      stringsAsFactors = FALSE
    )
  }
  
  # Check hru.con outflows 
  hru_outs <- .parse_con_outflows("hru.con")
  if (!is.null(hru_outs)) {
    n_with_outs <- sum(hru_outs$out_tot > 0, na.rm = TRUE)
    n_total <- nrow(hru_outs)
    if (n_with_outs == 0 && n_total > 0) {
      cat(sprintf("  [INFO] hru.con: %d connections, none have outflow targets (normal for standard mode)\n", n_total))
    } else {
      cat(sprintf("  [OK] hru.con: %d of %d connections have outflow targets\n",
                  n_with_outs, n_total))
    }
  }
  
  # Check rout_unit.con outflows
  rtu_outs <- .parse_con_outflows("rout_unit.con")
  if (!is.null(rtu_outs) && !is.null(cha_con)) {
    rtu_with_outs <- rtu_outs[rtu_outs$out_tot > 0, , drop = FALSE]
    n_with_outs <- nrow(rtu_with_outs)
    n_total <- nrow(rtu_outs)
    
    if (n_with_outs == 0 && n_total > 0) {
      msg <- "  [WARN] rout_unit.con: no connections have outflow targets - routing may be broken"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      # Check obj_typ values are valid
      valid_obj_typs <- c("sdc", "ru", "aqu", "res", "hru")
      bad_typs <- setdiff(unique(rtu_with_outs$obj_typ), valid_obj_typs)
      if (length(bad_typs) > 0) {
        msg <- sprintf("  [WARN] rout_unit.con has unknown obj_typ values: %s",
                       paste(bad_typs, collapse = ", "))
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] rout_unit.con: %d of %d connections have valid outflow targets\n",
                    n_with_outs, n_total))
      }
    }
  }
  
  # Check chandeg.con outflows
  cha_outs <- .parse_con_outflows("chandeg.con")
  if (!is.null(cha_outs)) {
    cha_with_outs <- cha_outs[cha_outs$out_tot > 0, , drop = FALSE]
    n_with_outs <- nrow(cha_with_outs)
    n_total <- nrow(cha_outs)
    
    # Exactly one channel should have out_tot == 0 (the outlet)
    n_outlets <- sum(cha_outs$out_tot == 0, na.rm = TRUE)
    if (n_outlets == 0 && n_total > 0) {
      msg <- "  [WARN] chandeg.con: no outlet channel found (all channels have outflow targets)"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else if (n_outlets > 1) {
      msg <- sprintf("  [WARN] chandeg.con: %d channels have no outflow (expected 1 outlet)", n_outlets)
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] chandeg.con: %d channels with outflows, %d outlet(s)\n",
                  n_with_outs, n_outlets))
    }
  }
  
  # ── 15. HRU hydrology_hyd consistency ──────────────────────────────────────
  cat("\n-- 15. HRU hydrology consistency (hru-data.hru vs hydrology.hyd) --\n")
  
  hyd_file <- read_swat("hydrology.hyd", output_dir)
  
  if (!is.null(hru_data) && !is.null(hyd_file)) {
    # hru-data.hru has a hydro column that references hydrology.hyd names
    hydro_col <- if ("hydro" %in% names(hru_data)) "hydro" else
      if ("hydro_id" %in% names(hru_data)) "hydro_id" else NULL
    
    if (!is.null(hydro_col)) {
      n_hrus <- nrow(hru_data)
      n_hyds <- nrow(hyd_file)
      if (n_hyds < n_hrus) {
        msg <- sprintf(
          "  [WARN] Only %d hydrology.hyd entries but %d HRUs - some HRUs lack hydrology",
          n_hyds, n_hrus)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] %d hydrology.hyd entries cover %d HRUs\n", n_hyds, n_hrus))
      }
    } else {
      cat("  [SKIP] Could not find hydro/hydro_id column in hru-data.hru\n")
    }
  }
  
  # ── 16. HRU topography consistency ──────────────────────────────────────
  cat("\n-- 16. HRU topography consistency (hru-data.hru vs topography.hyd) --\n")
  
  topo_file <- read_swat("topography.hyd", output_dir)
  
  if (!is.null(hru_data) && !is.null(topo_file)) {
    # Each HRU should have a topo entry
    topo_col <- if ("topo" %in% names(hru_data)) "topo" else
      if ("topo_id" %in% names(hru_data)) "topo_id" else NULL
    
    if (!is.null(topo_col)) {
      # Check that referenced topo names exist
      if (is.character(hru_data[[topo_col]])) {
        missing_topo <- setdiff(hru_data[[topo_col]], topo_file$name)
        if (length(missing_topo) > 0) {
          msg <- sprintf(
            "  [ERROR] %d topo references in hru-data.hru missing from topography.hyd",
            length(missing_topo))
          cat(msg, "\n")
          cat("  First 5:", paste(head(missing_topo, 5), collapse = ", "), "\n")
          issues[[length(issues) + 1]] <- msg
        } else {
          cat(sprintf("  [OK] All %d HRU topo references found in topography.hyd\n",
                      nrow(hru_data)))
        }
      } else {
        # ID-based reference: count check
        n_hru_topos <- sum(grepl("hru", topo_file$name, ignore.case = TRUE))
        n_rtu_topos <- nrow(topo_file) - n_hru_topos
        cat(sprintf("  [INFO] topography.hyd: %d total entries (%d RTU-level, %d HRU-level)\n",
                    nrow(topo_file), n_rtu_topos, n_hru_topos))
        if (n_hru_topos < nrow(hru_data)) {
          msg <- sprintf(
            "  [WARN] Only %d HRU topography entries but %d HRUs",
            n_hru_topos, nrow(hru_data))
          cat(msg, "\n")
          issues[[length(issues) + 1]] <- msg
        }
      }
    } else {
      cat("  [SKIP] Could not find topo/topo_id column in hru-data.hru\n")
    }
  }
  
  # ── 17. ls_unit_ele completeness ──────────────────────────────────────────
  cat("\n-- 17. Landscape unit element completeness (ls_unit.ele) --\n")
  
  ls_ele_file <- read_swat("ls_unit.ele", output_dir)
  
  if (!is.null(ls_ele_file) && !is.null(hru_data)) {
    n_ls_eles <- nrow(ls_ele_file)
    n_hrus_expected <- nrow(hru_data)
    
    if (n_ls_eles != n_hrus_expected) {
      msg <- sprintf(
        "  [WARN] ls_unit.ele has %d entries but expected %d (one per HRU)",
        n_ls_eles, n_hrus_expected)
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat(sprintf("  [OK] ls_unit.ele has %d entries matching %d HRUs\n",
                  n_ls_eles, n_hrus_expected))
    }
    
    # Check bsn_frac sums to ~1.0
    if ("bsn_frac" %in% names(ls_ele_file)) {
      bsn_sum <- sum(ls_ele_file$bsn_frac, na.rm = TRUE)
      if (abs(bsn_sum - 1.0) > 0.01) {
        msg <- sprintf(
          "  [WARN] ls_unit.ele bsn_frac sums to %.4f (expected ~1.0)", bsn_sum)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] ls_unit.ele bsn_frac sums to %.4f\n", bsn_sum))
      }
    }
  }
  
  # ── 18. HRU routing: hru_con columns ────────────────────────────────────
  cat("\n-- 18. HRU routing: hru.con structure --\n")
  
  if (!is.null(hru_con)) {
    # Check that hru_con has the hru_id / hru column (FK to hru_data_hru)
    has_hru_fk <- any(c("hru", "hru_id") %in% names(hru_con))
    if (!has_hru_fk) {
      msg <- "  [WARN] hru.con missing 'hru' / 'hru_id' column - HRU foreign key not set"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      cat("  [OK] hru.con has HRU foreign key column\n")
    }
    
    # Check that hru_con has wst column (weather station assignment)
    has_wst <- any(c("wst", "wst_id") %in% names(hru_con))
    if (!has_wst) {
      msg <- "  [WARN] hru.con missing 'wst' / 'wst_id' column - weather station not assigned"
      cat(msg, "\n")
      issues[[length(issues) + 1]] <- msg
    } else {
      # Check if any wst values are actually set (non-NA, non-null)
      wst_col <- if ("wst" %in% names(hru_con)) "wst" else "wst_id"
      n_wst_set <- sum(!is.na(hru_con[[wst_col]]) & hru_con[[wst_col]] != "null", na.rm = TRUE)
      if (n_wst_set == 0) {
        cat(sprintf("  [INFO] hru.con: wst column present but no weather stations assigned (%d HRUs)\n",
                    nrow(hru_con)))
      } else {
        cat(sprintf("  [OK] hru.con: %d of %d HRUs have weather stations assigned\n",
                    n_wst_set, nrow(hru_con)))
      }
    }
    
    # Check area > 0
    if ("area" %in% names(hru_con)) {
      zero_area <- sum(hru_con$area <= 0 | is.na(hru_con$area))
      if (zero_area > 0) {
        msg <- sprintf("  [WARN] %d HRU(s) in hru.con have zero or missing area", zero_area)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] All %d HRUs have positive area\n", nrow(hru_con)))
      }
    }
  }
  
  # ── 19. HRU routing: rout_unit_ele references valid RTUs ────────────────
  cat("\n-- 19. HRU routing: rout_unit.ele RTU references --\n")
  
  if (!is.null(rtu_ele) && !is.null(rtu_con)) {
    hru_eles <- rtu_ele[rtu_ele$obj_typ == "hru", , drop = FALSE]
    
    if (nrow(hru_eles) > 0) {
      # rtu_ele doesn't have rtu_id in the file output, but we can check
      # that each HRU name appears and fraction is valid
      bad_frac <- sum(is.na(hru_eles$frac) | hru_eles$frac <= 0 | hru_eles$frac > 1.0001)
      if (bad_frac > 0) {
        msg <- sprintf("  [WARN] %d HRU element(s) in rout_unit.ele have invalid fraction",
                       bad_frac)
        cat(msg, "\n")
        issues[[length(issues) + 1]] <- msg
      } else {
        cat(sprintf("  [OK] All %d HRU elements have valid fractions (0, 1]\n",
                    nrow(hru_eles)))
      }
    }
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
