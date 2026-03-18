#' Set up a SWAT+ project database
#'
#' Mirrors the functionality of \code{src/api/actions/setup_project.py} and
#' \code{src/api/database/project/setup.py}.
#' @keywords internal
NULL

# ===========================================================================
# Main entry point
# ===========================================================================

#' Set up (or re-set-up) a SWAT+ project database
#'
#' Creates all necessary tables, copies reference data from a datasets
#' database, and optionally runs GIS import.
#'
#' Mirrors \code{SetupProject} in
#' \code{src/api/actions/setup_project.py}.
#'
#' @param project_db      Path to the project \code{.sqlite} file (will be
#'   created if it does not exist).
#' @param datasets_db     Path to the SWAT+ datasets \code{.sqlite} file.
#'   Required when creating a new project; can be omitted if
#'   \code{project_config} already exists in the project DB.
#' @param editor_version  Version string of the editor (e.g. \code{"2.3.0"}).
#' @param project_name    Name for the project (used in \code{object_cnt}).
#' @param project_description Optional description stored in \code{object_cnt}.
#' @param is_lte          If \code{TRUE} use the LTE variant of SWAT+.
#'   Default \code{FALSE}.
#' @param constant_ps     If \code{TRUE} use constant point source values.
#'   Default \code{TRUE}.
#' @param overwrite_plants If \code{TRUE} overwrite existing plant data.
#'   Default \code{FALSE}.
#' @param run_gis_import  If \code{TRUE} and GIS tables are populated, run
#'   \code{\link{import_gis}} after setup.  Default \code{TRUE}.
#' @param verbose         Print progress messages.  Default \code{TRUE}.
#' @return Invisibly, the path to the project database.
#' @export
setup_project <- function(project_db,
                          datasets_db       = NULL,
                          editor_version    = NULL,
                          project_name      = NULL,
                          project_description = NULL,
                          is_lte            = FALSE,
                          constant_ps       = TRUE,
                          overwrite_plants  = FALSE,
                          run_gis_import    = TRUE,
                          verbose           = TRUE) {

  # If datasets_db not provided, read it from existing project_config
  if (is.null(datasets_db)) {
    if (!file.exists(project_db))
      stop("No datasets_db provided and project_db does not exist.")
    con_tmp <- swat_open_db(project_db)
    if (!swat_exists_table(con_tmp, "project_config")) {
      swat_close_db(con_tmp)
      stop("No datasets_db provided and project_config table does not exist.")
    }
    cfg <- DBI::dbGetQuery(con_tmp, "SELECT * FROM project_config LIMIT 1")
    swat_close_db(con_tmp)
    if (nrow(cfg) == 0L) stop("project_config table is empty.")
    datasets_db <- full_path(project_db, cfg$reference_db)
    if (is.null(project_name) || is.na(project_name))
      project_name <- cfg$project_name
  }

  if (!file.exists(datasets_db))
    stop("Datasets database not found: ", datasets_db)

  if (verbose) emit_progress(5, "Creating database tables...")
  create_project_db(project_db, overwrite = FALSE)

  con <- swat_open_db(project_db)
  on.exit(swat_close_db(con), add = TRUE)

  if (verbose) emit_progress(10, "Creating database tables...")
  create_project_tables(con)

  if (verbose) emit_progress(50, "Copying data from SWAT+ datasets database...")
  .initialize_project_data(
    project_con   = con,
    project_db    = project_db,
    datasets_db   = datasets_db,
    project_name  = project_description %||% project_name,
    is_lte        = is_lte,
    overwrite_plants = overwrite_plants
  )

  # Update / create project_config
  existing_cfg <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM project_config")$n
  rel_proj_db  <- basename(project_db)
  rel_ds_db    <- tryCatch(
    tools::file_path_as_absolute(datasets_db) |>
      (\(x) sub(paste0("^", dirname(tools::file_path_as_absolute(project_db)), .Platform$file.sep), "", x))(),
    error = function(e) basename(datasets_db)
  )

  if (existing_cfg == 0L) {
    DBI::dbExecute(con, "
      INSERT INTO project_config (project_name, project_db, reference_db,
        editor_version, is_lte, imported_gis, delineation_done, hrus_done)
      VALUES (?, ?, ?, ?, ?, 0, 0, 0)",
      params = list(project_name, rel_proj_db, rel_ds_db,
                    editor_version, as.integer(is_lte)))
  } else {
    DBI::dbExecute(con, "
      UPDATE project_config SET
        project_name    = ?,
        project_db      = ?,
        reference_db    = ?,
        editor_version  = ?,
        is_lte          = ?",
      params = list(project_name, rel_proj_db, rel_ds_db,
                    editor_version, as.integer(is_lte)))
  }

  if (verbose) emit_progress(80, "Project setup complete.")

  # Run GIS import if requested and GIS data exists
  if (run_gis_import && swat_exists_table(con, "gis_subbasins") &&
      swat_count(con, "gis_subbasins") > 0L) {
    swat_close_db(con)
    on.exit(NULL)
    import_gis(project_db,
               delete_existing = TRUE,
               constant_ps     = constant_ps,
               is_lte          = is_lte,
               verbose         = verbose)
  }

  if (verbose) emit_progress(100, "Done.")
  invisible(project_db)
}

# ===========================================================================
# Initialize project data from datasets database
# ===========================================================================

#' Copy reference data from datasets database into project database
#'
#' Mirrors \code{SetupProjectDatabase.initialize_data()} in
#' \code{src/api/database/project/setup.py}.
#'
#' @param project_con  An open \code{DBIConnection} to the project database.
#' @param project_db   Path to the project \code{.sqlite} file.
#' @param datasets_db  Path to the datasets \code{.sqlite} file.
#' @param project_name Project name string.
#' @param is_lte       Logical.
#' @param overwrite_plants Logical.
#' @return Invisibly \code{NULL}.
#' @export
initialize_project_data <- function(project_con,
                                    project_db,
                                    datasets_db,
                                    project_name      = NULL,
                                    is_lte            = FALSE,
                                    overwrite_plants  = FALSE) {
  .initialize_project_data(project_con, project_db, datasets_db,
                            project_name, is_lte, overwrite_plants)
}

.initialize_project_data <- function(project_con, project_db, datasets_db,
                                      project_name, is_lte, overwrite_plants) {

  # object_cnt
  if (swat_count(project_con, "object_cnt") < 1L) {
    DBI::dbExecute(project_con,
      "INSERT INTO object_cnt (name) VALUES (?)",
      params = list(project_name %||% "project"))
  } else {
    DBI::dbExecute(project_con,
      "UPDATE object_cnt SET name = ?",
      params = list(project_name %||% "project"))
  }

  # time_sim
  if (swat_count(project_con, "time_sim") < 1L) {
    DBI::dbExecute(project_con,
      "INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
       VALUES (0, 1980, 0, 1985, 0)")
  }

  # Tables to copy from datasets if empty in project
  copy_if_empty <- function(table) {
    if (swat_exists_table(project_con, table) &&
        swat_count(project_con, table) < 1L) {
      swat_copy_table(table, datasets_db, project_db)
    }
  }

  copy_if_empty("codes_bsn")
  copy_if_empty("parameters_bsn")

  # plants_plt
  if (overwrite_plants && swat_exists_table(project_con, "plants_plt")) {
    DBI::dbExecute(project_con, "DELETE FROM plants_plt")
  }
  copy_if_empty("plants_plt")

  copy_if_empty("urban_urb")

  if (!is_lte) {
    for (tbl in c("fertilizer_frt", "septic_sep", "snow_sno", "tillage_til",
                   "pesticide_pst", "cntable_lum", "ovn_table_lum",
                   "cons_prac_lum", "graze_ops", "harv_ops", "fire_ops",
                   "irr_ops", "sweep_ops", "chem_app_ops",
                   "bmpuser_str", "filterstrip_str", "grassedww_str",
                   "septic_str", "tiledrain_str", "cal_parms_cal")) {
      copy_if_empty(tbl)
    }
  }

  if (is_lte) {
    copy_if_empty("soils_lte_sol")
  }

  # Decision tables: copy selected tables only
  if (swat_count(project_con, "d_table_dtl") < 1L) {
    .copy_decision_tables(project_con, project_db, datasets_db, is_lte)
  }

  # Management schedules
  if (!is_lte && swat_exists_table(project_con, "management_sch") &&
      swat_count(project_con, "management_sch") < 1L) {
    swat_copy_table("management_sch",      datasets_db, project_db, include_id = TRUE)
    swat_copy_table("management_sch_auto", datasets_db, project_db, include_id = TRUE)
    swat_copy_table("management_sch_op",   datasets_db, project_db, include_id = TRUE)
  }

  # print_prt
  copy_if_empty("print_prt")
  .copy_print_prt_objects(project_con, project_db, datasets_db)

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Decision table copy
# ---------------------------------------------------------------------------

.copy_decision_tables <- function(project_con, project_db, datasets_db,
                                   is_lte) {
  ds_con <- swat_open_db(datasets_db)
  on.exit(swat_close_db(ds_con), add = TRUE)

  if (!swat_exists_table(ds_con, "d_table_dtl")) return(invisible(NULL))

  if (!is_lte) {
    valid_names <- c("corps_med_res1", "corps_med_res", "wetland",
                     "drawdown_days", "flood_season")
    valid_files <- c("lum.dtl", "res_rel.dtl")
    sql <- paste0(
      "SELECT * FROM d_table_dtl WHERE file_name IN ('lum.dtl','res_rel.dtl')",
      " AND (file_name != 'res_rel.dtl' OR name IN (",
      paste0("'", valid_names, "'", collapse = ","), "))"
    )
  } else {
    dt_names <- c("pl_grow_sum", "pl_end_sum", "pl_grow_win", "pl_end_win")
    sql <- paste0("SELECT * FROM d_table_dtl WHERE name IN (",
                  paste0("'", dt_names, "'", collapse = ","), ")")
  }

  dts <- DBI::dbGetQuery(ds_con, sql)
  if (nrow(dts) == 0L) return(invisible(NULL))

  for (i in seq_len(nrow(dts))) {
    dt <- dts[i, ]
    new_dt_id <- DBI::dbExecute(project_con,
      "INSERT INTO d_table_dtl (name, file_name, description) VALUES (?,?,?)",
      params = list(dt$name, dt$file_name, dt$description))
    new_dt_id <- DBI::dbGetQuery(project_con,
      "SELECT last_insert_rowid() AS id")$id

    # Copy conditions
    if (swat_exists_table(ds_con, "d_table_dtl_cond")) {
      conds <- DBI::dbGetQuery(ds_con,
        paste0("SELECT * FROM d_table_dtl_cond WHERE d_table_id=", dt$id))
      for (j in seq_len(nrow(conds))) {
        c <- conds[j, ]
        DBI::dbExecute(project_con,
          "INSERT INTO d_table_dtl_cond
           (d_table_id,var,obj,obj_num,lim_var,lim_op,lim_const,description)
           VALUES (?,?,?,?,?,?,?,?)",
          params = list(new_dt_id, c$var, c$obj, c$obj_num,
                        c$lim_var, c$lim_op, c$lim_const, c$description))
        new_cond_id <- DBI::dbGetQuery(project_con,
          "SELECT last_insert_rowid() AS id")$id
        if (swat_exists_table(ds_con, "d_table_dtl_cond_alt")) {
          alts <- DBI::dbGetQuery(ds_con,
            paste0("SELECT * FROM d_table_dtl_cond_alt WHERE cond_id=", c$id))
          for (k in seq_len(nrow(alts))) {
            DBI::dbExecute(project_con,
              "INSERT INTO d_table_dtl_cond_alt (cond_id, alt) VALUES (?,?)",
              params = list(new_cond_id, alts$alt[[k]]))
          }
        }
      }
    }

    # Copy actions
    if (swat_exists_table(ds_con, "d_table_dtl_act")) {
      acts <- DBI::dbGetQuery(ds_con,
        paste0("SELECT * FROM d_table_dtl_act WHERE d_table_id=", dt$id))
      for (j in seq_len(nrow(acts))) {
        a <- acts[j, ]
        DBI::dbExecute(project_con,
          "INSERT INTO d_table_dtl_act
           (d_table_id,act_typ,obj,obj_num,name,option,const,const2,fp)
           VALUES (?,?,?,?,?,?,?,?,?)",
          params = list(new_dt_id, a$act_typ, a$obj, a$obj_num,
                        a$name, a$option, a$const, a$const2, a$fp))
        new_act_id <- DBI::dbGetQuery(project_con,
          "SELECT last_insert_rowid() AS id")$id
        if (swat_exists_table(ds_con, "d_table_dtl_act_out")) {
          outs <- DBI::dbGetQuery(ds_con,
            paste0("SELECT * FROM d_table_dtl_act_out WHERE act_id=", a$id))
          for (k in seq_len(nrow(outs))) {
            DBI::dbExecute(project_con,
              "INSERT INTO d_table_dtl_act_out (act_id, outcome) VALUES (?,?)",
              params = list(new_act_id, outs$outcome[[k]]))
          }
        }
      }
    }
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Print prt objects copy
# ---------------------------------------------------------------------------

.copy_print_prt_objects <- function(project_con, project_db, datasets_db) {
  prt_id_row <- DBI::dbGetQuery(project_con,
    "SELECT id FROM print_prt LIMIT 1")
  if (nrow(prt_id_row) == 0L) return(invisible(NULL))
  prt_id <- prt_id_row$id

  ds_con <- swat_open_db(datasets_db)
  on.exit(swat_close_db(ds_con), add = TRUE)

  src_table <- if (swat_exists_table(ds_con, "print_prt_object"))
    "print_prt_object"
  else if (swat_exists_table(ds_con, "print_prt_objects"))
    "print_prt_objects"
  else return(invisible(NULL))

  objs <- DBI::dbGetQuery(ds_con,
    paste0("SELECT * FROM ", src_table, " ORDER BY id"))
  if (nrow(objs) == 0L) return(invisible(NULL))

  new_rows <- lapply(seq_len(nrow(objs)), function(i) {
    o <- objs[i, ]
    existing <- DBI::dbGetQuery(project_con,
      paste0("SELECT id FROM print_prt_object WHERE name='", o$name, "'"))
    if (nrow(existing) > 0L) return(NULL)
    list(
      print_prt_id = prt_id,
      name    = o$name,
      daily   = if ("daily"   %in% names(o)) o$daily   else 0L,
      monthly = if ("monthly" %in% names(o)) o$monthly else 0L,
      yearly  = if ("yearly"  %in% names(o)) o$yearly  else 0L,
      avann   = if ("avann"   %in% names(o)) o$avann   else 0L
    )
  })
  new_rows <- Filter(Negate(is.null), new_rows)
  if (length(new_rows) > 0L) {
    swat_bulk_insert(project_con, "print_prt_object",
                     do.call(rbind, lapply(new_rows, as.data.frame,
                                           stringsAsFactors = FALSE)))
  }
  invisible(NULL)
}

# Null-coalescing operator is defined in utils.R and available package-wide.
