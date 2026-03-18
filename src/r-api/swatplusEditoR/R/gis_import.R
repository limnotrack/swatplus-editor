#' Import GIS data into SWAT+ project database tables
#'
#' Mirrors the functionality of \code{src/api/actions/import_gis.py}.  Reads
#' the \code{gis_*} tables already present in the project database (populated
#' by \code{\link{write_gis_to_db}} or an external tool such as QSWAT+) and
#' generates all the corresponding SWAT+ model object tables
#' (\code{routing_unit}, \code{channel_lte_cha}, \code{reservoir_res},
#' \code{recall_rec}, \code{hru_data_hru}, \code{aquifer_aqu},
#' connection tables, etc.).
#'
#' @keywords internal
NULL

# Route category constants (mirrors Python RouteCat class)
ROUTE_CAT <- list(
  LSU    = "LSU",
  PT     = "PT",
  CH     = "CH",
  WTR    = "WTR",
  SUB    = "SUB",
  HRU    = "HRU",
  OUTLET = "X",
  RES    = "RES",
  PND    = "PND",
  AQU    = "AQU",
  DAQ    = "DAQ"
)

WATER_TYPES <- c("WTR", "RES", "PND")

# ===========================================================================
# Main entry point
# ===========================================================================

#' Import GIS tables into SWAT+ connection tables
#'
#' The primary entry-point for GIS import.  Mirrors
#' \code{GisImport.insert_default()} in
#' \code{src/api/actions/import_gis.py}.
#'
#' @param project_db     Path to the SWAT+ project \code{.sqlite} database.
#'   The \code{gis_*} tables must already be populated.
#' @param delete_existing If \code{TRUE} (and GIS has not yet been imported),
#'   delete any existing SWAT+ object data before re-importing.  Default
#'   \code{FALSE}.
#' @param constant_ps    If \code{TRUE}, treat point sources as constant
#'   (type 4).  Default \code{TRUE}.
#' @param is_lte         If \code{TRUE}, use the LTE (Landscape Threshold
#'   Erosion) version of the SWAT+ schema.  Default \code{FALSE}.
#' @param verbose        Print progress messages.  Default \code{TRUE}.
#' @return Invisibly, \code{TRUE} on success.
#' @export
import_gis <- function(project_db,
                       delete_existing = FALSE,
                       constant_ps     = TRUE,
                       is_lte          = FALSE,
                       verbose         = TRUE) {
  con <- swat_open_db(project_db)
  on.exit(swat_close_db(con), add = TRUE)

  # Check that gis_subbasins has data
  if (swat_count(con, "gis_subbasins") == 0L) {
    if (verbose) message("No GIS data to import (gis_subbasins is empty).")
    return(invisible(FALSE))
  }

  if (delete_existing) {
    if (verbose) emit_progress(5, "Deleting existing connections before importing from GIS...")
    .delete_existing_gis_data(con)
  }

  # Mapping tables: gis ID -> new SWAT+ sequential ID
  env <- new.env(parent = emptyenv())
  env$gis_to_rtu_ids      <- integer(0)
  env$gis_to_cha_ids      <- integer(0)
  env$gis_to_res_ids      <- integer(0)
  env$gis_to_hru_ids      <- integer(0)
  env$gis_to_aqu_ids      <- integer(0)
  env$gis_to_deep_aqu_ids <- integer(0)

  tryCatch({
    if (is_lte) {
      if (verbose) emit_progress(15, "Importing channels (LTE) from GIS...")
      .insert_om_water(con)
      .insert_channels_lte(con, env)

      if (verbose) emit_progress(40, "Importing landscape units (LTE) from GIS...")
      .insert_lsus_lte(con, env)

      if (verbose) emit_progress(50, "Importing HRUs (LTE) from GIS...")
      .insert_hru_ltes(con, env)

      if (verbose) emit_progress(90, "Importing connections (LTE) from GIS...")
      .insert_connections_lte(con, env)

    } else {
      if (verbose) emit_progress(15, "Importing routing units from GIS...")
      .insert_routing_units(con, env)

      if (verbose) emit_progress(30, "Importing channels from GIS...")
      .insert_om_water(con)
      .insert_channels_lte(con, env)

      if (verbose) emit_progress(40, "Importing reservoirs from GIS...")
      .insert_reservoirs(con, env)

      if (verbose) emit_progress(50, "Importing point sources from GIS...")
      .insert_recall(con, env, constant_ps)

      if (verbose) emit_progress(60, "Importing HRUs from GIS...")
      .insert_hrus(con, env)

      if (verbose) emit_progress(75, "Importing aquifers from GIS...")
      .insert_aquifers(con, env)

      if (verbose) emit_progress(90, "Importing connections from GIS...")
      .insert_connections(con, env)

      if (verbose) emit_progress(95, "Creating default landscape units...")
      .insert_lsus(con, env)
    }

    # Mark GIS as imported in project_config
    if (swat_exists_table(con, "project_config")) {
      DBI::dbExecute(con, "UPDATE project_config SET imported_gis = 1")
    }

    if (verbose) emit_progress(100, "GIS import complete.")
  }, error = function(e) {
    stop("GIS import failed: ", conditionMessage(e))
  })

  invisible(TRUE)
}

# ===========================================================================
# Routing units
# ===========================================================================

.insert_routing_units <- function(con, env) {
  if (swat_count(con, "rout_unit_rtu") > 0L) return(invisible(NULL))

  lsus <- swat_query(con, "SELECT * FROM gis_lsus ORDER BY id")
  if (nrow(lsus) == 0L) return(invisible(NULL))

  cnt <- max(lsus$id)
  topography <- vector("list", nrow(lsus))
  fields     <- vector("list", nrow(lsus))
  rtu_list   <- vector("list", nrow(lsus))
  rtu_cons   <- vector("list", nrow(lsus))

  for (i in seq_len(nrow(lsus))) {
    row <- lsus[i, ]
    new_id    <- i
    env$gis_to_rtu_ids[[as.character(row$id)]] <- new_id
    rtu_name  <- get_swat_name("rtu", row$id, cnt)
    slp_len   <- .get_slope_len(row$slope)

    topography[[i]] <- list(
      id       = new_id,
      name     = get_swat_name("toportu", row$id, cnt),
      slp      = row$slope / 100,
      slp_len  = slp_len,
      lat_len  = slp_len,
      dist_cha = 121.0,
      depos    = 0,
      type     = "sub"
    )
    fields[[i]] <- list(
      id   = new_id,
      name = get_swat_name("fld", row$id, cnt),
      len  = 500,
      wd   = 100,
      ang  = 30
    )
    rtu_list[[i]] <- list(
      id     = new_id,
      name   = rtu_name,
      topo_id  = new_id,
      field_id = new_id
    )
    rtu_cons[[i]] <- list(
      id     = new_id,
      rtu_id = new_id,
      name   = rtu_name,
      gis_id = row$id,
      elev   = row$elev,
      lat    = row$lat,
      lon    = row$lon,
      area   = row$area,
      ovfl   = 0,
      rule   = 0
    )
  }

  swat_bulk_insert(con, "topography_hyd", .list_to_df(topography))
  swat_bulk_insert(con, "field_fld",      .list_to_df(fields))
  swat_bulk_insert(con, "rout_unit_rtu",  .list_to_df(rtu_list))
  swat_bulk_insert(con, "rout_unit_con",  .list_to_df(rtu_cons))
  invisible(NULL)
}

# ===========================================================================
# OM water initialisation
# ===========================================================================

.insert_om_water <- function(con) {
  if (swat_count(con, "om_water_ini") > 0L) return(invisible(NULL))
  DBI::dbExecute(con, "
    INSERT INTO om_water_ini (name,flo,sed,orgn,sedp,no3,solp,chla,nh3,no2,cbod,dox,san,sil,cla,sag,lag,gravel)
    VALUES ('initomwater1',0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)")
  invisible(NULL)
}

# ===========================================================================
# Channels (LTE)
# ===========================================================================

.insert_channels_lte <- function(con, env) {
  if (swat_count(con, "channel_lte_cha") > 0L) return(invisible(NULL))

  channels <- swat_query(con, "SELECT * FROM gis_channels ORDER BY id")
  if (nrow(channels) == 0L) return(invisible(NULL))

  cnt <- max(channels$id)

  # Default initial_cha
  DBI::dbExecute(con, "INSERT INTO initial_cha (name, org_min_id) VALUES ('initcha1', 1)")
  init_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id

  # Default nutrients_cha
  DBI::dbExecute(con, "
    INSERT INTO nutrients_cha (name, plt_n, ptl_p, alg_stl, ben_disp, ben_nh3n, ptln_stl, ptlp_stl,
      cst_stl, ben_cst, cbn_bod_co, air_rt, cbn_bod_stl, ben_bod, bact_die, cst_decay, nh3n_no2n,
      no2n_no3n, ptln_nh3n, ptlp_solp, q2e_lt, q2e_alg, chla_alg, alg_n, alg_p, alg_o2_prod,
      alg_o2_resp, o2_nh3n, o2_no2n, alg_grow, alg_resp, slr_act, lt_co, const_n, const_p,
      lt_nonalg, alg_shd_l, alg_shd_nl, nh3_pref)
    VALUES ('nutcha1', 0, 0, 1, 0.05, 0.5, 0.05, 0.05, 2.5, 2.5, 1.71, 50, 0.36, 2, 2,
      1.71, 0.55, 1.1, 0.21, 0.35, 2, 2, 50, 0.08, 0.015, 1.6, 2, 3.5, 1.07, 2, 2.5,
      0.3, 0.75, 0.02, 0.025, 1, 0.03, 0.054, 0.5)")
  nut_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id

  hyd_chas    <- vector("list", nrow(channels))
  chan_chas   <- vector("list", nrow(channels))
  chan_cons   <- vector("list", nrow(channels))

  for (i in seq_len(nrow(channels))) {
    row <- channels[i, ]
    new_id   <- i
    env$gis_to_cha_ids[[as.character(row$id)]] <- new_id
    cha_name <- get_swat_name("cha", row$id, cnt)

    hyd_chas[[i]] <- list(
      id         = new_id,
      name       = paste0("hyd", cha_name),
      "order"    = as.character(row$strahler),
      wd         = row$wid2,
      dp         = row$dep2,
      slp        = row$slo2 / 100,
      len        = row$len2 / 1000,
      mann       = 0.05,
      k          = 1,
      erod_fact  = 0.01,
      cov_fact   = 0.005,
      sinu       = 1.05,
      eq_slp     = 0.001,
      d50        = 12,
      clay       = 50,
      carbon     = 0.04,
      dry_bd     = 1,
      side_slp   = 0.5,
      bankfull_flo = 0.5,
      fps        = 0.00001,
      fpn        = 0.1,
      n_conc     = 0,
      p_conc     = 0,
      p_bio      = 0
    )
    chan_chas[[i]] <- list(
      id      = new_id,
      name    = cha_name,
      hyd_id  = new_id,
      init_id = init_id,
      nut_id  = nut_id
    )
    chan_cons[[i]] <- list(
      id     = new_id,
      lcha_id = new_id,
      name   = cha_name,
      gis_id = row$id,
      lat    = row$midlat,
      lon    = row$midlon,
      area   = row$areac,
      ovfl   = 0,
      rule   = 0
    )
  }

  swat_bulk_insert(con, "hyd_sed_lte_cha",  .list_to_df(hyd_chas))
  swat_bulk_insert(con, "channel_lte_cha",  .list_to_df(chan_chas))
  swat_bulk_insert(con, "chandeg_con",       .list_to_df(chan_cons))
  invisible(NULL)
}

# ===========================================================================
# Reservoirs
# ===========================================================================

.insert_reservoirs <- function(con, env) {
  if (swat_count(con, "reservoir_res") > 0L) return(invisible(NULL))

  waters <- swat_query(con, "SELECT * FROM gis_water ORDER BY id")
  if (nrow(waters) == 0L) return(invisible(NULL))

  cnt <- max(waters$id)

  # Default init / sed / nut / weir
  DBI::dbExecute(con, "INSERT INTO initial_res (name, org_min_id) VALUES ('initres1', 1)")
  init_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id

  DBI::dbExecute(con, "
    INSERT INTO nutrients_res (name,mid_start,mid_end,mid_n_stl,n_stl,mid_p_stl,p_stl,
      chla_co,secchi_co,theta_n,theta_p,n_min_stl,p_min_stl)
    VALUES ('nutres1',5,10,5.5,5.5,10,10,1,1,1,1,0.1,0.01)")
  nut_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id

  DBI::dbExecute(con, "
    INSERT INTO sediment_res (name,sed_amt,d50,carbon,bd,sed_stl,stl_vel)
    VALUES ('sedres1',1,10,0,0,1,1)")
  sed_id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id

  DBI::dbExecute(con, "
    INSERT INTO weir_res (name,linear_c,exp_k,width,height)
    VALUES ('shape1',1.84,2.6,2.5,0)")

  # Look for a default release decision table
  res_rel_id <- tryCatch({
    r <- DBI::dbGetQuery(con,
      "SELECT id FROM d_table_dtl WHERE name='drawdown_days' LIMIT 1")
    if (nrow(r) == 0L) {
      r <- DBI::dbGetQuery(con,
        "SELECT id FROM d_table_dtl WHERE name='corps_med_res' LIMIT 1")
    }
    if (nrow(r) > 0L) r$id[[1L]] else NA_integer_
  }, error = function(e) NA_integer_)

  hyd_res_list  <- vector("list", nrow(waters))
  res_list      <- vector("list", nrow(waters))
  res_con_list  <- vector("list", nrow(waters))

  for (i in seq_len(nrow(waters))) {
    row <- waters[i, ]
    new_id   <- i
    env$gis_to_res_ids[[as.character(row$id)]] <- new_id
    res_name <- get_swat_name(tolower(row$wtype), row$id, cnt)

    hyd_res_list[[i]] <- list(
      id       = new_id,
      name     = res_name,
      yr_op    = 1,
      mon_op   = 1,
      area_ps  = row$area,
      vol_ps   = row$area * 10,
      area_es  = row$area * 1.15,
      vol_es   = row$area * 1.15 * 10,
      k        = 0,
      evap_co  = 0.6,
      shp_co1  = 0,
      shp_co2  = 0
    )
    res_list[[i]] <- list(
      id      = new_id,
      name    = res_name,
      rel_id  = if (is.na(res_rel_id)) NULL else res_rel_id,
      hyd_id  = new_id,
      init_id = init_id,
      sed_id  = sed_id,
      nut_id  = nut_id
    )
    res_con_list[[i]] <- list(
      id     = new_id,
      res_id = new_id,
      name   = res_name,
      gis_id = row$id,
      lat    = row$lat,
      lon    = row$lon,
      elev   = row$elev,
      area   = row$area,
      ovfl   = 0,
      rule   = 0
    )
  }

  swat_bulk_insert(con, "hydrology_res",  .list_to_df(hyd_res_list))
  swat_bulk_insert(con, "reservoir_res",  .list_to_df(res_list))
  swat_bulk_insert(con, "reservoir_con",  .list_to_df(res_con_list))
  invisible(NULL)
}

# ===========================================================================
# Recall (point sources)
# ===========================================================================

.insert_recall <- function(con, env, constant_ps = TRUE) {
  if (swat_count(con, "recall_rec") > 0L) return(invisible(NULL))

  pts <- swat_query(con,
    "SELECT * FROM gis_points WHERE ptype IN ('P','I') ORDER BY id")
  if (nrow(pts) == 0L) return(invisible(NULL))

  cnt <- max(pts$id)

  # Build routing dict for PT -> CH connections
  con_dict <- swat_query(con,
    "SELECT sourceid, sinkid, hyd_typ, percent FROM gis_routing
     WHERE sourcecat='PT' AND sinkcat='CH'")
  routing_map <- stats::setNames(
    split(con_dict, seq_len(nrow(con_dict))),
    as.character(con_dict$sourceid)
  )

  rec_con_list  <- vector("list", nrow(pts))
  rec_list      <- vector("list", nrow(pts))
  rec_data_list <- vector("list", nrow(pts))
  rec_con_out_list <- list()

  for (i in seq_len(nrow(pts))) {
    row      <- pts[i, ]
    new_id   <- i
    rec_name <- get_swat_name("pt", row$id, cnt)

    rec_con_list[[i]] <- list(
      id     = new_id,
      rec_id = new_id,
      name   = rec_name,
      gis_id = row$id,
      lat    = row$lat,
      lon    = row$lon,
      elev   = row$elev,
      area   = 0,
      ovfl   = 0,
      rule   = 0
    )
    rec_list[[i]] <- list(
      id      = new_id,
      name    = rec_name,
      rec_typ = if (constant_ps) 4L else 1L
    )
    rec_data_list[[i]] <- list(
      recall_rec_id = new_id,
      jday    = 1, mo = 1, day_mo = 1, yr = 1,
      ob_typ  = "pt_const",
      ob_name = rec_name,
      flo = 0, sed = 0, orgn = 0, sedp = 0,
      no3 = 0, solp = 0, chla = 0, nh3 = 0, no2 = 0, cbod = 0, dox = 0,
      sand = 0, silt = 0, clay = 0, sag = 0, lag = 0, gravel = 0, tmp = 0
    )

    con_row <- routing_map[[as.character(row$id)]]
    if (!is.null(con_row)) {
      cha_id <- env$gis_to_cha_ids[[as.character(con_row$sinkid)]]
      if (!is.null(cha_id)) {
        rec_con_out_list <- c(rec_con_out_list, list(list(
          recall_con_id = new_id,
          "order"   = 1L,
          obj_typ   = "sdc",
          obj_id    = cha_id,
          hyd_typ   = con_row$hyd_typ,
          frac      = con_row$percent / 100
        )))
      }
    }
  }

  swat_bulk_insert(con, "recall_con",     .list_to_df(rec_con_list))
  swat_bulk_insert(con, "recall_rec",     .list_to_df(rec_list))
  swat_bulk_insert(con, "recall_dat",     .list_to_df(rec_data_list))
  if (length(rec_con_out_list) > 0L)
    swat_bulk_insert(con, "recall_con_out", .list_to_df(rec_con_out_list))
  invisible(NULL)
}

# ===========================================================================
# HRUs (standard SWAT+)
# ===========================================================================

.insert_hrus <- function(con, env) {
  if (swat_count(con, "hru_data_hru") > 0L) return(invisible(NULL))

  hrus <- swat_query(con, "SELECT * FROM gis_hrus ORDER BY id")
  if (nrow(hrus) == 0L) return(invisible(NULL))

  cnt      <- max(hrus$id)
  bsn_area <- DBI::dbGetQuery(con, "SELECT SUM(area) AS s FROM gis_subbasins")$s

  # Create default nutrients_sol + soil_plant_ini
  DBI::dbExecute(con, "
    INSERT OR IGNORE INTO nutrients_sol (name,exp_co,lab_p,nitrate,fr_hum_act,
      hum_c_n,hum_c_p,inorgp,watersol_p,h3a_p,mehlich_p,bray_strong_p)
    VALUES ('soilnut1',0.0005,5,7,0.02,10,80,3.5,0.15,0.25,1.2,0.85)")
  nut_sol_id <- DBI::dbGetQuery(con,
    "SELECT id FROM nutrients_sol WHERE name='soilnut1'")$id

  DBI::dbExecute(con, paste0("
    INSERT OR IGNORE INTO soil_plant_ini (name,sw_frac,nutrients_id)
    VALUES ('soilplant1',0,", nut_sol_id, ")"))
  sp_id <- DBI::dbGetQuery(con,
    "SELECT id FROM soil_plant_ini WHERE name='soilplant1'")$id

  # Load soils dict
  soils_df  <- swat_query(con, "SELECT id, name, hyd_grp FROM soils_sol")
  soils_map <- stats::setNames(soils_df$id, tolower(soils_df$name))
  hyd_grp_map <- stats::setNames(as.character(soils_df$hyd_grp),
                                  tolower(soils_df$name))

  topo_offset <- swat_max_id(con, "topography_hyd") + 1L
  lum_dict    <- .insert_landuse(con, hrus)

  hyd_list    <- vector("list", nrow(hrus))
  topo_list   <- vector("list", nrow(hrus))
  hru_list    <- vector("list", nrow(hrus))
  con_list    <- vector("list", nrow(hrus))
  elem_list   <- vector("list", nrow(hrus))
  lsu_ele_list <- vector("list", nrow(hrus))

  for (i in seq_len(nrow(hrus))) {
    row        <- hrus[i, ]
    new_id     <- i
    hru_name   <- get_swat_name("hru", row$id, cnt)
    soil_key   <- tolower(as.character(row$soil))
    soil_id    <- soils_map[[soil_key]]
    hyd_grp    <- hyd_grp_map[[soil_key]]
    if (is.null(hyd_grp) || is.na(hyd_grp)) hyd_grp <- "B"

    hyd_vals   <- .get_perco_cn3_swf_latq_co(hyd_grp, row$slope / 100)
    slp_len    <- .get_slope_len(row$slope)

    hyd_list[[i]] <- list(
      id          = new_id,
      name        = get_swat_name("hyd", row$id, cnt),
      lat_ttime   = 0,
      lat_sed     = 0,
      can_max     = 1,
      esco        = 0.95,
      epco        = 0.5,
      orgn_enrich = 0,
      orgp_enrich = 0,
      cn3_swf     = hyd_vals$cn3_swf,
      bio_mix     = 0.2,
      perco       = hyd_vals$perco,
      lat_orgn    = 0,
      lat_orgp    = 0,
      pet_co      = 1,
      latq_co     = hyd_vals$latq_co
    )
    topo_list[[i]] <- list(
      id       = topo_offset + i - 1L,
      name     = get_swat_name("topohru", row$id, cnt),
      slp      = row$slope / 100,
      slp_len  = slp_len,
      lat_len  = slp_len,
      dist_cha = 121.0,
      depos    = 0,
      type     = "hru"
    )
    lu_id <- lum_dict[[tolower(as.character(row$landuse))]]
    hru_list[[i]] <- list(
      id                 = new_id,
      name               = hru_name,
      topo_id            = topo_offset + i - 1L,
      hydro_id           = new_id,
      soil_id            = if (is.null(soil_id)) NA_integer_ else soil_id,
      lu_mgt_id          = if (is.null(lu_id))   NA_integer_ else lu_id,
      soil_plant_init_id = sp_id,
      snow_id            = 1L
    )
    con_list[[i]] <- list(
      id     = new_id,
      hru_id = new_id,
      name   = hru_name,
      gis_id = row$id,
      elev   = row$elev,
      lat    = row$lat,
      lon    = row$lon,
      area   = row$arslp,
      ovfl   = 0,
      rule   = 0
    )
    rtu_id <- env$gis_to_rtu_ids[[as.character(row$lsu)]]
    elem_list[[i]] <- list(
      id     = new_id,
      name   = hru_name,
      rtu_id = if (is.null(rtu_id)) NA_integer_ else rtu_id,
      obj_typ = "hru",
      obj_id  = new_id,
      frac    = if (!is.na(row$arlsu) && row$arlsu > 0) row$arslp / row$arlsu else 1.0
    )
    lsu_ele_list[[i]] <- list(
      id             = new_id,
      name           = hru_name,
      obj_typ        = "hru",
      obj_typ_no     = new_id,
      bsn_frac       = if (!is.null(bsn_area) && bsn_area > 0) row$arslp / bsn_area else 0,
      sub_frac       = if (!is.na(row$arlsu) && row$arlsu > 0) row$arslp / row$arlsu else 1.0,
      reg_frac       = 0,
      ls_unit_def_id = if (is.null(rtu_id)) NA_integer_ else rtu_id
    )
  }

  swat_bulk_insert(con, "hydrology_hyd",  .list_to_df(hyd_list))
  swat_bulk_insert(con, "topography_hyd", .list_to_df(topo_list))
  swat_bulk_insert(con, "hru_data_hru",   .list_to_df(hru_list))
  swat_bulk_insert(con, "hru_con",        .list_to_df(con_list))
  swat_bulk_insert(con, "rout_unit_ele",  .list_to_df(elem_list))
  swat_bulk_insert(con, "ls_unit_ele",    .list_to_df(lsu_ele_list))
  invisible(NULL)
}

# ===========================================================================
# HRU LTE
# ===========================================================================

.insert_hru_ltes <- function(con, env) {
  if (swat_count(con, "hru_lte_hru") > 0L) return(invisible(NULL))

  hrus <- swat_query(con, "SELECT * FROM gis_hrus ORDER BY id")
  if (nrow(hrus) == 0L) return(invisible(NULL))

  cnt      <- max(hrus$id)
  bsn_area <- DBI::dbGetQuery(con, "SELECT SUM(area) AS s FROM gis_subbasins")$s

  soils_df  <- swat_query(con, "SELECT id, name, dp_tot, hyd_grp FROM soils_sol")
  soils_map <- stats::setNames(soils_df$id, tolower(soils_df$name))
  soils_dp  <- stats::setNames(soils_df$dp_tot, tolower(soils_df$name))

  hru_list  <- vector("list", nrow(hrus))
  con_list  <- vector("list", nrow(hrus))
  lsu_eles  <- vector("list", nrow(hrus))

  for (i in seq_len(nrow(hrus))) {
    row      <- hrus[i, ]
    new_id   <- i
    env$gis_to_hru_ids[[as.character(row$id)]] <- new_id
    hru_name <- get_swat_name("hru", row$id, cnt)

    slope    <- row$slope / 100
    slp_len  <- .get_slope_len(row$slope)
    xm       <- 0.6 * (1 - exp(-35.835 * slope))
    sin_sl   <- sin(atan(slope))
    usle_ls  <- (slp_len / 22.128)^xm * (65.41 * sin_sl^2 + 4.56 * sin_sl + 0.065)

    soil_key <- tolower(as.character(row$soil))
    soil_id  <- soils_map[[soil_key]]
    soil_dp  <- soils_dp[[soil_key]]

    hru_list[[i]] <- list(
      id         = new_id,
      name       = hru_name,
      area       = row$arslp,
      cn2        = 70,
      cn3_swf    = 0,
      t_conc     = 26,
      soil_dp    = if (is.null(soil_dp) || is.na(soil_dp)) 0 else soil_dp,
      perc_co    = 0,
      slp        = slope,
      slp_len    = slp_len,
      et_co      = 1,
      aqu_sp_yld = 0.05,
      alpha_bf   = 0.05,
      revap      = 0,
      rchg_dp    = 0.01,
      sw_init    = 0.5,
      aqu_init   = 3,
      aqu_sh_flo = 0,
      aqu_dp_flo = 300,
      snow_h2o   = 0,
      lat        = row$lat,
      soil_text_id = if (is.null(soil_id)) NA_integer_ else soil_id,
      trop_flag  = "non_trop",
      stress     = 0,
      pet_flag   = "harg",
      irr_flag   = "no_irr",
      irr_src    = "outside_bsn",
      t_drain    = 0,
      usle_k     = 0.3,
      usle_c     = 0.001,
      usle_p     = 1,
      usle_ls    = usle_ls
    )
    con_list[[i]] <- list(
      id     = new_id,
      lhru_id = new_id,
      name   = hru_name,
      gis_id = row$id,
      elev   = row$elev,
      lat    = row$lat,
      lon    = row$lon,
      area   = row$arslp,
      ovfl   = 0,
      rule   = 0
    )
    rtu_id <- env$gis_to_rtu_ids[[as.character(row$lsu)]]
    lsu_eles[[i]] <- list(
      id             = new_id,
      name           = hru_name,
      obj_typ        = "hlt",
      obj_typ_no     = new_id,
      bsn_frac       = if (!is.null(bsn_area) && bsn_area > 0) row$arslp / bsn_area else 0,
      sub_frac       = if (!is.na(row$arlsu) && row$arlsu > 0) row$arslp / row$arlsu else 1.0,
      reg_frac       = 0,
      ls_unit_def_id = if (is.null(rtu_id)) NA_integer_ else rtu_id
    )
  }

  swat_bulk_insert(con, "hru_lte_hru", .list_to_df(hru_list))
  swat_bulk_insert(con, "hru_lte_con", .list_to_df(con_list))
  swat_bulk_insert(con, "ls_unit_ele", .list_to_df(lsu_eles))
  invisible(NULL)
}

# ===========================================================================
# Aquifers
# ===========================================================================

.insert_aquifers <- function(con, env) {
  if (swat_count(con, "aquifer_aqu") > 0L) return(invisible(NULL))

  aquifers      <- swat_query(con, "SELECT * FROM gis_aquifers ORDER BY id")
  deep_aquifers <- swat_query(con, "SELECT * FROM gis_deep_aquifers ORDER BY id")

  # Create initial_aqu
  DBI::dbExecute(con, "INSERT OR IGNORE INTO initial_aqu (name) VALUES ('initaqu1')")
  init_id <- DBI::dbGetQuery(con,
    "SELECT id FROM initial_aqu WHERE name='initaqu1'")$id

  aqu_list <- list()
  con_list <- list()
  i <- 1L

  cnt    <- if (nrow(aquifers) > 0L)      max(aquifers$id)      else 0L
  cnt_dp <- if (nrow(deep_aquifers) > 0L) max(deep_aquifers$id) else 0L

  for (j in seq_len(nrow(aquifers))) {
    row <- aquifers[j, ]
    env$gis_to_aqu_ids[[as.character(row$id)]] <- i
    aqu_name <- get_swat_name("aqu", row$id, cnt)
    aqu_list <- c(aqu_list, list(.default_shallow_aqu(i, aqu_name, init_id)))
    con_list <- c(con_list, list(list(
      id     = i,
      aqu_id = i,
      name   = aqu_name,
      gis_id = row$id,
      lat    = row$lat,
      lon    = row$lon,
      area   = row$area,
      elev   = row$elev,
      ovfl   = 0,
      rule   = 0
    )))
    i <- i + 1L
  }

  for (j in seq_len(nrow(deep_aquifers))) {
    row <- deep_aquifers[j, ]
    env$gis_to_deep_aqu_ids[[as.character(row$id)]] <- i
    aqu_name <- get_swat_name("aqu_deep", row$id, cnt_dp)
    aqu_list <- c(aqu_list, list(.default_deep_aqu(i, aqu_name, init_id)))
    con_list <- c(con_list, list(list(
      id     = i,
      aqu_id = i,
      name   = aqu_name,
      gis_id = row$id,
      lat    = row$lat,
      lon    = row$lon,
      area   = row$area,
      elev   = row$elev,
      ovfl   = 0,
      rule   = 0
    )))
    i <- i + 1L
  }

  if (length(aqu_list) > 0L) {
    swat_bulk_insert(con, "aquifer_aqu", .list_to_df(aqu_list))
    swat_bulk_insert(con, "aquifer_con", .list_to_df(con_list))
  }
  invisible(NULL)
}

# ===========================================================================
# Landuse / LUM (supports insert_hrus)
# ===========================================================================

.insert_landuse <- function(con, hrus) {
  # Get distinct land uses from gis_hrus
  lus <- unique(tolower(na.omit(hrus$landuse)))
  lus <- lus[lus != "null" & nchar(lus) > 0L]
  lum_dict <- list()

  for (lu in lus) {
    lum_name <- paste0(lu, "_lum")
    existing <- DBI::dbGetQuery(con,
      paste0("SELECT id FROM landuse_lum WHERE name='", lum_name, "'"))
    if (nrow(existing) > 0L) {
      lum_dict[[lu]] <- existing$id[[1L]]
      next
    }

    # Defaults: cn2=5, cons_prac=1, ov_mann=2
    DBI::dbExecute(con, paste0("
      INSERT INTO landuse_lum (name, cn2_id, cons_prac_id, ov_mann_id)
      VALUES ('", lum_name, "', 5, 1, 2)"))
    lum_dict[[lu]] <- DBI::dbGetQuery(con,
      "SELECT last_insert_rowid() AS id")$id[[1L]]
  }

  lum_dict
}

# ===========================================================================
# Connections
# ===========================================================================

.insert_connections <- function(con, env) {
  pt_rows <- swat_query(con,
    "SELECT sourceid, sinkid, sinkcat, hyd_typ, percent FROM gis_routing
     WHERE sourcecat='PT'")
  pt_dict <- if (nrow(pt_rows) > 0L)
    stats::setNames(split(pt_rows, seq_len(nrow(pt_rows))),
                    as.character(pt_rows$sourceid))
  else list()

  # RTU connections
  rtu_outs <- .get_connections(con, env, c(ROUTE_CAT$LSU), "rtu_con_id",
                               env$gis_to_rtu_ids, pt_dict)
  swat_bulk_insert(con, "rout_unit_con_out", .list_to_df(rtu_outs))

  # Channel connections
  cha_outs <- .get_connections(con, env, c(ROUTE_CAT$CH), "chandeg_con_id",
                               env$gis_to_cha_ids, pt_dict)
  swat_bulk_insert(con, "chandeg_con_out", .list_to_df(cha_outs))

  # Aquifer connections
  aqu_outs <- .get_connections(con, env, c(ROUTE_CAT$AQU), "aquifer_con_id",
                               env$gis_to_aqu_ids, pt_dict)
  swat_bulk_insert(con, "aquifer_con_out", .list_to_df(aqu_outs))

  # Reservoir connections
  res_outs <- .get_connections(con, env,
                               c(ROUTE_CAT$WTR, ROUTE_CAT$PND, ROUTE_CAT$RES),
                               "reservoir_con_id",
                               env$gis_to_res_ids, pt_dict)
  swat_bulk_insert(con, "reservoir_con_out", .list_to_df(res_outs))
  invisible(NULL)
}

.insert_connections_lte <- function(con, env) {
  pt_rows <- swat_query(con,
    "SELECT sourceid, sinkid, sinkcat, hyd_typ, percent FROM gis_routing
     WHERE sourcecat='PT'")
  pt_dict <- if (nrow(pt_rows) > 0L)
    stats::setNames(split(pt_rows, seq_len(nrow(pt_rows))),
                    as.character(pt_rows$sourceid))
  else list()

  cha_outs <- .get_connections(con, env, c(ROUTE_CAT$CH), "chandeg_con_id",
                               env$gis_to_cha_ids, pt_dict, is_lte = TRUE)
  swat_bulk_insert(con, "chandeg_con_out", .list_to_df(cha_outs))

  # HRU -> CH connections (LTE)
  hru_rows <- swat_query(con,
    "SELECT sourceid, sinkid, hyd_typ, percent FROM gis_routing
     WHERE sourcecat='HRU' AND percent>0 AND sinkcat='CH'
     ORDER BY sourceid")
  hru_con_outs <- lapply(seq_len(nrow(hru_rows)), function(i) {
    row    <- hru_rows[i, ]
    hru_id <- env$gis_to_hru_ids[[as.character(row$sourceid)]]
    cha_id <- env$gis_to_cha_ids[[as.character(row$sinkid)]]
    if (is.null(hru_id) || is.null(cha_id)) return(NULL)
    list(
      hru_lte_con_id = hru_id,
      "order"  = 1L,
      obj_typ  = "sdc",
      obj_id   = cha_id,
      hyd_typ  = "tot",
      frac     = 1
    )
  })
  hru_con_outs <- Filter(Negate(is.null), hru_con_outs)
  if (length(hru_con_outs) > 0L)
    swat_bulk_insert(con, "hru_lte_con_out", .list_to_df(hru_con_outs))
  invisible(NULL)
}

.get_connections <- function(con, env, sourcecats, fk_col,
                              id_list, pt_dict, is_lte = FALSE) {
  cats_to_obj_typ <- list(
    CH  = "sdc", AQU = "aqu", DAQ = "aqu",
    LSU = "ru",  WTR = "res", RES = "res", PND = "res"
  )
  cats_to_id_list <- list(
    CH  = env$gis_to_cha_ids,
    AQU = env$gis_to_aqu_ids,
    DAQ = env$gis_to_deep_aqu_ids,
    LSU = env$gis_to_rtu_ids,
    WTR = env$gis_to_res_ids,
    RES = env$gis_to_res_ids,
    PND = env$gis_to_res_ids
  )
  supported_sinkcats <- if (is_lte) c("CH") else
    c("CH", "AQU", "DAQ", "LSU", "WTR", "RES", "PND")

  cat_sql <- paste0("'", sourcecats, "'", collapse = ",")
  rows <- swat_query(con, paste0(
    "SELECT sourceid, sourcecat, sinkid, sinkcat, hyd_typ, percent
     FROM gis_routing
     WHERE sourcecat IN (", cat_sql, ") AND percent>0 AND sinkcat<>'X'
     ORDER BY sourceid"))

  con_outs <- list()
  orders   <- list()

  for (i in seq_len(nrow(rows))) {
    row     <- rows[i, ]
    src_id  <- id_list[[as.character(row$sourceid)]]
    if (is.null(src_id)) next

    # Follow PT chains
    con_row <- row
    while (!is.null(con_row) && identical(as.character(con_row$sinkcat), "PT")) {
      next_row <- pt_dict[[as.character(con_row$sinkid)]]
      con_row  <- if (!is.null(next_row)) next_row else NULL
    }
    if (is.null(con_row)) next
    if (!as.character(con_row$sinkcat) %in% supported_sinkcats) next

    obj_typ  <- cats_to_obj_typ[[as.character(con_row$sinkcat)]]
    sink_lst <- cats_to_id_list[[as.character(con_row$sinkcat)]]
    sink_id  <- sink_lst[[as.character(con_row$sinkid)]]
    if (is.null(obj_typ) || is.null(sink_id)) next

    key <- as.character(src_id)
    orders[[key]] <- (orders[[key]] %||% 0L) + 1L

    con_outs <- c(con_outs, list(stats::setNames(
      list(src_id, orders[[key]], obj_typ, sink_id,
           as.character(con_row$hyd_typ), con_row$percent / 100),
      c(fk_col, "order", "obj_typ", "obj_id", "hyd_typ", "frac")
    )))
  }
  con_outs
}

# ===========================================================================
# LSU definitions
# ===========================================================================

.insert_lsus <- function(con, env) {
  if (swat_count(con, "hru_con") == 0L) return(invisible(NULL))

  rtu_cons <- swat_query(con, "SELECT id, rtu_id, name, area FROM rout_unit_con ORDER BY id")
  ele_rtus <- swat_query(con, "SELECT DISTINCT rtu_id FROM rout_unit_ele")$rtu_id
  ele_set  <- as.character(ele_rtus)

  defs <- lapply(seq_len(nrow(rtu_cons)), function(i) {
    row <- rtu_cons[i, ]
    if (!as.character(row$rtu_id) %in% ele_set) return(NULL)
    list(id = row$rtu_id, name = row$name, area = row$area)
  })
  defs <- Filter(Negate(is.null), defs)
  if (length(defs) > 0L)
    swat_bulk_insert(con, "ls_unit_def", .list_to_df(defs))
  invisible(NULL)
}

.insert_lsus_lte <- function(con, env) {
  lsus <- swat_query(con, "SELECT * FROM gis_lsus ORDER BY id")
  if (nrow(lsus) == 0L) return(invisible(NULL))

  hru_lsus <- swat_query(con, "SELECT DISTINCT lsu FROM gis_hrus")$lsu
  hru_set  <- as.character(hru_lsus)

  cnt  <- max(lsus$id)
  defs <- list()
  i    <- 1L
  for (j in seq_len(nrow(lsus))) {
    row <- lsus[j, ]
    env$gis_to_rtu_ids[[as.character(row$id)]] <- i
    if (!as.character(row$id) %in% hru_set) { i <- i + 1L; next }
    defs <- c(defs, list(list(
      id   = i,
      name = get_swat_name("lsu", row$id, cnt),
      area = row$area
    )))
    i <- i + 1L
  }
  if (length(defs) > 0L)
    swat_bulk_insert(con, "ls_unit_def", .list_to_df(defs))
  invisible(NULL)
}

# ===========================================================================
# Delete existing data
# ===========================================================================

.delete_existing_gis_data <- function(con) {
  tables_to_clear <- c(
    "topography_hyd", "hydrology_hyd", "field_fld",
    "om_water_ini", "soil_plant_ini",
    "hyd_sed_lte_cha", "initial_cha", "nutrients_cha", "channel_lte_cha",
    "aquifer_aqu", "initial_aqu",
    "reservoir_res", "initial_res", "hydrology_res", "nutrients_res",
    "sediment_res", "weir_res", "hydrology_wet", "wetland_wet",
    "recall_rec", "recall_dat",
    "rout_unit_rtu",
    "hru_data_hru", "hru_lte_hru",
    "nutrients_sol",
    "plant_ini_item", "plant_ini",
    "landuse_lum",
    "ls_unit_ele", "ls_unit_def",
    "rout_unit_con_out", "rout_unit_con",
    "aquifer_con_out", "aquifer_con",
    "chandeg_con_out", "chandeg_con",
    "reservoir_con_out", "reservoir_con",
    "recall_con_out", "recall_con",
    "hru_con_out", "hru_con",
    "hru_lte_con_out", "hru_lte_con"
  )
  for (tbl in tables_to_clear) {
    if (swat_exists_table(con, tbl)) {
      DBI::dbExecute(con, paste0("DELETE FROM ", tbl))
    }
  }
  invisible(NULL)
}

# ===========================================================================
# Hydrology helpers
# ===========================================================================

.get_slope_len <- function(slope) {
  if (slope < 1)  return(121)
  if (slope < 3)  return(90)
  if (slope < 5)  return(60)
  if (slope < 8)  return(30)
  10
}

.get_perco_cn3_swf_latq_co <- function(hyd_grp, slope, is_tile = FALSE) {
  leach_pot  <- "low"
  runoff_pot <- "low"
  if (!is_tile) {
    if (hyd_grp == "A") {
      leach_pot  <- "high"
      runoff_pot <- if (slope < 6) "low" else if (slope <= 12) "mod" else "high"
    } else if (hyd_grp == "B") {
      leach_pot  <- if (slope < 6) "high" else "mod"
      runoff_pot <- if (slope < 4) "low"  else if (slope <= 6) "mod" else "high"
    } else if (hyd_grp == "C") {
      leach_pot  <- if (slope < 12) "mod" else "low"
      runoff_pot <- if (slope < 2)  "low" else if (slope <= 6) "mod" else "high"
    } else if (hyd_grp == "D") {
      leach_pot  <- "low"
      runoff_pot <- if (slope < 2)  "low" else if (slope <= 4) "mod" else "high"
    }
  }
  list(
    perco   = if (leach_pot  == "high") 0.9  else if (leach_pot  == "mod") 0.5  else 0.05,
    cn3_swf = if (runoff_pot == "high") 0    else if (runoff_pot == "mod") 0.3  else 0.95,
    latq_co = if (runoff_pot == "high") 0.9  else if (runoff_pot == "mod") 0.2  else 0.01
  )
}

# ===========================================================================
# Aquifer default parameters
# ===========================================================================

.default_shallow_aqu <- function(id, name, init_id) {
  list(id = id, name = name, init_id = init_id,
       gw_flo = 0.05, dep_bot = 10, dep_wt = 3, no3_n = 0, sol_p = 0,
       carbon = 0.5,  flo_dist = 50, bf_max = 1, alpha_bf = 0.05,
       revap = 0.02,  rchg_dp = 0.05, spec_yld = 0.05, hl_no3n = 30,
       flo_min = 3,   revap_min = 5)
}

.default_deep_aqu <- function(id, name, init_id) {
  list(id = id, name = name, init_id = init_id,
       gw_flo = 0, dep_bot = 100, dep_wt = 20, no3_n = 0, sol_p = 0,
       carbon = 0.5, flo_dist = 50, bf_max = 1, alpha_bf = 0.01,
       revap = 0, rchg_dp = 0, spec_yld = 0.03, hl_no3n = 30,
       flo_min = 0, revap_min = 0)
}

# ===========================================================================
# Helper: convert list of named lists to data.frame
# ===========================================================================

.list_to_df <- function(lst) {
  if (length(lst) == 0L) return(data.frame())
  do.call(rbind, lapply(lst, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
}

# (Null-coalescing operator `%||%` is defined in utils.R and is available
#  package-wide through the shared R environment.)
