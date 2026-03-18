#' SQL DDL for the SWAT+ project database schema
#'
#' Contains \code{CREATE TABLE} statements that mirror the Peewee ORM models
#' defined in \code{src/api/database/project/}.  Tables are created with
#' \code{IF NOT EXISTS} so the function is safe to call on an existing database.
#'
#' @keywords internal
NULL

# ---------------------------------------------------------------------------
# Helper: create one table
# ---------------------------------------------------------------------------

.create_table <- function(con, sql) {
  DBI::dbExecute(con, sql)
}

# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------

#' Create all SWAT+ project database tables
#'
#' Mirrors \code{SetupProjectDatabase.create_tables()} in
#' \code{src/api/database/project/setup.py}.  Each table is created with
#' \code{IF NOT EXISTS}, so calling this on an existing database is safe.
#'
#' @param con A \code{DBIConnection} to an open project SQLite database.
#' @return Invisibly \code{NULL}.
#' @export
create_project_tables <- function(con) {

  # --- Config ---------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS file_cio_classification (
      id   INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT    NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS file_cio (
      id                 INTEGER PRIMARY KEY AUTOINCREMENT,
      classification_id  INTEGER REFERENCES file_cio_classification(id) ON DELETE CASCADE,
      order_in_class     INTEGER NOT NULL,
      file_name          TEXT    NOT NULL,
      customization      INTEGER NOT NULL DEFAULT 0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS project_config (
      id                      INTEGER PRIMARY KEY AUTOINCREMENT,
      project_name            TEXT,
      project_directory       TEXT,
      editor_version          TEXT,
      gis_type                TEXT,
      gis_version             TEXT,
      project_db              TEXT,
      reference_db            TEXT,
      wgn_db                  TEXT,
      wgn_table_name          TEXT,
      weather_data_dir        TEXT,
      weather_data_format     TEXT,
      netcdf_data_file        TEXT,
      input_files_dir         TEXT,
      input_files_last_written DATETIME,
      swat_last_run           DATETIME,
      swat_exe_filename       TEXT,
      delineation_done        INTEGER NOT NULL DEFAULT 0,
      hrus_done               INTEGER NOT NULL DEFAULT 0,
      soil_table              TEXT,
      soil_layer_table        TEXT,
      output_last_imported    DATETIME,
      imported_gis            INTEGER NOT NULL DEFAULT 0,
      is_lte                  INTEGER NOT NULL DEFAULT 0,
      use_gwflow              INTEGER NOT NULL DEFAULT 0
    )")

  # --- Basin ----------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS codes_bsn (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      pet      INTEGER NOT NULL DEFAULT 1,
      event    INTEGER NOT NULL DEFAULT 0,
      crack    INTEGER NOT NULL DEFAULT 0,
      rtu_evap INTEGER NOT NULL DEFAULT 0,
      gwflow   INTEGER NOT NULL DEFAULT 0,
      swift    INTEGER NOT NULL DEFAULT 0,
      carbon   INTEGER NOT NULL DEFAULT 2,
      lat_sed  INTEGER NOT NULL DEFAULT 0,
      nutrient INTEGER NOT NULL DEFAULT 1,
      ch_sed   INTEGER NOT NULL DEFAULT 1,
      ch_nu    INTEGER NOT NULL DEFAULT 0,
      soil_p   INTEGER NOT NULL DEFAULT 0,
      atmo_dep TEXT    NOT NULL DEFAULT 'no',
      stor_ws  INTEGER NOT NULL DEFAULT 0,
      pesticide INTEGER NOT NULL DEFAULT 0,
      pathogens INTEGER NOT NULL DEFAULT 0,
      hmet     INTEGER NOT NULL DEFAULT 0,
      salt     INTEGER NOT NULL DEFAULT 0,
      co2      INTEGER NOT NULL DEFAULT 330
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS parameters_bsn (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      pet_co          REAL    NOT NULL DEFAULT 1.0,
      esco            REAL    NOT NULL DEFAULT 0.95,
      epco            REAL    NOT NULL DEFAULT 1.0,
      evap_res_co     REAL    NOT NULL DEFAULT 1.0,
      evap_sub_co     REAL    NOT NULL DEFAULT 1.0,
      sub_lat_perc    REAL    NOT NULL DEFAULT 0.0,
      rtu_wtr_qual    INTEGER NOT NULL DEFAULT 0,
      sed_ch_init     REAL    NOT NULL DEFAULT 0.0,
      n_updis         REAL    NOT NULL DEFAULT 20.0,
      p_updis         REAL    NOT NULL DEFAULT 20.0,
      nperco          REAL    NOT NULL DEFAULT 0.2,
      pperco          REAL    NOT NULL DEFAULT 10.0,
      phoskd          REAL    NOT NULL DEFAULT 175.0,
      psp             REAL    NOT NULL DEFAULT 0.4,
      rsdco           REAL    NOT NULL DEFAULT 0.05,
      gpcd            REAL    NOT NULL DEFAULT 0.0,
      harg_petco      REAL    NOT NULL DEFAULT 0.0023,
      falltn          REAL    NOT NULL DEFAULT 0.5,
      fallpn          REAL    NOT NULL DEFAULT 0.5,
      cn_froz         REAL    NOT NULL DEFAULT 0.000862,
      denit_bact_cf   REAL    NOT NULL DEFAULT 1.0,
      bact_swf        REAL    NOT NULL DEFAULT 0.0,
      wq_cha          INTEGER NOT NULL DEFAULT 0,
      wq_res          INTEGER NOT NULL DEFAULT 0,
      plaps           REAL    NOT NULL DEFAULT 0.0,
      tlaps           REAL    NOT NULL DEFAULT 0.0
    )")

  # --- Simulation -----------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS time_sim (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      day_start INTEGER NOT NULL DEFAULT 0,
      yrc_start INTEGER NOT NULL DEFAULT 1980,
      day_end   INTEGER NOT NULL DEFAULT 0,
      yrc_end   INTEGER NOT NULL DEFAULT 1985,
      step      INTEGER NOT NULL DEFAULT 0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS constituents_cs (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL,
      pest_coms TEXT,
      path_coms TEXT,
      hmet_coms TEXT,
      salt_coms TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS print_prt (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      nyskip    INTEGER NOT NULL DEFAULT 0,
      day_start INTEGER NOT NULL DEFAULT 0,
      yrc_start INTEGER NOT NULL DEFAULT 0,
      day_end   INTEGER NOT NULL DEFAULT 0,
      yrc_end   INTEGER NOT NULL DEFAULT 0,
      interval  INTEGER NOT NULL DEFAULT 0,
      csvout    INTEGER NOT NULL DEFAULT 1,
      dbout     INTEGER NOT NULL DEFAULT 0,
      cdfout    INTEGER NOT NULL DEFAULT 0,
      crop_yld  TEXT    NOT NULL DEFAULT 'b',
      mgtout    INTEGER NOT NULL DEFAULT 0,
      hydcon    INTEGER NOT NULL DEFAULT 0,
      fdcout    INTEGER NOT NULL DEFAULT 0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS print_prt_aa_int (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      print_prt_id INTEGER REFERENCES print_prt(id) ON DELETE CASCADE,
      year         INTEGER NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS print_prt_object (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      print_prt_id INTEGER REFERENCES print_prt(id) ON DELETE CASCADE,
      name         TEXT    NOT NULL,
      daily        INTEGER NOT NULL DEFAULT 0,
      monthly      INTEGER NOT NULL DEFAULT 0,
      yearly       INTEGER NOT NULL DEFAULT 0,
      avann        INTEGER NOT NULL DEFAULT 0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS object_prt (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      ob_typ    TEXT NOT NULL,
      ob_typ_no INTEGER NOT NULL,
      hyd_typ   TEXT NOT NULL,
      filename  TEXT NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS object_cnt (
      id   INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT    NOT NULL,
      obj  INTEGER NOT NULL DEFAULT 0,
      hru  INTEGER NOT NULL DEFAULT 0,
      lhru INTEGER NOT NULL DEFAULT 0,
      rtu  INTEGER NOT NULL DEFAULT 0,
      mfl  INTEGER NOT NULL DEFAULT 0,
      aqu  INTEGER NOT NULL DEFAULT 0,
      cha  INTEGER NOT NULL DEFAULT 0,
      res  INTEGER NOT NULL DEFAULT 0,
      rec  INTEGER NOT NULL DEFAULT 0,
      exco INTEGER NOT NULL DEFAULT 0,
      dlr  INTEGER NOT NULL DEFAULT 0,
      can  INTEGER NOT NULL DEFAULT 0,
      pmp  INTEGER NOT NULL DEFAULT 0,
      out  INTEGER NOT NULL DEFAULT 0,
      lcha INTEGER NOT NULL DEFAULT 0,
      aqu2d INTEGER NOT NULL DEFAULT 0,
      hrd  INTEGER NOT NULL DEFAULT 0,
      wro  INTEGER NOT NULL DEFAULT 0
    )")

  # --- Climate --------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS weather_wgn_cli (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name     TEXT NOT NULL UNIQUE,
      lat      REAL NOT NULL,
      lon      REAL NOT NULL,
      elev     REAL NOT NULL,
      rain_yrs INTEGER NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS weather_wgn_cli_mon (
      id                  INTEGER PRIMARY KEY AUTOINCREMENT,
      weather_wgn_cli_id  INTEGER REFERENCES weather_wgn_cli(id) ON DELETE CASCADE,
      month               INTEGER NOT NULL,
      tmp_max_ave         REAL NOT NULL,
      tmp_min_ave         REAL NOT NULL,
      tmp_max_sd          REAL NOT NULL,
      tmp_min_sd          REAL NOT NULL,
      pcp_ave             REAL NOT NULL,
      pcp_sd              REAL NOT NULL,
      pcp_skew            REAL NOT NULL,
      wet_dry             REAL NOT NULL,
      wet_wet             REAL NOT NULL,
      pcp_days            REAL NOT NULL,
      pcp_hhr             REAL NOT NULL,
      slr_ave             REAL NOT NULL,
      dew_ave             REAL NOT NULL,
      wnd_ave             REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS weather_sta_cli (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name     TEXT NOT NULL UNIQUE,
      wgn_id   INTEGER REFERENCES weather_wgn_cli(id) ON DELETE SET NULL,
      pcp      TEXT,
      tmp      TEXT,
      slr      TEXT,
      hmd      TEXT,
      wnd      TEXT,
      pet      TEXT,
      atmo_dep TEXT,
      lat      REAL,
      lon      REAL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS weather_sta_cli_scale (
      id                  INTEGER PRIMARY KEY AUTOINCREMENT,
      weather_sta_cli_id  INTEGER REFERENCES weather_sta_cli(id) ON DELETE CASCADE,
      pcp  REAL,
      tmin REAL,
      tmax REAL,
      slr  REAL,
      hmd  REAL,
      wnd  REAL,
      pet  REAL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS weather_file (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      filename TEXT NOT NULL,
      type     TEXT NOT NULL,
      lat      REAL NOT NULL,
      lon      REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS atmo_cli (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      filename TEXT NOT NULL,
      timestep TEXT NOT NULL,
      mo_init  INTEGER NOT NULL,
      yr_init  INTEGER NOT NULL,
      num_aa   INTEGER NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS atmo_cli_sta (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      atmo_cli_id INTEGER REFERENCES atmo_cli(id) ON DELETE CASCADE,
      name        TEXT NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS atmo_cli_sta_value (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      sta_id  INTEGER REFERENCES atmo_cli_sta(id) ON DELETE CASCADE,
      timestep INTEGER NOT NULL,
      nh4_wet  REAL NOT NULL,
      no3_wet  REAL NOT NULL,
      nh4_dry  REAL NOT NULL,
      no3_dry  REAL NOT NULL
    )")

  # --- Hydrology ------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS hydrology_hyd (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      lat_ttime   REAL NOT NULL DEFAULT 0.0,
      lat_sed     REAL NOT NULL DEFAULT 0.0,
      can_max     REAL NOT NULL DEFAULT 1.0,
      esco        REAL NOT NULL DEFAULT 0.95,
      epco        REAL NOT NULL DEFAULT 0.5,
      orgn_enrich REAL NOT NULL DEFAULT 0.0,
      orgp_enrich REAL NOT NULL DEFAULT 0.0,
      cn3_swf     REAL NOT NULL DEFAULT 0.95,
      bio_mix     REAL NOT NULL DEFAULT 0.2,
      perco       REAL NOT NULL DEFAULT 0.05,
      lat_orgn    REAL NOT NULL DEFAULT 0.0,
      lat_orgp    REAL NOT NULL DEFAULT 0.0,
      pet_co      REAL NOT NULL DEFAULT 1.0,
      latq_co     REAL NOT NULL DEFAULT 0.01
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS topography_hyd (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name     TEXT NOT NULL UNIQUE,
      slp      REAL NOT NULL,
      slp_len  REAL NOT NULL,
      lat_len  REAL NOT NULL,
      dist_cha REAL NOT NULL DEFAULT 121.0,
      depos    REAL NOT NULL DEFAULT 0.0,
      type     TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS field_fld (
      id   INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE,
      len  REAL NOT NULL DEFAULT 500.0,
      wd   REAL NOT NULL DEFAULT 100.0,
      ang  REAL NOT NULL DEFAULT 30.0
    )")

  # --- Routing unit ---------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS rout_unit_dr (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      name       TEXT NOT NULL UNIQUE,
      temp       REAL NOT NULL DEFAULT 0.0,
      flo        REAL NOT NULL DEFAULT 0.0,
      sed        REAL NOT NULL DEFAULT 0.0,
      orgn       REAL NOT NULL DEFAULT 0.0,
      sedp       REAL NOT NULL DEFAULT 0.0,
      no3        REAL NOT NULL DEFAULT 0.0,
      solp       REAL NOT NULL DEFAULT 0.0,
      pest_sol   REAL NOT NULL DEFAULT 0.0,
      pest_sorb  REAL NOT NULL DEFAULT 0.0,
      chl_a      REAL NOT NULL DEFAULT 0.0,
      nh3        REAL NOT NULL DEFAULT 0.0,
      no2        REAL NOT NULL DEFAULT 0.0,
      cbn_bod    REAL NOT NULL DEFAULT 0.0,
      dis_ox     REAL NOT NULL DEFAULT 0.0,
      bact_p     REAL NOT NULL DEFAULT 0.0,
      bact_lp    REAL NOT NULL DEFAULT 0.0,
      met1       REAL NOT NULL DEFAULT 0.0,
      met2       REAL NOT NULL DEFAULT 0.0,
      met3       REAL NOT NULL DEFAULT 0.0,
      san        REAL NOT NULL DEFAULT 0.0,
      sil        REAL NOT NULL DEFAULT 0.0,
      cla        REAL NOT NULL DEFAULT 0.0,
      sag        REAL NOT NULL DEFAULT 0.0,
      lag        REAL NOT NULL DEFAULT 0.0,
      grv        REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS rout_unit_rtu (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      dlr_id      INTEGER REFERENCES rout_unit_dr(id) ON DELETE SET NULL,
      topo_id     INTEGER REFERENCES topography_hyd(id) ON DELETE SET NULL,
      field_id    INTEGER REFERENCES field_fld(id) ON DELETE SET NULL,
      description TEXT
    )")

  # --- Soils ----------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS soils_sol (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      hyd_grp   TEXT NOT NULL DEFAULT 'B',
      dp_tot    REAL NOT NULL DEFAULT 0.0,
      anion_excl REAL NOT NULL DEFAULT 0.5,
      perc_crk  REAL NOT NULL DEFAULT 0.0,
      texture   TEXT,
      description TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS soils_sol_layer (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      soil_id   INTEGER REFERENCES soils_sol(id) ON DELETE CASCADE,
      layer_num INTEGER NOT NULL,
      dp        REAL NOT NULL,
      bd        REAL NOT NULL DEFAULT 1.4,
      awc       REAL NOT NULL DEFAULT 0.1,
      soil_k    REAL NOT NULL DEFAULT 10.0,
      carbon    REAL NOT NULL DEFAULT 1.0,
      clay      REAL NOT NULL DEFAULT 20.0,
      silt      REAL NOT NULL DEFAULT 30.0,
      sand      REAL NOT NULL DEFAULT 50.0,
      rock      REAL NOT NULL DEFAULT 0.0,
      alb       REAL NOT NULL DEFAULT 0.1,
      usle_k    REAL NOT NULL DEFAULT 0.3,
      ec        REAL NOT NULL DEFAULT 0.0,
      ph        REAL NOT NULL DEFAULT 6.5,
      cal       REAL NOT NULL DEFAULT 0.0,
      ph2       REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS nutrients_sol (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      name       TEXT NOT NULL UNIQUE,
      exp_co     REAL NOT NULL DEFAULT 0.0005,
      lab_p      REAL NOT NULL DEFAULT 5.0,
      nitrate    REAL NOT NULL DEFAULT 7.0,
      fr_hum_act REAL NOT NULL DEFAULT 0.02,
      hum_c_n    REAL NOT NULL DEFAULT 10.0,
      hum_c_p    REAL NOT NULL DEFAULT 80.0,
      inorgp     REAL NOT NULL DEFAULT 3.5,
      watersol_p REAL NOT NULL DEFAULT 0.15,
      h3a_p      REAL NOT NULL DEFAULT 0.25,
      mehlich_p  REAL NOT NULL DEFAULT 1.2,
      bray_strong_p REAL NOT NULL DEFAULT 0.85
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS soils_lte_sol (
      id   INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE,
      cn_a REAL NOT NULL DEFAULT 0.0,
      cn_b REAL NOT NULL DEFAULT 0.0,
      cn_c REAL NOT NULL DEFAULT 0.0,
      cn_d REAL NOT NULL DEFAULT 0.0
    )")

  # --- Init (water / soil / plant) ------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS om_water_ini (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      name    TEXT NOT NULL UNIQUE,
      flo     REAL NOT NULL DEFAULT 0.0,
      sed     REAL NOT NULL DEFAULT 0.0,
      orgn    REAL NOT NULL DEFAULT 0.0,
      sedp    REAL NOT NULL DEFAULT 0.0,
      no3     REAL NOT NULL DEFAULT 0.0,
      solp    REAL NOT NULL DEFAULT 0.0,
      chla    REAL NOT NULL DEFAULT 0.0,
      nh3     REAL NOT NULL DEFAULT 0.0,
      no2     REAL NOT NULL DEFAULT 0.0,
      cbod    REAL NOT NULL DEFAULT 0.0,
      dox     REAL NOT NULL DEFAULT 0.0,
      san     REAL NOT NULL DEFAULT 0.0,
      sil     REAL NOT NULL DEFAULT 0.0,
      cla     REAL NOT NULL DEFAULT 0.0,
      sag     REAL NOT NULL DEFAULT 0.0,
      lag     REAL NOT NULL DEFAULT 0.0,
      gravel  REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS soil_plant_ini (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      name       TEXT NOT NULL UNIQUE,
      sw_frac    REAL NOT NULL DEFAULT 0.0,
      nutrients_id INTEGER REFERENCES nutrients_sol(id) ON DELETE SET NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS plant_ini (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      rot_yr_ini INTEGER NOT NULL DEFAULT 1
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS plant_ini_item (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      plant_ini_id INTEGER REFERENCES plant_ini(id) ON DELETE CASCADE,
      plnt_name_id INTEGER,
      lc_status    INTEGER NOT NULL DEFAULT 0,
      lai_init     REAL NOT NULL DEFAULT 0.0,
      bm_init      REAL NOT NULL DEFAULT 0.0,
      phu_init     REAL NOT NULL DEFAULT 0.0,
      plnt_pop     REAL NOT NULL DEFAULT 0.0,
      yrs_init     REAL NOT NULL DEFAULT 0.0,
      rsd_init     REAL NOT NULL DEFAULT 10000.0
    )")

  # --- HRU parameter databases ----------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS plants_plt (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      plnt_typ  TEXT NOT NULL DEFAULT 'warm_annual',
      gro_trig  TEXT NOT NULL DEFAULT 'temp',
      nfix_co   REAL NOT NULL DEFAULT 0.0,
      days_mat  INTEGER NOT NULL DEFAULT 0,
      bm_e      REAL NOT NULL DEFAULT 30.0,
      harv_idx  REAL NOT NULL DEFAULT 0.5,
      lai_pot   REAL NOT NULL DEFAULT 5.0,
      frac_hu1  REAL NOT NULL DEFAULT 0.05,
      lai_max1  REAL NOT NULL DEFAULT 0.05,
      frac_hu2  REAL NOT NULL DEFAULT 0.95,
      lai_max2  REAL NOT NULL DEFAULT 0.95,
      hu_lai_decl REAL NOT NULL DEFAULT 0.99,
      dlai_rate REAL NOT NULL DEFAULT 0.0,
      can_ht_max REAL NOT NULL DEFAULT 1.0,
      rt_dp_max REAL NOT NULL DEFAULT 2.0,
      tmp_opt   REAL NOT NULL DEFAULT 25.0,
      tmp_base  REAL NOT NULL DEFAULT 10.0,
      frac_n_yld REAL NOT NULL DEFAULT 0.025,
      frac_p_yld REAL NOT NULL DEFAULT 0.003,
      frac_n_em  REAL NOT NULL DEFAULT 0.04,
      frac_p_em  REAL NOT NULL DEFAULT 0.005,
      frac_n_50  REAL NOT NULL DEFAULT 0.03,
      frac_p_50  REAL NOT NULL DEFAULT 0.004,
      frac_n_mat REAL NOT NULL DEFAULT 0.025,
      frac_p_mat REAL NOT NULL DEFAULT 0.003,
      harv_idx_targ REAL NOT NULL DEFAULT 0.9,
      stcon_max  REAL NOT NULL DEFAULT 80.0,
      plnt_decomp REAL NOT NULL DEFAULT 0.05,
      leaf_tov   REAL NOT NULL DEFAULT 1.0,
      frac_fall  REAL NOT NULL DEFAULT 0.0,
      ext_co     REAL NOT NULL DEFAULT 0.65,
      bm_dieoff  REAL NOT NULL DEFAULT 0.0,
      rt_dieoff  REAL NOT NULL DEFAULT 0.0,
      leaf_dieoff REAL NOT NULL DEFAULT 0.0,
      co2_hi     REAL NOT NULL DEFAULT 660.0,
      bm_e_hi    REAL NOT NULL DEFAULT 40.0,
      wstrs_id   TEXT NOT NULL DEFAULT 'lsslope',
      wstrs_pmp  REAL NOT NULL DEFAULT 0.5,
      gsi        REAL NOT NULL DEFAULT 0.005,
      vpdfr      REAL NOT NULL DEFAULT 4.0,
      frgmax     REAL NOT NULL DEFAULT 0.75,
      wavp       REAL NOT NULL DEFAULT 8.0,
      usle_c_min REAL NOT NULL DEFAULT 0.001,
      tran_co    REAL NOT NULL DEFAULT 0.0,
      rsdin_co   REAL NOT NULL DEFAULT 0.0,
      land_use_lookup_id INTEGER
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS urban_urb (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      frac_imp  REAL NOT NULL DEFAULT 0.5,
      frac_dc_imp REAL NOT NULL DEFAULT 0.5,
      ov_mann   REAL NOT NULL DEFAULT 0.015,
      det_storage REAL NOT NULL DEFAULT 2.54,
      det_cn    REAL NOT NULL DEFAULT 98.0,
      wash_co   REAL NOT NULL DEFAULT 0.1,
      pothole_dep REAL NOT NULL DEFAULT 0.0,
      t_growing_season INTEGER NOT NULL DEFAULT 0,
      t_plant   REAL NOT NULL DEFAULT 0.0,
      bmp_frac  REAL NOT NULL DEFAULT 0.0,
      description TEXT
    )")

  # --- Aquifer --------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS initial_aqu (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      org_min_id INTEGER REFERENCES om_water_ini(id) ON DELETE SET NULL,
      description TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS aquifer_aqu (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      name       TEXT NOT NULL UNIQUE,
      init_id    INTEGER REFERENCES initial_aqu(id) ON DELETE SET NULL,
      gw_flo     REAL NOT NULL DEFAULT 0.05,
      dep_bot    REAL NOT NULL DEFAULT 10.0,
      dep_wt     REAL NOT NULL DEFAULT 3.0,
      no3_n      REAL NOT NULL DEFAULT 0.0,
      sol_p      REAL NOT NULL DEFAULT 0.0,
      carbon     REAL NOT NULL DEFAULT 0.5,
      flo_dist   REAL NOT NULL DEFAULT 50.0,
      bf_max     REAL NOT NULL DEFAULT 1.0,
      alpha_bf   REAL NOT NULL DEFAULT 0.05,
      revap      REAL NOT NULL DEFAULT 0.02,
      rchg_dp    REAL NOT NULL DEFAULT 0.05,
      spec_yld   REAL NOT NULL DEFAULT 0.05,
      hl_no3n    REAL NOT NULL DEFAULT 30.0,
      flo_min    REAL NOT NULL DEFAULT 3.0,
      revap_min  REAL NOT NULL DEFAULT 5.0
    )")

  # --- Recall (point sources) -----------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS recall_rec (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      name    TEXT NOT NULL UNIQUE,
      rec_typ INTEGER NOT NULL DEFAULT 4
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS recall_dat (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      recall_rec_id  INTEGER REFERENCES recall_rec(id) ON DELETE CASCADE,
      jday    INTEGER NOT NULL DEFAULT 1,
      mo      INTEGER NOT NULL DEFAULT 1,
      day_mo  INTEGER NOT NULL DEFAULT 1,
      yr      INTEGER NOT NULL DEFAULT 1,
      ob_typ  TEXT,
      ob_name TEXT,
      flo     REAL NOT NULL DEFAULT 0.0,
      sed     REAL NOT NULL DEFAULT 0.0,
      orgn    REAL NOT NULL DEFAULT 0.0,
      sedp    REAL NOT NULL DEFAULT 0.0,
      no3     REAL NOT NULL DEFAULT 0.0,
      solp    REAL NOT NULL DEFAULT 0.0,
      chla    REAL NOT NULL DEFAULT 0.0,
      nh3     REAL NOT NULL DEFAULT 0.0,
      no2     REAL NOT NULL DEFAULT 0.0,
      cbod    REAL NOT NULL DEFAULT 0.0,
      dox     REAL NOT NULL DEFAULT 0.0,
      sand    REAL NOT NULL DEFAULT 0.0,
      silt    REAL NOT NULL DEFAULT 0.0,
      clay    REAL NOT NULL DEFAULT 0.0,
      sag     REAL NOT NULL DEFAULT 0.0,
      lag     REAL NOT NULL DEFAULT 0.0,
      gravel  REAL NOT NULL DEFAULT 0.0,
      tmp     REAL NOT NULL DEFAULT 0.0
    )")

  # --- Channel --------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS initial_cha (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      org_min_id INTEGER REFERENCES om_water_ini(id) ON DELETE SET NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS nutrients_cha (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      plt_n       REAL NOT NULL DEFAULT 0.0,
      ptl_p       REAL NOT NULL DEFAULT 0.0,
      alg_stl     REAL NOT NULL DEFAULT 1.0,
      ben_disp    REAL NOT NULL DEFAULT 0.05,
      ben_nh3n    REAL NOT NULL DEFAULT 0.5,
      ptln_stl    REAL NOT NULL DEFAULT 0.05,
      ptlp_stl    REAL NOT NULL DEFAULT 0.05,
      cst_stl     REAL NOT NULL DEFAULT 2.5,
      ben_cst     REAL NOT NULL DEFAULT 2.5,
      cbn_bod_co  REAL NOT NULL DEFAULT 1.71,
      air_rt      REAL NOT NULL DEFAULT 50.0,
      cbn_bod_stl REAL NOT NULL DEFAULT 0.36,
      ben_bod     REAL NOT NULL DEFAULT 2.0,
      bact_die    REAL NOT NULL DEFAULT 2.0,
      cst_decay   REAL NOT NULL DEFAULT 1.71,
      nh3n_no2n   REAL NOT NULL DEFAULT 0.55,
      no2n_no3n   REAL NOT NULL DEFAULT 1.1,
      ptln_nh3n   REAL NOT NULL DEFAULT 0.21,
      ptlp_solp   REAL NOT NULL DEFAULT 0.35,
      q2e_lt      REAL NOT NULL DEFAULT 2.0,
      q2e_alg     REAL NOT NULL DEFAULT 2.0,
      chla_alg    REAL NOT NULL DEFAULT 50.0,
      alg_n       REAL NOT NULL DEFAULT 0.08,
      alg_p       REAL NOT NULL DEFAULT 0.015,
      alg_o2_prod REAL NOT NULL DEFAULT 1.6,
      alg_o2_resp REAL NOT NULL DEFAULT 2.0,
      o2_nh3n     REAL NOT NULL DEFAULT 3.5,
      o2_no2n     REAL NOT NULL DEFAULT 1.07,
      alg_grow    REAL NOT NULL DEFAULT 2.0,
      alg_resp    REAL NOT NULL DEFAULT 2.5,
      slr_act     REAL NOT NULL DEFAULT 0.3,
      lt_co       REAL NOT NULL DEFAULT 0.75,
      const_n     REAL NOT NULL DEFAULT 0.02,
      const_p     REAL NOT NULL DEFAULT 0.025,
      lt_nonalg   REAL NOT NULL DEFAULT 1.0,
      alg_shd_l   REAL NOT NULL DEFAULT 0.03,
      alg_shd_nl  REAL NOT NULL DEFAULT 0.054,
      nh3_pref    REAL NOT NULL DEFAULT 0.5
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS hyd_sed_lte_cha (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      order       TEXT,
      wd          REAL NOT NULL DEFAULT 1.0,
      dp          REAL NOT NULL DEFAULT 0.5,
      slp         REAL NOT NULL DEFAULT 0.001,
      len         REAL NOT NULL DEFAULT 1.0,
      mann        REAL NOT NULL DEFAULT 0.05,
      k           REAL NOT NULL DEFAULT 1.0,
      erod_fact   REAL NOT NULL DEFAULT 0.01,
      cov_fact    REAL NOT NULL DEFAULT 0.005,
      sinu        REAL NOT NULL DEFAULT 1.05,
      eq_slp      REAL NOT NULL DEFAULT 0.001,
      d50         REAL NOT NULL DEFAULT 12.0,
      clay        REAL NOT NULL DEFAULT 50.0,
      carbon      REAL NOT NULL DEFAULT 0.04,
      dry_bd      REAL NOT NULL DEFAULT 1.0,
      side_slp    REAL NOT NULL DEFAULT 0.5,
      bankfull_flo REAL NOT NULL DEFAULT 0.5,
      fps         REAL NOT NULL DEFAULT 0.00001,
      fpn         REAL NOT NULL DEFAULT 0.1,
      n_conc      REAL NOT NULL DEFAULT 0.0,
      p_conc      REAL NOT NULL DEFAULT 0.0,
      p_bio       REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS channel_lte_cha (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      name    TEXT NOT NULL UNIQUE,
      hyd_id  INTEGER REFERENCES hyd_sed_lte_cha(id) ON DELETE SET NULL,
      init_id INTEGER REFERENCES initial_cha(id) ON DELETE SET NULL,
      nut_id  INTEGER REFERENCES nutrients_cha(id) ON DELETE SET NULL
    )")

  # --- Reservoir -----------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS initial_res (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      org_min_id INTEGER REFERENCES om_water_ini(id) ON DELETE SET NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS nutrients_res (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      mid_start   INTEGER NOT NULL DEFAULT 5,
      mid_end     INTEGER NOT NULL DEFAULT 10,
      mid_n_stl   REAL NOT NULL DEFAULT 5.5,
      n_stl       REAL NOT NULL DEFAULT 5.5,
      mid_p_stl   REAL NOT NULL DEFAULT 10.0,
      p_stl       REAL NOT NULL DEFAULT 10.0,
      chla_co     REAL NOT NULL DEFAULT 1.0,
      secchi_co   REAL NOT NULL DEFAULT 1.0,
      theta_n     REAL NOT NULL DEFAULT 1.0,
      theta_p     REAL NOT NULL DEFAULT 1.0,
      n_min_stl   REAL NOT NULL DEFAULT 0.1,
      p_min_stl   REAL NOT NULL DEFAULT 0.01
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS sediment_res (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      name     TEXT NOT NULL UNIQUE,
      sed_amt  REAL NOT NULL DEFAULT 1.0,
      d50      REAL NOT NULL DEFAULT 10.0,
      carbon   REAL NOT NULL DEFAULT 0.0,
      bd       REAL NOT NULL DEFAULT 0.0,
      sed_stl  REAL NOT NULL DEFAULT 1.0,
      stl_vel  REAL NOT NULL DEFAULT 1.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS weir_res (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      linear_c  REAL NOT NULL DEFAULT 1.84,
      exp_k     REAL NOT NULL DEFAULT 2.6,
      width     REAL NOT NULL DEFAULT 2.5,
      height    REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS hydrology_res (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      yr_op       INTEGER NOT NULL DEFAULT 1,
      mon_op      INTEGER NOT NULL DEFAULT 1,
      area_ps     REAL NOT NULL DEFAULT 0.0,
      vol_ps      REAL NOT NULL DEFAULT 0.0,
      area_es     REAL NOT NULL DEFAULT 0.0,
      vol_es      REAL NOT NULL DEFAULT 0.0,
      k           REAL NOT NULL DEFAULT 0.0,
      evap_co     REAL NOT NULL DEFAULT 0.6,
      shp_co1     REAL NOT NULL DEFAULT 0.0,
      shp_co2     REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS reservoir_res (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      name    TEXT NOT NULL UNIQUE,
      rel_id  INTEGER,
      hyd_id  INTEGER REFERENCES hydrology_res(id) ON DELETE SET NULL,
      init_id INTEGER REFERENCES initial_res(id) ON DELETE SET NULL,
      sed_id  INTEGER REFERENCES sediment_res(id) ON DELETE SET NULL,
      nut_id  INTEGER REFERENCES nutrients_res(id) ON DELETE SET NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS hydrology_wet (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      hru_ps      REAL NOT NULL DEFAULT 0.1,
      dp_ps       REAL NOT NULL DEFAULT 20.0,
      hru_es      REAL NOT NULL DEFAULT 0.25,
      dp_es       REAL NOT NULL DEFAULT 100.0,
      k           REAL NOT NULL DEFAULT 0.01,
      evap        REAL NOT NULL DEFAULT 0.7,
      vol_area_co REAL NOT NULL DEFAULT 1.0,
      vol_dp_a    REAL NOT NULL DEFAULT 1.0,
      vol_dp_b    REAL NOT NULL DEFAULT 1.0,
      hru_frac    REAL NOT NULL DEFAULT 0.5
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS wetland_wet (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      name    TEXT NOT NULL UNIQUE,
      init_id INTEGER REFERENCES initial_res(id) ON DELETE SET NULL,
      hyd_id  INTEGER REFERENCES hydrology_wet(id) ON DELETE SET NULL,
      rel_id  INTEGER,
      sed_id  INTEGER REFERENCES sediment_res(id) ON DELETE SET NULL,
      nut_id  INTEGER REFERENCES nutrients_res(id) ON DELETE SET NULL
    )")

  # --- HRU data -------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS hru_data_hru (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      name           TEXT NOT NULL UNIQUE,
      topo_id        INTEGER REFERENCES topography_hyd(id) ON DELETE SET NULL,
      hydro_id       INTEGER REFERENCES hydrology_hyd(id) ON DELETE SET NULL,
      soil_id        INTEGER REFERENCES soils_sol(id) ON DELETE SET NULL,
      lu_mgt_id      INTEGER,
      soil_plant_init_id INTEGER REFERENCES soil_plant_ini(id) ON DELETE SET NULL,
      surf_stor_id   INTEGER REFERENCES wetland_wet(id) ON DELETE SET NULL,
      snow_id        INTEGER,
      field_id       INTEGER REFERENCES field_fld(id) ON DELETE SET NULL,
      description    TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS hru_lte_hru (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      name       TEXT NOT NULL UNIQUE,
      area       REAL NOT NULL DEFAULT 0.0,
      cn2        REAL NOT NULL DEFAULT 70.0,
      cn3_swf    REAL NOT NULL DEFAULT 0.0,
      t_conc     REAL NOT NULL DEFAULT 26.0,
      soil_dp    REAL NOT NULL DEFAULT 0.0,
      perc_co    REAL NOT NULL DEFAULT 0.0,
      slp        REAL NOT NULL DEFAULT 0.0,
      slp_len    REAL NOT NULL DEFAULT 0.0,
      et_co      REAL NOT NULL DEFAULT 1.0,
      aqu_sp_yld REAL NOT NULL DEFAULT 0.05,
      alpha_bf   REAL NOT NULL DEFAULT 0.05,
      revap      REAL NOT NULL DEFAULT 0.0,
      rchg_dp    REAL NOT NULL DEFAULT 0.01,
      sw_init    REAL NOT NULL DEFAULT 0.5,
      aqu_init   REAL NOT NULL DEFAULT 3.0,
      aqu_sh_flo REAL NOT NULL DEFAULT 0.0,
      aqu_dp_flo REAL NOT NULL DEFAULT 300.0,
      snow_h2o   REAL NOT NULL DEFAULT 0.0,
      lat        REAL NOT NULL DEFAULT 0.0,
      soil_text_id INTEGER REFERENCES soils_lte_sol(id) ON DELETE SET NULL,
      trop_flag  TEXT NOT NULL DEFAULT 'non_trop',
      grow_start_id INTEGER,
      grow_end_id   INTEGER,
      plnt_typ_id   INTEGER REFERENCES plants_plt(id) ON DELETE SET NULL,
      stress     REAL NOT NULL DEFAULT 0.0,
      pet_flag   TEXT NOT NULL DEFAULT 'harg',
      irr_flag   TEXT NOT NULL DEFAULT 'no_irr',
      irr_src    TEXT NOT NULL DEFAULT 'outside_bsn',
      t_drain    REAL NOT NULL DEFAULT 0.0,
      usle_k     REAL NOT NULL DEFAULT 0.3,
      usle_c     REAL NOT NULL DEFAULT 0.001,
      usle_p     REAL NOT NULL DEFAULT 1.0,
      usle_ls    REAL NOT NULL DEFAULT 0.0
    )")

  # --- LUM ------------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS cntable_lum (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      cn_a        REAL NOT NULL DEFAULT 0.0,
      cn_b        REAL NOT NULL DEFAULT 0.0,
      cn_c        REAL NOT NULL DEFAULT 0.0,
      cn_d        REAL NOT NULL DEFAULT 0.0,
      description TEXT,
      treat       TEXT,
      cond_cov    TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS ovn_table_lum (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      ovn_mean    REAL NOT NULL DEFAULT 0.0,
      ovn_min     REAL NOT NULL DEFAULT 0.0,
      ovn_max     REAL NOT NULL DEFAULT 0.0,
      description TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS cons_prac_lum (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      usle_p      REAL NOT NULL DEFAULT 1.0,
      slp_len_max REAL NOT NULL DEFAULT 0.0,
      description TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS management_sch (
      id   INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS management_sch_auto (
      id                  INTEGER PRIMARY KEY AUTOINCREMENT,
      management_sch_id   INTEGER REFERENCES management_sch(id) ON DELETE CASCADE,
      d_table_id          INTEGER,
      plant1              TEXT,
      plant2              TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS management_sch_op (
      id                  INTEGER PRIMARY KEY AUTOINCREMENT,
      management_sch_id   INTEGER REFERENCES management_sch(id) ON DELETE CASCADE,
      op_typ   TEXT NOT NULL,
      mon      INTEGER NOT NULL DEFAULT 0,
      day      INTEGER NOT NULL DEFAULT 0,
      hu_sch   REAL NOT NULL DEFAULT 0.0,
      op_data1 TEXT,
      op_data2 TEXT,
      op_data3 REAL NOT NULL DEFAULT 0.0,
      description TEXT,
      \"order\"  INTEGER NOT NULL DEFAULT 0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS landuse_lum (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      name       TEXT NOT NULL UNIQUE,
      cal_group  TEXT,
      plnt_com_id INTEGER REFERENCES plant_ini(id) ON DELETE SET NULL,
      mgt_id     INTEGER REFERENCES management_sch(id) ON DELETE SET NULL,
      cn2_id     INTEGER REFERENCES cntable_lum(id) ON DELETE SET NULL,
      cons_prac_id INTEGER REFERENCES cons_prac_lum(id) ON DELETE SET NULL,
      urban_id   INTEGER,
      urb_ro     TEXT,
      ov_mann_id INTEGER REFERENCES ovn_table_lum(id) ON DELETE SET NULL,
      description TEXT
    )")

  # --- Decision tables ------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS d_table_dtl (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL UNIQUE,
      file_name   TEXT,
      description TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS d_table_dtl_cond (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      d_table_id INTEGER REFERENCES d_table_dtl(id) ON DELETE CASCADE,
      var       TEXT NOT NULL,
      obj       TEXT,
      obj_num   INTEGER,
      lim_var   TEXT,
      lim_op    TEXT,
      lim_const REAL,
      description TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS d_table_dtl_cond_alt (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      cond_id INTEGER REFERENCES d_table_dtl_cond(id) ON DELETE CASCADE,
      alt     TEXT NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS d_table_dtl_act (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      d_table_id INTEGER REFERENCES d_table_dtl(id) ON DELETE CASCADE,
      act_typ   TEXT NOT NULL,
      obj       TEXT,
      obj_num   INTEGER,
      name      TEXT,
      option    TEXT,
      const     REAL,
      const2    REAL,
      fp        TEXT
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS d_table_dtl_act_out (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      act_id  INTEGER REFERENCES d_table_dtl_act(id) ON DELETE CASCADE,
      outcome TEXT NOT NULL
    )")

  # --- Connect --------------------------------------------------------------
  # Base connect columns shared by all *_con tables
  .create_connect_table(con, "hru_con",       "hru_id      INTEGER REFERENCES hru_data_hru(id) ON DELETE SET NULL")
  .create_connect_table(con, "hru_lte_con",   "lhru_id     INTEGER REFERENCES hru_lte_hru(id) ON DELETE SET NULL")
  .create_connect_table(con, "rout_unit_con", "rtu_id      INTEGER REFERENCES rout_unit_rtu(id) ON DELETE SET NULL")
  .create_connect_table(con, "aquifer_con",   "aqu_id      INTEGER REFERENCES aquifer_aqu(id) ON DELETE SET NULL")
  .create_connect_table(con, "channel_con",   "cha_id      INTEGER")
  .create_connect_table(con, "chandeg_con",   "lcha_id     INTEGER REFERENCES channel_lte_cha(id) ON DELETE SET NULL")
  .create_connect_table(con, "reservoir_con", "res_id      INTEGER REFERENCES reservoir_res(id) ON DELETE SET NULL")
  .create_connect_table(con, "recall_con",    "rec_id      INTEGER REFERENCES recall_rec(id) ON DELETE SET NULL")
  .create_connect_table(con, "exco_con",      "exco_id     INTEGER")
  .create_connect_table(con, "outlet_con",    "out         INTEGER")
  .create_connect_table(con, "delratio_con",  "dlr_id      INTEGER")

  # Con_out tables
  .create_con_out_table(con, "hru_con_out",       "hru_con_id")
  .create_con_out_table(con, "hru_lte_con_out",   "hru_lte_con_id")
  .create_con_out_table(con, "rout_unit_con_out", "rtu_con_id")
  .create_con_out_table(con, "aquifer_con_out",   "aquifer_con_id")
  .create_con_out_table(con, "channel_con_out",   "channel_con_id")
  .create_con_out_table(con, "chandeg_con_out",   "chandeg_con_id")
  .create_con_out_table(con, "reservoir_con_out", "reservoir_con_id")
  .create_con_out_table(con, "recall_con_out",    "recall_con_id")
  .create_con_out_table(con, "exco_con_out",      "exco_con_id")
  .create_con_out_table(con, "outlet_con_out",    "outlet_con_id")
  .create_con_out_table(con, "delratio_con_out",  "delratio_con_id")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS rout_unit_ele (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      name    TEXT NOT NULL UNIQUE,
      rtu_id  INTEGER REFERENCES rout_unit_con(id) ON DELETE SET NULL,
      obj_typ TEXT NOT NULL,
      obj_id  INTEGER NOT NULL,
      frac    REAL NOT NULL DEFAULT 1.0,
      dlr_id  INTEGER
    )")

  # --- Regions --------------------------------------------------------------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS ls_unit_def (
      id   INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT NOT NULL UNIQUE,
      area REAL NOT NULL DEFAULT 0.0
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS ls_unit_ele (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      name          TEXT NOT NULL UNIQUE,
      obj_typ       TEXT NOT NULL,
      obj_typ_no    INTEGER NOT NULL,
      bsn_frac      REAL NOT NULL DEFAULT 0.0,
      sub_frac      REAL NOT NULL DEFAULT 0.0,
      reg_frac      REAL NOT NULL DEFAULT 0.0,
      ls_unit_def_id INTEGER REFERENCES ls_unit_def(id) ON DELETE SET NULL
    )")

  # --- GIS tables (populated by QSWAT+ / ArcSWAT+ or by gis_read.R) -------
  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_subbasins (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      area    REAL NOT NULL,
      slo1    REAL NOT NULL,
      len1    REAL NOT NULL,
      sll     REAL NOT NULL,
      lat     REAL NOT NULL,
      lon     REAL NOT NULL,
      elev    REAL NOT NULL,
      elevmin REAL NOT NULL,
      elevmax REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_channels (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      subbasin INTEGER NOT NULL,
      areac   REAL NOT NULL,
      strahler INTEGER NOT NULL,
      len2    REAL NOT NULL,
      slo2    REAL NOT NULL,
      wid2    REAL NOT NULL,
      dep2    REAL NOT NULL,
      elevmin REAL NOT NULL,
      elevmax REAL NOT NULL,
      midlat  REAL NOT NULL,
      midlon  REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_lsus (
      id       INTEGER PRIMARY KEY AUTOINCREMENT,
      category INTEGER NOT NULL,
      channel  INTEGER NOT NULL,
      area     REAL NOT NULL,
      slope    REAL NOT NULL,
      len1     REAL NOT NULL,
      csl      REAL NOT NULL,
      wid1     REAL NOT NULL,
      dep1     REAL NOT NULL,
      lat      REAL NOT NULL,
      lon      REAL NOT NULL,
      elev     REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_hrus (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      lsu     INTEGER NOT NULL,
      arsub   REAL NOT NULL,
      arlsu   REAL NOT NULL,
      landuse TEXT,
      arland  REAL NOT NULL,
      soil    TEXT NOT NULL,
      arso    REAL NOT NULL,
      slp     TEXT NOT NULL,
      arslp   REAL NOT NULL,
      slope   REAL NOT NULL,
      lat     REAL NOT NULL,
      lon     REAL NOT NULL,
      elev    REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_water (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      wtype   TEXT NOT NULL,
      lsu     INTEGER NOT NULL,
      subbasin INTEGER NOT NULL,
      area    REAL NOT NULL,
      xpr     REAL NOT NULL,
      ypr     REAL NOT NULL,
      lat     REAL NOT NULL,
      lon     REAL NOT NULL,
      elev    REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_points (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      subbasin INTEGER NOT NULL,
      ptype   TEXT NOT NULL,
      xpr     REAL NOT NULL,
      ypr     REAL NOT NULL,
      lat     REAL NOT NULL,
      lon     REAL NOT NULL,
      elev    REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_routing (
      sourceid  INTEGER PRIMARY KEY,
      sourcecat TEXT NOT NULL,
      hyd_typ   TEXT,
      sinkid    INTEGER NOT NULL,
      sinkcat   TEXT NOT NULL,
      percent   REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_aquifers (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      category   INTEGER NOT NULL,
      subbasin   INTEGER NOT NULL,
      deep_aquifer INTEGER NOT NULL,
      area       REAL NOT NULL,
      lat        REAL NOT NULL,
      lon        REAL NOT NULL,
      elev       REAL NOT NULL
    )")

  .create_table(con, "
    CREATE TABLE IF NOT EXISTS gis_deep_aquifers (
      id      INTEGER PRIMARY KEY AUTOINCREMENT,
      subbasin INTEGER NOT NULL,
      area    REAL NOT NULL,
      lat     REAL NOT NULL,
      lon     REAL NOT NULL,
      elev    REAL NOT NULL
    )")

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Internal helpers for connect table generation
# ---------------------------------------------------------------------------

.create_connect_table <- function(con, table_name, extra_col) {
  sql <- paste0("
    CREATE TABLE IF NOT EXISTS ", table_name, " (
      id        INTEGER PRIMARY KEY AUTOINCREMENT,
      name      TEXT NOT NULL UNIQUE,
      gis_id    INTEGER,
      area      REAL NOT NULL DEFAULT 0.0,
      lat       REAL NOT NULL DEFAULT 0.0,
      lon       REAL NOT NULL DEFAULT 0.0,
      elev      REAL,
      wst_id    INTEGER REFERENCES weather_sta_cli(id) ON DELETE SET NULL,
      cst_id    INTEGER,
      ovfl      INTEGER NOT NULL DEFAULT 0,
      rule      INTEGER NOT NULL DEFAULT 0,
      ", extra_col, "
    )")
  DBI::dbExecute(con, sql)
}

.create_con_out_table <- function(con, table_name, fk_col) {
  fk_table <- sub("_out$", "", table_name)
  sql <- paste0("
    CREATE TABLE IF NOT EXISTS ", table_name, " (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      ", fk_col, "  INTEGER REFERENCES ", fk_table, "(id) ON DELETE CASCADE,
      \"order\"  INTEGER NOT NULL DEFAULT 1,
      obj_typ    TEXT NOT NULL,
      obj_id     INTEGER NOT NULL,
      hyd_typ    TEXT,
      frac       REAL NOT NULL DEFAULT 1.0
    )")
  DBI::dbExecute(con, sql)
}
