library(swatplusEditoR)
library(testthat)
library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# Helper: build a minimal populated in-memory project DB
# ---------------------------------------------------------------------------

minimal_gis_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  create_project_tables(con)

  # gis_subbasins (1 subbasin)
  DBI::dbExecute(con, "
    INSERT INTO gis_subbasins (area,slo1,len1,sll,lat,lon,elev,elevmin,elevmax)
    VALUES (1000.0, 2.0, 500.0, 300.0, 45.0, -90.0, 200.0, 150.0, 250.0)")

  # gis_lsus (2 LSUs)
  DBI::dbExecute(con, "
    INSERT INTO gis_lsus (category,channel,area,slope,len1,csl,wid1,dep1,lat,lon,elev)
    VALUES (1, 1, 500.0, 2.0, 200.0, 1.0, 5.0, 0.5, 45.0, -90.0, 200.0)")
  DBI::dbExecute(con, "
    INSERT INTO gis_lsus (category,channel,area,slope,len1,csl,wid1,dep1,lat,lon,elev)
    VALUES (1, 1, 500.0, 3.0, 200.0, 1.0, 5.0, 0.5, 45.1, -90.1, 210.0)")

  # gis_channels (1 channel)
  DBI::dbExecute(con, "
    INSERT INTO gis_channels (subbasin,areac,strahler,len2,slo2,wid2,dep2,elevmin,elevmax,midlat,midlon)
    VALUES (1, 1000.0, 1, 5000.0, 0.5, 10.0, 1.0, 150.0, 200.0, 45.0, -90.0)")

  # gis_hrus (2 HRUs, one per LSU)
  DBI::dbExecute(con, "
    INSERT INTO gis_hrus (lsu,arsub,arlsu,landuse,arland,soil,arso,slp,arslp,slope,lat,lon,elev)
    VALUES (1, 1000.0, 500.0, 'corn', 500.0, 'loam', 500.0, '1', 500.0, 2.0, 45.0, -90.0, 200.0)")
  DBI::dbExecute(con, "
    INSERT INTO gis_hrus (lsu,arsub,arlsu,landuse,arland,soil,arso,slp,arslp,slope,lat,lon,elev)
    VALUES (2, 1000.0, 500.0, 'frst', 500.0, 'loam', 500.0, '1', 500.0, 3.0, 45.1, -90.1, 210.0)")

  # gis_routing (LSU -> CH, CH -> outlet)
  DBI::dbExecute(con, "
    INSERT INTO gis_routing (sourceid, sourcecat, hyd_typ, sinkid, sinkcat, percent)
    VALUES (1, 'LSU', 'tot', 1, 'CH', 100.0)")
  DBI::dbExecute(con, "
    INSERT INTO gis_routing (sourceid, sourcecat, hyd_typ, sinkid, sinkcat, percent)
    VALUES (2, 'LSU', 'tot', 1, 'CH', 100.0)")

  # soils_sol (needed for HRU import)
  DBI::dbExecute(con, "
    INSERT INTO soils_sol (name, hyd_grp, dp_tot) VALUES ('loam', 'B', 1500.0)")

  # Default LUM helper tables (needed by .insert_landuse)
  DBI::dbExecute(con, "
    INSERT INTO cntable_lum (id,name,cn_a,cn_b,cn_c,cn_d) VALUES (5,'default',60,75,83,87)")
  DBI::dbExecute(con, "
    INSERT INTO cons_prac_lum (id,name,usle_p,slp_len_max) VALUES (1,'none',1.0,100.0)")
  DBI::dbExecute(con, "
    INSERT INTO ovn_table_lum (id,name,ovn_mean,ovn_min,ovn_max) VALUES (2,'default',0.1,0.05,0.15)")

  con
}

# ---------------------------------------------------------------------------
# import_gis: routing units
# ---------------------------------------------------------------------------

test_that("import_gis creates routing unit tables", {
  con  <- minimal_gis_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tmp  <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)

  # Write in-memory db to file for import_gis (which re-opens by path)
  DBI::dbDisconnect(con)
  create_project_db(tmp)
  con2 <- swat_open_db(tmp)

  # Seed required tables into the file DB
  create_project_tables(con2)
  for (tbl in c("gis_subbasins","gis_lsus","gis_channels","gis_hrus",
                "gis_routing","soils_sol","cntable_lum","cons_prac_lum",
                "ovn_table_lum")) {
    src <- minimal_gis_db()
    rows <- DBI::dbReadTable(src, tbl)
    DBI::dbDisconnect(src)
    if (nrow(rows) > 0L) DBI::dbAppendTable(con2, tbl, rows)
  }
  DBI::dbDisconnect(con2)

  import_gis(tmp, verbose = FALSE)

  con3 <- swat_open_db(tmp)
  on.exit(DBI::dbDisconnect(con3), add = TRUE)
  expect_gt(swat_count(con3, "rout_unit_rtu"), 0L)
  expect_gt(swat_count(con3, "topography_hyd"), 0L)
  expect_gt(swat_count(con3, "field_fld"), 0L)
  expect_gt(swat_count(con3, "rout_unit_con"), 0L)
})

# ---------------------------------------------------------------------------
# import_gis: channels
# ---------------------------------------------------------------------------

test_that("import_gis creates channel_lte_cha records", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  create_project_db(tmp)

  con <- swat_open_db(tmp)
  src <- minimal_gis_db()
  for (tbl in c("gis_subbasins","gis_lsus","gis_channels","gis_hrus",
                "gis_routing","soils_sol","cntable_lum","cons_prac_lum",
                "ovn_table_lum")) {
    rows <- DBI::dbReadTable(src, tbl)
    if (nrow(rows) > 0L) DBI::dbAppendTable(con, tbl, rows)
  }
  DBI::dbDisconnect(src)
  DBI::dbDisconnect(con)

  import_gis(tmp, verbose = FALSE)

  con2 <- swat_open_db(tmp)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  expect_gt(swat_count(con2, "channel_lte_cha"), 0L)
  expect_gt(swat_count(con2, "chandeg_con"),     0L)
})

# ---------------------------------------------------------------------------
# import_gis: HRUs
# ---------------------------------------------------------------------------

test_that("import_gis creates hru_data_hru records", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  create_project_db(tmp)

  con <- swat_open_db(tmp)
  src <- minimal_gis_db()
  for (tbl in c("gis_subbasins","gis_lsus","gis_channels","gis_hrus",
                "gis_routing","soils_sol","cntable_lum","cons_prac_lum",
                "ovn_table_lum")) {
    rows <- DBI::dbReadTable(src, tbl)
    if (nrow(rows) > 0L) DBI::dbAppendTable(con, tbl, rows)
  }
  DBI::dbDisconnect(src)
  DBI::dbDisconnect(con)

  import_gis(tmp, verbose = FALSE)

  con2 <- swat_open_db(tmp)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  n_hru <- swat_count(con2, "hru_data_hru")
  expect_equal(n_hru, 2L)
})

# ---------------------------------------------------------------------------
# import_gis: marks imported_gis in project_config
# ---------------------------------------------------------------------------

test_that("import_gis sets imported_gis = 1 in project_config", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  create_project_db(tmp)

  con <- swat_open_db(tmp)
  src <- minimal_gis_db()
  for (tbl in c("gis_subbasins","gis_lsus","gis_channels","gis_hrus",
                "gis_routing","soils_sol","cntable_lum","cons_prac_lum",
                "ovn_table_lum")) {
    rows <- DBI::dbReadTable(src, tbl)
    if (nrow(rows) > 0L) DBI::dbAppendTable(con, tbl, rows)
  }
  DBI::dbExecute(con,
    "INSERT INTO project_config (project_name, imported_gis) VALUES ('test', 0)")
  DBI::dbDisconnect(src)
  DBI::dbDisconnect(con)

  import_gis(tmp, verbose = FALSE)

  con2 <- swat_open_db(tmp)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  cfg <- DBI::dbGetQuery(con2, "SELECT imported_gis FROM project_config LIMIT 1")
  expect_equal(as.integer(cfg$imported_gis), 1L)
})

# ---------------------------------------------------------------------------
# Aquifer defaults
# ---------------------------------------------------------------------------

test_that(".get_slope_len returns correct values", {
  # Test via export or directly – use an environment workaround
  f <- swatplusEditoR:::.get_slope_len
  if (is.null(f)) skip("Internal function not accessible")
  expect_equal(f(0),  121)
  expect_equal(f(2),   90)
  expect_equal(f(4),   60)
  expect_equal(f(6),   30)
  expect_equal(f(10),  10)
})

test_that(".get_perco_cn3_swf_latq_co returns valid results for hyd group A", {
  f <- swatplusEditoR:::.get_perco_cn3_swf_latq_co
  if (is.null(f)) skip("Internal function not accessible")
  res <- f("A", 5)
  expect_true(is.list(res))
  expect_named(res, c("perco", "cn3_swf", "latq_co"))
  expect_equal(res$perco, 0.9)
})
