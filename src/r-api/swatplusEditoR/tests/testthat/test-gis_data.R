# Test GIS data reading functions

# Helper to create a test project with GIS data
create_gis_test_project <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()

  project <- list(
    project_dir = project_dir,
    db_file = db_path,
    hru_data = NULL,
    basin_data = NULL
  )

  project <- create_project_db(project, db_path, overwrite = TRUE)

  # Insert some test GIS data
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  DBI::dbExecute(con,
    "INSERT INTO gis_subbasins (id, area, slo1, len1, sll, lat, lon, elev, elevmin, elevmax)
     VALUES (1, 100.5, 0.05, 500, 0.04, -38.1, 176.3, 350, 300, 400)")
  DBI::dbExecute(con,
    "INSERT INTO gis_subbasins (id, area, slo1, len1, sll, lat, lon, elev, elevmin, elevmax)
     VALUES (2, 200.3, 0.08, 750, 0.06, -38.2, 176.4, 320, 280, 380)")

  DBI::dbExecute(con,
    "INSERT INTO gis_channels (id, subbasin, areac, strahler, len2, slo2, wid2, dep2, elevmin, elevmax, midlat, midlon)
     VALUES (1, 1, 50.0, 2, 1000, 0.01, 5.0, 1.5, 300, 350, -38.15, 176.35)")

  DBI::dbExecute(con,
    "INSERT INTO gis_hrus (id, lsu, arsub, arlsu, landuse, arland, soil, arso, slp, arslp, slope, lat, lon, elev)
     VALUES (1, 1, 50.0, 50.0, 'FRST', 50.0, 'LOAM', 50.0, '0-8', 50.0, 0.04, -38.1, 176.3, 350)")

  DBI::dbExecute(con,
    "INSERT INTO gis_routing (sourceid, sourcecat, hyd_typ, sinkid, sinkcat, percent)
     VALUES (1, 'sub', 'tot', 2, 'sub', 100.0)")

  DBI::dbDisconnect(con)
  project
}

# Helper to create a test project with required tables for standard channel testing
create_channel_test_project <- function() {
  project <- create_gis_test_project()
  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)

  # Create the required channel tables
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS channel_cha (
    id INTEGER PRIMARY KEY, name TEXT UNIQUE,
    init_id INTEGER, hyd_id INTEGER, sed_id INTEGER, nut_id INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS hydrology_cha (
    id INTEGER PRIMARY KEY, name TEXT UNIQUE,
    wd REAL, dp REAL, slp REAL, len REAL, mann REAL, k REAL)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS sediment_cha (
    id INTEGER PRIMARY KEY, name TEXT UNIQUE)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS nutrients_cha (
    id INTEGER PRIMARY KEY, name TEXT UNIQUE,
    alg_stl REAL, ben_disp REAL, ben_nh3n REAL, cbn_bod_co REAL,
    alg_grow REAL, nh3_pref REAL)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS initial_cha (
    id INTEGER PRIMARY KEY, name TEXT, org_min_id INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS om_water_ini (
    id INTEGER PRIMARY KEY, name TEXT)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS chandeg_con (
    id INTEGER PRIMARY KEY, name TEXT, gis_id INTEGER,
    area REAL, lat REAL, lon REAL, elev REAL, ovfl INTEGER, rule INTEGER)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS object_cnt (
    id INTEGER PRIMARY KEY, name TEXT, ls_area REAL, tot_area REAL,
    obj INTEGER DEFAULT 0, hru INTEGER DEFAULT 0, lhru INTEGER DEFAULT 0,
    rtu INTEGER DEFAULT 0, gwfl INTEGER DEFAULT 0, aqu INTEGER DEFAULT 0,
    cha INTEGER DEFAULT 0, res INTEGER DEFAULT 0, rec INTEGER DEFAULT 0,
    exco INTEGER DEFAULT 0, dlr INTEGER DEFAULT 0, can INTEGER DEFAULT 0,
    pmp INTEGER DEFAULT 0, out INTEGER DEFAULT 0, lcha INTEGER DEFAULT 0,
    aqu2d INTEGER DEFAULT 0, hrd INTEGER DEFAULT 0, wro INTEGER DEFAULT 0)")
  DBI::dbExecute(con, "INSERT OR IGNORE INTO object_cnt (id, name) VALUES (1, 'test')")
  DBI::dbExecute(con, "INSERT OR IGNORE INTO om_water_ini (id, name) VALUES (1, 'omwater1')")

  # Also create channel_con (LTE channel connection table) for counting

  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS channel_con (
    id INTEGER PRIMARY KEY, name TEXT)")

  DBI::dbDisconnect(con)
  project
}

test_that("read_gis_data returns all GIS tables", {
  project <- create_gis_test_project()
  on.exit(unlink(project$db_file))

  gis <- read_gis_data(project)
  expect_true(is.list(gis))
  expect_true("subbasins" %in% names(gis))
  expect_true("channels" %in% names(gis))
  expect_true("hrus" %in% names(gis))
  expect_true("routing" %in% names(gis))
})

test_that("read_gis_subbasins returns correct data", {
  project <- create_gis_test_project()
  on.exit(unlink(project$db_file))

  subs <- read_gis_subbasins(project)
  expect_equal(nrow(subs), 2)
  expect_true("area" %in% names(subs))
  expect_equal(subs$area[1], 100.5)
})

test_that("read_gis_channels returns correct data", {
  project <- create_gis_test_project()
  on.exit(unlink(project$db_file))

  channels <- read_gis_channels(project)
  expect_equal(nrow(channels), 1)
  expect_equal(channels$strahler[1], 2)
})

test_that("read_gis_hrus returns correct data", {
  project <- create_gis_test_project()
  on.exit(unlink(project$db_file))

  hrus <- read_gis_hrus(project)
  expect_equal(nrow(hrus), 1)
  expect_equal(hrus$landuse[1], "FRST")
  expect_equal(hrus$soil[1], "LOAM")
})

test_that("read_gis_routing returns correct data", {
  project <- create_gis_test_project()
  on.exit(unlink(project$db_file))

  routing <- read_gis_routing(project)
  expect_equal(nrow(routing), 1)
  expect_equal(routing$sourcecat[1], "sub")
  expect_equal(routing$percent[1], 100.0)
})

# -------------------------------------------------------------------
# Tests for standard channel insertion (.gis_insert_channels)
# -------------------------------------------------------------------

test_that(".gis_insert_channels populates channel_cha and hydrology_cha", {
  project <- create_channel_test_project()
  on.exit(unlink(project$db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Call the internal function
  swatplusEditoR:::.gis_insert_channels(con)

  # channel_cha should have 1 row (one channel from gis_channels)
  cha <- DBI::dbGetQuery(con, "SELECT * FROM channel_cha")
  expect_equal(nrow(cha), 1)
  expect_true(grepl("^cha", cha$name[1]))
  expect_equal(cha$hyd_id[1], 1L)

  # hydrology_cha should have 1 row
  hyd <- DBI::dbGetQuery(con, "SELECT * FROM hydrology_cha")
  expect_equal(nrow(hyd), 1)
  expect_equal(hyd$wd[1], 5.0)
  expect_equal(hyd$dp[1], 1.5)
  expect_true(hyd$mann[1] == 0.05)

  # sediment_cha should have one row per channel
  sed <- DBI::dbGetQuery(con, "SELECT * FROM sediment_cha")
  expect_equal(nrow(sed), 1)
  expect_true(grepl("^sed", sed$name[1]))

  # nutrients_cha should have 1 default row with physics-based defaults
  nut <- DBI::dbGetQuery(con, "SELECT * FROM nutrients_cha")
  expect_equal(nrow(nut), 1)
  expect_equal(nut$alg_stl[1],    1.0)
  expect_equal(nut$ben_disp[1],   0.05)
  expect_equal(nut$ben_nh3n[1],   0.5)
  expect_equal(nut$cbn_bod_co[1], 1.71)
  expect_equal(nut$alg_grow[1],   2.0)
  expect_equal(nut$nh3_pref[1],   0.5)

  # chandeg_con should have 1 row
  chandeg <- DBI::dbGetQuery(con, "SELECT * FROM chandeg_con")
  expect_equal(nrow(chandeg), 1)
  expect_equal(chandeg$gis_id[1], 1L)

  # channel_lte_cha should NOT be populated (standard mode)
  lte_count <- tryCatch(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM channel_lte_cha")$n[1],
    error = function(e) 0L)
  expect_equal(lte_count, 0L)
})

test_that(".gis_insert_channels is idempotent", {
  project <- create_channel_test_project()
  on.exit(unlink(project$db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  swatplusEditoR:::.gis_insert_channels(con)
  swatplusEditoR:::.gis_insert_channels(con)  # second call should be no-op

  cha <- DBI::dbGetQuery(con, "SELECT * FROM channel_cha")
  expect_equal(nrow(cha), 1)
})

# -------------------------------------------------------------------
# Tests for object_cnt update
# -------------------------------------------------------------------

test_that(".gis_update_object_cnt sets lcha from chandeg_con count", {
  project <- create_channel_test_project()
  on.exit(unlink(project$db_file))

  con <- DBI::dbConnect(RSQLite::SQLite(), project$db_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Insert a channel to populate chandeg_con
  swatplusEditoR:::.gis_insert_channels(con)

  # Update object counts
  swatplusEditoR:::.gis_update_object_cnt(con)

  cnt <- DBI::dbGetQuery(con, "SELECT * FROM object_cnt LIMIT 1")

  # lcha should be 1 (from 1 row in chandeg_con)
  expect_equal(cnt$lcha, 1L)
  # cha should be 0 (channel_con is empty for standard mode)
  expect_equal(cnt$cha, 0L)
  # obj should include lcha in the total
  expect_true(cnt$obj >= cnt$lcha)
})
