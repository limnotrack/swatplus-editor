library(swatplusEditoR)
library(testthat)
library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# Helper: create in-memory project DB with schema
# ---------------------------------------------------------------------------

new_project_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  create_project_tables(con)
  con
}

# ---------------------------------------------------------------------------
# create_project_tables
# ---------------------------------------------------------------------------

test_that("create_project_tables creates required tables", {
  con <- new_project_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  required <- c(
    "project_config",    "time_sim",       "print_prt",
    "codes_bsn",         "parameters_bsn",
    "hydrology_hyd",     "topography_hyd", "field_fld",
    "hru_data_hru",      "hru_lte_hru",
    "rout_unit_rtu",     "rout_unit_con",
    "aquifer_aqu",       "aquifer_con",
    "channel_lte_cha",   "chandeg_con",
    "reservoir_res",     "reservoir_con",
    "recall_rec",        "recall_con",
    "soils_sol",         "soils_sol_layer",
    "om_water_ini",      "soil_plant_ini",
    "plant_ini",         "plant_ini_item",
    "plants_plt",        "landuse_lum",
    "d_table_dtl",       "weather_sta_cli",
    "weather_wgn_cli",
    "gis_subbasins",     "gis_channels",   "gis_lsus",
    "gis_hrus",          "gis_routing",    "gis_water",
    "gis_points",        "gis_aquifers",   "gis_deep_aquifers",
    "ls_unit_def",       "ls_unit_ele",
    "rout_unit_ele"
  )
  tbls <- swat_get_table_names(con)
  for (t in required) {
    expect_true(t %in% tbls, info = paste("Missing table:", t))
  }
})

test_that("create_project_tables is idempotent", {
  con <- new_project_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_no_error(create_project_tables(con))   # second call must not error
})

# ---------------------------------------------------------------------------
# create_project_db (file-based)
# ---------------------------------------------------------------------------

test_that("create_project_db creates a file with the correct schema", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  create_project_db(tmp)
  expect_true(file.exists(tmp))
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_true(swat_exists_table(con, "project_config"))
  expect_true(swat_exists_table(con, "gis_subbasins"))
})

test_that("create_project_db with overwrite replaces existing file", {
  tmp <- tempfile(fileext = ".sqlite")
  on.exit(unlink(tmp), add = TRUE)
  create_project_db(tmp)
  # Dirty the file
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  DBI::dbExecute(con, "INSERT INTO project_config (project_name) VALUES ('old')")
  DBI::dbDisconnect(con)
  # Recreate with overwrite
  create_project_db(tmp, overwrite = TRUE)
  con2 <- DBI::dbConnect(RSQLite::SQLite(), tmp)
  on.exit(DBI::dbDisconnect(con2), add = TRUE)
  n <- DBI::dbGetQuery(con2, "SELECT COUNT(*) AS n FROM project_config")$n
  expect_equal(n, 0L)
})

# ---------------------------------------------------------------------------
# GIS table round-trip
# ---------------------------------------------------------------------------

test_that("can insert and retrieve gis_subbasins rows", {
  con <- new_project_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- data.frame(
    area = 100.0, slo1 = 2.5, len1 = 500.0, sll = 300.0,
    lat = 45.0, lon = -90.0, elev = 200.0, elevmin = 150.0, elevmax = 250.0,
    stringsAsFactors = FALSE
  )
  swat_bulk_insert(con, "gis_subbasins", df)
  result <- DBI::dbGetQuery(con, "SELECT * FROM gis_subbasins")
  expect_equal(nrow(result), 1L)
  expect_equal(result$area, 100.0)
  expect_equal(result$lat,  45.0)
})

test_that("can insert and retrieve gis_hrus rows", {
  con <- new_project_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  df <- data.frame(
    lsu = 1L, arsub = 100.0, arlsu = 80.0, landuse = "corn",
    arland = 80.0, soil = "loam", arso = 80.0, slp = "1",
    arslp = 80.0, slope = 2.5, lat = 45.0, lon = -90.0, elev = 200.0,
    stringsAsFactors = FALSE
  )
  swat_bulk_insert(con, "gis_hrus", df)
  result <- DBI::dbGetQuery(con, "SELECT * FROM gis_hrus")
  expect_equal(nrow(result), 1L)
  expect_equal(result$landuse, "corn")
})
