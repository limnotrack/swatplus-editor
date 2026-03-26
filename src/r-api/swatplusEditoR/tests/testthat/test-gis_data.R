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
