setup_swat_sim <- function(project_dir) {
  dem <- system.file("extdata", "ravn_dem.tif", package = "rQSWATPlus")
  landuse <- system.file("extdata", "ravn_landuse.tif", package = "rQSWATPlus")
  soil <- system.file("extdata", "ravn_soil.tif", package = "rQSWATPlus")
  lu_lookup <- system.file("extdata", "ravn_landuse.csv", package = "rQSWATPlus")
  soil_lookup <- system.file("extdata", "ravn_soil.csv", package = "rQSWATPlus")
  outlet <- system.file("extdata", "ravn_outlet.shp", package = "rQSWATPlus")
  
  project <- rQSWATPlus::qswat_run(
    project_dir = file.path(tempdir(), "ravn_quick"),
    dem_file = dem,
    landuse_file = landuse,
    soil_file = soil,
    landuse_lookup = lu_lookup,
    soil_lookup = soil_lookup,
    outlet_file = outlet,
    threshold = 500,
    slope_breaks = c(0, 5, 15, 9999),
    landuse_threshold = 5,
    soil_threshold = 5, 
    db_file = "swat.db",
    quiet = TRUE
  ) 
  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)
  stations <- data.frame(
    name = c("station1", "station2"),
    lat = c(-38.1, -38.2),
    lon = c(176.3, 176.4),
    pcp = c("pcp1.cli", "pcp2.cli"),
    tmp = c("tmp1.cli", "tmp2.cli"),
    slr = c("slr1.cli", "slr2.cli"),
    hmd = c("hmd1.cli", "hmd2.cli"),
    wnd = c("wnd1.cli", "wnd2.cli"),
    pet = c("pet1.cli", "pet2.cli"),
    atmo_dep = c("atmo1.cli", "atmo2.cli"),
    stringsAsFactors = FALSE
  )
  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2010)
  add_weather_stations(project, stations)
}

test_that("run SWAT+ with basic files", {

  output_dir <- tempfile("txtinout_")
  
  project_dir <- "swat_test"
  project <- setup_swat_sim(project_dir)
  
  write_config_files(project, output_dir = project_dir)
  
  run_swatplus(working_dir = output_dir)
})
