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
  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2010)
}

test_that("run SWAT+ with basic files", {

  # output_dir <- tempfile("txtinout_")
  
  project_dir <- "swat_test"
  project <- setup_swat_sim(project_dir)
  # Set weather dir to ERA5 data from package
  weather_dir <- system.file("extdata", "era5", package = "swatplusEditoR")

  write_config_files(project, output_dir = project_dir,
                     weather_dir = weather_dir)
  
  run_swatplus(working_dir = output_dir)
})
