setup_swat_sim <- function() {
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
}

test_that("run SWAT+ with basic files", {

  output_dir <- tempfile("txtinout_")
  on.exit({
    unlink(project$db_file)
    unlink(output_dir, recursive = TRUE)
  })
  
  project <- setup_swat_sim()
  
  write_config_files(project, output_dir = output_dir)
  
  run_swatplus(working_dir = )
})
