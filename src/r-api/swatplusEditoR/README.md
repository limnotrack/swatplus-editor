# swatplusEditoR

R interface to the SWAT+ Editor project database. Provides direct SQLite
access for managing weather stations, updating model parameters, configuring
groundwater flow (GWFLOW), and writing SWAT+ model configuration files.

## Installation

```r
# Install from source
devtools::install("src/r-api/swatplusEditoR")

# Or using remotes
remotes::install_local("src/r-api/swatplusEditoR")
```

## Quick Start

```r
library(swatplusEditoR)

# Create a project object (from your GIS/delineation workflow)
project <- list(
  project_dir = normalizePath("/path/to/project"),
  dem_file = normalizePath("/path/to/dem.tif"),
  landuse_file = normalizePath("/path/to/landuse.tif"),
  soil_file = normalizePath("/path/to/soil.tif"),
  landuse_lookup = normalizePath("/path/to/lu_lookup.csv"),
  soil_lookup = normalizePath("/path/to/soil_lookup.csv"),
  outlet_file = NULL,
  crs = "EPSG:2193",
  units = "metric",
  cell_size = c(30, 30),
  extent = c(0, 100, 0, 100),
  nrow = 100,
  ncol = 100,
  # Files populated during delineation
  fel_file = NULL, p_file = NULL, sd8_file = NULL,
  slp_file = NULL, ang_file = NULL, ad8_file = NULL,
  sca_file = NULL, src_stream_file = NULL,
  src_channel_file = NULL, ord_file = NULL,
  tree_file = NULL, coord_file = NULL,
  stream_file = NULL, watershed_file = NULL,
  # HRU results
  hru_data = NULL,
  basin_data = NULL,
  stream_threshold = NULL,
  channel_threshold = NULL,
  # Database file
  db_file = NULL
)

# Step 1: Create the project database
project <- create_project_db(project, "/path/to/project/swatplus.sqlite")

# Step 2: Load and validate the project
project <- load_project(project)

# Step 3: Read GIS data
gis <- read_gis_data(project)
subbasins <- read_gis_subbasins(project)
channels <- read_gis_channels(project)
hrus <- read_gis_hrus(project)

# Step 4: Add weather station data
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
add_weather_stations(project, stations)

# Add weather generators
wgn <- data.frame(
  name = "wgn_station1",
  lat = -38.1, lon = 176.3, elev = 350, rain_yrs = 30
)
add_weather_generators(project, wgn)

# Step 5: Update model parameters
update_parameters(project, "hydrology_hyd", list(cn2 = 65))
update_parameters(project, "hydrology_hyd", list(cn2 = 70), ids = c(1, 2, 3))

# Step 6: Set simulation time
set_simulation_time(project, day_start = 1, yrc_start = 2000,
                    day_end = 365, yrc_end = 2010)

# Step 7: Configure GWFLOW (optional)
init_gwflow(project, cell_size = 200, row_count = 100, col_count = 150)
update_gwflow_zones(project, zone_id = 1, aquifer_k = 15.0)

# Step 8: Write configuration files
write_config_files(project)
# Or with the editor executable for complete file generation:
write_config_files(project, editor_exe = "/path/to/swatplus_api.py")

# Step 9: Run SWAT+
run_swatplus(project, swat_exe = "/path/to/swatplus")
```

## Functions Reference

### Project Management
- `create_project_db()` - Create a new SWAT+ project database
- `load_project()` - Load and validate a project
- `get_project_config()` - Get project configuration
- `get_project_info()` - Get project summary information

### GIS Data
- `read_gis_data()` - Read all GIS tables
- `read_gis_subbasins()` - Read subbasin data
- `read_gis_channels()` - Read channel data
- `read_gis_hrus()` - Read HRU data
- `read_gis_lsus()` - Read landscape unit data
- `read_gis_aquifers()` - Read aquifer data
- `read_gis_deep_aquifers()` - Read deep aquifer data
- `read_gis_water()` - Read water body data
- `read_gis_points()` - Read point feature data
- `read_gis_routing()` - Read routing connectivity data

### Weather Data
- `add_weather_stations()` - Add weather stations
- `list_weather_stations()` - List all weather stations
- `update_weather_station()` - Update a weather station
- `remove_weather_stations()` - Remove weather stations
- `add_weather_generators()` - Add weather generator data
- `set_weather_dir()` - Set weather data directory
- `match_weather_stations()` - Match stations to spatial objects

### Parameters
- `update_parameters()` - Update parameters in any table
- `set_simulation_time()` - Set simulation time period
- `set_print_options()` - Set print/output options

### GWFLOW
- `get_gwflow_status()` - Check GWFLOW status
- `init_gwflow()` - Initialize GWFLOW module
- `get_gwflow_base()` - Get GWFLOW base configuration
- `update_gwflow_base()` - Update GWFLOW base settings
- `get_gwflow_zones()` - Get GWFLOW zones
- `update_gwflow_zones()` - Update GWFLOW zone parameters

### File Writing & Model Execution
- `write_config_files()` - Write SWAT+ input files
- `run_swatplus()` - Run the SWAT+ model

### Database Access
- `open_project_db()` - Open a database connection (for custom queries)

## Database Tables

The project SQLite database contains the following GIS tables (matching
the SWAT+ Editor format):

| Table | Description |
|-------|-------------|
| `gis_aquifers` | Aquifer properties (category, area, coordinates) |
| `gis_channels` | Channel geometry (length, slope, width, depth) |
| `gis_deep_aquifers` | Deep aquifer properties |
| `gis_hrus` | HRU data (landuse, soil, slope, area) |
| `gis_lsus` | Landscape units |
| `gis_points` | Point features (outlets, inlets) |
| `gis_routing` | Connectivity routing between features |
| `gis_subbasins` | Subbasin geometry (area, slope, elevation) |
| `gis_water` | Water bodies |

## Requirements

- R >= 4.0.0
- DBI
- RSQLite
- jsonlite
- httr (optional, for API server communication)
