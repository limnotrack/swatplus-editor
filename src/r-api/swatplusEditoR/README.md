# swatplusEditoR

An R package that replicates the functionality of the [SWAT+ Editor](https://github.com/limnotrack/swatplus-editor) Python API (`src/api`), adding new capabilities to read spatial data (shapefiles and rasters) directly from R and to delineate watersheds using TauDEM without requiring [QSWAT+](https://github.com/swat-model/QSWATPlus).

---

## Overview

The SWAT+ Editor is a graphical interface for building and managing [SWAT+](https://swatplus.gitbook.io/docs) (Soil and Water Assessment Tool – Plus) hydrological model projects.  Its backend is written in Python (located under `src/api/`).

In the standard desktop workflow, watershed delineation is handled by [QSWAT+](https://github.com/swat-model/QSWATPlus) — a QGIS plugin that calls [TauDEM](https://hydrology.usu.edu/taudem/taudem5/) internally — and the SWAT+ Editor Python API only imports the *results* of that step.

**swatplusEditoR** provides an R-native equivalent of the full workflow so that R users can:

- **Delineate** a watershed from a DEM using TauDEM (via the [`traudem`](https://github.com/lucarraro/traudem) package), replicating what QSWAT+ does — without needing QGIS or QSWAT+ installed.
- Read **shapefiles** (subbasins, channels, landscape units, HRUs, water bodies, point sources, aquifers) using the [`sf`](https://r-spatial.github.io/sf/) package.
- Read **raster** data (DEM, land-use, soils) using the [`terra`](https://rspatial.org/terra) package.
- **Write** all spatial and tabular GIS data to the dedicated SWAT+ SQLite project database.
- Import GIS tables into the full SWAT+ model object tables (routing units, channels, HRUs, aquifers, connections, etc.).
- Import **weather data** (observed files, WGN, SWAT 2012 format, atmospheric deposition).
- **Write** all SWAT+ plain-text input files from the database.
- **Run** the SWAT+ executable.
- **Read** simulation output CSV files back into SQLite.
- Execute the **complete workflow** from a single `run_all()` call.

For a step-by-step walkthrough, see the
[**Getting Started vignette**](vignettes/swatplusEditoR.Rmd).

---

## Installation

```r
# Install from the local source directory
install.packages("src/r-api/swatplusEditoR",
                 repos = NULL,
                 type  = "source")
```

Or with `devtools` / `pak`:

```r
devtools::install_local("src/r-api/swatplusEditoR")
pak::local_install("src/r-api/swatplusEditoR")
```

### Dependencies

| Package    | Purpose                                       |
|------------|-----------------------------------------------|
| `DBI`      | Generic database interface                    |
| `RSQLite`  | SQLite driver                                 |
| `sf`       | Shapefile / vector spatial data I/O           |
| `terra`    | Raster data I/O                               |
| `dplyr`    | Data manipulation helpers                     |
| `stringr`  | String utilities                              |
| `jsonlite` | JSON progress messages                        |

---

## Quick Start

### 1 – Create a project database and import GIS shapefiles

```r
library(swatplusEditoR)

project_db  <- "/path/to/myproject/myproject.sqlite"
datasets_db <- "/path/to/swatplus_datasets.sqlite"
gis_dir     <- "/path/to/qswatplus/output"   # contains subs1.shp, rivs1.shp, etc.

# Create the project database with the full schema
create_project_db(project_db)

# Open a connection to start populating the database
con <- swat_open_db(project_db)
create_project_tables(con)

# Read all standard shapefiles from the GIS directory
gis_data <- read_swatplus_gis(gis_dir, dem_file = file.path(gis_dir, "dem.tif"))

# Write them into the project database
write_gis_to_db(con,
                subbasins     = gis_data$subbasins,
                channels      = gis_data$channels,
                lsus          = gis_data$lsus,
                hrus          = gis_data$hrus,
                water         = gis_data$water,
                points        = gis_data$points,
                aquifers      = gis_data$aquifers,
                deep_aquifers = gis_data$deep_aquifers,
                routing       = gis_data$routing)

swat_close_db(con)

# Set up the project (copies reference data from datasets DB)
setup_project(project_db  = project_db,
              datasets_db = datasets_db,
              editor_version = "2.3.0",
              project_name   = "My First R SWAT+ Project")
```

### 2 – Import weather data and write input files

```r
# Import WGN generator data
import_wgn(project_db, wgn_db = "/path/to/wgn.sqlite")

# Import observed weather files
import_weather(project_db, weather_dir = "/path/to/weather")
match_weather_stations(project_db)

# Write all SWAT+ input text files
write_swatplus_files(project_db, output_dir = "/path/to/project/Scenarios/Default/TxtInOut")
```

### 3 – Run the model and read output

```r
run_all(
  project_db     = project_db,
  swat_exe       = "/path/to/rev61.3.5_64rel.exe",
  output_dir     = "/path/to/project/Scenarios/Default/TxtInOut",
  output_db      = "/path/to/myproject_output.sqlite",
  yrc_start      = 2000L,
  yrc_end        = 2010L,
  run_setup      = FALSE,   # already done above
  run_gis_import = FALSE,
  run_write_files = FALSE   # already done above
)
```

### 4 – Complete workflow in one call

```r
run_all(
  project_db     = project_db,
  datasets_db    = datasets_db,
  swat_exe       = "/path/to/rev61.3.5_64rel.exe",
  output_dir     = "/path/to/txtinout",
  wgn_db         = "/path/to/wgn.sqlite",
  weather_dir    = "/path/to/weather",
  yrc_start      = 1990L, yrc_end = 2020L,
  editor_version = "2.3.0",
  project_name   = "Demo"
)
```

---

## Package Structure

```
src/r-api/swatplusEditoR/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── db_utils.R         # Low-level SQLite helpers  (≈ database/lib.py)
│   ├── utils.R            # String/number/path utils  (≈ helpers/utils.py)
│   ├── schema.R           # SQL DDL – all project DB tables
│   ├── gis_read.R         # Read shapefiles + rasters → data.frames  [NEW]
│   ├── gis_import.R       # GIS → SWAT+ object tables (≈ import_gis.py)
│   ├── create_db.R        # Create project/output DBs (≈ create_databases.py)
│   ├── setup_project.R    # Project initialisation    (≈ setup_project.py)
│   ├── import_weather.R   # WGN / observed weather    (≈ import_weather.py)
│   ├── write_files.R      # Write all input text files(≈ write_files.py)
│   ├── read_output.R      # Read simulation output    (≈ read_output.py)
│   └── run_all.R          # Orchestrate workflow      (≈ run_all.py)
└── tests/
    └── testthat/
        ├── helper-setup.R
        ├── test-db_utils.R
        ├── test-utils.R
        ├── test-schema.R
        ├── test-gis_import.R
        ├── test-import_weather.R
        ├── test-write_files.R
        └── test-read_output.R
```

---

## Key Function Reference

| R Function | Python Equivalent |
|---|---|
| `create_project_db()` | `CreateProjectDb.create()` |
| `create_project_tables()` | `SetupProjectDatabase.create_tables()` |
| `read_subbasins_shp()` | *(new – reads shapefiles directly)* |
| `read_channels_shp()` | *(new)* |
| `read_lsus_shp()` | *(new)* |
| `read_hrus_shp()` | *(new)* |
| `read_dem_raster()` | *(new – reads DEM rasters)* |
| `write_gis_to_db()` | *(new – writes shapefile data to GIS tables)* |
| `read_swatplus_gis()` | *(new – reads entire QSWAT+ output folder)* |
| `import_gis()` | `GisImport.insert_default()` |
| `setup_project()` | `SetupProject` class |
| `import_wgn()` | `WgnImport` class |
| `import_weather()` | `WeatherImport` class |
| `match_weather_stations()` | `update_closest_lat_lon()` |
| `import_atmo_dep()` | `AtmoImport` class |
| `write_swatplus_files()` | `WriteFiles.write()` |
| `read_output()` | `ReadOutput` class |
| `run_all()` | `RunAll` class |
| `swat_bulk_insert()` | `db_lib.bulk_insert()` |
| `swat_copy_table()` | `db_lib.copy_table()` |
| `get_swat_name()` | `get_name()` |
| `weather_sta_name()` | `weather_sta_name()` |

---

## Development

```r
# Run the test suite
testthat::test_local("src/r-api/swatplusEditoR")

# Build documentation
roxygen2::roxygenise("src/r-api/swatplusEditoR")
```
