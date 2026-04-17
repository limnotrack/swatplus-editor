# GIS data reading functions for swatplusEditoR
# Read gis_* tables from the SWAT+ project SQLite database

#' Read all GIS data from the project database
#'
#' Returns a list containing all available GIS tables as data.frames.
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A named list of data.frames for each GIS table present in the
#'   database.
#' @export
#' @examples
#' \dontrun{
#' gis <- read_gis_data(project)
#' names(gis)
#' head(gis$subbasins)
#' }
read_gis_data <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))

  tables <- list_db_tables(con)
  gis_names <- c("gis_aquifers", "gis_channels", "gis_deep_aquifers",
                  "gis_hrus", "gis_lsus", "gis_points", "gis_routing",
                  "gis_subbasins", "gis_water")

  result <- list()
  for (tbl in gis_names) {
    short_name <- sub("^gis_", "", tbl)
    if (tbl %in% tables) {
      result[[short_name]] <- read_db_table(con, tbl)
    }
  }

  message("Read ", length(result), " GIS tables from database")
  result
}

#' Read GIS subbasins table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with subbasin data (area, slope, length, elevation).
#' @export
read_gis_subbasins <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_subbasins")
}

#' Read GIS channels table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with channel data (length, slope, width, depth).
#' @export
read_gis_channels <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_channels")
}

#' Read GIS HRUs table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with HRU data (landuse, soil, slope, area).
#' @export
read_gis_hrus <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_hrus")
}

#' Read GIS landscape units table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with LSU data.
#' @export
read_gis_lsus <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_lsus")
}

#' Read GIS aquifers table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with aquifer data.
#' @export
read_gis_aquifers <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_aquifers")
}

#' Read GIS deep aquifers table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with deep aquifer data.
#' @export
read_gis_deep_aquifers <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_deep_aquifers")
}

#' Read GIS water bodies table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with water body data.
#' @export
read_gis_water <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_water")
}

#' Read GIS points table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with point feature data.
#' @export
read_gis_points <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_points")
}

#' Read GIS routing table
#'
#' @param project List. A SWAT+ project object with \code{db_file}.
#' @return A data.frame with routing connectivity data.
#' @export
read_gis_routing <- function(project) {
  validate_project(project)
  con <- open_project_db(project$db_file)
  on.exit(close_db(con))
  read_db_table(con, "gis_routing")
}
