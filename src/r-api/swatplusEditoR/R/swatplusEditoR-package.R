#' @keywords internal
"_PACKAGE"

#' swatplusEditoR: R Interface to SWAT+ Editor API
#'
#' Provides an R interface to the SWAT+ Editor project database.
#' Supports direct SQLite access for managing weather stations, updating model
#' parameters, configuring groundwater flow (GWFLOW), and writing SWAT+ model
#' configuration files.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{load_project}}}{Load a SWAT+ project from an R project object}
#'   \item{\code{\link{add_weather_stations}}}{Add weather station data}
#'   \item{\code{\link{update_parameters}}}{Update model parameters}
#'   \item{\code{\link{write_config_files}}}{Write SWAT+ model configuration files}
#'   \item{\code{\link{init_gwflow}}}{Initialize groundwater flow module}
#' }
#'
#' @name swatplusEditoR-package
#' @aliases swatplusEditoR
NULL
