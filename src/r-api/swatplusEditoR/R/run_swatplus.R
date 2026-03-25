#' Run the SWAT+ model executable
#'
#' Functions for launching the SWAT+ model process, either the bundled
#' Windows executable shipped with the package or a user-supplied one.
#' Uses \pkg{processx} for reliable cross-platform process management with
#' live stdout streaming and a clean exit-code check.
#'
#' @keywords internal
NULL

# ===========================================================================
# Helpers
# ===========================================================================

#' Return the path to the bundled SWAT+ Windows executable
#'
#' The package ships a pre-built SWAT+ Windows executable in
#' \code{inst/extbin/}.  This function resolves its path at run time so that
#' callers do not need to hard-code file names.
#'
#' @return A character string: the full path to the bundled
#'   \code{.exe} file, or \code{NULL} when no executable is found (e.g. on
#'   non-Windows platforms or in a source install without the binary).
#' @export
swatplus_exe <- function() {
  exe_dir <- system.file("extbin", package = "swatplusEditoR")
  if (!nzchar(exe_dir) || !dir.exists(exe_dir)) return(NULL)
  exes <- list.files(exe_dir, pattern = "\\.exe$", full.names = TRUE)
  if (length(exes) == 0L) return(NULL)
  # Return the most recent executable by name (newest version last alphabetically)
  exes[[length(exes)]]
}

# ===========================================================================
# Main function
# ===========================================================================

#' Run the SWAT+ model
#'
#' Launches the SWAT+ executable in \code{working_dir} using
#' \pkg{processx}, streams stdout/stderr in real time (when \code{verbose =
#' TRUE}), and returns a tidy summary of the run.
#'
#' Compared to the bare \code{system2()} call that was used previously,
#' \pkg{processx} provides:
#' \itemize{
#'   \item Non-blocking, line-by-line streaming of model output.
#'   \item A clean exit-code check: non-zero exit raises an R warning rather
#'     than silently succeeding.
#'   \item Elapsed-time reporting.
#'   \item A \code{timeout} parameter so long-running models can be killed.
#' }
#'
#' @param swat_exe     Path to the SWAT+ executable.  Defaults to
#'   \code{\link{swatplus_exe}()} (the bundled Windows binary).  On
#'   non-Windows platforms you must supply a compatible native binary.
#' @param working_dir  Directory that contains the SWAT+ input files
#'   (\code{file.cio}, \code{basins.def}, etc.).  The executable is launched
#'   with this as its working directory.  Defaults to the current working
#'   directory.
#' @param args         Character vector of additional command-line arguments
#'   passed to the executable (rarely needed; SWAT+ reads \code{file.cio}
#'   automatically).  Default \code{character(0)}.
#' @param timeout      Maximum number of seconds to wait for the model to
#'   finish.  Passed to \code{processx::run()}.  Use \code{Inf} (the default)
#'   for no timeout.
#' @param echo         Print stdout/stderr lines to the R console as they
#'   arrive.  Default \code{TRUE} when \code{verbose = TRUE}.
#' @param verbose      Print progress messages via \code{\link{emit_progress}}.
#'   Default \code{TRUE}.
#'
#' @return Invisibly, a named list:
#' \describe{
#'   \item{\code{status}}{Integer exit code (0 = success).}
#'   \item{\code{stdout}}{Character vector of stdout lines.}
#'   \item{\code{stderr}}{Character vector of stderr lines.}
#'   \item{\code{elapsed}}{Elapsed wall-clock time in seconds (\code{numeric}).}
#'   \item{\code{success}}{Logical: \code{TRUE} when \code{status == 0}.}
#' }
#'
#' @seealso \code{\link{run_all}}, \code{\link{swatplus_exe}}
#' @importFrom utils tail
#' @export
run_swatplus <- function(swat_exe    = swatplus_exe(),
                         working_dir = getwd(),
                         args        = character(0),
                         timeout     = Inf,
                         echo        = verbose,
                         verbose     = TRUE) {

  if (!requireNamespace("processx", quietly = TRUE))
    stop(
      "Package 'processx' is required to run the SWAT+ model.\n",
      "Install it with: install.packages('processx')",
      call. = FALSE
    )

  # --- Validate inputs -------------------------------------------------------
  if (is.null(swat_exe) || !nzchar(swat_exe))
    stop("No SWAT+ executable supplied and none found via swatplus_exe().",
         call. = FALSE)
  if (!file.exists(swat_exe))
    stop("SWAT+ executable not found: ", swat_exe, call. = FALSE)
  if (!dir.exists(working_dir))
    stop("Working directory not found: ", working_dir, call. = FALSE)

  if (verbose)
    emit_progress("Running SWAT+ model...")

  t_start <- proc.time()[["elapsed"]]

  # processx::run() blocks until the process exits, streaming output when
  # echo = TRUE.  It never raises on non-zero exit (we check status ourselves).
  proc_timeout <- if (is.finite(timeout)) timeout else NULL

  result <- processx::run(
    command    = swat_exe,
    args       = args,
    wd         = working_dir,
    timeout    = proc_timeout,
    echo       = echo,
    echo_cmd   = FALSE,
    error_on_status = FALSE   # we inspect status ourselves
  )

  elapsed <- proc.time()[["elapsed"]] - t_start

  if (result$status != 0L) {
    stderr_tail <- paste(
      utils::tail(strsplit(result$stderr, "\n", fixed = TRUE)[[1L]], 10L),
      collapse = "\n"
    )
    warning(
      sprintf("SWAT+ exited with status %d.\n%s",
              result$status, stderr_tail),
      call. = FALSE
    )
  } else if (verbose) {
    emit_progress(sprintf(
      "SWAT+ completed successfully in %.1f s.", elapsed
    ))
  }

  out <- list(
    status  = result$status,
    stdout  = strsplit(result$stdout, "\n", fixed = TRUE)[[1L]],
    stderr  = strsplit(result$stderr, "\n", fixed = TRUE)[[1L]],
    elapsed = elapsed,
    success = result$status == 0L
  )
  invisible(out)
}
