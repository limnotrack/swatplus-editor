## Tests for run_swatplus.R
## ─────────────────────────────────────────────────────────────────────────────
## Tests are organised in two tiers:
##   1. API / validation tests — always run, no processx or SWAT+ binary needed
##   2. Minimal reprex test   — requires processx + runnable SWAT+ binary
##      (skipped automatically on non-Windows or when the exe is not executable)

library(swatplusEditoR)

# ===========================================================================
# 1. API / validation tests
# ===========================================================================

test_that("swatplus_exe() returns NULL or a valid .exe path", {
  exe <- swatplus_exe()
  if (is.null(exe)) {
    # Acceptable when the package is installed without the binary or on Linux
    expect_null(exe)
  } else {
    expect_type(exe, "character")
    expect_match(exe, "\\.exe$", ignore.case = TRUE)
    expect_true(file.exists(exe))
  }
})

test_that("swatplus_exe() points to the bundled binary when extbin is present", {
  exe_dir <- system.file("extbin", package = "swatplusEditoR")
  skip_if(!nzchar(exe_dir), "extbin directory not found in installed package")
  skip_if(!dir.exists(exe_dir), "extbin directory does not exist")

  exes <- list.files(exe_dir, pattern = "\\.exe$")
  skip_if(length(exes) == 0L, "No .exe found in extbin")

  exe <- swatplus_exe()
  expect_false(is.null(exe))
  expect_true(file.exists(exe))
  # Must be inside extbin
  expect_true(startsWith(normalizePath(exe), normalizePath(exe_dir)))
})

test_that("run_swatplus() errors when swat_exe is NULL", {
  skip_if_not_installed("processx")
  expect_error(
    run_swatplus(swat_exe = NULL, working_dir = tempdir()),
    "No SWAT\\+ executable supplied"
  )
})

test_that("run_swatplus() errors when swat_exe path does not exist", {
  skip_if_not_installed("processx")
  expect_error(
    run_swatplus(swat_exe = "/nonexistent/swatplus.exe", working_dir = tempdir()),
    "not found"
  )
})

test_that("run_swatplus() errors when working_dir does not exist", {
  skip_if_not_installed("processx")
  skip_if(.Platform$OS.type != "windows", "need a runnable executable for this check")
  exe <- swatplus_exe()
  skip_if(is.null(exe), "no bundled exe available")
  expect_error(
    run_swatplus(swat_exe = exe, working_dir = "/nonexistent/dir"),
    "Working directory not found"
  )
})

test_that("run_swatplus() returns expected list structure on process error", {
  skip_if_not_installed("processx")
  # Use a real executable that exits immediately (non-zero status expected
  # without a valid file.cio) — this confirms the return-value shape.
  skip_if(.Platform$OS.type != "windows", "bundled exe is Windows-only")
  exe <- swatplus_exe()
  skip_if(is.null(exe), "no bundled exe available")

  tmp <- tempdir()
  # Run with an empty directory: SWAT+ will fail to find file.cio → non-zero exit
  result <- suppressWarnings(
    run_swatplus(swat_exe = exe, working_dir = tmp, verbose = FALSE, echo = FALSE)
  )

  expect_type(result,       "list")
  expect_named(result,      c("status", "stdout", "stderr", "elapsed", "success"),
               ignore.order = TRUE)
  expect_type(result$status,  "integer")
  expect_type(result$stdout,  "character")
  expect_type(result$stderr,  "character")
  expect_type(result$elapsed, "double")
  expect_type(result$success, "logical")
  # With no file.cio, SWAT+ should exit non-zero
  expect_false(result$success)
})

# ===========================================================================
# 2. Minimal reprex — build + run a SWAT+ simulation
# ===========================================================================
# This test shows the complete end-to-end workflow in a compact form:
#   create_project_db()  → write_swatplus_files()  → run_swatplus()
#
# It is skipped automatically unless:
#   (a) we are on Windows,
#   (b) the bundled SWAT+ exe is present, AND
#   (c) processx is installed.
#
# Running this locally on Windows provides confidence that the whole pipeline
# works without QSWAT+ or any other external tool.

test_that("minimal reprex: write input files and run SWAT+", {
  skip_if_not_installed("processx")
  skip_if(.Platform$OS.type != "windows",
          "bundled SWAT+ exe is Windows-only")

  exe <- swatplus_exe()
  skip_if(is.null(exe) || !file.exists(exe),
          "bundled SWAT+ exe not found")

  # ── Step 1: create a temporary project ───────────────────────────────────
  tmp    <- tempfile("swat_reprex_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  db_path  <- file.path(tmp, "project.sqlite")
  txt_path <- file.path(tmp, "txtinout")

  create_project_db(db_path)
  con <- swat_open_db(db_path)
  create_project_tables(con)

  # ── Step 2: minimal project_config ────────────────────────────────────────
  DBI::dbExecute(con,
    "UPDATE project_config
     SET project_name='reprex', editor_version='2.3.0',
         input_files_dir=?",
    params = list(txt_path))
  DBI::dbExecute(con,
    "INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
     VALUES (0, 2000, 0, 2000, 0)")
  swat_close_db(con)

  # ── Step 3: write SWAT+ input files ───────────────────────────────────────
  write_swatplus_files(db_path, txt_path, verbose = FALSE)

  # Confirm the primary SWAT+ control file was created
  expect_true(file.exists(file.path(txt_path, "file.cio")))

  # ── Step 4: run SWAT+ ─────────────────────────────────────────────────────
  result <- run_swatplus(
    swat_exe    = exe,
    working_dir = txt_path,
    verbose     = FALSE,
    echo        = FALSE
  )

  # Shape checks — the model may fail if the project is too minimal, but
  # the wrapper must return the correct structure regardless.
  expect_type(result, "list")
  expect_named(result,
    c("status", "stdout", "stderr", "elapsed", "success"),
    ignore.order = TRUE)
  expect_type(result$elapsed, "double")
  expect_gte(result$elapsed, 0)

  # Print first few stdout lines so failures are easier to diagnose in CI
  if (length(result$stdout) > 0L)
    cat("SWAT+ output (first 5 lines):\n",
        paste(head(result$stdout, 5L), collapse = "\n"), "\n", sep = "")
})
