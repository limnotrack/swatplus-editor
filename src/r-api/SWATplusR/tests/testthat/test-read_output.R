library(SWATplusR)
library(testthat)

# ---------------------------------------------------------------------------
# read_output
# ---------------------------------------------------------------------------

test_that("read_output imports a simple CSV output file", {
  prj_file  <- tempfile(fileext = ".sqlite")
  out_db    <- tempfile(fileext = ".sqlite")
  txt_dir   <- tempdir()
  on.exit({
    unlink(prj_file)
    unlink(out_db)
    # clean up created test files
    unlink(file.path(txt_dir, "files_out.out"))
    unlink(file.path(txt_dir, "hru_wb_aa.csv"))
  }, add = TRUE)

  # Create minimal project DB
  create_project_db(prj_file)
  con <- swat_open_db(prj_file)
  DBI::dbExecute(con, "
    INSERT INTO project_config (project_name, input_files_dir)
    VALUES ('test', '.')")
  swat_close_db(con)

  # Write a dummy output CSV
  csv_path <- file.path(txt_dir, "hru_wb_aa.csv")
  write.csv(
    data.frame(name = c("hru001","hru002"),
               gis_id = c(1, 2),
               precip = c(500.1, 600.2),
               stringsAsFactors = FALSE),
    csv_path, row.names = FALSE
  )

  # Write files_out.out
  writeLines("hru_wb_aa.csv", file.path(txt_dir, "files_out.out"))

  counts <- read_output(prj_file, output_db = out_db,
                        txt_dir = txt_dir, verbose = FALSE)

  expect_true("hru_wb_aa" %in% names(counts))
  expect_equal(counts$hru_wb_aa, 2L)

  out_con <- swat_open_db(out_db)
  on.exit(swat_close_db(out_con), add = TRUE)
  expect_true(swat_exists_table(out_con, "hru_wb_aa"))
  expect_equal(swat_count(out_con, "hru_wb_aa"), 2L)
})

# ---------------------------------------------------------------------------
# clean_column_name
# ---------------------------------------------------------------------------

test_that("clean_column_name removes special characters", {
  expect_equal(clean_column_name("my col!"), "my_col_")
  expect_equal(clean_column_name("123abc"),  "x123abc")
  expect_equal(clean_column_name("valid_name"), "valid_name")
})

# ---------------------------------------------------------------------------
# reset_output_db
# ---------------------------------------------------------------------------

test_that("reset_output_db drops non-metadata tables", {
  out_db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(out_db), add = TRUE)

  create_output_db(out_db)
  con <- swat_open_db(out_db)
  DBI::dbExecute(con, "CREATE TABLE tmp_output (x INTEGER)")
  DBI::dbDisconnect(con)

  n <- reset_output_db(out_db)
  expect_true(n >= 1L)

  con2 <- swat_open_db(out_db)
  on.exit(swat_close_db(con2), add = TRUE)
  expect_false(swat_exists_table(con2, "tmp_output"))
  expect_true(swat_exists_table(con2, "project_config"))
})
