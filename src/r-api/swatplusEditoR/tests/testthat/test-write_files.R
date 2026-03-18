library(swatplusEditoR)
library(testthat)

# ---------------------------------------------------------------------------
# write_swatplus_files: basic round-trip
# ---------------------------------------------------------------------------

minimal_project_db <- function(path) {
  create_project_db(path)
  con <- swat_open_db(path)

  DBI::dbExecute(con, "
    INSERT INTO project_config (project_name, input_files_dir, is_lte)
    VALUES ('test_proj', 'input', 0)")
  DBI::dbExecute(con, "
    INSERT INTO time_sim (day_start, yrc_start, day_end, yrc_end, step)
    VALUES (0, 2010, 0, 2015, 0)")
  DBI::dbExecute(con, "
    INSERT INTO codes_bsn (pet,event,crack,rtu_evap,gwflow,swift,carbon,
      lat_sed,nutrient,ch_sed,ch_nu,soil_p,atmo_dep,stor_ws,pesticide,
      pathogens,hmet,salt,co2)
    VALUES (1,0,0,0,0,0,2,0,1,1,0,0,'no',0,0,0,0,0,330)")
  DBI::dbExecute(con, "
    INSERT INTO parameters_bsn (pet_co,esco,epco,evap_res_co,evap_sub_co,sub_lat_perc)
    VALUES (1.0,0.95,1.0,1.0,1.0,0.0)")
  DBI::dbExecute(con, "
    INSERT INTO print_prt (nyskip,day_start,yrc_start,day_end,yrc_end,interval,
      csvout,dbout,cdfout,crop_yld,mgtout,hydcon,fdcout)
    VALUES (0,0,0,0,0,0,1,0,0,'b',0,0,0)")
  DBI::dbExecute(con, "
    INSERT INTO object_cnt (name) VALUES ('test_proj')")

  swat_close_db(con)
}

test_that("write_swatplus_files creates expected output files", {
  prj_file <- tempfile(fileext = ".sqlite")
  out_dir  <- tempfile()
  on.exit({ unlink(prj_file); unlink(out_dir, recursive = TRUE) }, add = TRUE)

  minimal_project_db(prj_file)
  n <- write_swatplus_files(prj_file, out_dir, verbose = FALSE)

  expect_true(file.exists(file.path(out_dir, "time.sim")))
  expect_true(file.exists(file.path(out_dir, "print.prt")))
  expect_true(file.exists(file.path(out_dir, "codes.bsn")))
  expect_true(file.exists(file.path(out_dir, "parameters.bsn")))
  expect_true(file.exists(file.path(out_dir, "object.cnt")))
  expect_gt(n, 0L)
})

test_that("time.sim file contains correct year", {
  prj_file <- tempfile(fileext = ".sqlite")
  out_dir  <- tempfile()
  on.exit({ unlink(prj_file); unlink(out_dir, recursive = TRUE) }, add = TRUE)

  minimal_project_db(prj_file)
  write_swatplus_files(prj_file, out_dir, verbose = FALSE)

  lines <- readLines(file.path(out_dir, "time.sim"))
  expect_true(any(grepl("2010", lines)))
  expect_true(any(grepl("2015", lines)))
})

test_that("write_swatplus_files updates input_files_last_written", {
  prj_file <- tempfile(fileext = ".sqlite")
  out_dir  <- tempfile()
  on.exit({ unlink(prj_file); unlink(out_dir, recursive = TRUE) }, add = TRUE)

  minimal_project_db(prj_file)
  write_swatplus_files(prj_file, out_dir, verbose = FALSE)

  con <- swat_open_db(prj_file)
  on.exit(swat_close_db(con), add = TRUE)
  ts <- DBI::dbGetQuery(con,
    "SELECT input_files_last_written FROM project_config")$input_files_last_written
  expect_false(is.na(ts))
})
