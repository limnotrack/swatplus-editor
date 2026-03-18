library(SWATplusR)
library(testthat)

# ---------------------------------------------------------------------------
# weather_sta_name
# ---------------------------------------------------------------------------

test_that("weather_sta_name generates correct name for northern hemisphere", {
  name <- weather_sta_name(45.123, -93.456)
  # lat: round(45.123 * 1000) = 45123, northern -> n
  # lon: round(93.456 * 1000) = 93456, western  -> w
  expect_equal(name, "s45123n93456w")
})

test_that("weather_sta_name handles southern and eastern hemisphere", {
  name <- weather_sta_name(-20.0, 30.0)
  expect_equal(name, "s20000s30000e")
})

# ---------------------------------------------------------------------------
# import_wgn (file-based)
# ---------------------------------------------------------------------------

test_that("import_wgn imports WGN stations and monthly values", {
  # Create a minimal WGN database
  wgn_file <- tempfile(fileext = ".sqlite")
  prj_file <- tempfile(fileext = ".sqlite")
  on.exit({ unlink(wgn_file); unlink(prj_file) }, add = TRUE)

  # Build WGN source
  wgn_con <- DBI::dbConnect(RSQLite::SQLite(), wgn_file)
  DBI::dbExecute(wgn_con, "
    CREATE TABLE weather_wgn_cli (
      id INTEGER PRIMARY KEY, name TEXT, lat REAL, lon REAL,
      elev REAL, rain_yrs INTEGER,
      tmp_max_ave_1 REAL, tmp_min_ave_1 REAL, tmp_max_sd_1 REAL,
      tmp_min_sd_1 REAL, pcp_ave_1 REAL, pcp_sd_1 REAL, pcp_skew_1 REAL,
      wet_dry_1 REAL, wet_wet_1 REAL, pcp_days_1 REAL, pcp_hhr_1 REAL,
      slr_ave_1 REAL, dew_ave_1 REAL, wnd_ave_1 REAL,
      tmp_max_ave_2 REAL, tmp_min_ave_2 REAL, tmp_max_sd_2 REAL,
      tmp_min_sd_2 REAL, pcp_ave_2 REAL, pcp_sd_2 REAL, pcp_skew_2 REAL,
      wet_dry_2 REAL, wet_wet_2 REAL, pcp_days_2 REAL, pcp_hhr_2 REAL,
      slr_ave_2 REAL, dew_ave_2 REAL, wnd_ave_2 REAL,
      tmp_max_ave_3 REAL, tmp_min_ave_3 REAL, tmp_max_sd_3 REAL,
      tmp_min_sd_3 REAL, pcp_ave_3 REAL, pcp_sd_3 REAL, pcp_skew_3 REAL,
      wet_dry_3 REAL, wet_wet_3 REAL, pcp_days_3 REAL, pcp_hhr_3 REAL,
      slr_ave_3 REAL, dew_ave_3 REAL, wnd_ave_3 REAL,
      tmp_max_ave_4 REAL, tmp_min_ave_4 REAL, tmp_max_sd_4 REAL,
      tmp_min_sd_4 REAL, pcp_ave_4 REAL, pcp_sd_4 REAL, pcp_skew_4 REAL,
      wet_dry_4 REAL, wet_wet_4 REAL, pcp_days_4 REAL, pcp_hhr_4 REAL,
      slr_ave_4 REAL, dew_ave_4 REAL, wnd_ave_4 REAL,
      tmp_max_ave_5 REAL, tmp_min_ave_5 REAL, tmp_max_sd_5 REAL,
      tmp_min_sd_5 REAL, pcp_ave_5 REAL, pcp_sd_5 REAL, pcp_skew_5 REAL,
      wet_dry_5 REAL, wet_wet_5 REAL, pcp_days_5 REAL, pcp_hhr_5 REAL,
      slr_ave_5 REAL, dew_ave_5 REAL, wnd_ave_5 REAL,
      tmp_max_ave_6 REAL, tmp_min_ave_6 REAL, tmp_max_sd_6 REAL,
      tmp_min_sd_6 REAL, pcp_ave_6 REAL, pcp_sd_6 REAL, pcp_skew_6 REAL,
      wet_dry_6 REAL, wet_wet_6 REAL, pcp_days_6 REAL, pcp_hhr_6 REAL,
      slr_ave_6 REAL, dew_ave_6 REAL, wnd_ave_6 REAL,
      tmp_max_ave_7 REAL, tmp_min_ave_7 REAL, tmp_max_sd_7 REAL,
      tmp_min_sd_7 REAL, pcp_ave_7 REAL, pcp_sd_7 REAL, pcp_skew_7 REAL,
      wet_dry_7 REAL, wet_wet_7 REAL, pcp_days_7 REAL, pcp_hhr_7 REAL,
      slr_ave_7 REAL, dew_ave_7 REAL, wnd_ave_7 REAL,
      tmp_max_ave_8 REAL, tmp_min_ave_8 REAL, tmp_max_sd_8 REAL,
      tmp_min_sd_8 REAL, pcp_ave_8 REAL, pcp_sd_8 REAL, pcp_skew_8 REAL,
      wet_dry_8 REAL, wet_wet_8 REAL, pcp_days_8 REAL, pcp_hhr_8 REAL,
      slr_ave_8 REAL, dew_ave_8 REAL, wnd_ave_8 REAL,
      tmp_max_ave_9 REAL, tmp_min_ave_9 REAL, tmp_max_sd_9 REAL,
      tmp_min_sd_9 REAL, pcp_ave_9 REAL, pcp_sd_9 REAL, pcp_skew_9 REAL,
      wet_dry_9 REAL, wet_wet_9 REAL, pcp_days_9 REAL, pcp_hhr_9 REAL,
      slr_ave_9 REAL, dew_ave_9 REAL, wnd_ave_9 REAL,
      tmp_max_ave_10 REAL, tmp_min_ave_10 REAL, tmp_max_sd_10 REAL,
      tmp_min_sd_10 REAL, pcp_ave_10 REAL, pcp_sd_10 REAL, pcp_skew_10 REAL,
      wet_dry_10 REAL, wet_wet_10 REAL, pcp_days_10 REAL, pcp_hhr_10 REAL,
      slr_ave_10 REAL, dew_ave_10 REAL, wnd_ave_10 REAL,
      tmp_max_ave_11 REAL, tmp_min_ave_11 REAL, tmp_max_sd_11 REAL,
      tmp_min_sd_11 REAL, pcp_ave_11 REAL, pcp_sd_11 REAL, pcp_skew_11 REAL,
      wet_dry_11 REAL, wet_wet_11 REAL, pcp_days_11 REAL, pcp_hhr_11 REAL,
      slr_ave_11 REAL, dew_ave_11 REAL, wnd_ave_11 REAL,
      tmp_max_ave_12 REAL, tmp_min_ave_12 REAL, tmp_max_sd_12 REAL,
      tmp_min_sd_12 REAL, pcp_ave_12 REAL, pcp_sd_12 REAL, pcp_skew_12 REAL,
      wet_dry_12 REAL, wet_wet_12 REAL, pcp_days_12 REAL, pcp_hhr_12 REAL,
      slr_ave_12 REAL, dew_ave_12 REAL, wnd_ave_12 REAL
    )")

  # Insert one station with all monthly columns as 0
  col_names <- DBI::dbListFields(wgn_con, "weather_wgn_cli")
  vals <- c(1, "'sta1'", 45.0, -90.0, 250.0, 30,
            rep(0.0, length(col_names) - 6))
  DBI::dbExecute(wgn_con,
    paste0("INSERT INTO weather_wgn_cli VALUES (",
           paste(vals, collapse = ","), ")"))
  DBI::dbDisconnect(wgn_con)

  # Build project DB
  create_project_db(prj_file)

  n <- import_wgn(prj_file, wgn_file, verbose = FALSE)
  expect_equal(n, 1L)

  prj_con <- swat_open_db(prj_file)
  on.exit(swat_close_db(prj_con), add = TRUE)
  expect_equal(swat_count(prj_con, "weather_wgn_cli"), 1L)
  expect_equal(swat_count(prj_con, "weather_wgn_cli_mon"), 12L)
})

# ---------------------------------------------------------------------------
# import_atmo_dep
# ---------------------------------------------------------------------------

test_that("import_atmo_dep imports atmospheric deposition from CSV", {
  prj_file <- tempfile(fileext = ".sqlite")
  csv_file <- tempfile(fileext = ".csv")
  on.exit({ unlink(prj_file); unlink(csv_file) }, add = TRUE)

  write.csv(
    data.frame(year = 1990:1992, nh4_wet = 0.1, no3_wet = 0.2,
               nh4_dry = 0.05, no3_dry = 0.1),
    csv_file, row.names = FALSE
  )
  create_project_db(prj_file)
  n <- import_atmo_dep(prj_file, csv_file, verbose = FALSE)
  expect_equal(n, 3L)

  con <- swat_open_db(prj_file)
  on.exit(swat_close_db(con), add = TRUE)
  expect_equal(swat_count(con, "atmo_cli_sta_value"), 3L)
})
