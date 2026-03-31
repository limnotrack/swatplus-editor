# Test GWFLOW configuration functions

create_gwflow_test_project <- function() {
  db_path <- tempfile(fileext = ".sqlite")
  project_dir <- tempdir()

  project <- list(
    project_dir = project_dir,
    db_file = db_path
  )

  project <- create_project_db(project, db_path, overwrite = TRUE)
  set_simulation_time(project, day_start = 1, yrc_start = 2000,
                      day_end = 365, yrc_end = 2010)
  project
}

test_that("get_gwflow_status returns status", {
  project <- create_gwflow_test_project()
  on.exit(unlink(project$db_file))

  status <- get_gwflow_status(project)
  expect_true(is.list(status))
  expect_false(status$use_gwflow)
  expect_false(status$can_enable)
})

test_that("init_gwflow creates GWFLOW tables and config", {
  project <- create_gwflow_test_project()

  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)

  # Check base config
  base <- get_gwflow_base(project)
  expect_equal(base$cell_size, 200)
  expect_equal(base$row_count, 50)
  expect_equal(base$col_count, 60)
  expect_equal(base$recharge, 1)

  # Check default zone created
  zones <- get_gwflow_zones(project)
  expect_equal(nrow(zones), 1)
  expect_equal(zones$zone_id[1], 1)
  expect_equal(zones$aquifer_k[1], 10.0)

  # Check project config updated
  config <- get_project_config(project)
  expect_equal(config$use_gwflow, 1L)
})

test_that("update_gwflow_base modifies settings", {
  project <- create_gwflow_test_project()
  on.exit(unlink(project$db_file))

  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)
  update_gwflow_base(project, recharge = 2, daily_output = 1)

  base <- get_gwflow_base(project)
  expect_equal(base$recharge, 2)
  expect_equal(base$daily_output, 1)
})

test_that("update_gwflow_zones modifies zone parameters", {
  project <- create_gwflow_test_project()
  on.exit(unlink(project$db_file))

  init_gwflow(project, cell_size = 200, row_count = 50, col_count = 60)
  update_gwflow_zones(project, zone_id = 1, aquifer_k = 15.0,
                      specific_yield = 0.15)

  zones <- get_gwflow_zones(project)
  expect_equal(zones$aquifer_k[1], 15.0)
  expect_equal(zones$specific_yield[1], 0.15)
})
