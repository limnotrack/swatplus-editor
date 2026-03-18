library(swatplusEditoR)
library(testthat)

# ---------------------------------------------------------------------------
# is_number
# ---------------------------------------------------------------------------

test_that("is_number identifies numeric strings", {
  expect_true(is_number("3.14"))
  expect_true(is_number(42))
  expect_true(is_number(-1.5))
  expect_false(is_number("abc"))
  expect_false(is_number(NULL))
  expect_false(is_number(NA))
})

# ---------------------------------------------------------------------------
# get_swat_name
# ---------------------------------------------------------------------------

test_that("get_swat_name produces zero-padded names", {
  expect_equal(get_swat_name("hru",  1L,  9L), "hru1")
  expect_equal(get_swat_name("hru",  1L, 10L), "hru01")
  expect_equal(get_swat_name("hru", 12L, 100L), "hru012")
  expect_equal(get_swat_name("cha",  3L, 999L), "cha003")
})

# ---------------------------------------------------------------------------
# string_pad / num_pad / int_pad
# ---------------------------------------------------------------------------

test_that("string_pad right-justifies to the specified width", {
  result <- string_pad("abc", default_pad = 8, spaces_after = 0)
  expect_equal(nchar(result), 8L)
  expect_match(result, "     abc$")
})

test_that("string_pad uses NULL_STR for empty values", {
  result <- string_pad(NULL, default_pad = 6, spaces_after = 0)
  expect_equal(trimws(result), NULL_STR)
})

test_that("num_pad formats to the correct number of decimals", {
  result <- trimws(num_pad(3.14159, decimals = 2,
                            default_pad = 8))
  expect_equal(result, "3.14")
})

test_that("int_pad produces zero decimal places", {
  result <- trimws(int_pad(7L, default_pad = 4))
  expect_equal(result, "7")
})

# ---------------------------------------------------------------------------
# remove_space
# ---------------------------------------------------------------------------

test_that("remove_space replaces spaces with underscores by default", {
  expect_equal(remove_space("hello world"), "hello_world")
  expect_equal(remove_space("  trim "),     "trim")
})

test_that("remove_space handles NULL and empty gracefully", {
  expect_null(remove_space(NULL))
  expect_equal(remove_space(""), "")
})

# ---------------------------------------------------------------------------
# val_if_null
# ---------------------------------------------------------------------------

test_that("val_if_null returns fallback for NULL", {
  expect_equal(val_if_null(NULL, 99), 99)
})

test_that("val_if_null returns fallback for literal 'null' string", {
  expect_equal(val_if_null("null", "default"), "default")
})

test_that("val_if_null returns the value when not null", {
  expect_equal(val_if_null(42, 0), 42)
  expect_equal(val_if_null("real", "default"), "real")
})

# ---------------------------------------------------------------------------
# split_multiple_delimiters
# ---------------------------------------------------------------------------

test_that("split_multiple_delimiters splits on comma and space", {
  result <- split_multiple_delimiters("a,b c,d")
  expect_equal(result, c("a", "b", "c", "d"))
})

test_that("split_multiple_delimiters removes empty tokens", {
  result <- split_multiple_delimiters("a,,b")
  expect_equal(result, c("a", "b"))
})

# ---------------------------------------------------------------------------
# are_paths_equal
# ---------------------------------------------------------------------------

test_that("are_paths_equal handles equivalent paths", {
  tmp <- tempdir()
  p1  <- file.path(tmp, "foo")
  p2  <- file.path(tmp, "bar", "..", "foo")
  expect_true(are_paths_equal(p1, p2))
})

test_that("are_paths_equal detects different paths", {
  tmp <- tempdir()
  p1  <- file.path(tmp, "foo")
  p2  <- file.path(tmp, "bar")
  expect_false(are_paths_equal(p1, p2))
})

# ---------------------------------------------------------------------------
# add_months
# ---------------------------------------------------------------------------

test_that("add_months correctly advances by one month", {
  d <- as.Date("2020-01-31")
  expect_equal(add_months(d, 1L), as.Date("2020-02-29"))  # leap year
})

test_that("add_months handles year rollover", {
  d <- as.Date("2020-11-15")
  expect_equal(add_months(d, 2L), as.Date("2021-01-15"))
})

# ---------------------------------------------------------------------------
# get_valid_filename
# ---------------------------------------------------------------------------

test_that("get_valid_filename removes special characters", {
  expect_equal(get_valid_filename("my file!.txt"), "my_file.txt")
  expect_equal(get_valid_filename(" spaces "),     "spaces")
})
