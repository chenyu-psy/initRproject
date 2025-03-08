#' @importFrom testthat test_that expect_equal expect_error expect_warning
NULL

test_that("process_packages validates inputs correctly", {
  expect_error(process_packages(NULL), "must be a non-empty character vector")
  expect_error(process_packages(character(0)), "must be a non-empty character vector")
  expect_error(process_packages(123), "must be a non-empty character vector")

  expect_error(process_packages("dplyr", quietly = "yes"), "must be a single logical value")
  expect_error(process_packages("dplyr", auto_install = "yes"), "must be a single logical value")
})

# Tests for internal functions would be in a separate test file
# and would use the ::: operator to access them
test_that("extract_package_name works correctly", {
  # Using ::: to access internal functions
  expect_equal(initRproject:::extract_package_name("dplyr"), "dplyr")
  expect_equal(initRproject:::extract_package_name("tidyverse/ggplot2"), "ggplot2")
  expect_equal(initRproject:::extract_package_name("username/package@v1.0.0"), "package")

  expect_error(initRproject:::extract_package_name(""), "must be a single non-empty character string")
})
