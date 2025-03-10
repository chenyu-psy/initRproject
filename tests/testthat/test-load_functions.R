# tests/testthat/test-load_functions.R

context("Function loading utilities")

test_that("load_functions loads R files correctly", {
  # Create a temporary directory for testing
  tmp_dir <- file.path(tempdir(), "test_functions")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # Clean up after tests
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create test R files
  writeLines("test_func <- function() { return(42) }", file.path(tmp_dir, "test1.R"))
  writeLines("add_numbers <- function(x, y) { x + y }", file.path(tmp_dir, "test2.R"))

  # Create a subdirectory with more R files
  sub_dir <- file.path(tmp_dir, "subdir")
  dir.create(sub_dir, showWarnings = FALSE)
  writeLines("multiply <- function(x, y) { x * y }", file.path(sub_dir, "test3.R"))

  # Test basic functionality
  result <- load_functions(tmp_dir)
  expect_true(is.character(result))
  expect_equal(length(result), 2)

  # Test that functions are loaded in the global environment
  expect_true(exists("test_func", envir = .GlobalEnv))
  expect_true(exists("add_numbers", envir = .GlobalEnv))
  expect_false(exists("multiply", envir = .GlobalEnv))  # Should not load from subdirectory by default

  # Test function behavior
  expect_equal(test_func(), 42)
  expect_equal(add_numbers(5, 3), 8)

  # Clean up global environment
  rm(list = c("test_func", "add_numbers"), envir = .GlobalEnv)

  # Test recursive loading
  result_recursive <- load_functions(tmp_dir, recursive = TRUE)
  expect_equal(length(result_recursive), 3)

  # Test that all functions are loaded with recursive=TRUE
  expect_true(exists("test_func", envir = .GlobalEnv))
  expect_true(exists("add_numbers", envir = .GlobalEnv))
  expect_true(exists("multiply", envir = .GlobalEnv))

  # Test function from subdirectory
  expect_equal(multiply(6, 7), 42)

  # Clean up global environment
  rm(list = c("test_func", "add_numbers", "multiply"), envir = .GlobalEnv)

  # Test pattern matching
  result_pattern <- load_functions(tmp_dir, pattern = "test1\\.R$")
  expect_equal(length(result_pattern), 1)

  # Test that only matching functions are loaded
  expect_true(exists("test_func", envir = .GlobalEnv))
  expect_false(exists("add_numbers", envir = .GlobalEnv))

  # Clean up global environment
  rm(list = "test_func", envir = .GlobalEnv)

  # Test quiet parameter
  expect_silent(load_functions(tmp_dir, quiet = TRUE))

  # Clean up global environment
  rm(list = c("test_func", "add_numbers"), envir = .GlobalEnv)

  # Create a file with an error
  writeLines("this_will_error <- function() { 1 + }", file.path(tmp_dir, "error.R"))

  # Test error handling
  expect_warning(load_functions(tmp_dir, quiet = FALSE), "Failed to load file")

  # Test non-existent directory
  expect_error(load_functions("non_existent_directory"), "does not exist")

  # Test invalid directory_path parameter
  expect_error(load_functions(c("dir1", "dir2")), "must be a single character string")
  expect_error(load_functions(123), "must be a single character string")

  # Test empty directory
  empty_dir <- file.path(tempdir(), "empty_dir")
  dir.create(empty_dir, showWarnings = FALSE)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_warning(result_empty <- load_functions(empty_dir), "No R files found")
  expect_equal(length(result_empty), 0)
})

test_that("load_functions handles file with multiple functions", {
  # Create a temporary directory for testing
  tmp_dir <- file.path(tempdir(), "multi_func_test")
  dir.create(tmp_dir, showWarnings = FALSE)

  # Clean up after tests
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a file with multiple functions
  writeLines(
    "function1 <- function() { return('one') }
     function2 <- function() { return('two') }
     function3 <- function() { return('three') }",
    file.path(tmp_dir, "multiple_functions.R")
  )

  # Test loading multiple functions from a single file
  load_functions(tmp_dir)

  # Check that all functions are loaded
  expect_true(exists("function1", envir = .GlobalEnv))
  expect_true(exists("function2", envir = .GlobalEnv))
  expect_true(exists("function3", envir = .GlobalEnv))

  # Test function behavior
  expect_equal(function1(), "one")
  expect_equal(function2(), "two")
  expect_equal(function3(), "three")

  # Clean up global environment
  rm(list = c("function1", "function2", "function3"), envir = .GlobalEnv)
})

test_that("load_functions handles files with non-function objects", {
  # Create a temporary directory for testing
  tmp_dir <- file.path(tempdir(), "mixed_objects_test")
  dir.create(tmp_dir, showWarnings = FALSE)

  # Clean up after tests
  on.exit(unlink(tmp_dir, recursive = TRUE))

  # Create a file with mixed object types
  writeLines(
    "test_function <- function(x) { x * 2 }
     test_variable <- 42
     test_list <- list(a = 1, b = 2)
     test_dataframe <- data.frame(x = 1:3, y = c('a', 'b', 'c'))",
    file.path(tmp_dir, "mixed_objects.R")
  )

  # Test loading various object types
  load_functions(tmp_dir)

  # Check that all objects are loaded
  expect_true(exists("test_function", envir = .GlobalEnv))
  expect_true(exists("test_variable", envir = .GlobalEnv))
  expect_true(exists("test_list", envir = .GlobalEnv))
  expect_true(exists("test_dataframe", envir = .GlobalEnv))

  # Test object values
  expect_equal(test_function(5), 10)
  expect_equal(test_variable, 42)
  expect_equal(test_list$a, 1)
  expect_equal(nrow(test_dataframe), 3)

  # Clean up global environment
  rm(list = c("test_function", "test_variable", "test_list", "test_dataframe"), envir = .GlobalEnv)
})
