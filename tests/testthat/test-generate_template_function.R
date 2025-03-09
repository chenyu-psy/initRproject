test_that("generate_template_function creates valid template code", {
  # Create temporary directory for test files
  temp_dir <- tempdir()

  # Create test files

  # 1. Quarto file with minimal content
  qmd_file <- file.path(temp_dir, "test_template.qmd")
  qmd_content <- c(
    "---",
    "title: Test Quarto Document",
    "format: html",
    "---",
    "",
    "# Introduction",
    "",
    "This is a test Quarto document."
  )
  writeLines(qmd_content, qmd_file)

  # 2. R script file with minimal content
  r_file <- file.path(temp_dir, "test_template.R")
  r_content <- c(
    "# Test R Script",
    "",
    "# Load packages",
    "library(dplyr)",
    "library(ggplot2)"
  )
  writeLines(r_content, r_file)

  # Test generating template functions

  # 1. Test with Quarto file
  qmd_template_code <- generate_template_function(
    qmd_file,
    "test_qmd",
    "Test Quarto template"
  )

  # 2. Test with R script file
  r_template_code <- generate_template_function(
    r_file,
    "test_r",
    "Test R script template"
  )

  # Test the structure of the generated code using fixed strings instead of regex

  # 1. Check Quarto template code
  expect_true(grepl("create_test_qmd_content <- function", qmd_template_code, fixed = TRUE))
  expect_true(grepl("Test Quarto template", qmd_template_code, fixed = TRUE))
  expect_true(grepl("if (!type %in% c(\"qmd\", \"rmd\"))", qmd_template_code, fixed = TRUE))
  expect_true(grepl("yaml_header <- c(\"---\", stringr::str_glue('title: \"{title}\"'))", qmd_template_code, fixed = TRUE))
  expect_true(grepl("if (!is.null(author))", qmd_template_code, fixed = TRUE))

  # 2. Check R template code
  expect_true(grepl("create_test_r_content <- function", r_template_code, fixed = TRUE))
  expect_true(grepl("Test R script template", r_template_code, fixed = TRUE))
  expect_true(grepl("if (type != \"r\")", r_template_code, fixed = TRUE))
  expect_true(grepl("header <- c(", r_template_code, fixed = TRUE))
  expect_true(grepl("stringr::str_glue(\"# {title}\")", r_template_code, fixed = TRUE))

  # Test error conditions

  # 1. Non-existent file
  expect_error(
    generate_template_function("non_existent_file.qmd", "test"),
    "File does not exist"
  )

  # 2. Invalid template name
  expect_error(
    generate_template_function(qmd_file, "Invalid Name"),
    "template_name must start with a lowercase letter"
  )

  # 3. Unsupported file type
  txt_file <- file.path(temp_dir, "test.txt")
  writeLines("This is a text file", txt_file)
  expect_error(
    generate_template_function(txt_file, "test_txt"),
    "File must be a Quarto \\(.qmd\\), R Markdown \\(.rmd\\), or R script \\(.R\\) file"
  )
})
