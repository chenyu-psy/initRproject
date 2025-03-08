# tests/testthat/test-create_template.R

library(testthat)

# Create a temporary directory for testing
test_that("create_template creates files correctly", {
  # Set up temporary directory for tests
  temp_dir <- tempdir()
  old_wd <- getwd()
  setwd(temp_dir)
  on.exit({
    setwd(old_wd)
    unlink(file.path(temp_dir, "test_templates"), recursive = TRUE)
  })

  # Create a test directory
  test_dir <- file.path(temp_dir, "test_templates")
  dir.create(test_dir, showWarnings = FALSE)

  # Test 1: Basic functionality with data_analysis template
  test_file <- file.path(test_dir, "test_analysis.qmd")
  result <- create_template(
    file_path = test_file,
    title = "Test Analysis",
    author = "Test Author",
    type = "qmd",
    template = "data_analysis"
  )

  # Check if file was created
  expect_true(file.exists(test_file))

  # Check if content is correct
  content <- readLines(test_file)
  expect_true(any(grepl('title: "Test Analysis"', content)))
  expect_true(any(grepl('author: "Test Author"', content)))
  expect_true(any(grepl("# 1 Initial setting", content)))

  # Test 2: Simple report template
  test_file2 <- file.path(test_dir, "test_report.qmd")
  result2 <- create_template(
    file_path = test_file2,
    title = "Test Report",
    author = "Test Author",
    type = "qmd",
    template = "simple_report"
  )

  # Check if file was created
  expect_true(file.exists(test_file2))

  # Check if content is correct
  content2 <- readLines(test_file2)
  expect_true(any(grepl('title: "Test Report"', content2)))
  expect_true(any(grepl("# Introduction", content2)))

  # Test 3: RMD file type
  test_file3 <- file.path(test_dir, "test_analysis.rmd")
  result3 <- create_template(
    file_path = test_file3,
    title = "Test RMD",
    author = "Test Author",
    type = "rmd",
    template = "data_analysis"
  )

  # Check if file was created
  expect_true(file.exists(test_file3))

  # Test 4: Default file path generation
  # Instead of mocking, we'll just test that a message is shown and a file is created
  expect_message(
    result4 <- create_template(title = "Default Test"),
    regexp = "No file path provided"
  )

  # The default file name should include today's date
  today_date <- format(Sys.Date(), "%Y%m%d")
  default_file <- file.path(getwd(), paste0("analysis_", today_date, ".qmd"))

  # Check if the default file was created
  expect_true(file.exists(default_file))

  # Clean up the default file
  unlink(default_file)
})

test_that("create_template handles errors correctly", {
  # Set up temporary directory for tests
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_templates_errors")
  dir.create(test_dir, showWarnings = FALSE)

  # Test 1: Invalid template type
  expect_error(
    create_template(
      file_path = file.path(test_dir, "invalid_template.qmd"),
      template = "nonexistent_template"
    ),
    regexp = "Unknown template type"
  )

  # Test 2: Invalid file type
  expect_warning(
    create_template(
      file_path = file.path(test_dir, "wrong_extension.txt"),
      type = "qmd"
    ),
    regexp = "File extension"
  )

  # Test 3: File already exists
  existing_file <- file.path(test_dir, "existing_file.qmd")
  writeLines("test", existing_file)
  expect_error(
    create_template(file_path = existing_file),
    regexp = "File already exists"
  )

  # Test 4: Invalid title parameter
  expect_error(
    create_template(
      file_path = file.path(test_dir, "invalid_title.qmd"),
      title = c("Title1", "Title2")
    ),
    regexp = "title must be a single character string"
  )

  # Test 5: Invalid author parameter
  expect_error(
    create_template(
      file_path = file.path(test_dir, "invalid_author.qmd"),
      author = c("Author1", "Author2")
    ),
    regexp = "author must be a single character string"
  )

  # Test 6: Invalid type parameter
  expect_error(
    create_template(
      file_path = file.path(test_dir, "invalid_type.qmd"),
      type = "invalid"
    ),
    regexp = 'type must be either "qmd" or "rmd"'
  )

  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("create_template creates directories as needed", {
  # Set up temporary directory for tests
  temp_dir <- tempdir()
  nested_dir <- file.path(temp_dir, "nested", "path", "for", "testing")
  test_file <- file.path(nested_dir, "nested_test.qmd")

  # The function should create all necessary directories
  result <- create_template(
    file_path = test_file,
    title = "Nested Test"
  )

  # Check if directories and file were created
  expect_true(dir.exists(nested_dir))
  expect_true(file.exists(test_file))

  # Clean up
  unlink(file.path(temp_dir, "nested"), recursive = TRUE)
})

test_that("create_template handles file extensions correctly", {
  # Set up temporary directory for tests
  temp_dir <- tempdir()

  # Test 1: No extension provided
  no_ext_path <- file.path(temp_dir, "no_extension")
  result1 <- create_template(file_path = no_ext_path, type = "qmd")
  expect_true(file.exists(paste0(no_ext_path, ".qmd")))

  # Test 2: Wrong extension provided
  wrong_ext_path <- file.path(temp_dir, "wrong_extension.md")
  expect_warning(
    result2 <- create_template(file_path = wrong_ext_path, type = "qmd"),
    regexp = "File extension"
  )
  expect_true(file.exists(file.path(temp_dir, "wrong_extension.qmd")))

  # Clean up
  unlink(paste0(no_ext_path, ".qmd"))
  unlink(file.path(temp_dir, "wrong_extension.qmd"))
})

test_that("create_template returns file path invisibly", {
  # Set up temporary directory for tests
  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "return_test.qmd")

  # Capture the return value
  return_value <- create_template(file_path = test_file)

  # Check if the return value is the file path
  expect_equal(return_value, test_file)

  # Clean up
  unlink(test_file)
})
