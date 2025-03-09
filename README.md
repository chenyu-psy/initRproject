
# initRproject

<!-- badges: start -->

[![R-CMD-check](https://github.com/chenyu-psy/initRproject/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chenyu-psy/initRproject/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->


## Overview

`initRproject` is an R package designed to streamline the initialization of R projects by providing three key functions:

1.  `process_packages`: Efficiently installs and loads required packages
2.  `create_template`: Generates structured Quarto (.qmd), R Markdown (.rmd), or R script (.R) templates
3.  `generate_template_function`: Creates custom template functions from your existing files

This package is particularly useful for ensuring consistent project setup and documentation across your R workflows.

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("chenyu-psy/initRproject")
```

## Key Features

### Process Packages

The `process_packages()` function handles both installation and loading of packages in a single call:

``` r
library(initRproject)

# Install and load multiple packages
process_packages(c(
  # CRAN packages
  "dplyr", "ggplot2", "dplyr",
  # GitHub packages
  "chenyu-psy/smartr@v0.2.2"
))
```

Features: 
- Automatically detects and installs missing packages
- Supports both CRAN and GitHub packages
- Provides options for quiet installation

### Create Templates

The `create_template()` function generates structured document templates for data analysis:

``` r
# Create a data analysis template
create_template(
  file_path = "script/analysis.qmd",
  title = "My Data Analysis",
  author = "Your Name",
  template = "data_analysis"
)
```

Available templates: 
- `data_analysis`: Comprehensive template with sections for setup, data preparation, visualization, and statistical analysis 
- `simple_report`: Basic report structure for simpler documentation needs
- You can create your own custom templates (see below)

### Generate Custom Template Functions

The `generate_template_function()` allows you to create your own template functions from existing files:

``` r
# Generate a template function from an existing file
template_code <- generate_template_function(
  file_path = "my_custom_analysis.qmd",
  template_name = "custom_analysis",
  description = "My custom analysis template with specialized sections"
)

# Save the generated code to a file in the R directory
writeLines(template_code, "R/available_templates.R")

# After reloading the package, you can use your custom template
create_template(template = "custom_analysis")
```

Features:
- Supports Quarto (.qmd), R Markdown (.rmd), and R script (.R) files
- Automatically extracts structure and content from your existing files
- Generated template functions maintain the same format type as the source file
- Includes proper documentation and validation

## Auto-loading on Project Startup

To automatically load `initRproject` when your R project is launched, add the following to your project's `.Rprofile` file:

``` r
if (interactive()) {
  message("Loading initRproject...")
  if (!require("initRproject", quietly = TRUE)) {
    message("Installing initRproject from GitHub...")
    if (!require("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    remotes::install_github("chenyu-psy/initRproject")
    library(initRproject)
  }
}
```

To create or edit your `.Rprofile` file:

``` r
# Open .Rprofile for editing
file.edit(".Rprofile")
```

## Creating Custom Templates

There are two ways to create custom templates:

### 1. Using the generate_template_function

The easiest way to create a custom template is to:

1. Create a file (Quarto, R Markdown, or R script) with your desired structure
2. Use `generate_template_function()` to create a template function from this file
3. Save the generated code to the `R/available_templates.R` file in your fork of the package
4. Reload the package to make your new template available

``` r
# Generate template function from an existing file
code <- generate_template_function(
  "my_perfect_template.qmd", 
  "perfect_analysis",
  "A comprehensive template for perfect data analysis"
)

# Save to the available_templates file
dir.create("R/available_templates", recursive = TRUE, showWarnings = FALSE)
writeLines(code, "R/available_templates.R")
```

### 2. Writing a Template Function Manually

You can also create template functions manually:

1. Fork the repository on GitHub
2. Clone your fork locally
3. Create a new template function in the `R/available_templates.R` file following this pattern:

``` r
#' Create Your Template Content
#'
#' @description
#' Your template description here.
#'
#' @param title Character string. The document title.
#' @param author Character string or NULL. The document author.
#' @param type Character string. The document type.
#'
#' @return Character vector containing the document content.
#'
#' @importFrom stringr str_glue
#' @keywords internal
create_your_template_content <- function(title, author, type) {
  # Validate file type
  type <- tolower(type)
  if (!type %in% c("qmd", "rmd")) {
    stop("This template only supports Quarto and R Markdown files.")
  }
  
  # Create your template content here
  # Return a character vector with the content
}
```

4. Install your customized version using `remotes::install_github("yourusername/initRproject")`

## Usage Examples

### Basic Workflow

``` r
library(initRproject)

# Process required packages
process_packages(c("tidyverse", "brms", "ggplot2"))

# Create a new analysis document
create_template(
  file_path = "analysis/experiment_results.qmd",
  title = "Experiment Results Analysis",
  author = "Research Team"
)
```

### Using Different Template Types

``` r
# Create a data analysis template (default)
create_template("analysis_detailed.qmd")

# Create a simple report
create_template(
  "report.qmd",
  title = "Monthly Report",
  template = "simple_report"
)
```

### Creating and Using Custom Templates

``` r
# Generate a template function from your existing file
my_template_code <- generate_template_function(
  "my_best_analysis.qmd",
  "best_practice",
  "Template following our lab's best practices"
)

# Save it to a file
writeLines(my_template_code, "create_best_practice_content.R")

# Source the file to make it available in your current session
source("create_best_practice_content.R")

# Use your new template
create_template(
  "new_analysis.qmd",
  title = "New Analysis Using Best Practices",
  template = "best_practice"
)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1.  Fork the repository
2.  Create your feature branch (`git checkout -b feature/amazing-feature`)
3.  Commit your changes (`git commit -m 'Add some amazing feature'`)
4.  Push to the branch (`git push origin feature/amazing-feature`)
5.  Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.


