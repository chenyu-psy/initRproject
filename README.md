# initRproject

<!-- badges: start -->

[![R-CMD-check](https://github.com/chenyu-psy/initRproject/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chenyu-psy/initRproject/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

`initRproject` is an R package designed to streamline the initialization of R projects by providing two key functions:

1.  `process_packages`: Efficiently installs and loads required packages
2.  `create_template`: Generates structured Quarto (.qmd) or R Markdown (.rmd) templates

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
- Automatically detects and installs missing packages; 
- Supports both CRAN and GitHub packages; 
- Provides options for quiet installation.

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
- you can fork this repository and create your own template

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

You can create your own templates by forking this repository and adding new template functions:

1.  Fork the repository on GitHub
2.  Clone your fork locally
3.  Create a new template function following this pattern:

``` r
create_your_template_content <- function(title, author, type) {
  # Create your template content here
  # Return a character vector with the content
}
```

4.  Register your template by ensuring its name follows the pattern `create_*_content`
5.  Install your customized version using `remotes::install_github("yourusername/initRproject")`

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

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1.  Fork the repository
2.  Create your feature branch (`git checkout -b feature/amazing-feature`)
3.  Commit your changes (`git commit -m 'Add some amazing feature'`)
4.  Push to the branch (`git push origin feature/amazing-feature`)
5.  Open a Pull Request

## License

This project is licensed under the MIT License - see the LICENSE file for details.
