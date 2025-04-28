

#' Create Data Analysis Document Content
#'
#' Creates content for a data analysis template with sections for setup,
#' data preparation, visualization, and statistical analysis.
#'
#' @param title Character string. The document title.
#' @param author Character string or NULL. The document author.
#' @param type Character string. The document type ("qmd" or "rmd").
#'   Note: "r" type is not currently supported for this template.
#'
#' @return Character vector containing the document content.
#'
#' @importFrom stringr str_glue
#' @keywords internal
create_data_analysis_content <- function(title, author, type) {
  # Check if type is "r" and stop with informative message
  if (tolower(type) == "r") {
    stop("R script templates are not currently supported for the data_analysis template. This feature is reserved for future implementation.")
  }

  # Start with YAML header
  yaml_header <- c("---", stringr::str_glue('title: "{title}"'))

  # Add author if provided
  if (!is.null(author)) {
    yaml_header <- c(yaml_header, stringr::str_glue('author: "{author}"'))
  }

  # Complete the YAML header with format settings
  yaml_header <- c(
    yaml_header,
    "format: html",
    "editor: source",
    "editor_options: ",
    "  chunk_output_type: console",
    "---",
    ""
  )

  # Document body with predefined content
  document_body <- c(
    "# 1 Initial setting",
    "",
    "## 1.1 clear workspace and set default color",
    "",
    "```{r reset, include=FALSE}",
    "graphics.off()",
    "rm(list=ls(all.names=TRUE)) # Remove ",
    "options(digits = 3)",
    "options(ggplot2.discrete.colour= c(\"#615F63\",\"#FF7F6F\",\"#2F7FC1\",\"#FFBE7A\",\"#8FC0A9\",\"#8A1C56\"))",
    "options(ggplot2.discrete.fill= c(\"#615F63\",\"#FF7F6F\",\"#2F7FC1\",\"#FFBE7A\",\"#8FC0A9\",\"#8A1C56\"))",
    "```",
    "",
    "## 1.2 Import functions and packages",
    "",
    "```{r}",
    "",
    "# Source all local R functions silently",
    "invisible(",
    "  sapply(",
    "    list.files(\"./functions\", pattern = \"\\\\.R$\", full.names = TRUE),",
    "    function(file) suppressMessages(suppressWarnings(source(file, echo = FALSE)))",
    "  )",
    ")",
    "",
    "# Install and load packages",
    "packages = c(",
    "  # CRAN packages",
    "  \"tidyverse\", \"brms\", \"tidybayes\", \"emmeans\", \"bayestestR\", \"ggpubr\",",
    "  # Github packages",
    "  \"chenyu-psy/smartr@v0.2.6\", ",
    "  \"venpopov/bmm\"",
    "  )",
    "process_packages(packages, quietly = TRUE)",
    "",
    "",
    "# theme",
    "theme_set(theme_bw()) # using `theme_bw` as the default",
    "dodge2 = position_dodge(.2)",
    "",
    "",
    "# task name and path",
    "task <- \"task_name\"",
    "model_path <- stringr::str_glue(\"./models/models_{task}/\")",
    "sample_path <- stringr::str_glue(\"./samples/sample_{task}/\")",
    "figure_path <- stringr::str_glue(\"./figures/figures_{task}/\")",
    "bf_path <- stringr::str_glue(\"./models/BayesFactor_{task}/\")",
    "# check whether the folders are existent or not. If not, create a new one",
    "dir.create(file.path(model_path), showWarnings = FALSE)",
    "dir.create(file.path(sample_path), showWarnings = FALSE)",
    "dir.create(file.path(figure_path), showWarnings = FALSE)",
    "dir.create(file.path(bf_path), showWarnings = FALSE)",
    "",
    "```",
    "",
    "# 2 Data Preparation",
    "",
    "## 2.1 Import data",
    "",
    "``` {r}",
    "",
    "```",
    "",
    "# 3 Data Visualization",
    "",
    "",
    "",
    "# 4 Statistical Analysis",
    "",
    "",
    "",
    ""
  )

  # Combine header and body
  c(yaml_header, document_body)
}

#' Create Simple Report Document Content
#'
#' Creates content for a simple report template with basic structure.
#' This is an example of an additional template type.
#'
#' @param title Character string. The document title.
#' @param author Character string or NULL. The document author.
#' @param type Character string. The document type ("qmd" or "rmd").
#'   Note: "r" type is not currently supported for this template.
#'
#' @return Character vector containing the document content.
#'
#' @importFrom stringr str_glue
#' @keywords internal
create_simple_report_content <- function(title, author, type) {
  # Check if type is "r" and stop with informative message
  if (tolower(type) == "r") {
    stop("R script templates are not currently supported for the simple_report template. This feature is reserved for future implementation.")
  }

  # Start with YAML header
  yaml_header <- c("---", stringr::str_glue('title: "{title}"'))

  # Add author if provided
  if (!is.null(author)) {
    yaml_header <- c(yaml_header, stringr::str_glue('author: "{author}"'))
  }

  # Complete the YAML header with format settings
  yaml_header <- c(
    yaml_header,
    "format: html",
    "---",
    ""
  )

  # Document body with simple structure
  document_body <- c(
    "# Introduction",
    "",
    "This is a simple report template.",
    "",
    "# Methods",
    "",
    "```{r setup}",
    "library(tidyverse)",
    "```",
    "",
    "# Results",
    "",
    "# Discussion",
    "",
    "# References",
    ""
  )

  # Combine header and body
  c(yaml_header, document_body)
}
