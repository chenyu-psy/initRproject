#' Create a Quarto, R Markdown, or R Script Template File
#'
#' @description
#' Creates a new Quarto (.qmd), R Markdown (.rmd), or R script (.R) file at the specified path
#' with customizable title and author information. Different template types are
#' available to suit various document purposes.
#'
#' @param file_path Character string. The path where the template file will be created.
#'   If not provided, the file will be created in the current working directory
#'   with a default name based on the current date. If the directory doesn't exist,
#'   it will be created.
#' @param title Character string. The title to include in the YAML header of the document.
#'   Defaults to "Data Analysis".
#' @param author Character string. The author name to include in the YAML header.
#'   If provided, will be added to the YAML header. If NULL (default), no author field is added.
#' @param type Character string. The type of file to create, either "qmd", "rmd", or "r".
#'   Note that "r" type support depends on the specific template being used.
#'   Defaults to "qmd".
#' @param template Character string. The template style to use. Available templates are
#'   automatically detected based on the available create_*_content functions.
#'   Default is "data_analysis".
#'
#' @return Invisibly returns the path to the created file.
#'
#' @examples
#' \dontrun{
#' # Create a default data analysis template
#' create_template()
#'
#' # Create a specific template type
#' create_template(template = "data_analysis")
#'
#' # Create a custom template with specific parameters
#' create_template("~/Documents/my_analysis.qmd",
#'                 title = "Experiment Results",
#'                 author = "Jane Doe")
#' }
#'
#' @importFrom stringr str_c str_glue str_replace str_to_lower str_detect
#' @export
create_template <- function(file_path = NULL,
                            title = "Data Analysis",
                            author = NULL,
                            type = "qmd",
                            template = "data_analysis") {

  # Generate default file path if not provided
  if (is.null(file_path)) {
    date_str <- format(Sys.Date(), "%Y%m%d")
    file_path <- file.path(getwd(), stringr::str_glue("analysis_{date_str}.{type}"))
    message(stringr::str_glue("No file path provided. Using default: {file_path}"))
  }

  # Input validation
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string")
  }

  if (!is.character(title) || length(title) != 1) {
    stop("title must be a single character string")
  }

  if (!is.null(author) && (!is.character(author) || length(author) != 1)) {
    stop("author must be a single character string or NULL")
  }

  # Validate and standardize the file type
  type <- stringr::str_to_lower(type)
  if (!type %in% c("qmd", "rmd", "r")) {
    stop('type must be one of "qmd", "rmd", or "r"')
  }

  # Dynamically check for available template functions
  # Look for functions named create_*_content in the package environment
  package_env <- environment()  # Or use getNamespace("yourpackagename")
  # Or directly use the functions from your package
  available_functions <- ls(pattern = "^create_.*_content$", envir = asNamespace("initRproject"))

  available_templates <- stringr::str_replace(available_functions, "^create_(.*)_content$", "\\1")

  # Validate template type
  if (!template %in% available_templates) {
    stop(stringr::str_glue(
      "Unknown template type: '{template}'. Available templates: {paste(available_templates, collapse = ', ')}"
    ))
  }

  # Ensure file has the correct extension
  file_ext <- tools::file_ext(file_path)
  if (file_ext == "") {
    # No extension provided, append the correct one
    file_path <- stringr::str_c(file_path, ".", type)
  } else if (stringr::str_to_lower(file_ext) != type) {
    # Wrong extension provided, warn and correct
    warning(stringr::str_glue(
      "File extension '.{file_ext}' doesn't match the specified type '{type}'. Changing extension to '.{type}'."
    ))
    file_path <- stringr::str_c(tools::file_path_sans_ext(file_path), ".", type)
  }

  # Create directory if it doesn't exist
  dir_path <- dirname(file_path)
  if (!dir.exists(dir_path)) {
    dir_create_result <- dir.create(dir_path, recursive = TRUE)
    if (!dir_create_result) {
      stop(stringr::str_glue("Failed to create directory: {dir_path}"))
    }
  }

  # Check if file already exists
  if (file.exists(file_path)) {
    stop(stringr::str_glue("File already exists: {file_path}"))
  }

  # Dynamically call the appropriate content creation function
  content_function_name <- stringr::str_glue("create_{template}_content")
  if (exists(content_function_name, envir = package_env)) {
    content_function <- get(content_function_name, envir = package_env)
    template_content <- content_function(title, author, type)
  } else {
    stop(stringr::str_glue("Template function '{content_function_name}' not found"))
  }

  # Write the template to file
  tryCatch({
    writeLines(template_content, file_path)
    message(stringr::str_glue("Template file created successfully: {file_path}"))
  }, error = function(e) {
    stop(stringr::str_glue("Failed to write template file: {e$message}"))
  })

  # Return the file path invisibly
  invisible(file_path)
}


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
    "  \"chenyu-psy/smartr@v0.2.2\", ",
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
