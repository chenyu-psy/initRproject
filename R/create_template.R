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


#' Generate Template Creation Function Code
#'
#' @description
#' Analyzes an existing Quarto (.qmd), R Markdown (.rmd), or R script (.R) file
#' and generates the R code for a template creation function that can be added
#' to the package. This allows users to easily create their own custom templates.
#' The generated template function will only support the same file type(s) as the source file.
#'
#' @param file_path Character string. The path to an existing file that will serve
#'   as the template source.
#' @param template_name Character string. The name to use for the new template function.
#'   This will be used to create a function named `create_\{template_name\}_content`.
#'   Should be a valid R function name component (lowercase, no spaces).
#' @param description Character string. A brief description of the template that will
#'   be included in the function documentation. Defaults to "Custom template".
#'
#' @return Character string containing the R code for the new template creation function.
#'   This code can be copied into a package file or saved to a new file.
#'
#' @examples
#' \dontrun{
#' # Generate a template function from an existing Quarto file
#' code <- generate_template_function("my_analysis.qmd", "publication_ready")
#'
#' # View the generated code
#' cat(code)
#'
#' # Save the generated code to a file
#' writeLines(code, "create_publication_ready_content.R")
#' }
#'
#' @importFrom stringr str_glue str_replace str_to_lower str_detect
#' @export
generate_template_function <- function(file_path,
                                       template_name,
                                       description = "Custom template") {

  # Input validation
  if (!is.character(file_path) || length(file_path) != 1) {
    stop("file_path must be a single character string")
  }

  if (!file.exists(file_path)) {
    stop(stringr::str_glue("File does not exist: {file_path}"))
  }

  if (!is.character(template_name) || length(template_name) != 1) {
    stop("template_name must be a single character string")
  }

  # Check if template_name is a valid R function name component
  if (!grepl("^[a-z][a-z0-9_]*$", template_name)) {
    stop("template_name must start with a lowercase letter and contain only lowercase letters, numbers, and underscores")
  }

  # Determine file type
  file_ext <- tolower(tools::file_ext(file_path))
  if (!file_ext %in% c("qmd", "rmd", "r")) {
    stop("File must be a Quarto (.qmd), R Markdown (.rmd), or R script (.R) file")
  }

  # Read the file content
  file_content <- readLines(file_path, warn = FALSE)

  # Call the appropriate helper function based on file type
  if (file_ext %in% c("qmd", "rmd")) {
    generate_markdown_template_function(file_content, template_name, description, file_ext)
  } else if (file_ext == "r") {
    generate_r_template_function(file_content, template_name, description)
  }
}

#' Generate Template Function for Quarto or R Markdown Files
#'
#' Helper function to generate template creation code for Quarto or R Markdown files.
#'
#' @param file_content Character vector containing the content of the source file.
#' @param template_name Character string. The name for the new template function.
#' @param description Character string. A brief description of the template.
#' @param file_ext Character string. The file extension of the source file ("qmd" or "rmd").
#'
#' @return Character string containing the R code for the new template creation function.
#'
#' @keywords internal
generate_markdown_template_function <- function(file_content, template_name, description, file_ext) {
  # Generate the function code
  function_name <- stringr::str_glue("create_{template_name}_content")

  # Create the function code with documentation
  function_code <- c(
    stringr::str_glue("#' Create {stringr::str_to_title(stringr::str_replace_all(template_name, '_', ' '))} Content"),
    "#'",
    stringr::str_glue("#' @description"),
    stringr::str_glue("#' {description}"),
    "#'",
    "#' @param title Character string. The document title.",
    "#' @param author Character string or NULL. The document author.",
    "#' @param type Character string. The document type.",
    "#'   Must be either \"qmd\" or \"rmd\".",
    "#'",
    "#' @return Character vector containing the document content.",
    "#'",
    "#' @importFrom stringr str_glue",
    "#' @keywords internal",
    stringr::str_glue("{function_name} <- function(title, author, type) {{"),
    "",
    "  # Validate file type",
    "  type <- tolower(type)",
    "  if (!type %in% c(\"qmd\", \"rmd\")) {",
    stringr::str_glue("    stop(\"The {template_name} template only supports Quarto (.qmd) and R Markdown (.rmd) files.\")"),
    "  }",
    "",
    "  # Start with YAML header",
    "  yaml_header <- c(\"---\", stringr::str_glue('title: \"{title}\"'))",
    "",
    "  # Add author if provided",
    "  if (!is.null(author)) {",
    "    yaml_header <- c(yaml_header, stringr::str_glue('author: \"{author}\"'))",
    "  }",
    ""
  )

  # Extract YAML header from the original file
  yaml_start <- which(file_content == "---")[1]
  yaml_end <- which(file_content == "---")[2]

  if (!is.na(yaml_start) && !is.na(yaml_end) && yaml_end > yaml_start) {
    yaml_content <- file_content[(yaml_start + 1):(yaml_end - 1)]

    # Find and remove title and author lines
    title_line_index <- grep("^title:", yaml_content)
    author_line_index <- grep("^author:", yaml_content)

    if (length(title_line_index) > 0) {
      yaml_content <- yaml_content[-title_line_index]
    }

    if (length(author_line_index) > 0) {
      yaml_content <- yaml_content[-author_line_index]
    }

    # Add remaining YAML content
    if (length(yaml_content) > 0) {
      yaml_lines <- paste0("    \"", gsub("\"", "\\\\\"", yaml_content), "\"")

      function_code <- c(
        function_code,
        "  # Complete the YAML header with additional settings",
        "  yaml_header <- c(",
        "    yaml_header,",
        yaml_lines,
        "  )",
        ""
      )
    } else {
      function_code <- c(
        function_code,
        "  # Complete the YAML header",
        "  yaml_header <- c(",
        "    yaml_header,",
        "    \"format: html\"",
        "  )",
        ""
      )
    }
  } else {
    # If no YAML header found, add a basic one
    function_code <- c(
      function_code,
      "  # Complete the YAML header",
      "  yaml_header <- c(",
      "    yaml_header,",
      "    \"format: html\"",
      "  )",
      ""
    )
  }

  # Add the document body
  body_start <- if (!is.na(yaml_end)) yaml_end + 1 else 1
  if (body_start <= length(file_content)) {
    body_lines <- paste0("    \"", gsub("\"", "\\\\\"", file_content[body_start:length(file_content)]), "\"")

    function_code <- c(
      function_code,
      "  # Document body",
      "  document_body <- c(",
      body_lines,
      "  )",
      "",
      "  # Combine header and body",
      "  c(yaml_header, document_body)"
    )
  } else {
    function_code <- c(
      function_code,
      "  # Empty document body",
      "  document_body <- c(\"\")",
      "",
      "  # Combine header and body",
      "  c(yaml_header, document_body)"
    )
  }

  # Close the function
  function_code <- c(function_code, "}")

  # Return the complete function code as a single string
  paste(function_code, collapse = "\n")
}

#' Generate Template Function for R Script Files
#'
#' Helper function to generate template creation code for R script files.
#'
#' @param file_content Character vector containing the content of the source file.
#' @param template_name Character string. The name for the new template function.
#' @param description Character string. A brief description of the template.
#'
#' @return Character string containing the R code for the new template creation function.
#'
#' @keywords internal
generate_r_template_function <- function(file_content, template_name, description) {
  # Generate the function code
  function_name <- stringr::str_glue("create_{template_name}_content")

  # Create the function code with documentation
  function_code <- c(
    stringr::str_glue("#' Create {stringr::str_to_title(stringr::str_replace_all(template_name, '_', ' '))} Content"),
    "#'",
    stringr::str_glue("#' @description"),
    stringr::str_glue("#' {description}"),
    "#'",
    "#' @param title Character string. The document title.",
    "#' @param author Character string or NULL. The document author.",
    "#' @param type Character string. The document type.",
    "#'   Must be \"r\".",
    "#'",
    "#' @return Character vector containing the document content.",
    "#'",
    "#' @importFrom stringr str_glue",
    "#' @keywords internal",
    stringr::str_glue("{function_name} <- function(title, author, type) {{"),
    "",
    "  # Validate file type",
    "  type <- tolower(type)",
    "  if (type != \"r\") {",
    stringr::str_glue("    stop(\"The {template_name} template only supports R script (.r) files.\")"),
    "  }",
    "",
    "  # Create R script header with comments",
    "  header <- c(",
    "    \"# ==============================================================================\",",
    "    stringr::str_glue(\"# {title}\"),",
    "    \"# ==============================================================================\",",
    "    \"\"",
    "  )",
    "",
    "  # Add author if provided",
    "  if (!is.null(author)) {",
    "    header <- c(",
    "      header[1:2],",
    "      stringr::str_glue(\"# Author: {author}\"),",
    "      header[3:length(header)]",
    "    )",
    "  }",
    "",
    "  # Add creation date",
    "  header <- c(",
    "    header,",
    "    stringr::str_glue(\"# Created: {format(Sys.Date(), '%Y-%m-%d')}\"),",
    "    \"# ==============================================================================\",",
    "    \"\"",
    "  )",
    ""
  )

  # Add the document body, skipping any header comments that were replaced
  skip_lines <- 0
  for (i in 1:min(20, length(file_content))) {
    if (grepl("^#.*=+", file_content[i]) ||
        grepl("^#.*[Tt]itle", file_content[i]) ||
        grepl("^#.*[Aa]uthor", file_content[i]) ||
        grepl("^#.*[Cc]reated", file_content[i]) ||
        grepl("^#.*[Dd]ate", file_content[i])) {
      skip_lines <- i
    } else if (!grepl("^#", file_content[i]) && !grepl("^\\s*$", file_content[i])) {
      break
    }
  }

  body_start <- skip_lines + 1
  if (body_start <= length(file_content)) {
    body_lines <- paste0("    \"", gsub("\"", "\\\\\"", file_content[body_start:length(file_content)]), "\"")

    function_code <- c(
      function_code,
      "  # Document body",
      "  document_body <- c(",
      body_lines,
      "  )",
      "",
      "  # Combine header and body",
      "  c(header, document_body)"
    )
  } else {
    function_code <- c(
      function_code,
      "  # Empty document body",
      "  document_body <- c(\"\")",
      "",
      "  # Combine header and body",
      "  c(header, document_body)"
    )
  }

  # Close the function
  function_code <- c(function_code, "}")

  # Return the complete function code as a single string
  paste(function_code, collapse = "\n")
}
