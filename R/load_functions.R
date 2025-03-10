#' Load R Functions from a Directory
#'
#' @description
#' This function loads all R script files (with .R extension) from a specified directory
#' by sourcing each file. It provides a convenient way to load multiple function definitions
#' at once.
#'
#' @param directory_path Character string specifying the path to the directory containing
#'        R script files. Defaults to "./functions".
#' @param recursive Logical indicating whether to recursively search for R files in subdirectories.
#'        Defaults to FALSE.
#' @param pattern Character string containing a regular expression to match file names.
#'        Defaults to "\\.R$" (files ending with .R).
#' @param quiet Logical indicating whether to suppress messages and warnings during loading.
#'        Defaults to TRUE.
#'
#' @return Invisibly returns a character vector of the file paths that were loaded.
#'
#' @examples
#' \dontrun{
#' # Load all R files from the default directory
#' load_functions()
#'
#' # Load all R files from a custom directory
#' load_functions("path/to/my/functions")
#'
#' # Load files with verbose output
#' load_functions(quiet = FALSE)
#' }
#'
#' @export
load_functions <- function(directory_path = "./functions",
                           recursive = FALSE,
                           pattern = "\\.R$",
                           quiet = TRUE) {
  # Input validation
  if (!is.character(directory_path) || length(directory_path) != 1) {
    stop("'directory_path' must be a single character string")
  }

  if (!dir.exists(directory_path)) {
    stop("Directory '", directory_path, "' does not exist")
  }

  # Find all R files in the directory
  r_files <- list.files(
    path = directory_path,
    pattern = pattern,
    full.names = TRUE,
    recursive = recursive
  )

  if (length(r_files) == 0) {
    warning("No R files found in directory '", directory_path, "'")
    return(invisible(character(0)))
  }

  # Source each file
  loaded_files <- character(0)
  for (file in r_files) {
    tryCatch({
      if (quiet) {
        suppressMessages(suppressWarnings(source(file, echo = FALSE)))
      } else {
        source(file, echo = FALSE)
      }
      loaded_files <- c(loaded_files, file)
    }, error = function(e) {
      warning("Failed to load file '", file, "': ", e$message)
    })
  }

  # Return the list of loaded files invisibly
  invisible(loaded_files)
}
