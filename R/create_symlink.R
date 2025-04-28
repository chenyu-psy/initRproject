#' Create a Symbolic Link to a Directory
#'
#' @description
#' Creates a symbolic link from a link path to a target directory path.
#' On Windows, this function will provide instructions for creating symbolic links
#' as they require administrator privileges and different commands.
#'
#' @param target_path Character string. The target directory path that the symlink will point to.
#' @param link_path Character string. The path where the symbolic link will be created.
#'
#' @return Invisibly returns TRUE if the symlink was created successfully, FALSE otherwise.
#'
#' @details
#' On Unix-like systems (macOS, Linux), this function creates a symbolic link using the 'ln -s' command.
#' On Windows, symbolic links require administrator privileges and use different commands, so this
#' function will provide instructions for manual creation.
#'
#' @examples
#' \dontrun{
#' create_symlink("~/data_storage", "~/data")
#' }
#'
#' @export
create_symlink <- function(target_path, link_path) {
  # Input validation
  if (!is.character(target_path) || length(target_path) != 1 || is.na(target_path)) {
    stop("'target_path' must be a single character string", call. = FALSE)
  }
  if (!is.character(link_path) || length(link_path) != 1 || is.na(link_path)) {
    stop("'link_path' must be a single character string", call. = FALSE)
  }

  # Check operating system
  is_windows <- .Platform$OS.type == "windows"

  if (is_windows) {
    # Windows handling
    warning("Symbolic link creation on Windows requires administrator privileges and different commands.", call. = FALSE)
    message("To create a symbolic link on Windows, follow these steps:")
    message("1. Open Command Prompt as Administrator")
    message("2. Run the following command:")

    # Normalize paths for Windows
    target_path_abs <- normalizePath(target_path, mustWork = FALSE)
    link_path_abs <- normalizePath(link_path, mustWork = FALSE)

    # Windows uses mklink /D for directory symbolic links
    message(paste0('mklink /D "', link_path_abs, '" "', target_path_abs, '"'))

    message("\nAlternatively, on Windows 10 with Developer Mode enabled, you can create symbolic links without admin privileges.")

    return(invisible(FALSE))
  }

  # Unix-like systems (macOS, Linux)
  # Normalize paths
  target_path_abs <- normalizePath(target_path, mustWork = FALSE)
  link_path_abs <- normalizePath(link_path, mustWork = FALSE)

  # Check if paths are identical
  if (target_path_abs == link_path_abs) {
    stop("'target_path' and 'link_path' cannot be the same", call. = FALSE)
  }

  # Ensure target_path exists
  if (!dir.exists(target_path_abs)) {
    create_result <- tryCatch({
      dir.create(target_path_abs, recursive = TRUE)
      TRUE
    }, error = function(e) {
      warning("Failed to create directory: ", target_path_abs,
              "\nError: ", e$message, call. = FALSE)
      FALSE
    })

    if (create_result) {
      message("Created directory: ", target_path_abs)
    } else {
      return(invisible(FALSE))
    }
  }

  # Try a completely different approach - use a temporary script
  temp_script <- tempfile(fileext = ".sh")

  script_content <- paste0(
    "#!/bin/bash\n",
    "# Remove existing file/directory if it exists\n",
    "if [ -e \"", link_path, "\" ] || [ -L \"", link_path, "\" ]; then\n",
    "  rm -rf \"", link_path, "\"\n",
    "  echo \"Removed existing path\"\n",
    "fi\n",
    "\n",
    "# Create parent directory if needed\n",
    "mkdir -p \"$(dirname \"", link_path, "\")\"\n",
    "\n",
    "# Create the symlink\n",
    "ln -s \"", target_path_abs, "\" \"", link_path, "\"\n",
    "exit $?\n"
  )

  # Write the script to a file
  writeLines(script_content, temp_script)

  # Make the script executable
  Sys.chmod(temp_script, mode = "0755")

  # Execute the script
  message("Running shell script to create symlink...")
  status <- system(paste("bash", shQuote(temp_script)), wait = TRUE)

  # Clean up
  unlink(temp_script)

  if (status == 0) {
    message("Successfully created symlink: ", link_path, " -> ", target_path_abs)
    return(invisible(TRUE))
  } else {
    warning("Failed to create symlink. Script returned status: ", status, call. = FALSE)

    # Try one more approach - direct command with absolute paths
    message("Trying one more approach with absolute paths...")

    # First remove any existing file
    system(paste("rm -rf", shQuote(link_path_abs)), wait = TRUE)

    # Small delay to ensure filesystem catches up
    Sys.sleep(0.5)

    # Create the symlink with absolute paths
    final_cmd <- paste("ln -s", shQuote(target_path_abs), shQuote(link_path_abs))
    message("Running command: ", final_cmd)

    final_status <- system(final_cmd, wait = TRUE)

    if (final_status == 0) {
      message("Successfully created symlink with absolute paths.")
      return(invisible(TRUE))
    } else {
      warning("All attempts to create symlink failed. Please try manually:\n",
              "rm -rf ", shQuote(link_path_abs), "\n",
              "ln -s ", shQuote(target_path_abs), " ", shQuote(link_path_abs),
              call. = FALSE)
      return(invisible(FALSE))
    }
  }
}
