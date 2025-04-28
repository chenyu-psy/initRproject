#' Create a Symbolic Link
#'
#' @description
#' Creates a symbolic link from a link path to a target path.
#' On Windows, this function will provide instructions for creating symbolic links
#' as they require administrator privileges and different commands.
#'
#' @param target_path Character string. The target path that the symlink will point to.
#' @param link_path Character string. The path where the symbolic link will be created.
#' @param merge Logical. If TRUE and both target_path and link_path exist as directories,
#'        content from link_path will be merged into target_path. If FALSE, an error will be thrown
#'        when both directories contain files. Default is FALSE.
#'
#' @return Invisibly returns TRUE if the symlink was created successfully, FALSE otherwise.
#'
#' @details
#' On Unix-like systems (macOS, Linux), this function creates a symbolic link using the 'ln -s' command.
#' On Windows, symbolic links require administrator privileges and use different commands, so this
#' function will provide instructions for manual creation.
#'
#' If target_path already exists, the function will use it as is. If link_path exists as a regular
#' directory, the function will attempt to move its contents to target_path based on the merge
#' parameter.
#'
#' @examples
#' \dontrun{
#' create_symlink("~/data_storage", "~/data")
#' }
#'
#' @export
create_symlink <- function(target_path, link_path, merge = FALSE) {
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

  # Handle existing directories
  target_exists <- dir.exists(target_path_abs)
  link_exists <- dir.exists(link_path)
  link_is_symlink <- link_exists && nchar(Sys.readlink(link_path)) > 0

  # If link_path exists and is a regular directory (not a symlink)
  if (link_exists && !link_is_symlink) {
    link_files <- list.files(link_path, all.files = TRUE, no.. = TRUE, recursive = FALSE)

    # If target_path also exists
    if (target_exists) {
      target_files <- list.files(target_path_abs, all.files = TRUE, no.. = TRUE, recursive = FALSE)

      # Check for conflicts
      conflicts <- intersect(link_files, target_files)
      if (length(conflicts) > 0 && !merge) {
        stop("Both target_path and link_path contain files with the same names: ",
             paste(conflicts, collapse = ", "),
             ". Set merge=TRUE to merge directories.", call. = FALSE)
      }

      message("Target directory already exists. ",
              if(merge) "Merging" else "Moving",
              " files from link_path...")
    }

    # Move contents from link_path to target_path
    move_directory_contents(link_path, target_path_abs, merge)
  }

  # Ensure target_path exists
  if (!target_exists) {
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
  } else {
    message("Using existing target directory: ", target_path_abs)
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

#' Move Contents from One Directory to Another
#'
#' @param source_dir Character string. The source directory.
#' @param target_dir Character string. The target directory.
#' @param merge Logical. If TRUE, files with the same name will be skipped with a warning.
#'        If FALSE, the function will stop if there are conflicts.
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#'
#' @keywords internal
move_directory_contents <- function(source_dir, target_dir, merge = FALSE) {
  # List all files in source directory (including hidden files)
  files <- list.files(source_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)

  if (length(files) == 0) {
    message("No files to move from ", source_dir)
    return(invisible(TRUE))
  }

  # Ensure target directory exists
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
    message("Created directory: ", target_dir)
  }

  # Move each file
  move_success <- TRUE
  for (file_path in files) {
    target_path <- file.path(target_dir, basename(file_path))

    # Check if target already exists
    if (file.exists(target_path)) {
      if (merge) {
        warning("Skipping file as target already exists: ", target_path, call. = FALSE)
        move_success <- FALSE
        next
      } else {
        stop("Cannot move file because target already exists: ", target_path,
             ". Set merge=TRUE to skip existing files.", call. = FALSE)
      }
    }

    # Attempt to move the file
    move_result <- tryCatch({
      file.rename(file_path, target_path)
    }, error = function(e) {
      warning("Failed to move file from ", file_path, " to ", target_path,
              "\nError: ", e$message, call. = FALSE)
      FALSE
    })

    if (!move_result) {
      move_success <- FALSE
    }
  }

  if (move_success) {
    message("Moved files from ", source_dir, " to ", target_dir)
  } else if (merge) {
    message("Moved some files from ", source_dir, " to ", target_dir,
            " (some were skipped due to conflicts)")
  }

  return(invisible(move_success))
}
