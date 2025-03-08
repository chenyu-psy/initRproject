#' Extract Package Name from Package Identifier
#'
#' Parses a package identifier and extracts the actual package name.
#' Works with both CRAN packages and GitHub packages (with or without version).
#'
#' @param pkg_id Character string. Package identifier (e.g., "dplyr" for CRAN,
#'               "username/reponame" or "username/reponame@v0.1.1" for GitHub)
#' @return Character string. The extracted package name.
#' @keywords internal
extract_package_name <- function(pkg_id) {
  # Input validation
  if (!is.character(pkg_id) || length(pkg_id) != 1 || is.na(pkg_id) || pkg_id == "") {
    stop("Package identifier must be a single non-empty character string")
  }

  # CRAN package (no slash in identifier)
  if (!grepl("/", pkg_id)) {
    return(pkg_id)
  }

  # GitHub package - extract base name
  # First remove version part (if present)
  pkg_base <- strsplit(pkg_id, "@")[[1]][1]
  # Extract repository name (part after the last slash)
  return(sub(".*[/]", "", pkg_base))
}


#' Create Package Status Data Frame
#'
#' Helper function to initialize the package status tracking data frame.
#'
#' @param packages Character vector of package identifiers.
#' @return Data frame with initialized package status information.
#' @keywords internal
create_package_status_df <- function(packages) {
  data.frame(
    package_id = packages,
    package_name = vapply(packages, extract_package_name, character(1)),
    installed = FALSE,
    loaded = FALSE,
    message = character(length(packages)),
    stringsAsFactors = FALSE
  )
}

#' Check Installed Packages
#'
#' Helper function to check which packages are already installed.
#'
#' @param pkg_status Data frame tracking package status.
#' @return Updated pkg_status data frame.
#' @keywords internal
check_installed_packages <- function(pkg_status) {
  for (i in seq_len(nrow(pkg_status))) {
    pkg_name <- pkg_status$package_name[i]
    if (base::requireNamespace(pkg_name, quietly = TRUE)) {
      pkg_status$installed[i] <- TRUE
      pkg_status$message[i] <- paste(pkg_name, "already installed")
    }
  }
  return(pkg_status)
}

#' Install Missing Packages
#'
#' Helper function to install packages that are not yet installed.
#'
#' @param pkg_status Data frame tracking package status.
#' @param auto_install Logical. Whether to install without prompting.
#' @param github_auth Character. GitHub authentication token.
#' @return Updated pkg_status data frame.
#' @keywords internal
install_missing_packages <- function(pkg_status, auto_install, github_auth) {
  # Identify uninstalled packages
  uninstalled_idx <- which(!pkg_status$installed)

  if (length(uninstalled_idx) > 0) {
    # Separate into CRAN and GitHub packages
    uninstalled_pkgs <- pkg_status$package_id[uninstalled_idx]
    is_github <- grepl("/", uninstalled_pkgs)

    cran_pkgs <- uninstalled_pkgs[!is_github]
    github_pkgs <- uninstalled_pkgs[is_github]

    # Process CRAN packages
    if (length(cran_pkgs) > 0) {
      pkg_status <- install_cran_packages(pkg_status, cran_pkgs, auto_install)
    }

    # Process GitHub packages
    if (length(github_pkgs) > 0) {
      pkg_status <- install_github_packages(pkg_status, github_pkgs, auto_install, github_auth)
    }
  }

  return(pkg_status)
}

#' Install CRAN Packages
#'
#' Helper function to install packages from CRAN.
#'
#' @param pkg_status Data frame tracking package status.
#' @param cran_pkgs Character vector of CRAN package identifiers.
#' @param auto_install Logical. Whether to install without prompting.
#' @return Updated pkg_status data frame.
#' @keywords internal
install_cran_packages <- function(pkg_status, cran_pkgs, auto_install) {
  if (length(cran_pkgs) == 0) return(pkg_status)

  # Determine whether to install
  should_install <- auto_install

  if (!auto_install) {
    cat("The following CRAN packages need to be installed:\n")
    cat(paste(" -", cran_pkgs), sep = "\n")
    response <- readline(prompt = "Do you want to install these CRAN packages? (y/n): ")
    should_install <- tolower(substring(response, 1, 1)) == "y"
  }

  if (should_install) {
    for (pkg_id in cran_pkgs) {
      i <- which(pkg_status$package_id == pkg_id)
      pkg_name <- pkg_status$package_name[i]

      tryCatch({
        utils::install.packages(pkg_id)
        if (base::requireNamespace(pkg_name, quietly = TRUE)) {
          pkg_status$installed[i] <- TRUE
          pkg_status$message[i] <- paste(pkg_name, "installed from CRAN")
        } else {
          pkg_status$message[i] <- paste("Failed to install", pkg_name, "from CRAN")
        }
      }, error = function(e) {
        pkg_status$message[i] <- paste("Error installing", pkg_name, ":", e$message)
      })
    }
  } else {
    for (pkg_id in cran_pkgs) {
      i <- which(pkg_status$package_id == pkg_id)
      pkg_status$message[i] <- "Installation skipped by user"
    }
  }

  return(pkg_status)
}

#' Install GitHub Packages
#'
#' Helper function to install packages from GitHub.
#'
#' @param pkg_status Data frame tracking package status.
#' @param github_pkgs Character vector of GitHub package identifiers.
#' @param auto_install Logical. Whether to install without prompting.
#' @param github_auth Character. GitHub authentication token.
#' @return Updated pkg_status data frame.
#' @keywords internal
install_github_packages <- function(pkg_status, github_pkgs, auto_install, github_auth) {
  if (length(github_pkgs) == 0) return(pkg_status)

  # Determine whether to install
  should_install <- auto_install

  if (!auto_install) {
    cat("The following GitHub packages need to be installed:\n")
    cat(paste(" -", github_pkgs), sep = "\n")
    response <- readline(prompt = "Do you want to install these GitHub packages? (y/n): ")
    should_install <- tolower(substring(response, 1, 1)) == "y"
  }

  if (should_install) {
    # Check for remotes package
    has_remotes <- ensure_remotes_package(pkg_status, github_pkgs, auto_install)

    if (has_remotes) {
      # Install GitHub packages
      for (pkg_id in github_pkgs) {
        i <- which(pkg_status$package_id == pkg_id)
        pkg_name <- pkg_status$package_name[i]

        tryCatch({
          # Use authentication if provided
          if (!is.null(github_auth)) {
            remotes::install_github(pkg_id, auth_token = github_auth)
          } else {
            remotes::install_github(pkg_id)
          }

          if (base::requireNamespace(pkg_name, quietly = TRUE)) {
            pkg_status$installed[i] <- TRUE
            pkg_status$message[i] <- paste(pkg_name, "installed from GitHub")
          } else {
            pkg_status$message[i] <- paste("Failed to install", pkg_name, "from GitHub")
          }
        }, error = function(e) {
          pkg_status$message[i] <- paste("Error installing", pkg_name, ":", e$message)
        })
      }
    }
  } else {
    for (pkg_id in github_pkgs) {
      i <- which(pkg_status$package_id == pkg_id)
      pkg_status$message[i] <- "Installation skipped by user"
    }
  }

  return(pkg_status)
}

#' Ensure Remotes Package
#'
#' Helper function to ensure the remotes package is installed.
#'
#' @param pkg_status Data frame tracking package status.
#' @param github_pkgs Character vector of GitHub package identifiers.
#' @param auto_install Logical. Whether to install without prompting.
#' @return Logical indicating whether remotes is available.
#' @keywords internal
ensure_remotes_package <- function(pkg_status, github_pkgs, auto_install) {
  if (base::requireNamespace("remotes", quietly = TRUE)) {
    return(TRUE)
  }

  # remotes package is not installed
  cat("The 'remotes' package is required to install GitHub packages.\n")

  should_install_remotes <- auto_install

  if (!auto_install) {
    remotes_response <- readline(prompt = "Do you want to install the 'remotes' package? (y/n): ")
    should_install_remotes <- tolower(substring(remotes_response, 1, 1)) == "y"
  }

  if (should_install_remotes) {
    tryCatch({
      utils::install.packages("remotes")
      if (base::requireNamespace("remotes", quietly = TRUE)) {
        return(TRUE)
      } else {
        warning("Failed to install 'remotes' package. GitHub packages will be skipped.")
        for (pkg_id in github_pkgs) {
          i <- which(pkg_status$package_id == pkg_id)
          pkg_status$message[i] <- "Skipped: 'remotes' package installation failed"
        }
        return(FALSE)
      }
    }, error = function(e) {
      warning(paste("Error installing 'remotes' package:", e$message))
      for (pkg_id in github_pkgs) {
        i <- which(pkg_status$package_id == pkg_id)
        pkg_status$message[i] <- "Skipped: 'remotes' package installation failed"
      }
      return(FALSE)
    })
  } else {
    for (pkg_id in github_pkgs) {
      i <- which(pkg_status$package_id == pkg_id)
      pkg_status$message[i] <- "Skipped: 'remotes' package not installed"
    }
    return(FALSE)
  }
}

#' Load Packages
#'
#' Helper function to load installed packages.
#'
#' @param pkg_status Data frame tracking package status.
#' @return Updated pkg_status data frame.
#' @keywords internal
load_packages <- function(pkg_status) {
  for (i in seq_len(nrow(pkg_status))) {
    pkg_name <- pkg_status$package_name[i]

    if (pkg_status$installed[i]) {
      tryCatch({
        library(pkg_name, character.only = TRUE)
        pkg_status$loaded[i] <- TRUE

        # Only update message if it doesn't already contain an installation message
        if (!grepl("installed", pkg_status$message[i])) {
          pkg_status$message[i] <- paste(pkg_name, "loaded successfully")
        } else {
          pkg_status$message[i] <- paste(pkg_status$message[i], "and loaded successfully")
        }
      }, error = function(e) {
        pkg_status$message[i] <- paste("Error loading", pkg_name, ":", e$message)
      })
    }
  }

  return(pkg_status)
}

#' Process Multiple Packages
#'
#' Processes a vector of package identifiers - installing and loading each package.
#' Tracks the status of each operation and returns detailed results.
#'
#' @param packages Character vector. Package identifiers to process.
#' @param quietly Logical. Whether to suppress the return of the status data frame.
#'                If TRUE, the function returns invisibly. Default is FALSE.
#' @param auto_install Logical. If TRUE, installs packages without prompting.
#'                     Default is FALSE.
#' @param github_auth Character. GitHub authentication token for private repositories.
#'                   Default is NULL (no authentication).
#' @return Data frame with columns for package information and status.
#' @examples
#' \dontrun{
#' # Process multiple packages with interactive prompts
#' result <- process_packages(c("dplyr", "tidyverse/ggplot2"))
#'
#' # Process packages automatically without prompts
#' result <- process_packages(c("dplyr", "stringr"), auto_install = TRUE)
#' }
#' @export
process_packages <- function(packages, quietly = FALSE, auto_install = FALSE,
                             github_auth = NULL) {
  # Input validation
  if (!is.character(packages) || length(packages) == 0) {
    stop("'packages' must be a non-empty character vector")
  }

  if (!is.logical(quietly) || length(quietly) != 1 || is.na(quietly)) {
    stop("'quietly' must be a single logical value (TRUE or FALSE)")
  }

  if (!is.logical(auto_install) || length(auto_install) != 1 || is.na(auto_install)) {
    stop("'auto_install' must be a single logical value (TRUE or FALSE)")
  }

  # Create a data frame to track package information
  pkg_status <- create_package_status_df(packages)

  # Check which packages are already installed
  pkg_status <- check_installed_packages(pkg_status)

  # Install missing packages if needed
  pkg_status <- install_missing_packages(pkg_status, auto_install, github_auth)

  # Load packages
  pkg_status <- load_packages(pkg_status)

  # Return results based on quietly parameter
  if (!quietly) {
    return(pkg_status)
  }
  invisible(pkg_status)
}
