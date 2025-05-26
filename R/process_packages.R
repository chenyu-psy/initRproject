#' Parse Package Specifications
#'
#' @description Parses package specifications into a structured data frame with
#' information about package name, source (CRAN or GitHub), account (for GitHub packages),
#' and version or ref (for CRAN: version; for GitHub: branch/tag/commit/version).
#'
#' @param packages Character vector of package specifications. Formats supported:
#'   - "pkg_name": Standard CRAN package
#'   - "pkg_name@vX.Y.Z": CRAN package with version
#'   - "account/pkg_name": GitHub package
#'   - "account/pkg_name@vX.Y.Z": GitHub package with version/tag/commit
#'   - "account/pkg_name@branch": GitHub package with branch/tag/commit
#'
#' @return A data frame with columns:
#'   - name: Package name
#'   - source: "cran" or "github"
#'   - account: GitHub account (NA for CRAN packages)
#'   - version: Version, branch, tag, or commit (NA if not specified)
#'
#' @examples
#' parse_packages(c("dplyr", "ggplot2", "brms@v2.22.0", "chenyu-psy/smartr@v0.3.0", "chenyu-psy/smartr@develop"))
#'
#' @export
parse_packages <- function(packages) {
  # Input validation
  if (!is.character(packages)) {
    stop("Package specifications must be provided as a character vector")
  }
  if (length(packages) == 0) {
    stop("At least one package specification must be provided")
  }

  # Initialize result data frame
  result <- data.frame(
    name = character(length(packages)),
    source = character(length(packages)),
    account = rep(NA_character_, length(packages)),
    version = rep(NA_character_, length(packages)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(packages)) {
    pkg_spec <- packages[i]
    version_part <- NA_character_
    pkg_part <- pkg_spec

    # Split by @ if present
    if (grepl("@", pkg_spec, fixed = TRUE)) {
      parts <- strsplit(pkg_spec, "@", fixed = TRUE)[[1]]
      pkg_part <- parts[1]
      version_part <- parts[2]
    }

    # Check if it's a GitHub package (contains '/')
    if (grepl("/", pkg_part, fixed = TRUE)) {
      gh_parts <- strsplit(pkg_part, "/", fixed = TRUE)[[1]]
      result$account[i] <- gh_parts[1]
      result$name[i] <- gh_parts[2]
      result$source[i] <- "github"
    } else {
      result$name[i] <- pkg_part
      result$source[i] <- "cran"
    }
    result$version[i] <- version_part
  }

  return(result)
}


#' Check Installed Packages and Versions/Refs
#'
#' @description Checks if the specified packages are installed and, if a version or ref is required,
#' whether the installed version meets or exceeds the requirement (for version numbers).
#' For GitHub packages with non-numeric refs (e.g., "develop"), only installation status is checked.
#'
#' @param parsed_df Data frame as returned by `parse_packages()`.
#'
#' @return Data frame with columns:
#'   - name: Package name
#'   - source: "cran" or "github"
#'   - account: GitHub account (NA for CRAN)
#'   - version: Required version/ref (NA if not specified)
#'   - is_installed: TRUE if installed, FALSE otherwise
#'   - has_required_version: TRUE if installed version meets/exceeds requirement (for version numbers), NA otherwise
#'   - installed_version: Installed version (NA if not installed)
#'
#' @importFrom dplyr rename mutate left_join case_when
#'
#' @examples
#' pkgs <- c("dplyr", "ggplot2", "brms@2.22.0", "chenyu-psy/smartr@0.3.0", "chenyu-psy/smartr@develop")
#' parsed <- parse_packages(pkgs)
#' check_installed_packages(parsed)
#'
#' @export
check_installed_packages <- function(parsed_df) {

  # Input validation
  required_cols <- c("name", "source", "account", "version")
  if (!is.data.frame(parsed_df) || !all(required_cols %in% names(parsed_df))) {
    stop("Input must be a data frame as returned by parse_packages()")
  }

  # Get installed packages and their versions
  installed <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)

  # Check if each package is installed and its version
  results <- parsed_df %>%
    left_join(installed[, c("Package", "Version")], by = c("name" = "Package")) %>%
    rename(installed_version = Version) %>%
    mutate(
      is_installed = !is.na(installed_version),
      has_required_version = case_when(
        !is_installed ~ FALSE,
        is.na(version) ~ TRUE,  # No version specified, so no check
        installed_version >= gsub("[A-Za-z]", "", version) ~ TRUE,
        TRUE ~ FALSE
      )
    )


  return(results)
}


#' Install a GitHub Package with Automatic v-Prefix Retry
#'
#' @description
#' Attempts to install a GitHub package using \code{remotes::install_github()}. If the specified
#' \code{ref} (tag/branch/commit) looks like a version number (e.g., "0.3.0") and installation fails,
#' the function automatically retries with a "v" prefix (e.g., "v0.3.0"), which is a common GitHub tag convention.
#'
#' @param repo Character string in the format "account/repo".
#' @param ref Character string specifying the tag, branch, or commit to install. If \code{NULL}, the default branch is used.
#' @param quietly Logical; if \code{TRUE}, suppresses installation messages.
#' @param ... Additional arguments passed to \code{remotes::install_github()}.
#'
#' @return Logical. \code{TRUE} if installation succeeded, \code{FALSE} otherwise.
#'
#' @details
#' This function is useful when users specify a version tag without the "v" prefix, but the GitHub repository uses "v" (e.g., "v1.2.3").
#' It first tries the user-supplied \code{ref}, and if that fails and the ref is a version-like string without "v", it retries with "v" prepended.
#'
#' @examples
#' \dontrun{
#' # Try to install tag "0.3.0" or "v0.3.0" from GitHub
#' install_github_with_v_retry("chenyu-psy/smartr", ref = "0.3.0")
#' }
#' @export
install_github_with_v_retry <- function(repo, ref = NULL, quietly = FALSE, ...) {
  tryCatch({
    remotes::install_github(repo, ref = ref, quiet = quietly, ...)
    TRUE
  }, error = function(e) {
    if (!is.null(ref) && grepl("^\\d+(\\.\\d+)*$", ref)) {
      ref_v <- paste0("v", ref)
      if (!quietly) message(sprintf("Installation with ref='%s' failed. Retrying with ref='%s'...", ref, ref_v))
      tryCatch({
        remotes::install_github(repo, ref = ref_v, quiet = quietly, ...)
        TRUE
      }, error = function(e2) {
        if (!quietly) message(sprintf("Both attempts failed: %s", e2$message))
        FALSE
      })
    } else {
      if (!quietly) message(sprintf("Installation failed: %s", e$message))
      FALSE
    }
  })
}



#' Interactively or Automatically Install/Update Packages
#'
#' @param check_df Data frame as returned by check_installed_packages().
#' @param cran_repos CRAN repository URL.
#' @param quietly Logical, if TRUE, skip all prompts and install/update automatically.
#' @return Data frame of actions taken.
#' @export
interactive_package_manager <- function(check_df, cran_repos = "https://cran.rstudio.com/", quietly = FALSE) {
  required_cols <- c("name", "source", "account", "version", "is_installed", "has_required_version", "installed_version")
  if (!is.data.frame(check_df) || !all(required_cols %in% names(check_df))) {
    stop("Input must be a data frame as returned by check_installed_packages()")
  }
  ask_user <- function(prompt) {
    repeat {
      cat(prompt, "[y/n]: ")
      ans <- tolower(trimws(readline()))
      if (ans %in% c("y", "n")) return(ans == "y")
      cat("Please enter 'y' or 'n'.\n")
    }
  }
  if (!requireNamespace("remotes", quietly = TRUE)) {
    if (!quietly) cat("The 'remotes' package is required for GitHub installations. Installing now...\n")
    install.packages("remotes", repos = cran_repos, quiet = quietly, ask = FALSE)
  }
  actions <- data.frame(
    name = check_df$name,
    source = check_df$source,
    account = check_df$account,
    version = check_df$version,
    action = rep(NA_character_, nrow(check_df)),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(check_df))) {
    pkg <- check_df[i, ]
    pkg_label <- if (pkg$source == "cran") {
      if (!is.na(pkg$version)) paste0(pkg$name, "@", pkg$version) else pkg$name
    } else {
      if (!is.na(pkg$version)) paste0(pkg$account, "/", pkg$name, "@", pkg$version) else paste0(pkg$account, "/", pkg$name)
    }
    # 1. Missing CRAN package
    if (!pkg$is_installed && pkg$source == "cran") {
      if (quietly) {
        install.packages(pkg$name, repos = cran_repos, quiet = quietly, ask = FALSE)
        actions$action[i] <- "installed"
      } else if (ask_user(sprintf("CRAN package '%s' is missing. Install?", pkg_label))) {
        install.packages(pkg$name, repos = cran_repos, quiet = quietly, ask = FALSE)
        actions$action[i] <- "installed"
      } else {
        actions$action[i] <- "skipped"
      }
    }
    # 2. Missing GitHub package
    else if (!pkg$is_installed && pkg$source == "github") {
      repo <- paste0(pkg$account, "/", pkg$name)
      ref <- pkg$version
      if (quietly) {
        ok <- install_github_with_v_retry(repo, ref, quietly = quietly)
        actions$action[i] <- if (ok) "installed" else "failed"
      } else if (ask_user(sprintf("GitHub package '%s' is missing. Install?", pkg_label))) {
        ok <- install_github_with_v_retry(repo, ref, quietly = quietly)
        actions$action[i] <- if (ok) "installed" else "failed"
      } else {
        actions$action[i] <- "skipped"
      }
    }
    # 3. Outdated CRAN package
    else if (pkg$source == "cran" && !is.na(pkg$version) && !is.na(pkg$has_required_version) && !pkg$has_required_version) {
      if (quietly) {
        install.packages(pkg$name, repos = cran_repos, quiet = quietly, ask = FALSE)
        actions$action[i] <- "updated"
      } else if (ask_user(sprintf("CRAN package '%s' is installed (version %s), but version %s is required. Update?",
                                  pkg$name, pkg$installed_version, pkg$version))) {
        install.packages(pkg$name, repos = cran_repos, quiet = quietly, ask = FALSE)
        actions$action[i] <- "updated"
      } else {
        actions$action[i] <- "skipped"
      }
    }
    # 4. Outdated GitHub package with version
    else if (pkg$source == "github" && !is.na(pkg$version) && grepl("^v?\\d+(\\.\\d+)*$", pkg$version) &&
             !is.na(pkg$has_required_version) && !pkg$has_required_version) {
      repo <- paste0(pkg$account, "/", pkg$name)
      ref <- pkg$version
      if (quietly) {
        ok <- install_github_with_v_retry(repo, ref, quietly = quietly)
        actions$action[i] <- if (ok) "updated" else "failed"
      } else if (ask_user(sprintf("GitHub package '%s' is installed (version %s), but version %s is required. Update?",
                                  repo, pkg$installed_version, pkg$version))) {
        ok <- install_github_with_v_retry(repo, ref, quietly = quietly)
        actions$action[i] <- if (ok) "updated" else "failed"
      } else {
        actions$action[i] <- "skipped"
      }
    }
    # 5. GitHub package with non-version ref (e.g., develop, main)
    else if (pkg$source == "github" && !is.na(pkg$version) && !grepl("^v?\\d+(\\.\\d+)*$", pkg$version)) {
      repo <- paste0(pkg$account, "/", pkg$name)
      ref <- pkg$version
      if (quietly) {
        ok <- install_github_with_v_retry(repo, ref, quietly = quietly)
        actions$action[i] <- if (ok) "updated" else "failed"
      } else if (ask_user(sprintf("GitHub package '%s' is installed, but ref '%s' is specified. Update to this ref?",
                                  repo, ref))) {
        ok <- install_github_with_v_retry(repo, ref, quietly = quietly)
        actions$action[i] <- if (ok) "updated" else "failed"
      } else {
        actions$action[i] <- "skipped"
      }
    } else {
      actions$action[i] <- "none"
    }
  }
  if (!quietly) cat("All packages have been installed with the required version or higher.\n")
  invisible(actions)
}




#' Load All Packages from Parsed Data Frame
#'
#' @param parsed_df Data frame as returned by parse_packages().
#' @param quietly Logical, suppress messages if TRUE.
#' @return Invisibly returns a vector of loaded package names.
#' @export
load_parsed_packages <- function(parsed_df, quietly = FALSE) {
  loaded <- character(0)
  for (i in seq_len(nrow(parsed_df))) {
    pkg_name <- parsed_df$name[i]
    tryCatch({
      suppressMessages(library(pkg_name, character.only = TRUE, quietly = quietly, warn.conflicts = FALSE))
      loaded <- c(loaded, pkg_name)
    }, error = function(e) {
      if (!quietly) message(sprintf("Failed to load package '%s': %s", pkg_name, e$message))
    })
  }
  invisible(loaded)
}


#' Process, Install/Update, and Load R Packages Interactively or Quietly
#'
#' @param packages Character vector of package specifications.
#' @param cran_repos CRAN repository URL (default: "https://cran.rstudio.com/").
#' @param quietly Logical, if TRUE, skip all prompts and install/update/load automatically.
#'
#' @return Invisibly returns a list with actions and loaded packages.
#' @export
process_packages <- function(packages, cran_repos = "https://cran.rstudio.com/", quietly = FALSE) {
  # Step 1: Parse package specifications
  parsed <- parse_packages(packages)

  # Step 2: Check installed packages and versions
  checked <- check_installed_packages(parsed)

  # Step 3: Interactively or quietly install/update as needed
  actions <- interactive_package_manager(checked, cran_repos = cran_repos, quietly = quietly)

  # Step 4: Load all packages quietly or interactively
  loaded <- load_parsed_packages(parsed, quietly = quietly)

  invisible(list(actions = actions, loaded = loaded))
}
