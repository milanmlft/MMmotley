#' Find all R-packages used in a project.
#'
#' This is essentially a wrapper around [`renv::dependencies()`], but returns
#' the packages as a character vector which can be supplied directly to
#' [`install.packages()`] or similar. In addition, it figures out which packages
#' are installed from GitHub and prepends the correct username/organization to
#' the package name, for use in e.g. [`remotes::install_github()`] or
#' [`BiocManager::install()`]. Optionally, an R script is written containing
#' code to install the packages.
#'
#' **NOTE** that this function only works in projects with
#' [*renv*](https://rstudio.github.io/renv/) set up! At the least, it requires a
#' [`renv.lock`](https://rstudio.github.io/renv/reference/lockfiles.html) file
#' to be present.
#'
#' @param path Path to the project to be parsed. Used the current directory by
#'   default.
#' @param write_script Should an install script be written? Can be either
#'   logical or a character string containing the name of the file to be
#'   written. Default: `FALSE`.
#'
#' @return If `write_script = FALSE`, returns a character vector of all packages
#'   used in the project. If `write_script = TRUE`, writes a file `install.R`
#'   (or a custom file name if provided) containing code to install all found
#'   packages.
#' @author Milan Malfait
#'
#' @examples
#' \dontrun{
#' find_project_dependencies()
#'
#' ## Writing to a custom file
#' tmp <- tempfile()
#' find_project_dependencies(write_script = tmp)
#' }
#' @export
find_project_dependenices <- function(path = ".", write_script = FALSE) {

  if (!file.exists("renv.lock")) {
    stop("This function should be used in a renv-based project.",
      "\nSet up renv first using `renv::init()`.", call. = FALSE)
  }

  deps <- renv::dependencies()
  deps <- unique(deps$Package)
  deps <- deps[!(deps %in% c("BiocManager", "renv"))] # don't include these

  ## Get source repositories from renv.lock file
  lock <- jsonlite::read_json("renv.lock")
  bioc_ver <- lock$Bioconductor$Version
  lock_pkgs <- lock$Packages
  pkgs <- lock_pkgs[names(lock_pkgs) %in% deps]
  is_gh <- vapply(pkgs, function(x) x$Source == "GitHub", logical(1))

  gh_pkgs <- pkgs[is_gh]
  gh_pkg_remotes <- vapply(gh_pkgs, function(x) {
    sprintf("%s/%s@%s", x$RemoteUsername, x$RemoteRepo, x$RemoteRef)
  }, character(1))

  out <- names(pkgs)
  out[which(is_gh)] <- gh_pkg_remotes[names(which(is_gh))]

  if (!isFALSE(write_script)) {
    stopifnot(is.logical(write_script) | is.character(write_script))
    if (is.character(write_script)) {
      fname <- write_script
    } else {
      fname <- "install.R"
    }
    .write_install_script(out, filename = fname, bioc_version = bioc_ver)
    invisible(out)
  } else {
    out
  }
}

.write_install_script <- function(to_install, filename, bioc_version) {
  out_file <- file(filename)
  writeLines(
    paste(
      'install.packages(c("remotes", "BiocManager"))',
      paste0('BiocManager::install(version = "', bioc_version, '")'),
      "\nBiocManager::install(c(",
      paste("  ", sub("(.*)", '"\\1"', to_install), collapse = ",\n"),
      "))",
      sep = "\n"
    ),
    con = out_file
  )
  close(out_file)
  invisible(TRUE)
}
