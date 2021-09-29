


#' Check bootcamp version
#'
#' Check the installed bootcamp version
#'
#' With this function you can check if you have the most recent version of
#' the bootcamp package installed.
#'
#' If you are current, the function returns a message that tells you so.
#' If there is a more recent version on Github, the function will inform you
#' that an update is available. It will also offer you the option to update
#' the package by simply pressing the "1" or "Y" key.
#'
#' @return nothing relevant, this function is useful for its side effect.
#' @export
#'
#' @examples
#' \dontrun{
#' check_bootcamp()
#' }
check_bootcamp <- function() {
  check_and_update_github("SNAnalyst/bootcamp_2021")
}





check_and_update_github <- function(pkg) {
  check <- check_github(pkg = pkg)

  if (is.na(check$installed_version)) {
    print(paste0("The ", check$package, " package is not installed\n"))
    choice <- utils::menu(c("Y", "N"), title = c("Do you want me to install", pkg, "?"))
    if (choice == 1) {
      remotes::install_github(repo = pkg, dependencies = TRUE)
    }
    invisible(NA)
  } else if (check$installed_version >= check$latest_version) {
    print(paste0("The installed ", check$package, " package is up-to-date"))
    invisible(TRUE)
  } else {
    print(paste0("You do not have the latest version of the ", check$package, " package."))

    choice <- utils::menu(c("Y", "N"), title = "Do you want me to update bootcamp2021?")
    if (choice == 1) {
      remotes::install_github(repo = pkg, dependencies = TRUE)
    }
    invisible(FALSE)
  }
}





check_github <- function(pkg) {
  installed_version <- tryCatch(utils::packageVersion(gsub(".*/", "", pkg)), error=function(e) NA)

  url <- paste0("https://raw.githubusercontent.com/", pkg, "/master/DESCRIPTION")

  x <- readLines(url)
  remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])

  res <- list(package = pkg,
              installed_version = installed_version,
              latest_version = remote_version,
              up_to_date = NA)

  if (is.na(installed_version)) {
    message(paste("##", pkg, "is not installed..."))
  } else {
    if (remote_version > installed_version) {
      res$up_to_date <- FALSE
    } else if (remote_version == installed_version) {
      res$up_to_date <- TRUE
    }
  }

  return(res)
}

