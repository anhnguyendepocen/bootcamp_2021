

#' Check for presence of packages
#' 
#' Checks whether a set of packages is present and of the correct version
#' 
#' Useful inside a learnr tutorial.
#' 
#' \code{reqs} is a data.frame with column names "pkg", "version", "where". 
#' All items are \code{character}! 
#' 
#' \itemize{
#' \item{pkg}{names of the packages}
#' \item{version}{the minimally required version}
#' \item{where}{location of the package for download}
#' }
#' 
#' For \code{where} there are two options. If the package resides on CRAN, 
#' it should be "CRAN". If it is on github, it should be "username/reponame" (ie. 
#' whatever would go inside \code{remotes::install_github(where)}).
#' 
#' First, the function checks if a package is installed and, if installed, whether 
#' it has the required version number or higher. 
#' If eigher of these checks fail, it will attempt to download/upgrade the package.
#' 
#' After a first pass along all packages (as described above), a second check is 
#' performed if a package is now installed and, if installed, whether 
#' it has the required version number or higher. 
#' 
#' If a package is still not installed, a message is returned that tells the user 
#' how to install it manually. 
#' If a package still does not at least have the required version, 
#' a message is returned that tells the user how to upgrade it manually. 
#'
#' @param reqs data.frame (see details)
#'
#' @return character string, summarizing the results
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' pkgs <- matrix(c(
#' "gradethis", "0.2.3.9001", "rstudio/gradethis",
#' "igraph", "1.2.6", "CRAN",
#' "influenceR", "0.1.0", "CRAN",
#' "intergraph", "2.0-2", "CRAN",
#' "network", "1.17.1", "CRAN",
#' "networkD3", "0.4", "CRAN",
#' "sna", "2.6", "CRAN",
#' "SNA4DSData", "0.9.9000", "SNAnalyst/SNA4DSData"
#' ), byrow = TRUE, ncol = 3) |> 
#'   as.data.frame() |> 
#'   setNames(c("pkg", "version", "where"))
#'   
#' check_packages(pkgs)
#' }
check_packages <- function(reqs) {
  ok <- 0
  
  all_installed <- utils::installed.packages()
  cat("...checking individual packages now...\n")
  for (pak in 1:nrow(reqs)) {
    installed <- which(all_installed[, "Package"] == reqs[pak, "pkg"]) |> unname()
    if (length(installed) == 0) {   # not installed
      if (reqs[pak, "where"] == "CRAN") {
        try(utils::install.packages(reqs[pak, "pkg"], dependencies = TRUE), silent = TRUE)
      } else {  # package from github
        cat("...attempting to download a package from github...\n")
        try(remotes::install_github(reqs[pak, "where"]), silent = TRUE)
      }
    } else {  # installed
      # check whether the version is up-to-date
      # newer version is needed
      if (all_installed[installed, "Version"] < reqs[pak, "version"]) {
        if (reqs[pak, "where"] == "CRAN") {
          try(utils::install.packages(reqs[pak, "pkg"], dependencies = TRUE), silent = TRUE)
        } else {  # package from github
          cat("...attempting to download a package from github...\n")
          try(remotes::install_github(reqs[pak, "where"]), silent = TRUE)
        }
      } else {  # correct version present
        ok <- ok + 1
      }
    }
  }
  
  # initial check is done, missing packages and/or packages with older versions
  # have been attempted to download
  # Now check if this solved it
  # If not, tell the user how to solve it manually
  pkg_missing <- NULL
  pkg_low <- NULL
  
  if (ok == nrow(reqs)) {  # all packages are according to reqs
    verdict <- "Wonderful, all is fine."
    return(verdict)
  } else { # not all correct packages were present initially, so check if that has been corrected
    all_installed <- utils::installed.packages()
    for (pak in 1:nrow(reqs)) {
      installed <- which(all_installed[, "Package"] == reqs[pak, "pkg"]) |> unname()
      if (length(installed) == 0) {   # not installed
        pkg_missing <- c(pkg_missing, pak)
      } else { # is installed, check version
        if (all_installed[installed, "Version"] < reqs[pak, "version"]) { # version too low
          pkg_low <- c(pkg_low, pak)
        }
      }
    }
  }
  
  if ((is.null(pkg_low)) & (is.null(pkg_missing))) {
    verdict <- "Deficiencies have been fixed, all is fine now."
    return(verdict)
  }
  
  # Not all has been solved automatically
  # Generate output to the user on how (s)he can solve this manually
  verdict <- logical(0)
  # missing packages
  if (!is.null(pkg_missing)) {  # some packages are still missing
    names_missing <- paste0("'", reqs[pkg_missing, "pkg"], "'", collapse = ", ")
    verdict <- c(verdict, paste0("The following packages are missing: ", names_missing)) |> 
      c("Install these using:")
    reqs_missing <- reqs[pkg_missing, ]
    
    # missing from CRAN
    reqs_missing_cran <- reqs_missing[reqs_missing[, "where"] == "CRAN", "pkg", drop = TRUE]
    
    if (nrow(reqs_missing_cran) > 0) {
      verdict <- c(verdict, glue::glue("     install.packages('{package}')", package = reqs_missing_cran))
    }
    
    reqs_missing_github <- reqs_missing[reqs_missing[, "where"] != "CRAN", ]
    if (nrow(reqs_missing_github) > 0) {
      verdict <- c(verdict, 
                        glue::glue("     remotes::install_github('{location}')", 
                                          location = reqs_missing_github[, "where", drop = TRUE])) |> 
        c("", "") ## add two empty lines below
    }
  }
  
  if (!is.null(pkg_low)) { # There are packages with too low versions
    pkgs_low <- reqs[pkg_low, "pkg"]
    names_low <- paste0("'", pkgs_low, "'", collapse = ", ")
    
    verdict <- c(verdict, "", "", paste0("The version of the following packages is too low:", names_low)) |> 
      c("Upgrade using:") |> 
      c((glue::glue('      update.packages({package})', package = pkgs_low)))
  }
  
  return(noquote(verdict))
}







#' Check for correct version of RStudio
#' 
#' Check for correct version of RStudio
#' 
#' Checks whether the user is using the correct version of RStudio (or higher)
#'
#' @param version required version number (or higher)
#'
#' @return message with the result
#' @keywords internal
check_rstudio <- function(version = 4.1717) {
  ver <- rstudioapi::versionInfo()$version
  
  ver <- sub("1.", "", ver)
  
  if(ver < version) {
    
    verdict <- "Sorry! Your Rstudio is not update. Please download version 1.4.1725 or above"
    
  } else {
    
    verdict <- "Great! Your Rstudio is up to date!"
  }
  return(verdict)
  
}





#' Check for correct version of R
#' 
#' Check for correct version of R
#' 
#' Checks whether the user is using the correct version of R (exactly). 
#' Note that this checks for the exact version: if a user has a higher version of 
#' R, the function will report that the user is not using the correct version.
#'
#' @param Major Major version number, e.g., 4
#' @param Minor Minor version number
#'
#' @return message with the result
#' @keywords internal
check_r_equal <- function(Major = 4, Minor = 1.1) {
  major <- R.Version()$major
  minor <- R.Version()$minor
  
  if((major == Major) & (minor == Minor )) {
    
    verdict <- "Great! You have the correct R version!"
    
  } else {
    
    verdict <- "Sorry! You have the wrong R version. Please update R to version 4.1.1"
    
  }
  return(verdict)
}