# INDEX <- read.table(
#   file = "C:/Dropbox/R/eigen_packages/=git/bootcamp_2021/doc/INDEX",
#   header = TRUE, sep = ",", strip.white = TRUE)


#' Open a bootcamp_2021 vignette in the browser
#'
#' Open a bootcamp_2021 vignette in your browser
#'
#' Shows a list of currently available online vignettes for the #' \code{bootcamp_2021} package.
#' The user can pick the preferred vignette by entering the number that corresponds
#' to the preferred vignette. The vignette will then open in the user's default
#' web browser.
#'
#' When \code{graphics} is \code{TRUE}, a graphical choice menu is shown. If that
#' is not preferred, or if the user's machine lacks the graphical tools needed,
#' setting \code{graphics} to \code{FALSE} will show the list of vignettes in the
#' R console.
#'
#' @param graphics logical, should the list of options be shown as a clickable
#' graphical menu?
#'
#' @return nothing
#' @export
#' @examples
#' \dontrun{
#' open_bootcamp_2021_vignettes()
#' }
open_bootcamp_2021_vignettes <- function(graphics = TRUE) {
  paths <- find.package("bootcamp_2021", lib.loc = NULL, quiet = TRUE)

  if (dir.exists(file.path(paths, "doc"))) {
    paths <- file.path(paths, "doc")
  } else if (dir.exists(file.path(paths, "inst", "doc"))) {
    paths <- file.path(paths, "inst", "doc")
  } else {
    stop("The folder containing the vignettes does not exist")
  }

  INDEX <- utils::read.table(
    file = file.path(paths, "INDEX"),
    header = TRUE, sep = ",", strip.white = TRUE)

  INDEX$DOC <- file.path(paths, INDEX$DOC)

  perform <- glue::glue("browseURL('{vignette}')", vignette = INDEX$DOC)
  cat("\n\nPlease pick which vignette you want to open, it will show in your default browser.\n")
  cat("The following vignettes are currently available to pick from:\n")
  pick <- utils::menu(INDEX$TITLE, graphics = graphics, title = "Select your favorite vignette")
  glue::identity_transformer(perform[pick], .GlobalEnv)
}
