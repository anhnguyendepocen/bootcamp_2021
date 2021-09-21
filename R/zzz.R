

.onLoad <- function(libname, pkgname) {
  op <- options()
  invisible()
}


.onAttach <- function(lib, pkg,...){
  print_message <-  paste("\n",
                          "Welcome to bootcamp_2021 version ", utils::packageDescription("bootcamp_2021")$Version,
                          "\n",
                          "Type ?bootcamp_2021 to access the package documentation\n\n",
                          "To suppress this message use:\n",
                          "\tsuppressPackageStartupMessages(library(bootcamp_2021))\n\n",
                          "You can check if you have the latest version with 'check_bootcamp_2021()'\n",
                          sep = "")
  packageStartupMessage(print_message)
}


