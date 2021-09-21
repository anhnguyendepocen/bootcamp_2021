
#' Save internal data
#'
#' Save and add to internal data
#'
#' Saves internal data. There are arguments whether or not existing objects
#' can be safely overwritten and whether the data object can be overwritten.
#'
#' NOTE: this is experimental, warranty until the doorstep at best.
#'
#' @param ... the names of the objects to be saved (as symbols or character strings).
#' @param overwrite_vars logical, is it OK to replace/overwrite existing objects
#' with new values?
#' @param overwrite By default the function will not overwrite existing files.
#' If you really want to do so, set this to \code{TRUE.}
#' @param compress Choose the type of compression used by save(). Should be one
#' of "gzip", "bzip2", or "xz".
#' @param version The serialization format version to use. The default, 2, was
#' the default format from R 1.4.0 to 3.5.3. Version 3 became the default from
#' R 3.6.0 and can only be read by R versions 3.5.0 and higher.
#'
#' @return nothing
#' @keywords internal
save_internal_data <- function(..., overwrite_vars = FALSE, overwrite = FALSE,
                                compress = "bzip2", version = 2) {
  check_is_package("use_data()")
  objs <- get_objs_from_dots(dots(...))
  use_dependency("R", "depends", "2.10")

  usethis::use_directory("R")
  paths <- fs::path("R", "sysdata.rda")
  objs <- list(objs)

  check_files_absent(usethis::proj_path(paths), overwrite = overwrite)

  usethis::ui_done("Saving {usethis::ui_value(unlist(objs))} to {usethis::ui_value(paths)}")

  envir <- parent.frame()
  temp_env <- new.env()

  # copy the original objects from the sysdata file to this new environment
  load(usethis::proj_path(paths), envir = temp_env)
  # copy new objects to this new environment
  overwritten <- NULL
  lapply(objs[[1]], function(z) {
    bestaat_al_in_oude_file <- z %in% ls(envir = temp_env)

    if (bestaat_al_in_oude_file) {
      overwritten <<- c(overwritten, z)
    }

    if (bestaat_al_in_oude_file & !overwrite_vars) {
      msg <- paste0("An object called ", z, "already exists in sysdata.rda, please set overwrite = TRUE")
      stop(msg)
    } else {
      assign(z, get(z, envir = envir), envir = temp_env)
    }
  })

  save(list = ls(envir = temp_env), file = usethis::proj_path(paths),
       envir = temp_env, compress = compress, version = version)

    if (length(overwritten) > 0) {
    message("The following object have been overwritten: ", paste(overwritten, collapse = ", "), ".\n")
  }
  invisible()
}





#' @keywords internal
check_is_package <- function(whos_asking = NULL) {
  if (is_package()) {
    return(invisible())
  }
  message <- "Project {ui_value(project_name())} is not an R package."
  if (!is.null(whos_asking)) {
    message <- c("{usethis::ui_code(whos_asking)} is designed to work with packages.",
                 message)
  }
  usethis::ui_stop(message)
}


#' @keywords internal
get_objs_from_dots <- function(.dots) {
  if (length(.dots) == 0L) {
    usethis::ui_stop("Nothing to save.")
  }
  is_name <- vapply(.dots, is.symbol, logical(1))
  if (any(!is_name)) {
    usethis::ui_stop("Can only save existing named objects.")
  }
  objs <- vapply(.dots, as.character, character(1))
  duplicated_objs <- which(stats::setNames(duplicated(objs),
                                           objs))
  if (length(duplicated_objs) > 0L) {
    objs <- unique(objs)
    usethis::ui_warn("Saving duplicates only once: {ui_value(names(duplicated_objs))}")
  }
  objs
}


#' @keywords internal
use_dependency <- function(package, type, min_version = NULL) {
  stopifnot(rlang::is_string(package))
  stopifnot(rlang::is_string(type))
  if (package != "R" && !rlang::is_installed(package)) {
    usethis::ui_stop(c("{usethis::ui_value(package)} must be installed before you can ",
              "take a dependency on it."))
  }
  if (isTRUE(min_version)) {
    min_version <- utils::packageVersion(package)
  }
  version <- if (is.null(min_version))
    "*"
  else paste0(">= ", min_version)
  types <- c("Depends", "Imports", "Suggests",
             "Enhances", "LinkingTo")
  names(types) <- tolower(types)
  type <- types[[match.arg(tolower(type), names(types))]]
  deps <- desc::desc_get_deps(usethis::proj_get())
  existing_dep <- deps$package == package
  existing_type <- deps$type[existing_dep]
  existing_ver <- deps$version[existing_dep]
  is_linking_to <- (existing_type != "LinkingTo" & type ==
                      "LinkingTo") | (existing_type == "LinkingTo" &
                                        type != "LinkingTo")
  if (!any(existing_dep) || any(is_linking_to)) {
    usethis::ui_done("Adding {usethis::ui_value(package)} to {usethis::ui_field(type)} field in DESCRIPTION")
    desc::desc_set_dep(package, type, version = version,
                       file = usethis::proj_get())
    return(invisible())
  }
  existing_type <- setdiff(existing_type, "LinkingTo")
  delta <- sign(match(existing_type, types) - match(type, types))
  if (delta < 0) {
    usethis::ui_warn("Package {usethis::ui_value(package)} is already listed
                     in \\\n      {usethis::ui_value(existing_type)} in DESCRIPTION, no change made.")
  }
  else if (delta == 0 && !is.null(min_version)) {
    upgrade <- existing_ver == "*" || numeric_version(min_version) >
      version_spec(existing_ver)
    if (upgrade) {
      usethis::ui_done("Increasing {ui_value(package)} version to {ui_value(version)} in DESCRIPTION")
      desc::desc_set_dep(package, type, version = version,
                         file = usethis::proj_get())
    }
  }
  else if (delta > 0) {
    if (existing_type != "LinkingTo") {
      usethis::ui_done("\n        Moving {usethis::ui_value(package)}
                       from {usethis::ui_field(existing_type)} to {usethis::ui_field(type)}
                       \\\n        field in DESCRIPTION\n        ")
      desc::desc_del_dep(package, existing_type, file = usethis::proj_get())
      desc::desc_set_dep(package, type, version = version,
                         file = usethis::proj_get())
    }
  }
  invisible()
}

#' @keywords internal
check_files_absent <- function(paths, overwrite) {
  if (overwrite) {
    return()
  }
  ok <- !fs::file_exists(paths)
  if (all(ok)) {
    return()
  }
  usethis::ui_stop("\n    {usethis::ui_path(paths[!ok])} already exist.,\n
                   Use {usethis::ui_code('overwrite = TRUE')} to overwrite.\n    ")
}




#' @keywords internal
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' @keywords internal
version_spec <- function(x) {
  x <- gsub("(<=|<|>=|>|==)\\s*", "", x)
  numeric_version(x)
}


#' @keywords internal
is_package <- function(base_path = usethis::proj_get()) {
  res <- tryCatch(rprojroot::find_package_root_file(path = base_path),
                  error = function(e) NULL)
  !is.null(res)
}
