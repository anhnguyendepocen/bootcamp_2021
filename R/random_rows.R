
#' Print random rows
#' 
#' Print rows from a matrix or data.frame
#' 
#' Useful for inspection of a dataset. Will print randomly drawn 
#' rows from a \code{matrix} or \code{data.frame}.
#' 
#' The rows are printed in the order or appearance in \code{x}.
#'
#' @param x the \code{matrix} or \code{data.frame}
#' @param nobs the number of rows (observations) that should be printed.
#'
#' @return nothing, output is printed
#' @export
#'
#' @examples
#' random_rows(bootcamp2021::movie)
random_rows <- function(x, nobs = 10) {
  
  if (is.data.frame(x) + is.matrix(x) != 1) {
    stop("'x' should be a data.frame or a matrix")
  }
  
  len <- nrow(x)
  
  if (nobs > len) {
    message("'nobs' is larger than the number of 
            rows in 'x', 'x' will be printed entirely.")
    return(x)
  } else {
    to_show <- sample(1:len, size = nobs, replace = FALSE) |> 
      sort()
    return(x[to_show, ])
  }
}
