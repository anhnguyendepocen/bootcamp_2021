
#' Partial correlation matrix
#' 
#' Determine a partial correlation matrix
#' 
#' \code{partial_corr} computes the pairwise *partial correlation* coefficients 
#' from either a correlation matrix or a data matrix.
#' 
#' The approach is to find the complete partial correlation matrix 
#' (that is, partial all the other variables out of each variable). 
#' This may be done by simply specifying the raw data or correlation matrix. 
#' (In the case of raw data, correlations will be found according to \code{use} 
#' and \code{method}.) In this case, just specify the data matrix.
#' This is useful in the case of multiple regression. 
#'
#' @param x correlation matrix or dataset in the form of a \code{matrix} or \code{data.frame}
#' @param use an optional character string giving a method for computing 
#' covariances in the presence of missing values. This must be (an abbreviation of) 
#' one of the strings \code{"everything"}, \code{"all.obs"}, \code{"complete.obs"}, 
#' \code{"na.or.complete"}, or \code{"pairwise.complete.obs"}
#' @param method a character string indicating which correlation coefficient 
#' (or covariance) is to be computed. One of \code{"pearson"} (default), 
#' \code{"kendall"}, or \code{"spearman"}: can be abbreviated 
#'
#' @return the matrix or partial correlations
#' @export
#'
#' @examples
#' partial_corr(bootcamp2021::movie[, -3])
#' partial_corr(bootcamp2021::centrality)
partial_corr <- function(x, use = "everything", 
                         method = c("pearson", "kendall", "spearman")) {
  if (inherits(x, "data.frame") + inherits(x, "matrix") == 0) {
    stop("'x' should be a matrix or a data.frame")
  }
  
  # check if x might be a correlation matrix
  appears_cor <- FALSE
  if (NROW(x) == NCOL(x)) {
    if (is.data.frame(x)) {
      if (isSymmetric(unclass(unname(as.matrix(x))))) {
        value <- TRUE
      }
    }
    else {
      if (isSymmetric(unclass(unname(x)))) {
        appears_cor <- TRUE
      }
    }
  }
  appears_cor <- appears_cor && isTRUE(all.equal(prod(diag(as.matrix(x))), 1))
  appears_cor <- appears_cor && isTRUE((min(x, na.rm = TRUE) >= -1) & 
                                         (max(x, na.rm = TRUE) <= 1))
  
  if (!appears_cor) { # the correlation needs to be created still
    res <- tryCatch(stats::cor(x = x, use = use, method = method), 
                    silent = TRUE, error = function(e) e)
    # check if it worked
    if (inherits(res, "error") | inherits(res, "simpleError")) {
      klassen <- sapply(x, class)
      klasse_factor <- colnames(x)[which(klassen == "factor")]
      klasse_karakter <- colnames(x)[which(klassen == "character")]
      cat("\nError message:", res$message, "\n\n")
      if (length(klasse_factor) > 0) {
        cat("The following variables are factors: ", klasse_factor, "\n\n")
      }
      if (length(klasse_karakter) > 0) {
        cat("The following variables are strings: ", klasse_karakter, "\n\n")
      }
      if ((length(klasse_factor) > 0) + (length(klasse_karakter) > 0) > 0) {
        cat(">>> Please remove these variables from 'x'\n\n")
      }
      return(invisible(NULL))
    }
  } else { # x is already a correlation matrix
    res <-  x
  }
  
  partial_from_cor(res)
}






partial_from_cor <- function(x) {
  # check if x looks like a corr matrix
  appears_cor <- FALSE
  if (NROW(x) == NCOL(x)) {
    if (is.data.frame(x)) {
      if (isSymmetric(unclass(unname(as.matrix(x))))) {
        value <- TRUE
      }
    } else {
      if (isSymmetric(unclass(unname(x)))) {
        appears_cor <- TRUE
      }
    }
  }
  appears_cor <- appears_cor && isTRUE(all.equal(prod(diag(as.matrix(x))), 1))
  appears_cor <- appears_cor && isTRUE((min(x, na.rm = TRUE) >= -1) & 
                                         (max(x, na.rm = TRUE) <= 1))
  if (!appears_cor) stop("Please provide an accurate correlation matrix")
  
  # determine pseudo inverse
  msvd = svd(x)
  if (length(msvd$d) == 0) {
    m <- array(0, dim(m)[2:1])
  } else {
    m <- msvd$v %*% (1/msvd$d * t(msvd$u))
  }
  
  # invert, then negate off-diagonal entries
  m = -m
  diag(m) = -diag(m)
  
  # standardize and return 
  m <- stats::cov2cor(m)
  rownames(m) <- rownames(x)
  colnames(m) <- colnames(x)
  m
}

