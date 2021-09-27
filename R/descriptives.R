
#' Univariate descriptives 
#' 
#' Determine univariate descriptives
#' 
#' Determines a series of univariate statistics for the variables in a dataset. 
#' The result is printed if \code{print} is \code{TRUE}. It is also invisibly 
#' returned, so it is possible to not print the result, but only use the 
#' descriptives data.frame object for further handling.
#' 
#' Currently implemented:
#' 
#' \describe{
#' \item{column}{column number of the variable inside \code{x}}
#' \item{n_valid}{number of non-missing observations}
#' \item{n_na}{number of \code{NA}'s}
#' \item{mean}{mean}
#' \item{trimmed_mean}{trimmed mean}
#' \item{sd}{standard deviation}
#' \item{se}{standard error}
#' \item{coefvar}{ceofficient of variation: sd/mean}
#' \item{skewness}{skewness}
#' \item{kurtosis}{kurtosis}
#' \item{normal.w}{Shapiro-Wilk test of normality}
#' \item{normal.p}{p-value of Shapiro-Wilk test of normality}
#' \item{min}{minimum}
#' \item{max}{maximum}
#' \item{median}{median}
#' \item{range}{max - min}
#' \item{mad}{mean absolute deviation}
#' \item{quantiles}{the specified quantile levels}
#' \item{IQR}{interquartile range Q0.75 - Q0.25}
#' }
#' 
#' @param x A data frame, vector, or matrix
#' @param na.rm logical, \code{na.rm = FALSE} will delete the case in the 
#' computation of statistics such as \code{mean}, \code{sd}, et cetera
#' @param normal logical, if \code{TRUE} skewness and kurtosis are computed and the Shapiro-Wilk test of normality.
#' @param ranges logical, if \code{TRUE} (the default) min, max, range, 
#' mad, median are computed
#' @param trim numeric between 0 and 1 (default trim = .1) –- drops the top and 
#' bottom trim fraction in the computation of the mean
#' @param fast if TRUE, will do fewer statistics for an improvement in speed. 
#' If default \code{NULL}, will switch to fast mode for large 
#' (ncol * nrow > 10^7) 
#' problems, otherwise defaults to \code{fast = FALSE}
#' @param quantiles if not \code{NULL}, will find the specified quantiles 
#' (e.g. \code{quant = c(.25, .75)} will find the 25th and 75th percentiles)
#' @param IQR logical, if \code{TRUE} (default is \code{FALSE}) the interquartile 
#' range is computed
#' @param omit_nonnumeric logical, should non-numeric variables be dropped? 
#' Defaults to \code{TRUE} because most desciptives in this function are not 
#' meaningful for factors or characters.
#' @param complete_cases logical, should only complete cases be used? 
#' If \code{TRUE}, all rows in \code{x} that have a \code{NA} in them will be 
#' completely dropped. Only useful if this is the intended behavior in the 
#' analysis that will be run with the data. Default is \code{FALSE}.
#' @param digits numeric, the number of decimals to print
#' @param print logical, should the descriptives be printed to the console? 
#' Defaults to \code{TRUE}.
#'
#' @return optionally prints the result to the console. The resulting data.frame 
#' is invisibly returned.
#' @export
#'
#' @examples
#' descriptives(bootcamp2021::movie)
#' descriptives(bootcamp2021::movie, omit_nonnumeric = FALSE)
#' descriptives(bootcamp2021::movie, digits = 3)
#' 
#' result <- descriptives(bootcamp2021::movie, print = FALSE)
#' print(result)
#' 
#' descriptives(bootcamp2021::movie, quantiles = c(.1, .9), IQR = TRUE)
descriptives <-function (x,
                          na.rm = TRUE,
                          normal = FALSE,
                          ranges = TRUE,
                          trim = .1,
                          fast = NULL,
                          quantiles = NULL,
                          IQR = FALSE,
                          omit_nonnumeric = TRUE,
                          complete_cases = FALSE,
                          digits = 2,
                          print = TRUE) {
  
  if (is.null(dim(x))) {
    x <- as.data.frame(matrix(x, ncol = 1))
  }
  
  if (inherits(x, "tbl")) {
    class(x) <- "data.frame"
  }
  
  valid <- function(x) {sum(!is.na(x))}
  isna <- function(x) {sum(is.na(x))}
  stats <- as.data.frame(matrix(nrow = ncol(x)))
  colnames(stats) <- "weggooien"
  rownames(stats) <- colnames(x)
  stats$n_valid <- suppressWarnings(apply(x, 2, valid))
  stats$n_na <- suppressWarnings(apply(x, 2, isna))
  
  column <- 1:ncol(x)
  if (complete_cases) {
    x <- stats::na.omit(x)
    message("Incomplete cases removed")
  }   #just complete cases
  
  # fast
  if (is.null(fast)) {
    if (prod(dim(x)) > 10 ^ 7) {
      fast <- TRUE
    } else {
      fast <- FALSE
    }
  }  #the default is to use fast for large data sets
  
  if (fast) {
    normal <- FALSE
  }
  
  if (ncol(x) < 2)  { 
    if (is.data.frame(x)) {
      x <- x[, 1, drop = FALSE]
    }   # so, x is a single column data frame
  }
  
  chars <- unname(which(sapply(x, class) == "character") )
  if (length(chars) > 0) {  # there are character vectors in x
    if (omit_nonnumeric) {
      char_names <- colnames(x)[chars]  # these are the string vars
      x <-  x[, -chars]
      column <- column[-chars]
      weg_in_stats <- which(rownames(stats) %in% char_names)
      stats <- stats[-weg_in_stats, ]
      message("'x' contains character vectors (", paste(char_names, collapse = " "), ")--they have been removed")
    } else {
      x[, chars] <- suppressWarnings(as.numeric(x[, chars]))
      message("'x' contains character vectors, these yield only NA, NaN, or Inf")
      colnames(x)[chars] <- paste0(colnames(x)[chars], "**")
    }
  }
  
  facts <- unname(which(sapply(x, class) == "factor"))
  if (length(facts) > 0) { # there are factors in x
    if (omit_nonnumeric) {
      fact_names <- colnames(x)[facts]  # these are the string vars
      x <-  x[, -facts]
      column <- column[-facts]
      weg_in_stats <- which(rownames(stats) %in% fact_names)
      stats <- stats[-weg_in_stats, ]
      message("'x' contains factors (", paste(fact_names, collapse = " "), ")--they have been removed")
    } else {
      x[, facts] <- suppressWarnings(as.numeric(x[, facts]))
      message("'x' contains factors, they have been converted to numeric.\n
                Check if this is what you want.")
      colnames(x)[facts] <- paste(colnames(x)[facts], "*")
    }
  }

  if (ncol(x) == 0) {
    message("There are no variables to calculate descriptives on")
    return(invisible())
  }
  
  stats$column <- column
  stats$mean <- suppressWarnings(apply(x, 2, mean, na.rm = na.rm))
  stats$trimmed_mean <- suppressWarnings(apply(x, 2, mean, 
                                               na.rm = na.rm, trim = trim))
  stats$sd <- suppressWarnings(apply(x, 2, stats::sd, na.rm = na.rm))
  stats$se <- suppressWarnings(stats$sd/sqrt(stats$n_valid))
  stats$coefvar <- suppressWarnings(stats$sd/stats$mean)
  
  if (normal) {
    stats$skewness <- suppressWarnings(apply(x, 2, skewness, na.rm = na.rm))
    stats$kurtosis <- suppressWarnings(apply(x, 2, kurtosis, na.rm = na.rm))
    
    
    klassen <- sapply(x, class)
    lijst <- lapply(1:length(klassen), function(z) {
      if (klassen[z] == "numeric") {
        stats::shapiro.test(x[, z])
      } else {
        NA
      }
    })
    
    stats$normal.w <- sapply(lijst, function(z) {
      if (length(z) == 4) {
        unname(z$statistic)
      } else {
        NA
      }
    })
    
    stats$normal.p <- sapply(lijst, function(z) {
      if (length(z) == 4) {
        unname(z$p.value)
      } else {
        NA
      }
    })
  }
  
  if (ranges) {
    stats$min <- suppressWarnings(apply(x, 2, min, na.rm = na.rm))
    stats$max <- suppressWarnings(apply(x, 2, max, na.rm = na.rm))
    stats$range <- suppressWarnings(stats$max - stats$min)
    if (!fast) {
      stats$mad <- suppressWarnings(apply(x, 2, stats::mad, na.rm = na.rm))
      stats$median <- suppressWarnings(apply(x, 2, stats::median, na.rm = na.rm))
    }
  }

  if (!is.null(quantiles)) {
    Qnt <- apply(x, 2, stats::quantile, prob = quantiles, na.rm = TRUE)
    if (is.null(dim(Qnt))) {
      Qnt <- as.matrix(Qnt)
      colnames(Qnt) <- paste0("Q", as.list(as.list(match.call())$quantile)[[2]])
      stats <- cbind(stats, Qnt)
    } else { # bij meerdere stats is het al een matrix met rijnamen
      Qnt <- t(Qnt)
      stats <- cbind(stats, Qnt)
    }
  }
  
  if (IQR) {
    Quart <- t(apply(x, 2, stats::quantile, prob = c(.25, .75), na.rm = TRUE))
    stats$IQR <- Quart[, 2] - Quart[, 1]
  }

  all_stats_possible <- c(
    "column", "n_valid", "n_na", 
    "mean", "trimmed_mean", "sd", "se", "coefvar")
  if (normal) {all_stats_possible <- c(all_stats_possible, "skewness", "kurtosis", "normal.w", "normal.p")}
  if (ranges) {all_stats_possible <- c(all_stats_possible, "min", "max")}
  if (ranges + !fast == 2) {all_stats_possible <- 
    c(all_stats_possible, "median", "range", "mad")}
  if (!is.null(quantiles)) {all_stats_possible <- c(all_stats_possible, colnames(Qnt))}
  if (IQR) {all_stats_possible <- c(all_stats_possible, "IQR")}
  
  stats$weggooien <- NULL
  present <- which(all_stats_possible %in% colnames(stats))
  # gewenste volgorde
  stats <- stats[, all_stats_possible[present]]
  
  if(print) {
    if (length(dim(stats)) == 1) {
      class(stats) <- "list"
      attr(stats, "call") <- NULL
      print(round(stats, digits = digits))
    } else  {
      class(stats) <- "data.frame"
      print(round(stats, digits = digits))
    }
  }
  
  invisible(stats)
}
  
  


#' Kurtosis computation
#' 
#' Calculates the kurtosis of a variable
#' 
#' Calculates kurtosis coefficient for given variable, 
#' \code{matrix}, or \code{data.frame}.
#' 
#' Kurtosis is a measure of whether the data are heavy-tailed or
#' light-tailed relative to a normal distribution.
#' That is, data sets with high kurtosis tend to have heavy tails, or
#' outliers. Data sets with low kurtosis tend to have light tails, or
#' lack of outliers. A uniform distribution would be the extreme case.
#' 
#' The maximum kurtosis leads to this measure being close to 
#' n (=the number of observations). Minimal kurtosis is 0.
#' 
#' @param x a \code{variable}, \code{matrix} or a \code{data.frame}
#' @param na.rm logical, should \code{NA}s be removed before
#' computation?
#'
#' @return numeric
#' @export
#'
#' @examples
#' kurtosis(rnorm(100)) # about 0, minimal kurtosis
#' 
#' # slightly heavier tail
#' kurtosis(rt(100, 10))# approx .35
#' # quite heavy tail
#' kurtosis(rt(100, 1))# approx 84
#' 
#' # maximal kurtosis
#' kurtosis(c(rep(10, times = 1000), 101)) # 994.006
kurtosis <- function (x, na.rm = TRUE) {
  if (is.variable(x)) {
    if (na.rm) 
      x <- stats::na.omit(x)
    m <- base::mean(x)
    s <- stats::sd(x)
    n <- length(x)
    (((base::sum((x - m)^4)/n)/s^4) - 3)
  }
  else {
    if (is.matrix(x)) 
      apply(x, 2, kurtosis, na.rm = na.rm)
    else if (is.data.frame(x)) 
      sapply(x, kurtosis, na.rm = na.rm)
    else stop("unsupported type")
  }
}






#' Skewness
#' 
#' Calculates the skewness of a variable
#' 
#' Skewness is a measure of symmetry, or more precisely, the lack of symmetry. A distribution, or data set, is symmetric if it looks the same to the left and right of the center point.
#' 
#' The skewness for a normal distribution is zero, and any symmetric
#' data should have a skewness near zero. Negative values for the
#' skewness indicate data that are skewed left and positive values for
#' the skewness indicate data that are skewed right. By skewed left, we
#' mean that the left tail is long relative to the right tail. Similarly,
#' skewed right means that the right tail is long relative to the left
#' tail. If the data are multimodal, then this may affect the sign of
#' the skewness.
#' 
#' For left skewed data, you get a negative value for \code{skewness}. 
#' For right skewed data, you get a positive value for \code{skewness}. 
#'
#' @param x a \code{variable}, \code{matrix} or a \code{data.frame}
#' @param na.rm logical, should \code{NA}s be removed before
#' computation?
#'
#' @return numeric
#' @export
#'
#' @examples
#' data <- c(rep(10, 3), rep(15, 7), rep(25, 9), 
#' rep(35, 6), rep(45, 6), rep(55, 2), rep(65, 1), rep(95, 1))
#' 
#' # right-skewed data
#' hist(data)
#' skewness(data) # 1.277999
#' kurtosis(data) # 2.169775
#' 
#' # left-skewed data
#' data0 <- -data
#' hist(data0)
#' skewness(data0) # -1.277999
#' kurtosis(data0) # 2.169775
#' 
#' skewness(rnorm(100)) # approx 0
skewness <- function (x, na.rm = TRUE) {
  if (is.variable(x)) {
    if (na.rm) 
      x <- stats::na.omit(x)
    m <- base::mean(x)
    s <- stats::sd(x)
    n <- length(x)
    (base::sum((x - m)^3)/n)/s^3
  }
  else {
    if (is.matrix(x)) 
      apply(x, 2, skewness, na.rm = na.rm)
    else if (is.data.frame(x)) 
      sapply(x, skewness, na.rm = na.rm)
    else stop("unsupported type")
  }
}


# from rapportools
is.variable <- function(x) {
  if (base::missing(x)) 
    stop("test object not provided")
  is.atomic(x) & !is.null(x) & is.null(dim(x))
}

    
