#' Coefficient plot for lm object
#' 
#' Draw a simple coefficient plot for an lm object
#' 
#' The function takes the output from \code{lm} and draws a plot 
#' with the parameter estimates and their surrounding confidence 
#' intervals.
#' 
#' A dataframe containing the confidence intervals is invisibly returned.
#' 
#' 
#' @param lm_object output object from \code{lm}
#' @param CI_level confidence level
#' @param two_sided logical, a single-sided result is returned when \code{FALSE}
#' @param intercept logical, should the intercept be drawn too?
#' @param cex.var fontsize of the variable names
#' @param cex.pts size of data points
#' @param col.pts color of points and segments, default is black
#' @param pch.pts symbol of points, default is solid dot
#' @param var.las orientation of variable names against the axis, default is 2. see the usage of las in par
#' @param main The main title (on top) using font and size (character expansion) par("font.main") and color par("col.main")
#' @param xlab X axis label using font and character expansion par("font.lab") and color par("col.lab")
#' @param ylab Y axis label, same font attributes as xlab
#' @param mar numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(1, 8, 5.1, 2).
#' @param offset add extra spaces to separate from the existing dots
#' @param ... further arguments passed to the base R plot method
#'
#' @return Plot of the coefficients from a lm fit. Invisibly returns the 
#' accompanying confidence interval.
#' @export
#'
#' @examples
#' y1 <- rnorm(1000,50,23)
#' y2 <- rbinom(1000, 1, prob = 0.72)
#' x1 <- rnorm(1000, 50, 2)
#' x2 <- rbinom(1000, 1, prob = 0.63)
#' x3 <- rpois(1000, 2)
#' x4 <- runif(1000, 40, 100)
#' x5 <- rbeta(1000, 2, 2)
#'  
#' longnames <- c("a long name01", "a long name02",
#' "a long name03", "a long name04", "a long name05")
#'  
#' fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5)
#' coefplot_lm(fit1)
#' coefplot_lm(fit1, intercept = TRUE)
coefplot_lm <- function (lm_object,
                         CI_level = .95,
                         two_sided = TRUE,
                         intercept = FALSE,
                         cex.var = 0.8,
                         cex.pts = 0.9,
                         col.pts = 1,
                         pch.pts = 20,
                         var.las = 2,
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         mar = c(1, 8, 5.1, 2),
                         offset = 0.1,
                         ...) {
  if (!inherits(lm_object, "lm")) {
    stop("You need to provide a 'lm' object to be plotted")
  }
  
  if (two_sided) {
    CI = round(stats::qt(1 - (1 - CI_level) / 2, 9999999), digits = 2)
  } else {
    CI = round(stats::qt(1 - (1 - CI_level), 9999999), digits = 2)
  }
  
  coefs <- summary(lm_object)$coef[, 1]
  sds <- summary(lm_object)$coef[, 2]
  varnames <- names(coefs)
  
  chk.int <- attr(stats::terms(lm_object), "intercep")
  if (chk.int &
      intercept | !chk.int & intercept | !chk.int & !intercept) {
    intercept <- TRUE
    coefs <- coefs
    sds <- sds
    varnames <- varnames
  } else if (chk.int & !intercept) {
    coefs <- coefs[-1]
    sds <- sds[-1]
    varnames <- varnames[-1]
  }
  
  coefs <- unlist(coefs)
  n.x <- length(coefs)
  idx <- seq(1, n.x)
  
  coefs.h <- coefs + CI * sds
  coefs.l <- coefs - CI * sds
  
  old.par <- graphics::par(no.readonly = TRUE)
  min.mar <- graphics::par("mar")
  if (is.null(main)) {
    main <- "Regression Estimates"
  }
  if (is.null(xlab)) {
    xlab <- ""
  }
  if (is.null(ylab)) {
    ylab <- ""
  }
  graphics::par(mar = mar)
  maxchar <- max(sapply(varnames, nchar))
  
  k <- 1 / n.x
  mar[2] <- max(min.mar[2], trunc(mar[2] + maxchar / 10)) + 0.1
  graphics::par(mar = mar)
  
  
  
    plot(
      c(.9*coefs.l, 1.1*coefs.h),  # add a bit space to left and right
      c(idx + k, idx - k),
      type = "n",
      axes = F,
      main = main,
      xlab = xlab,
      ylab = ylab,
      ...
    )

      graphics::axis(3)  # axis on top
      graphics::axis(2,  # left axis
        n.x:1,
        varnames[n.x:1],
        las = var.las,
        tck = FALSE,
        lty = 0,
        cex.axis = cex.var
      )

    graphics::abline(v = 0, lty = 2)
    graphics::points(coefs,
           idx,
           pch = pch.pts,
           cex = cex.pts,
           col = col.pts)
      graphics::segments(coefs.l,
               idx,
               coefs.h,
               idx,
               lwd = 1,
               col = col.pts)
    
    ci <- data.frame(lower_bound = coefs.l, upper_bound = coefs.h)
    invisible(ci)
}
