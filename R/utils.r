## S3 GENERIC FUNCTIONS

## multisample

#' Print/Summary Function for chantahm.bio532
#' 
#' @param x print/summary object (from \code{\link{gpagen}})
#' @param ... other arguments passed to print/summary
#' @export
#' @author Michael Collyer
#' @keywords utilities
print.multisample <- function (x, ...) {
  if(is.null(x$means)) CLT <- FALSE else CLT <- TRUE
  N <- length(x$samples)
  p <- NCOL(x$samples[[1]])
  cat("\n\n")
  cat(paste("A population with", p, "variable(s) was sampled", N, "times"))
  cat("\n\n")
  cat(paste("Each sample had", x$sample.size, "subjects"))
 
  if(CLT){
    if(p == 1) {
      cat("\n\n")
      cat(paste("The mean of the population is", x$mu))
      cat("\n\n")
      cat(paste("The standard deviation of the population is", x$sigma))
      cat("\n\n")
      cat(paste("The expected SE is", x$expected.se))
      cat("\n\n")
      cat(paste("The mean of the sampling distribution of means is", 
                mean(x$means)))
      cat("\n\n")
      cat(paste("The sd of the sampling distribution of means is", x$sd.means))
    } else {
      cat("\n\n")
      cat("The means of the population variables are\n\n")
      print(x$mu)
      cat("\n\n")
      cat("The standard deviations of the population variables are\n\n")
      print(x$sigma)
      cat("\n\n")
      cat("The expected SE for each variable are\n\n")
      print(x$expected.se)
      cat("\n\n")
      cat("The variable means of the sampling distribution are\n\n")
      print(apply(x$means, 1, mean))
      cat("\n\n")
      cat("The variable standard deviations of the sampling distribution are\n\n")
      print(x$sd.means)
    }
  }
  invisible(x)
}

#' Print/Summary Function for chatham.bio532
#' 
#' @param object print/summary object (from \code{\link{gpagen}})
#' @param ... other arguments passed to print/summary
#' @export
#' @author Michael Collyer
#' @keywords utilities
summary.multisample <- function(object, ...) {
  x <- object
  print.multisample(x, ...)
}

#' Plot Function for chatham.bio532
#' 
#' @param x plot object (from \code{\link{gpagen}})
#' @param ... other arguments passed to plotAllSpecimens (see hist)
#' @export
#' @author Michael Collyer
#' @keywords utilities
#' @keywords visualization
plot.multisample <- function(x, ...){
  if(length(x$mu) > 1) stop("Plot option is only available for univarite data at this time")
  if(is.null(x$means[[1]])) stop("CLT must be true for plotting functions")
  dots <- list(...)
  col <- dots$col
  breaks <- dots$breaks
  if(is.null(breaks)) breaks <- "Sturges"
  X <- x$means
  hist(X, main = "Sampling Distribution of means", breaks=breaks, 
       col=col, xlab = "Means", freq=FALSE)
  xx <- density(X)$x
  d <- dnorm(xx, mean = x$mu, sd=x$expected.se)
  points(xx,d, type="l", col="red", lwd=2)
  cat("The red curve is based on the Central Limit Theorem, 
      and the true population mean and standard deviation")
}

