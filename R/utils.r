## S3 GENERIC FUNCTIONS

## multisample

#' Print/Summary Function for chantahm.bio532
#' 
#' @param x print/summary object (from \code{\link{multisample}})
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
#' @param object print/summary object (from \code{\link{multisample}})
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
#' @param x plot object (from \code{\link{multisample}})
#' @param ... other arguments passed to plot (see hist)
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

## confidence.int

#' Print/Summary Function for chantahm.bio532
#' 
#' @param x print/summary object (from \code{\link{confidence.int}})
#' @param ... other arguments passed to print/summary
#' @export
#' @author Michael Collyer
#' @keywords utilities
print.confidence.int <- function (x, ...) {
  cat("\n\n")
  cat("Confidence interval based on bootstrap resampling procedure")
  cat("\n\n")
  cat(paste(x$iter, "Permutations used"))
  cat("\n\n")
  print(x$conf.int)
  cat("\n\n\n")
  cat("Confidence interval based on theoretical expectation (t-distibution)")
  y <- x$random.samples[[1]]
  s <- sd(y); ym <- mean(y); n <- x$n; alpha <- x$alpha; tc <- qt(1-alpha/2, n-1)
  lcl <- ym - tc*s/sqrt(n); ucl <- ym + tc*s/sqrt(n)
  CI <- c(lcl, ucl); names(CI) <- names(x$conf.int)
  cat("\n\n")
  print(CI)
  cat("\n\n")
  invisible(x)
}

#' Print/Summary Function for chatham.bio532
#' 
#' @param object print/summary object (from \code{\link{confidence.int}})
#' @param ... other arguments passed to print/summary
#' @export
#' @author Michael Collyer
#' @keywords utilities
summary.confidence.int <- function(object, ...) {
  x <- object
  print.confidence.int(x, ...)
}

#' Plot Function for chatham.bio532
#' 
#' @param x plot object (from \code{\link{confidence.int}})
#' @param ... other arguments passed to plot (see hist)
#' @export
#' @author Michael Collyer
#' @keywords utilities
#' @keywords visualization
plot.confidence.int <- function(x, ...){
  dots <- list(...)
  col <- dots$col
  breaks <- dots$breaks
  if(is.null(breaks)) breaks <- "Sturges"
  X <- x$means
  n <- x$n; alpha <- x$alpha
  par(mfcol=c(1,2))
  hist(X, main = "Sampling Distribution of means", breaks=breaks, 
       col=col, xlab = "Means", freq=TRUE)
  CI <- x$conf.int
  abline(v=CI[1], col="dark blue", lwd = 2,lty=3)
  abline(v=CI[2], col="dark blue", lwd = 2,lty=3)
  Z <- scale(X)
  hist(scale(X), main = "Standardized Distribution of means", breaks=breaks, 
       col=col, xlab = "t-value", freq=FALSE)
  abline(v=qt(alpha/2, n-1), col="dark blue", lwd = 2,lty=3); 
  abline(v=qt(1-alpha/2, n-1), col="dark blue", lwd = 2, lty=3)
  xx <- density(Z)$x
  d <- dt(xx, x$n-1)
  points(xx,d, type="l", col="red", lwd=2)
  cat("The red curve is based on the t-distribution")
}

## two.sample.test

#' Print/Summary Function for chantahm.bio532
#' 
#' @param x print/summary object (from \code{\link{two.sample.test}})
#' @param ... other arguments passed to print/summary
#' @export
#' @author Michael Collyer
#' @keywords utilities
print.two.sample.test <- function (x, ...) {
  mu <- x$mu
  alpha <- x$alpha
  s <- x$pooled.se
  vars <- x$vars
  ns <- x$group.n
  vd <- sum(vars^2/ns^2*(1/(ns-1)))
  v <- sum(vars/ns)^2/vd
  tc1 <- qt(1-alpha, v)
  tc2 <- qt(1-alpha/2, v)
  md <- x$means[1] - x$means[2]
  cat("\nCall:\n")
  cat(deparse(x$call), fill=TRUE, "\n")
  cat("\nTwo-sample test: comaprison of sample means\n")
  cat(paste("Hypothesized difference in means:", mu, "\n"))
  cat("Based on full randomization of values between samples (for P-values)\n\n")
  cat("Sample sizes:\n")
  print(x$group.n)
  cat("\nSample means:\n")
  print(x$means)
  cat("\nSample standard deviations:\n")
  print(sqrt(x$vars))
  cat("\nPooled standard error (using Welch method):\n")
  cat(x$pooled.se)
  cat("\n\nEffect size (aka t-statistic):\n")
  cat(x$t.stat)
  cat("\n\nP-value, less than or equal to observed: ")
  p <- rank(x$random.ts)[1]/length(x$random.ts)
  cat(p)
  cat("\n\nP-value, greater than or equal to observed: ")
  p <- as.vector(1-(rank(x$random.ts)[1]-1)/length(x$random.ts))
  cat(p)
  cat("\n\nP-value, two-tailed: ")
  p <- as.vector(1-(rank(abs(x$random.ts))[1]-1)/length(x$random.ts))
  cat(p)
  cat("\n\n")
  cat("------------------------------------------------------------------------------\n\n")
  cat("\nConfidence Intervals, based on resampling experiment\n\n")
  cat(paste((1-alpha)*100, "% Confidence interval, less than or equal to observed:\n"))  
  print(x$conf.int.neg)
  cat("\n")
  cat(paste((1-alpha)*100, "% Confidence interval, greater than or equal to observed:\n"))
  print(x$conf.int.pos)
  cat("\n")
  cat(paste((1-alpha)*100, "% Confidence interval, two-tailed:\n"))
  print(x$conf.int.two.tail)
  cat("\n")
  cat("Confidence Intervals, based on t-distribution (Welch method)\n\n")
  cat(paste(round(v,3), "degrees of freedom\n\n"))
  cat(paste((1-alpha)*100, "% Confidence interval, less than or equal to observed:\n"))  
  CI.neg <- c(md - tc1*s, Inf)
  names(CI.neg) <- c(paste(alpha*100, "%"), "")
  print(CI.neg)
  cat("\n")
  cat(paste((1-alpha)*100, "% Confidence interval, greater than or equal to observed:\n"))
  CI.pos <- c(-Inf, md + tc1*s)
  names(CI.pos) <- c("", paste((1-alpha)*100, "%"))
  print(CI.pos)
  cat("\n")
  cat(paste((1-alpha)*100, "% Confidence interval, two-tailed:\n"))
  CI2 <- c(md - tc2*s, md + tc2*s)
  names(CI2) <- c(paste((alpha/2)*100,"%"), paste((1-alpha/2)*100,"%"))
  print(CI2)
  
  invisible(x)
}

#' Print/Summary Function for chatham.bio532
#' 
#' @param object print/summary object (from \code{\link{two.sample.test}})
#' @param ... other arguments passed to print/summary
#' @export
#' @author Michael Collyer
#' @keywords utilities
summary.two.sample.test <- function(object,...) print.two.sample.test(object,...)

#' Plot Function for chatham.bio532
#' 
#' @param x plot object (from \code{\link{two.sample.test}})
#' @param method Whether to plot histograms of test statistic distributions or linear model diagnostics
#' @param conf.int If histogram is chosen for method, the choice for confidence interval display.  2T produces a 
#' two-tailed interpretation of a confidence interval; PT produces a positive limit (if the alternative
#' hypothesis is expected to be "greater than"); NT produces a negative limit (if the alternative
#' hypothesis is expected to be "less than").  The confidence intervals are automatically found from the alpha 
#' argument of the test that was performed.
#' @param ... other arguments passed to plot (see hist)
#' @export
#' @author Michael Collyer
#' @keywords utilities
#' @keywords visualization
plot.two.sample.test <- function(x, method = c("histogram", "diagnostic"),
                                 conf.int = c("2T", "PT", "NT"), ...){
  method <- match.arg(method)
  conf.int <- match.arg(conf.int)
  if(method == "diagnostic") plot(lm(x$y~x$X-1)) else
  {
    mu <- x$mu
    alpha <- x$alpha
    s <- x$pooled.se
    vars <- x$vars
    ns <- x$group.n
    vd <- sum(vars^2/ns^2*(1/(ns-1)))
    df <- sum(vars/ns)^2/vd
    tc1 <- qt(1-alpha, df)
    tc2 <- qt(1-alpha/2, df)
    md <- x$means[1] - x$means[2]
    y <- x$random.mean.difs
    ts <- x$random.ts
    iter <- length(y)
    if(conf.int == "NT") {
      lc <- 1-alpha
      uc <- 1
      ucl <- -Inf
      lcl <- quantile(y, lc)
    } else if(conf.int == "PT") {
      lc <- alpha
      uc <- 0
      ucl <- Inf
      lcl <- quantile(y, lc)
    } else {
      lc <- alpha/2
      uc <- 1 - alpha/2
      lcl <- quantile(y, lc)
      ucl <- quantile(y, uc)
    }
    dots <- list(...)
    cols <- dots$col
    breaks <- dots$breaks
    if(is.null(breaks) & iter > 500)  breaks <- 50 
    if(is.null(cols)) cols <- rgb(0,1,0,0.5)
    par(mfcol=c(1,2))
    hist(y, breaks=breaks, col=cols, main=expression(paste(mu," marked by blue line")),
         xlab = "Random mean differences (null model)", cex.main = 0.7)
    abline(v = mu, lwd=3, col="dark blue")
    abline(v = lcl, lwd=2, lty=3, col="red")
    abline(v = ucl, lwd=2, lty=3, col="red")
    hist(ts, breaks=breaks, col=cols, main="observed t-stat marked by orange line", 
         xlab = "Random t-values (null model)", freq=F, cex.main = 0.7)
    xx = seq(round(min(ts),2), round(max(ts),2),0.01)
    tx = dt(xx, df)
    points(xx, tx, type="l", col="dark blue", lwd=3)
    if(conf.int == "NT") {
      lcp <- 0; ucp <- alpha
    } else if(conf.int == "PT") {
      lcp <- 1-alpha ; ucp <- 1
    } else {
      lcp <- alpha/2; ucp <- 1-alpha/2
    }
    lclp <- qt(lcp, v)
    uclp <- qt(ucp, v)
    abline(v = lclp, lwd=2, lty=3, col="red")
    abline(v = uclp, lwd=2, lty=3, col="red")
    abline(v = ts[1], lwd=3, col="dark orange")
  }
}
