#' Generate confidence intervals
#'
#' Function generates one-sample confidence intervals from a resampling (bootstrap) procedure
#' 
#' @param y A vector of data
#' @param alpha The significance level
#' @param iter Number of resampling (bootstrap) iterations
#' @export
#' @author Michael Collyer
#' @return The function returns a list of samples, means from samples, and (1-alpha)*100%
#' confidence intervals.
#' @examples
#' 
#' Y <- rnorm(10000) # population
#' y <- sample(Y, size = 30) # sample from population
#' CI <- confidence.int(y, alpha = 0.05, iter=1000)
#' summary(CI)
#' plot(CI)
#' mean(Y) # does confidence interval contain population mean?

confidence.int <- function(y, alpha = 0.05, iter = 1000){
  if(!is.vector(y)) stop("Data input can only be vectors")
  if(alpha > 1) stop("alpha must be a value between 0-1")
  if(alpha < 0) stop("alpha must be a value between 0-1")
  n <- length(y)
  ind <- lapply(1:iter, function(j) sample(1:n, replace = TRUE))
  ind[[1]] <- 1:n
  yr <- lapply(1:iter, function(j) {k <- ind[[j]]; y[k]})
  means <- sapply(yr, mean)
  alpha2 <- alpha/2
  lci = alpha2; uci = 1-lci
  lcl = quantile(means, lci)
  ucl = quantile(means, uci)
  out <- list(random.samples = yr, means = means, alpha=alpha,
              iter=iter, n = length(y), conf.int = c(lcl,ucl))
  class(out) <- "confidence.int"
  out
}