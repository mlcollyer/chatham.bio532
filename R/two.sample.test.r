#' Perform a two-sample hypothesis test for two independent samples
#'
#' Function performs a two-sample hypothesis test for two independent samples, based on
#' resampling (randomization) procedure, and estimates confidence intervals.  Parametric confidence
#' intervals are also estimatd.  
#' 
#' @param f A formula in the form y ~ x.  The left-hand side of the formula, y,  is the variable one wishes to 
#' analyze.  The right-hand side, x, is a variable to describe the different samples (a factor).  It is 
#' important that the formula is set up this way.  Do not provide two different vectors!
#' @param data The data frame from which the variables can be found.
#' @param alpha The significance level for the test (accepted proability of a type I error).
#' @param mu The hypothesized difference in population means (defaults to 0).  This is the expected difference
#' as stated in a null hypothesis.
#' @param iter Number of resampling iterations.  The oberved value counts as one iteration, so this number
#' should be the desired number of permutations - 1.  For example, if 1,000 permutations are desired, this
#' value should be 999.
#' @param seed If one should wish to define the random seed for the random permutations (for advanced users).
#' @export
#' @author Michael Collyer
#' @return The function returns a list containg means, sample sizes, variances, pooled standard error, the
#' t-value, linear model fitted values and residuals, random mean differences, random t-values, and 
#' confidence intervals for all types of alternative hypotheses.
#' @examples
#' 
#' data(lowbwt)
#' 
#' # TST = two-sample test
#' 
#' TST1 <- two.sample.test(sbp ~ sex, data = lowbwt, alpha = 0.05, mu = 0, iter = 999)
#' summary(TST1)
#' plot(TST1, method = "hist", conf.int = "2T")
#' 
#' TST2 <- two.sample.test(sbp ~ grmhem, data = lowbwt, alpha = 0.05, mu = 0, iter = 999)
#' summary(TST2)
#' plot(TST1, method = "hist", conf.int = "2T")
#' plot(TST1, method = "hist", conf.int = "NT")
two.sample.test <- function(f, data = NULL, alpha=0.05, mu = 0, iter = 999, seed = NULL){
  if(class(f) != "formula") stop("First argument must be a formula; e.g., y ~ x")
  form <- f 
  form <- update(f, ~.  + 0)
  if(is.null(data)) y <- eval(form[[2]], envir = parent.frame()) else y <- eval(form[[2]], envir = data)
  nt <- length(y)
  X <- model.matrix(form, data = data)
  if(NCOL(X) > 2) stop("This test can only be performed with two samples.  You appear to have more than two samples")
  if(NCOL(X) == 1){
    if(any(X != 0 & X != 1)) stop("The grouping variable does not appear to be a factor")
    x <- as.factor(as.vector(X))
    X <- model.matrix(~x+0)
    names(X) <- levels(x)
  }
  n1 <- nt-sum(X[,2]); n2 <- sum(X[,2])
  pb <- txtProgressBar(min = 0, max = 10, initial = 0, style=3)
  ct <- 1
  setTxtProgressBar(pb,ct)
  ind <- perm.index(length(y), iter, seed=seed)
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  yr <- lapply(1:(iter+1), function(j) y[ind[[j]]])
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  lms <- Map(function(y) lm.fit(X,y), yr)
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  means <- Map(function(y) y$coef, lms)
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  vars <- Map(function(y) lm.fit(X, y$residuals^2)$coef*c(n1/(n1-1), n2/(n2-1)), lms)
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  pooled.se <- Map(function(y) sqrt(y[1]/n1+y[2]/n2), vars)
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  mean.difs <- lapply(1:length(means), function(j) means[[j]][1] - means[[j]][2])
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  ts <- Map(function(y,s) (y-mu)/s, mean.difs, pooled.se)
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  group.n = c(n1,n2); names(group.n) <- names(means[[1]])
  random.mean.difs = unlist(mean.difs); 
  random.mean.difs[-1] <- random.mean.difs[-1]+random.mean.difs[1]
  names(random.mean.difs) <- NULL
  random.ts = unlist(ts); names(random.ts) <- NULL
  CI2 <- c(quantile(random.mean.difs, alpha/2), quantile(random.mean.difs, 1-alpha/2))
  CI.pos <- c(-Inf,quantile(random.mean.difs, 1-alpha))
  CI.neg <- c(quantile(random.mean.difs, alpha), Inf)
  out <- list( means = means[[1]], vars = vars[[1]], pooled.se = pooled.se[[1]],
               group.n = group.n, t.stat = ts[[1]], 
               residuals = lms[[1]]$residuals, fitted = lms[[1]]$fitted,
               random.mean.difs = random.mean.difs, random.ts = random.ts, 
               conf.int.two.tail = CI2, conf.int.neg = CI.neg , conf.int.pos  = CI.pos,
               X = X, y = y, alpha=alpha, mu=mu, call = match.call())
  ct <- ct +1
  setTxtProgressBar(pb,ct)
  class(out) <- "two.sample.test"
  close(pb)
  out
}