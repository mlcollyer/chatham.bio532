#' Perform Analysis of Variance for a linear model
#'
#' Function performs analysis of variance using sequential sums of squares for a linear
#' model formula, as provided.
#' 
#' @param f A formula in the form y ~ x.  The left-hand side of the formula, y,  is the variable one wishes to 
#' analyze.  The right-hand side, x, is a variable to describe the different samples (a factor).  It is 
#' important that the formula is set up this way.  Do not provide two different vectors!
#' @param data The data frame from which the variables can be found.
#' @param iter Number of resampling iterations.  The oberved value counts as one iteration, so this number
#' should be the desired number of permutations - 1.  For example, if 1,000 permutations are desired, this
#' value should be 999.
#' @param seed If one should wish to define the random seed for the random permutations (for advanced users).
#' @export
#' @author Michael Collyer
#' @return The function returns a list containg an ANOVA table, SS, df, coefficients of
#' determination, F values, and other goodies for advanced users.
#' @examples
#' data(cad)
#' cad$center <- as.factor(cad$center) # change numeric variable to factor
#' 
#' ANOVA1 <- ANOVA(age ~ center, data = cad, iter = 999)
#' summary(ANOVA1)
#' plot(ANOVA1, method = "hist")
#' plot(ANOVA1, method = "diagnostic")
#' 

ANOVA <- function(f, data = NULL, iter = 999, seed = NULL){
  form <- as.formula(f)
  fit <- procD.lm(form, data = data, seed=seed)
  fit$call <- form
  fit$seed <- seed
  class(fit) <- "ANOVA"
  return(fit)
}