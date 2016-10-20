#' Perform non-parametric (randomization) test of mean differences for ANOVA
#'
#' Function performs all possible pairwise tests of mean differences for a class ANOVA
#' object, by calculating confidence intervals
#' 
#' @param A A class ANOVA object from a previous analysis
#' @param alpha The significance level for confidence intervals.
#' @export
#' @author Michael Collyer
#' @return The function returns a table of confidence intervals for all pairwise comparisons
#' of means.
#' @examples
#' data(cad)
#' cad$center <- as.factor(cad$center) # change numeric variable to factor
#' 
#' ANOVA1 <- ANOVA(age ~ center, data = cad, iter = 999)
#' summary(ANOVA1)
#' plot(ANOVA1, method = "hist")
#' pw <- pairwise.means.test(ANOVA1)
#' summary(pw)
#' plot(pw)

pairwise.means.test <- function(A, alpha = 0.05) {
  if(class(A) != "ANOVA") stop("Object is not class ANOVA")
  Y <- as.matrix(A$Y)
  form <- A$call
  form <- form[-2]
  form <- update(form, Y ~.) 
  dat <- A$data
  if(NCOL(Y) > 1) dat <- data.frame(Y=Y,dat[,-(1:NCOL(Y))])
  pfit <- geomorph:::procD.fit(form, data = dat)
  gp <- geomorph:::single.factor(pfit)
  if(length(gp) == 0) stop("No factor levels for which to define groups")
  means <- aggregate(pfit$Y ~ gp, FUN=mean)
  mean.names <- means[,1]
  means <- means[,-1] 
  if(is.matrix(means)) rownames(means) <- mean.names else names(means) <- mean.names
  nmeans <- NROW(means)
  pairs <- combn(nmeans, 2)
  mean.difs <- array(1:NCOL(pairs)) 
  if(!is.data.frame(means)) for(i in 1:NCOL(pairs)){
    mean.difs[i] <- means[pairs[1,i]] - means[pairs[2,i]]
    } else mean.difs <- as.vector(dist(means))
  seed <- A$seed
  iter <- A$permutations - 1
  n <- NROW(Y)
  ind <- geomorph:::perm.index(n,iter=iter, seed=seed)
  pb <- txtProgressBar(min = 0, max = 3, initial = 0, 
                       style = 3)
  step <- 1
  Yr <- lapply(1:length(ind), function(j){
    Y[ind[[j]],]
  })
  setTxtProgressBar(pb, step)
  step <- step +1
  
  mr <- lapply(1:length(ind), function(j){
    y <- Yr[[j]]
    as.matrix(coef(lm(y~gp+0)))
  })
  setTxtProgressBar(pb, step)
  step <- step +1
  
  if(!is.matrix(means)) mrd <- sapply(1:length(ind), function(j){
    mean.difs.r <- array(NCOL(pairs)) 
    for(i in 1:NCOL(pairs)){
      m <- mr[[j]]
      mean.difs.r[i] <- m[pairs[1,i]] - m[pairs[2,i]]
    }
    mean.difs.r
  }) else mrd <- sapply(1:length(ind), function(j){
    m <- mr[[j]]
    as.vector(dist(m))
  })
  setTxtProgressBar(pb, step)
  step <- step +1
  close(pb)
  
  lc <- function(x) quantile(x, alpha/2)
  uc <- function(x) quantile(x, 1-alpha/2)
  
  lcl <- apply(mrd, 1, lc); ucl <- apply(mrd, 1, uc)  
  lcl <- lcl+mean.difs; ucl <- ucl+mean.difs
  tab.out <- data.frame(mean.difs = mean.difs, lcl = lcl, ucl = ucl)
  
  out.names <- array(NROW(tab.out))
  for(i in 1:ncol(pairs)) out.names[i] <- paste(levels(gp)[pairs[1,i]], 
                                             levels(gp)[pairs[2,i]],
                                             sep = "-")
  rownames(tab.out) <- out.names
  out <- list(table = tab.out,
              alpha = alpha,
              variable = as.character(A$call[[2]])
              )
  class(out) <- "pairwise.means.test"
  out
}