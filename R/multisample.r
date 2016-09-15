#' Generate multiple samples
#'
#' Function samples many times from the same population.
#' 
#' @param population A vector of data for the whole population
#' @param size The size of the sample
#' @param permutations The number of times to sample from the population
#' @param CLT A logical value to indicate if summary statistics should be generated, 
#' # based on the central limit theorem.
#' @export
#' @author Michael Collyer
#' @return The function returns a list of samples.  If CLT = TRUE, it also returns summary
#' statistics from the samples, plus expected values based on the central limit theorem.
#' @examples
#' 
#' Y <- rnorm(10000)
#' mySamples <- multisample(population = Y, size = 50, permutations = 100, CLT = FALSE)
#' summary(mySamples)
#' mySamples$samples[1:3]
#' 
#' mySamples <- multisample(population = Y, size = 50, permutations = 100, CLT = TRUE)
#' summary(mySamples)
#' plot(mySamples)

#' 
multisample <- function(population, size, permutations = 1000, CLT = FALSE){
  n <- size; p <- permutations
  res <- vector("list", p)
  if(is.matrix(population)) {
    mat.samp <- function(.) population[sample(ind, n),]
    ind <- 1:NROW(population)
    res <- lapply(res, mat.samp)
  } else {
    population <- as.vector(population)
    vec.samp <- function(.) population[sample(ind,n)]
    ind <- 1:length(population)
    res <- lapply(res, vec.samp)
  }
  means <- sd.means <- mu <- sd.pop <- expected.means.sd <- NULL
  sdn <- function(x){
    x <- scale(x, scale=FALSE)
    sqrt(sum(x^2)/length(x))
  }
  if(CLT){
    if(is.matrix(population)){
      means <- sapply(res, colMeans)
      sd.means <- apply(means,1,sdn)
      mu <- apply(population, 2, mean)
      sd.pop <- apply(population, 2, sdn)
      expected.means.sd <- sd.pop/sqrt(n)
    } else {
      means <- sapply(res, mean)
      sd.means <- sdn(means)
      mu <- mean(population)
      sd.pop <- sdn(population)
      expected.means.sd <- sd.pop/sqrt(n)
    }

  }
  out <- list(samples = res, means = means, sd.means = sd.means, 
              mu=mu, sigma = sd.pop, expected.se = expected.means.sd,
              sample.size = n)
  class(out) <- "multisample"
  out
}