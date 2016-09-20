#---------------------------------------------------------------------------------
#
#  BIO532 R CONFIDENCE INTERVALS
#
#---------------------------------------------------------------------------------

# Let's create a hypothetical population

Y <- rnorm(n = 100000, mean = 30, sd = 3)
summary(Y)
hist(Y)

# Now let's sample from Y, a sample, y, of size 20

n = 20
y = sample(Y, size = n)
y
summary(y)
hist(y)

# Note 
mean(Y)
mean(y)

# Let's create a sampling distribution.  Recall, this is the process of sampling
# samples of size, n, from a population of N, many times

library(chatham.bio532)
?multisample

ySamples = multisample(population = Y, size = n, permutations = 10000, CLT = TRUE)
summary(ySamples)
plot(ySamples, breaks = 50, col = "yellow")

# Remember what the CLT tells us

mean(ySamples$means)
mean(Y)
sd(ySamples$means)
sd(Y)/sqrt(n)

### IMPORTANT ###

# While it is all good and fun to create a population and sample from it, it is
# a bit impractical.  In general, a population mean and standard deviation is 
# unknown to us.  This is why we are sampling.

# We want to knw how confident we can be that our sample comes close to estimating the 
# population mean.  If we cannot go back to the population over and over, how might 
# we do that?

# RESAMPLING

# Resampling is a process of using a sample as a proxy for the population, and 
# sampling, again and again, from the sample, to create a sampling distribution

# Using our sample, y

library(chatham.bio532)

yCI <- confidence.int(y)

yCI$random.samples
yCI$means

plot(yCI)
