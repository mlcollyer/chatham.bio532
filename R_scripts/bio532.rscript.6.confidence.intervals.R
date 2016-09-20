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

sample(y, replace = TRUE)

# The replace = TRUE part is important.  If FALSE, the process just shuffles the data; 
# if TRUE, it creates a new hypothetical sample from the data

# Let's do this many times (don't worry about code too much here)

perms = 1000
resample = function(.) sample(y, size=n, replace = TRUE)
y.r = lapply(1:perms, resample)

y.r # 1000 resamples of y



# Let's look at the interquartile range of the sampling distribution of means

IQR(ySamples$means)
summary(ySamples$means)

# How do we intrpret this information?  We know that 50% of the values fall within the IQR,
# but does that tell us anything?

# YES!

# Because the sampling distribution is also a probability distribution, it gives us a probability
# of capturing the true population mean from our sampling.  We should be ~50% confident
# that the true population mean falls within this range.

mean(Y) # population mean


ySamples$samples

# Can plot too
plot(ySamples)

# A better example... yes, 10,000 permutations!

ySamples = multisample(population = Y, size = n, permutations = 10000, CLT=TRUE)
summary(ySamples)
plot(ySamples, breaks = 50, col="dark green")

# Let's do this again with a different kind of distribution

Y = rpois(10000, lambda =2) # Poisson distribution
hist(Y, col="orange") # not normal!

ySamples = multisample(population = Y, size = n, permutations = 10000, CLT=TRUE)
summary(ySamples)
plot(ySamples, breaks = 50, col="dark orange")

# Let's go crazy!
ySamples = multisample(population = Y, size = n, permutations = 50000, CLT=TRUE)
summary(ySamples)
plot(ySamples, breaks = 50, col="dark orange")

# Remember, a Poisson distribution is discrete, not continuous
# Nevertheless, the CLT still holds, pretty much

# Another

Y = rlnorm(10000, mean = 2, sd = 0.5) # Lognormal distribution
hist(Y, col="yellow") # not normal!

ySamples = multisample(population = Y, size = n, permutations = 10000, CLT=TRUE)
summary(ySamples)
plot(ySamples, breaks = 50, col=" yellow")

#-----------------------------------------------------------------------------------
# Parting thoughts

# 1) CLT important!  Repat, CLT important!
# 2) Theory tells us what to expect if we were to sample a population many times
# 3) Therefore, we can assume som ethings about statistics we estimate for population 
# apramters.  This will be important when we wish to comapre population parameters
# in inferrential tests.