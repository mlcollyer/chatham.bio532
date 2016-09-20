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
# a bit impractical.  In general, a population mean and standard deviation are 
# unknown to us.  This is why we are sampling.

# We want to know how confident we can be that our sample comes close to estimating the 
# population mean.  If we cannot go back to the population over and over, how might 
# we do that?

# RESAMPLING

# Resampling is a process of using a sample as a proxy for the population, and 
# sampling, again and again, from the sample, to create a sampling distribution

# Using our sample, y

library(chatham.bio532)

yCI = confidence.int(y)

yCI$random.samples
yCI$means

plot(yCI)

# So what is a confidence interval?  

# It is a set of limits on the probability that the "true" population mean
# is found, given your sample.  Let's say we want to be 60% confident for the 
# estimation of a population mean.  Then we are defining confidence limits
# at the edge of sampling distribution that eliminate 40% of the possible values.
# 70% confident; eliminate 30%; 80% confident; eliminate 20%, etc.

# We seek a (1-alpha)*100% confidence interval, which means we have an alpha 
# probability of missing the true population mean.  The more certain we want to be,
# the larger the interval must be

yCI = confidence.int(y, alpha = 0.20, iter = 10000)
summary(yCI)
mean(Y)
plot(yCI, col=rgb(0.5, 0.5, 0, 0.6), breaks=50)

# Note the difference between empirical and theoretical outcomes

# Let's try a bigger sample

y = sample(Y, size = 50)

yCI = confidence.int(y, alpha = 0.20, iter = 10000)
summary(yCI)
mean(Y)
plot(yCI, col=rgb(0.5, 0.7, 0.1, 0.6), breaks=50)

# surprising?  Why did things get better?

# Now think about what this means for the theory?

# (1-alpha)*100% CI = sample mean +/- t-alpha/2 * sample sd/sqrt(n)

# 95% CI the standard...

y = sample(Y, size = 50)

yCI = confidence.int(y, alpha = 0.05, iter = 10000)
summary(yCI)
mean(Y)
plot(yCI, col=rgb(0.1, 0.3, 1, 0.1), breaks=50)

# -------------------------------------------------------------------------------
# use confidence.int on your own from here

?confidence.int