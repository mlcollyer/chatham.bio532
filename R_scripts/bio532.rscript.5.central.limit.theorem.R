#---------------------------------------------------------------------------------
#
#  BIO532 R CENTRAL LIMIT THEOREM
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

# This is not far-fetched.  Could a researcher really sample 100,000 subjects in a real
# population?  Probably not without extreme work!  But is a sample of 20 legitimate?
# What if we repeated this many times? 

library(chatham.bio532)
?multisample

# A simple example
ySamples = multisample(population = Y, size = n, permutations = 10)
summary(ySamples)

# Make sure to use CLT
ySamples = multisample(population = Y, size = n, permutations = 10, CLT=TRUE)
summary(ySamples)

attributes(ySamples) # shows all the parts

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