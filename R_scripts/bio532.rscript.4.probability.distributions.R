#---------------------------------------------------------------------------------
#
#  BIO532 R PROBABILITY DISTRIBUTIONS
#
#---------------------------------------------------------------------------------

# Binomial distribution (DISCRETE PROBABILITY DISTRIBUTION)

# See Q 11, on p. 192 of textbook
# See p. 168 for binomial probability distribution definitions

n = 10 # sample size

# 11.a. How many possible ways to order 10 subjects in the sample?

# n factorial
factorial(10)

# 11.b. How many possible ways to select 4 subjects from 10?

# n choose x
x = 4
? choose
# note that R uses n choose k as terminology
choose(n=n, k=x)

# 11.c. What is the probability of choosing exactly 3 LH people of 10 sampled?

x = 3

# solution = n choose x times p^x times (1-p)^(n-x)

p = 0.098 # this is the "true" probability and events are assumed independent

# Thus,

choose(n=n, k=x)*p^x*(1-p)^(n-x)

# or one can use pbinom

? pbinom

# IMPORTANT.  This is a cumulative mass function (CMF) !!!!

pbinom(q = x, size = n, prob = p)

# This is the probability of getting 0, 1, 2, or 3, not exactly 3

# Let's do this again, this time without too many coefficients

pbinom(q = 3, size = 10, prob = 0.098) # probability of 0-3 LH in 10
pbinom(q = 2, size = 10, prob = 0.098) # probability of 0-2 LH in 10

# The probability of exactly 3 LH sampled in a sample of 10 is the difference

pbinom(q = 3, size = 10, prob = 0.098) - pbinom(q = 2, size = 10, prob = 0.098)

# which is the same as

choose(n=10, k=3)*p^3*(1-p)^(10-3)

# Using pbinom might not seem worth it, but it is great in some circumstances,
# For example, 11.d.  Probability of at least 6 LH sampled in sample of 10.

# First, recognize that this is the same as Pr(X = 6|10) + Pr(X = 7|10) + ... Pr(X = 10|10)
# Second, recognize that means calculating choose(n=n, k=x)*p^x*(1-p)^(n-x) 5 times
# and adding the probabilities.
# Third, recognize this could also be done as 
# 1 = [Pr(X = 0|10) + Pr(X = 2|10) + ... Pr(X = 5|10)]
# Still a lot of calculations...

# Easiest way

Pr.to.5 = pbinom(q=5, size = 10, p = 0.098)
Pr.to.5
1 - Pr.to.5 # There's the answer!

# 11.e.  Proability of at most 2 LH in sample of 10?

pbinom(q=2, size=10, p = 0.098)

#----------------------------------------------------------------------------------

# Normal distribution (CONTINUOUS PROBABILITY DISTRIBUTION)
# Just for fun, let's redo the previous problem (c-e) but scale everything by 10
# I.e., a sample of 100 subjects

# In simplest order...

# 11.e. Pr(X <= 20|100)

# There is no way anyone wants to do 20 probability calculations and add them up

# but cumulative functions help

pbinom(q=20, size=100, p = 0.098)

# Another way to go about this is to use the normal approximation
# Always add or subtract 0.5 if the values are really discrete, to get the 
# edge of the interval

# mu = n*p; sigma = sqrt(n*p*(1-0))

mu = 100*p
sigma = sqrt(100*p*(1-p))

?pnorm

pnorm(q = 20+0.5, mean = mu, sd = sigma)

# WHEN YOU ARE USING R, IT DOS NOT MATTER WHICH METHOD YOU USE, BUT ASK YOURSELF 
# IF I HAD TO DO THIS BY HAND, SAY ON AN EXAM, WHICH METHOD WOULD I USE?
# OR, WHAT IF I WAS ASKED HOW WELL THE NORMAL DISTRIBUTION APPROXIMATES THE BINOMIAL 
# DISTRIBUTION?  CAN I ANSWER THAT?

# ---------------------------------------------------------------------------------

# Distribution demonstrations (you would never need to reporduce something like this)

n = 100
X = seq(0,n,1) # all possible x values
p = 0.098

? dbinom # this is how one find probabilities using a Mass or Density function

Pr <- dbinom(x=X, size = n, prob = p)
Pr # 101 probabilities!

plot(X, Pr, type = "h", col = "dark blue")

# Let's approximate it with the normal distribution

? dnorm
mu <- n*p
sigma = sqrt(n*p*(1-p))

Pr.a <- dnorm(x=X, mean = mu, sd = sigma)
points(X, Pr.a, type="l", col="dark red") # add line to existing plot

# Not perfect, but pretty close!

# --------------------------------------------------------------------------------

# Parting thoughts

# 1) Probability with discrete probability distributions means using the
# addition rule and complimentary rule quite a bit.  This is done for the discrete
# states where probability is estimate (see blue columns in plot)
# 2) Probability with continuous probability distributions means finding the area
# under the curve (AUC).  The portion of area up to a quantile is proprtional
# to probability, if the AUC = 1.
# 3) For any normal distribution, the distribution can be "standardized".  This
# is done by mean-centering and dividing by the standard deviation to produce
# z-scores.  The new distribution has mean of 0, standard deviation of 1, and AUC=1.
# ALWAYS!

