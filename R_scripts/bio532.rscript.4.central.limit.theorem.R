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
# What if we repeated this many times? (Don't worry too much about the code.)

permutations = 1000
samples <- vector("list", permutations)
samples

# fill this list

lapply(samples, sample, x=Y, size=n)
