#---------------------------------------------------------------------------------
#
#  BIO532 R TWO-SAMPLE TESTS
#
#---------------------------------------------------------------------------------

library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Let's use some familiar data
data("lowbwt")

### Review 

# SBP for females

sbpf = lowbwt$sbp[lowbwt$sex == 0]

# SBP for males

sbpm = lowbwt$sbp[lowbwt$sex == 1]

# confidence intervals

ciF = confidence.int(y=sbpf, alpha = 0.01, iter = 1000)
ciM = confidence.int(y=sbpm, alpha = 0.01, iter = 1000)
summary(ciF)
summary(ciM)

plot(ciF)
plot(ciM)

# Is there any evidence to suggest that females and males differ in mean SBP?

#-----------------------------------------------------------------------------------------

# Two-sample test

# let's do this by hand first

# ALWAYS STATE THESE

# null hypothesis, H0: muf - mum = 0
# alternative hypothesis, HA: muf - mum != 0
# alpha = 0.05 (standard), two tailed test (use alpha/2)

# t-stat = (sample mean 1 - sample mean 2)/pooled se

mf = mean(sbpf)
mm = mean(sbpm)
mf
mm

dif <- mf-mm

varf = var(sbpf)
varm = var(sbpm)

# let's assume unequal variances

nf = length(sbpf)
nm = length(sbpm)

pooled.se = sqrt(varf/nf + varm/nm)

t.stat = dif/pooled.se
t.stat

# how do we get a probability for this?

# first one needs the df (see p. 271)

dfn <- (varf/nf + varm/nm)^2
dfd <- (varf/nf)^2/(nf-1) + (varm/nm)^2/(nm-1)
df <- dfn/dfd
df

# Note

nf + nm - 2

# The "Welch" method of df estimation is conservative.  It makes the t-distribution 
# more platykurtic

# P-value (parametric)
pt(t.stat, df)

# Note, one could also use Table A.4, p. A-10

# Since P-value > alpha/2, fail to reject the null hypothesis

# Note: R has a canned parametric t-test
? t.test

t.test(sbpf, sbpm, alternative = "two.sided", mu = 0, 
       var.equal = FALSE, paired = FALSE, conf.level = 0.95)


### IMPORTANT.  IF YOU ARE GIVEN A PROBLEM WITH ONLY MEANS AND STANDARD DEVIATIONS, YOU
### HAVE NO CHOICE BUT TO USE A PARAMETRIC SOLUTION TO THE PROBLEM, AND PERFORM
### THE CALCULATIONS BY HAND

# --------------------------------------------------------------------------------------

# Since we have the data, we can also do a non-parametrc test

? two.sample.test

# Let's use the example
TST1 <- two.sample.test(sbp ~ sex, data = lowbwt, 
                        alpha = 0.05, mu = 0, iter = 999)
summary(TST1)

# Notice how much more comprehensive this is!
# Now plot results

plot(TST1, method = "hist", conf.int = "2T")

# Might be better with more permutations

TST1 <- two.sample.test(sbp ~ sex, data = lowbwt, 
                        alpha = 0.05, mu = 0, iter = 9999)
summary(TST1)
plot(TST1)

# Note, there are several plot options too
? plot.two.sample.test

#.e.g., for a one-sided test (postive direction)
plot(TST1, conf.int = "PT")

# ---------------------------------------------------------------------------

# Try other examples 

# ---------------------------------------------------------------------------

# Caution... Chapter 11, problem 7, p. 279

y12 <- c(73, 58, 67, 93, 33, 18, 147)
y24 <- c(24, 27, 49, 59, 0, 11, 43)

y <- c(y12, y24)
x <- as.factor(c(rep(0,7), rep(1,7)))

cigData <- data.frame(y=y, x=x)

# null hypothesis, H0: mu12 - mu24 = 0
# alternative hypothesis, HA: mu12 - mu24 > 0
# alpha = 0.05 (standard), one tailed test (positive tail)

TST3 <- two.sample.test(y~x, data = cigData, alpha = 0.05, mu = 0)
summary(TST3)
plot(TST3, conf.int = "PT")

# Conclusion, cortisol levels signficantly decrease with time.










# wrong. Wrong! WRONG!!!!











# This is a paired design!

# null hypothesis, H0: mu(d) = 0
# alternative hypothesis, HA: mu(d) > 0
# alpha = 0.05 (standard), one tailed test (positive tail)

d <- y12 - y24
ci.d <- confidence.int(d, alpha = 0.05)
summary(ci.d)
plot(ci.d)

# Note: data might not be normally distributed
boxplot(d)

# One huge outlier - maybe not trust parametric results



