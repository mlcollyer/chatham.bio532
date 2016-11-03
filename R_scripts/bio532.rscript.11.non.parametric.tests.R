#---------------------------------------------------------------------------------
#
#  BIO532 R Common "non-parametric" tests
#
#---------------------------------------------------------------------------------

library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Using examples form text book

# sign test, Wilcoxon signed rank test (see Table 13.1, p303)

# make data frame
CF <- c(1153, 1132, 1165, 1460, 1634, 1493, 1358, 
            1453, 1185, 1824, 1793, 1930, 2075)
healthy <- c(996, 1080, 1182, 1452, 1162, 1619, 1140, 
            1123, 1113, 1463, 1632, 1614, 1836)

REE <- data.frame(CF, healthy)

# parametric paired t-test, assume alpha = 0.05
t.test(CF, healthy, paired = T, data=REE, alternative = "greater")
t.test(CF, healthy, paired = T, data=REE, alternative = "two.sided")

qqnorm(CF-healthy) # really no problem to use parametric test...

# randomization test (one sample confidence interval)

confidence.int(CF-healthy)

# sign test: there is no built in test, but easy to do by hand

binom.test(11, 13, 0.5, alternative = "greater")

# by hand
signs <- sign(CF-healthy)

# number positives
sum(signs[signs == 1])

# number negatives
sum(signs[signs == -1])

# Probabilty Y <= 2  or Y > = 11 from binomial distribution, with p = 0.5

pbinom(q = 2, size = 13, prob = 0.5)
pbinom(q = 10, size = 13, prob = 0.5, lower.tail = FALSE)

# Wilcoxon signed rank test

wilcox.test(CF, healthy, paired = TRUE, data = REE, alternative = "greater")
wilcox.test(CF, healthy, paired = TRUE, data = REE, alternative = "two.sided")

# Note

sign.rank <- rank(abs(CF-healthy))*sign(CF-healthy)
sign.rank
sum(sign.rank[sign.rank > 0])

#-------------------------------------------------------------------------------

# Same procedure, table 13.2, p306

placebo <- c(224, 80, 75, 541, 74, 85, 293, 
        -23, 525, -38, 508, 255, 525, 1023)
drug <- c(213, 95, 33, 440, -32, -28, 445, 
             -178, 367, 140, 323, 10, 65, 343)

FVC <- data.frame(placebo, drug)

# parametric paired t-test, assume alpha = 0.05
t.test(placebo, drug, paired = T, data=FVC, alternative = "greater")
t.test(placebo, drug, paired = T, data=FVC, alternative = "two.sided")

qqnorm(placebo - drug) # really no problem to use parametric test...

# randomization test (one sample confidence interval)

confidence.int(placebo - drug)

# sign test: there is no built in test, but easy to do by hand

signs <- sign(placebo - drug)

# number positives
sum(signs[signs == 1])

# number negatives
sum(signs[signs == -1])

# Probabilty Y <= 3  or Y > = 11 from binomial distribution, with p = 0.5

pbinom(q = 3, size = 14, prob = 0.5)
pbinom(q = 10, size = 14, prob = 0.5, lower.tail = FALSE)

# Wilcoxon signed rank test

wilcox.test(placebo, drug, paired = TRUE, data = REE, alternative = "greater")

#-------------------------------------------------------------------------------

# Mann Whitney Wilcoxon rank sum test (see Table 13.3, p309)

mMA <- c(34.5, 37.5, 39.5, 40.0, 45.5, 47.0, 47.0, 47.5, 48.7, 49.0, 
         51.0, 51.0, 52.0, 53.0, 54.0, 54.0, 55.0, 56.5, 57.0, 58.5,
         58.5,
         28.0, 35.0, 37.0, 37.0, 43.5, 44.0, 45.5, 46.0, 48.0, 48.3, 
         48.7, 51.0, 52.0, 53.0, 53.0, 54.0, 54.0, 55.0)
exposure <- c(rep("low", 21), rep("high", 18))
phenyl <- data.frame(mMA, exposure)

# parametric t.test, assume two-tailed alternative and alpha = 0.05

t.test(mMA ~ exposure, data = phenyl, var.equal = TRUE)

# paramteric ANOVA

ANOVA1 <- aov(mMA ~ exposure, data = phenyl)
summary(ANOVA1)
plot(ANOVA1)

# non-parametric (randomization) ANOVA

ANOVA2 <- ANOVA(mMA ~ exposure, data = phenyl)
summary(ANOVA2)
plot(ANOVA2)
plot(ANOVA2, method = "diagnostic")

# Mann Whitney Wilcoxon

wilcox.test(mMA ~ exposure, data = phenyl)

# Note 

ranks <- by(rank(mMA), exposure, sum)
ranks

ns <- by(rank(mMA), exposure, length)
ns

# Sum rank - n(n+1)/2, by group

by(rank(mMA), exposure, sum) - ns*(ns+1)/2

# Kruskal Wallace Test

kruskal.test(mMA ~ exposure, data = phenyl)

# Don't worry about how Chi-square is calculated, but realize it can be done with 
# 2+ groups


# RECOMMENDED PARADIGM 

# What is the central question?  Do I need to know means?
# Is it possible to use a resampling prcoedure?  Can I do this with a computer?
# If I wish to use a parametric test, do my data meet assumptions?
# Would a transformation help for parametric tests, if this is what I prefer?
# Can I change the data to ranks or just measure - or + outcomes, without losing
# the main objective?


