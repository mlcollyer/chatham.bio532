#---------------------------------------------------------------------------------
#
#  BIO532 R ANOVA (for single factors, to compare means)
#
#---------------------------------------------------------------------------------

library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Example from Chapter 12 of text
# P. 287

# Data entry by hand
vol <- c(3.23, 3.47, 1.86, 2.47, 3.01, 1.69, 2.10, 2.81, 3.28, 3.36,
         2.61, 2.91, 1.98, 2.57, 2.08, 2.47, 2.47, 2.74, 2.88, 2.63,
         2.53,
         3.22, 2.88, 1.71, 2.89, 3.77, 3.29, 3.39, 3.86, 2.64, 2.71,
         2.71, 3.41, 2.87, 2.61, 3.39, 3.17,
         2.79, 3.22, 2.25, 2.98, 2.47, 2.77, 2.95, 3.56, 2.88, 2.63,
         3.38, 3.07, 2.81, 3.17, 2.23, 2.19, 4.06, 1.98, 2.81, 2.85,
         2.43, 3.20, 3.53)

hospital <- as.factor(c(rep("JH", 21), rep("LA", 16), rep("SL", 23)))

# Summaries
by(vol, hospital, length)
by(vol, hospital, mean)
by(vol, hospital, sd)

### PARAMETRIC ANOVA -----------------------------------------------------

exp.vol <- data.frame(vol = vol, hospital = hospital)

# ANOVA
ANOVA1 <- aov(vol ~ hospital, data = exp.vol)
summary(ANOVA1)

# Pairwise t-test

pairwise.t.test(x = vol, g = hospital, data = exp.vol, 
                p.adjust.method = "bonf")

# Note: R does not help with finding confidence intervals for 
# t-test with Bonferroni corrections of alpha

# Pairwise test using Tukey's Honest Significant Difference

TukeyHSD(ANOVA1)

# Not as conservative.  Same as t-test, but uses q distribution
# instead of t-distribution

### NON-PARAMETRIC ANOVA (Randomization test) -------------------------------

# from chatham.bio532 library

ANOVA2 <- ANOVA(vol ~ hospital, data = exp.vol)
summary(ANOVA2)

# Notice the slightly different result (but borderline).
# Non-parametric tests do not require strict assumptions
# Can be more powerful
# Notice also there are more statistics - not needed at this time

plot(ANOVA2, method = "histogram")

# Pairwise tests

PW <- pairwise.means.test(ANOVA2, alpha = 0.05)
summary(PW)
plot(PW)

# want to do a Bonferroni correction?

alpha.b <- 0.05/choose(3,2) # see P. 296
alpha.b

PW <- pairwise.means.test(ANOVA2, alpha = alpha.b)
summary(PW)
plot(PW)

# NOTE: THERE IS NO RIGHT OR WRONG WHEN DOING THESE TESTS, IN TERMS OF ALPHA
# CORRECTION OR USING DIFFERENT METHODS (LIKE TUKEY'S HSD).
# ONE JUST HAS TO BE SMART ABOUT THE QUESTION THAT IS ADDRESSED
# IN THIS EXAMPLE, RESEARCHERS WANT TO KNOW IF THEY CAN POOL HOSPITALS
# TOGETHER BY ASKING IF THE MEANS FOR EXP. VOL. ARE DIFFERENT.
# ONE WOULD NEED TO ASK IN THIS BORDERLINE CASE, WHICH WOULD BE MORE DANGEROUS?
# 1) POOL DATA TOGETHER, EVEN THOUGH HOSPITALS MIGHT BE SLIGHTLY DIFFERENT
# 2) DO NOT POOL DATA TOGETHER, EVEN THOUGH DIFFERENCES AMONG HOSPITALS ARE NOT 
# LARGE?

# THIS IS WHERE STATISTICS GETS AMBIGUOUS!



