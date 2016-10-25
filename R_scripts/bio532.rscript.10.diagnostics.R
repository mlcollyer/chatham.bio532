#---------------------------------------------------------------------------------
#
#  BIO532 R Diagnostics and transformations
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

# are the assumptions okay?

plot(ANOVA1)

# It checks out!

# Another example
data("lowbwt")
lowbwt$sex <- as.factor(sex)
lowbwt$tox <- as.factor(tox)

ANOVA2 <- aov(sbp ~ sex, data = lowbwt)
summary(ANOVA2)
plot(ANOVA2)


# Oh-oh

# but notice what happens with this transformation


ANOVA3 <- aov(sqrt(sbp) ~ sex, data = lowbwt)
summary(ANOVA3)
plot(ANOVA3)


### NON-PARAMETRIC ANOVA (Randomization test) -------------------------------

# from chatham.bio532 library

ANOVA4 <- ANOVA(sbp ~ sex, data = lowbwt)
summary(ANOVA4)
plot(ANOVA4, method = "histogram")
plot(ANOVA4, method = "diagnostic")

ANOVA5 <- ANOVA(sqrt(sbp) ~ sex, data = lowbwt)
summary(ANOVA5)
plot(ANOVA5, method = "histogram")
plot(ANOVA5, method = "diagnostic")

# The diagnostics are the same, but it does not matter much
# as the assumptions are relaxed.

