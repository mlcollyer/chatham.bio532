#---------------------------------------------------------------------------------
#
#  BIO532 R General Linear Model Approaches
#
#---------------------------------------------------------------------------------

library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Follow example in problems # 8 and # 9 from pg. 467

data("lowbwt")

# sbp = continuous variable (dependent variable for these examples)
# sex = factor (infant sex, 0 = Female; 1 = Male)
# tox = factor (mother's toxemia, 0 = No; 1 = Yes)
# grmhem = factor (germinal matrix hemorrhage, 0 = No; 1 = Yes)
# apgar5 = continuous (ordinal really) variable for apgar score at 5 minutes
# gestage = continuous (age at gestation, in weeks)

# Factorial ANOVA ----------------------------------------------------------------

# First, manipulate lowbwt

lowbwt$sex <- as.factor(lowbwt$sex)
lowbwt$tox <- as.factor(lowbwt$tox)
lowbwt$grmhem <- as.factor(lowbwt$grmhem)

fit <- lm(sbp ~ sex * tox, data = lowbwt) # the * means all factors indicated, 
# plus interaction

# coefficients
fit

# hypothesis tests
summary(fit)

# ANOVA
anova(fit)

# Diagnostics
plot(fit)

# Non-parametric ANOVA

fit2 <- ANOVA(sbp ~ sex * tox, data = lowbwt)
summary(fit2)

# Note: no need to do this in this case, but one could do the following

TukeyHSD(aov(fit)) # parametric

# or

pairwise.means.test(fit2) # non-parametric

# --------------------------------------------------------------------------------

# multiple regression / ANCOVA

fit <- lm(sbp ~ sex * apgar5, data = lowbwt) # factor-covariate interaction

# coefficients
fit

# hypothesis tests
summary(fit)

# ANOVA
anova(fit)

# Diagnostics
plot(fit)

# Non-parametric ANOVA

fit2 <- ANOVA(sbp ~ sex * apgar5, data = lowbwt)
summary(fit2)

# No applicable pairwise comparison of LS means - would have to do by hand
# if desired

#---------------------------------------------------------------------------------

# Model Selection

# There are various other models we could play with.  But rather than sopend all 
# day trying all kinds of permutations, let's put the computer to work!

# stepwise procedure
?step

# let's make a crazy model with everyfactor, covariate, and all interactions

fit <- lm(sbp ~ sex * tox * grmhem * apgar5 + 
            sex * tox * grmhem * gestage, data = lowbwt)

# coefficients
fit

# hypothesis tests
summary(fit)

# Note some "NAs" because it is not possible to estimate coefficients for 
# interactions that do not exist

fit.step <- step(fit)

# coefficients
fit.step

# hypothesis tests
summary(fit.step)

# diagnostics
plot(fit.step)

# Interpretation... this is tricky!  Will depend on outcomes

# Here is an advanced script to make a nice plot

plot(lowbwt$gestage, lowbwt$sbp, pch=19, 
     col = as.numeric(lowbwt$grmhem),
     xlab = "Gestational Age, weeks",
     ylab = "SBP, beats per minute")

yhat <- fit.step$fitted.values

df <- data.frame(g = lowbwt$gestage, yhat = yhat, 
                 t=lowbwt$grmhem)
df < - df[order(df[,1]),]

points(df$g[df$t == "0"], df$yhat[df$t == "0"], type="l", 
       col = 1, lwd=2)
points(df$g[df$t == "1"], df$yhat[df$t == "1"], type="l", 
       col = 2, lwd=2)



