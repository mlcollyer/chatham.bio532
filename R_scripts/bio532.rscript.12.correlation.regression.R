#---------------------------------------------------------------------------------
#
#  BIO532 R Correlation and regression (i.e., linear association)
#---------------------------------------------------------------------------------

library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Follow example in problem # 5 from pg. 412

# make data frame
CL <- c(5.12, 6.18, 6.77, 6.65, 6.36, 5.90, 5.48, 
            6.02, 10.34, 8.51)
TL <- c(2.30, 2.54, 2.95, 3.77, 4.18, 5.31, 5.53, 
            8.83, 9.48, 14.20)

HGC <- data.frame(CL, TL)

# always a good rule of thumb, plot your data!

# use
?par
# for plot parameters


# 5.a two-way scatterplot

plot(CL, TL)
plot(CL, TL, pch = 19, col="yellow")
plot(CL, TL, pch = 20)
plot(CL, TL, pch = 21)
plot(CL, TL, pch = 21, bg = "yellow")

# 5.b Does there are appear to be a linear relationship?

# 5.c Compute the Pearson r

# ------ BEFORE DOING THIS, LET'S IDENTIFY THE PARTS WE (MIGHT) NEED! -----------

# n, x-bar, y-bar, SSX, SSY, SC

# the simple parts
n <- length(CL)
n

x.bar <- mean(CL)
x.bar

y.bar <- mean(TL)
y.bar

# The less simple parts, SSX, SSY, SC

# Here is a function in R for centering and scaling variables

?scale

# The scale function is a function that turns values into z-scores; E.g.,

scale(CL)

# Recall that the numerator is y - mean(y).  This is centering
# Recall that the denominator is sd.  This is scaling.  
# If we only want to center to get SS

scale(CL, center= TRUE, scale = FALSE)

# Thus, SSX are these values squared then summed

SSX = sum(scale(CL, center= TRUE, scale = FALSE)^2)
SSX

# SSY, done similarly

SSY = sum(scale(TL, center= TRUE, scale = FALSE)^2)
SSY

# SC, also similarly

SC = sum(scale(CL, center= TRUE, scale = FALSE) * 
           scale(TL, center= TRUE, scale = FALSE))
SC

# Finally, r = SC/sqrt(SSX*SSY)

r = SC/sqrt(SSX*SSY)
r

# Note: an alternative way to describe R is the SC of standardized variables, 
# divided by degrees of freedom; thus

r = sum(scale(CL, center= TRUE, scale = TRUE) * 
          scale(TL, center= TRUE, scale = TRUE))/(n-1)

# This is algebra, nothing more!  r = SC/sqrt(SSX*SSY) is the simplest algebra
# to do by hand.  Computationally, it is the same as finding 1/(n-1) * sum of 
# (standardized x residual * standardized y residual), as per page 400 in the 
# text book.

# ---------------------------------------------------------------------------------
# Note, easy to do with R function, cor

cor(CL, TL)

# ---------------------------------------------------------------------------------

# 5.d Test the hypohtesis rho = 0
# Let's assume direction for alternative is not implied
# H0: rho = 0
# Ha: rho != 0
# alpha = 0.05

# t = r/sqrt((1-r^2)/(n-2))

t = r/sqrt((1-r^2)/(n-2))
t
# can get P-value from t-distribution
pt(t, n-2, lower.tail = FALSE)

# Don't forget we must double this for two-tailed test
pt(t, n-2, lower.tail = FALSE)*2

# ---------------------------------------------------------------------------------
# Note, easy to do with R function, cor.test

cor.test(CL, TL, alternative = "two.sided")

# ---------------------------------------------------------------------------------

# 5.e Calculate the Spearman correlation

# We must make two new variables, based on ranks

Rx <- rank(CL)
Ry <- rank(TL)

Rx
Ry

# Now perform Pearson correlation on ranks

SSRX <- sum(scale(Rx, center=TRUE, scale = FALSE)^2)
SSRY <- sum(scale(Ry, center=TRUE, scale = FALSE)^2)
SCR <- sum(scale(Rx, center=TRUE, scale = FALSE) * 
             scale(Ry, center=TRUE, scale = FALSE))

# Thus, rs
rs = SCR/sqrt(SSRX*SSRY)
rs

# Spearman correlation is Pearson correlation on ranks

# Short-cut (by hand)

d <- Rx-Ry

rs = 1 - 6*(sum(d^2))/(n*(n^2-1))
rs

# --------------------------------------------------------------------------------
# Note, easy to do with R function, cor

cor(Rx, Ry)

# or

cor(CL, TL, method = "spearman")

# --------------------------------------------------------------------------------

# 5.f  compare r and rs

r
rs

# is this a surprise?

par(mfcol=c(1,2))
plot(CL, TL, pch=19)
plot(Rx, Ry, pch=19)
par(mfcol=c(1,1))

# Note

data.frame(CL, Rx, TL, Ry)

# 5.g

# let's just cut corners...

# ---------------------------------------------------------------------------------
# Note, easy to do with R function, cor.test

cor.test(CL, TL, alternative = "two.sided", method = "spearman")

# ---------------------------------------------------------------------------------

# Note: 
sum(d^2)

# ---------------------------------------------------------------------------------

# LINEAR REGRESSION

# we can use the same results to perform linear regression, assuming CL is an 
# independent avriable (probably silly, but just to demonstrate)

b = SC/SSX
a = y.bar - b*x.bar 

a # intercept
b # slope

# one can cut to the chase with the lm (linear model) function

fit <- lm(TL ~ CL)
fit # just the coefficients

summary(fit)

# Wow!  Look at all of those results!
# Let's confirm some of them

r^2 # same as Multiple R-squared

cor.test(TL, CL) # same t for CL slope; same P-value, same df

summary(aov(fit)) # same ANOVA information

ANOVA(fit)

# Here are some parts that might be interesting

fit$fitted.values
fit$residuals

# We can calculate SSE as follows:

SSE <- sum(fit$residuals^2)
SSE

# and the standard error of the slope as

se.b <- sqrt(SSE/(n-2))
se.b # same as Residual standard error

# Note
b/(se.b*sqrt(1/SSX)) # same as t value

# The lm function does it all!!!!

#---------------------------------------------------------------------------------

# NEVER forget diagnostics

plot(fit)

# note too the second line of 

summary(fit)

# if residuals were symmetrically distributed, median = 0, |Q1| = |Q3|,
# |min| = |max|

#  Bad, bad, bad!  Conclusions could be inappropriate

# -------------------------------------------------------------------------------

# Options?

# 1) go with spearman correlation?
# 2) Use a non-parametric (randomization) test?

fit2 <- ANOVA(fit) # chatham.bio532 function
summary(fit2)

# 3) Data transformation?

fit3 <- lm(log(TL) ~ log(CL))
summary(fit3)
plot(fit3)

# better...

# Note that Spearman correlation, a randomization ANOVA, and log-
# transformation all ended up with similar conclusions

# --------------------------------------------------------------------------------

# Note on plotting

plot(CL, TL, pch = 19)

# add line described intercept and slope

abline(a, b, col="red")

# or a short-cut, if one already has a least-squares fit from lm

plot(CL, TL, pch = 19)
abline(fit, col = "green")

# Also

plot(log(CL), log(TL), pch=23, bg = "dark red")
abline(fit3, col = "dark blue")
