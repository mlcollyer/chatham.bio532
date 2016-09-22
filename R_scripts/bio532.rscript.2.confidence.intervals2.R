#---------------------------------------------------------------------------------
#
#  BIO532 R CONFIDENCE INTERVALS Part 2, what are they good for?
#
#---------------------------------------------------------------------------------

library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Let's try some familiar data
data("lowbwt")

# SBP for females

sbpf = lowbwt$sbp[lowbwt$sex == 0]

# SBP for males

sbpm = lowbwt$sbp[lowbwt$sex == 1]

ciF = confidence.int(y=sbpf, alpha = 0.01, iter = 1000)
ciM = confidence.int(y=sbpm, alpha = 0.01, iter = 1000)
summary(ciF)
summary(ciM)

plot(ciF)
plot(ciM)

# Is there any evidence to suggest that females and males differ in mean SBP?

# An SBP = 70 mmHG is considered dangerously low.  Is there any evidence to suggest
# that newborns with low birth weight are at risk?


