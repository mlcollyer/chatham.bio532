#---------------------------------------------------------------------------------
#
#  BIO532 R LOADING DATA, how to load data into a data frame
#
#---------------------------------------------------------------------------------

# There are two basic ways to load data into R.

# One way is to read data from a text file.  E.g., 

myData <- read.csv(file.choose()) # or use Environment/Import Dataset from R Studio

# The other way is to load data saved as R data files.  Note: one can save data is this format
# but we are not going to worry about that right now, as it is a bit esoteric.  But, luckily for
# BIO532 students, all relevant homework data sets are saved as Rdata files in the chatham.bio532
# package.

# Thus,
library(devtools)
install_github("mlcollyer/chatham.bio532")
library(chatham.bio532)

# Notice all of the data files
?chatham.bio532

# One can simply do this now

data(bed)
data(cigarett)
data(water)
