#---------------------------------------------------------------------------------
#
#  BIO532 R LIBRARIES, how to install packages and load libraries
#
#---------------------------------------------------------------------------------

# What makes R so wonderful is that anyone can contributes packages
# that can be useful to other users.  R is a community!

# What is a package?  It is a set of tools and data used for a specific purpose
# or set of purposes.  For example, to the right (in R studio) click on "Packages"
# in the lower right panel.

# Find the packgae MASS, and click in the box.  Notice what happened in the console.
# A shortcut is the following

library(MASS)

# The library is the installed set of tools and data, ready to use; the package is 
# literally the package for delivering those tools.  Want to know about the library 
# you are using?  Go to the help or

? MASS

# Some packages are already installed.  Sometimes, one must install new packages.
# Let's install an important package, called devtools.  At the top of the screen
# (if using R Studio), click on the pull-down menu for "Tools", then select "Install
# packages"

# Make sure of the following: the repository = CRAN; the library is set to the default 
# location; the "install dependencies" box is checked.  (Dependencies are other packages
# on which the targeted package depends and needs to operate as desired.).  Then type 
# "devtools" in the package box (without quotes), and click "install".

# Go to the Packages tab and notice this package (plus others) are now there.  
# Load devtools now.

? devtools # see what this package does

# We installed this package for an explicit reason.  Look at the following

?install_github

# GitHub is a web-based repository service for storing, updating, and providing 
# computer programs and packages, like the one we will use predominantly this semester.

# This package is called chatham.bio532

# One can find it here 

# https://github.com/mlcollyer/chatham.bio532

# It is not much to look at, but it provide a service.  Namely, it allows you
# to load the library we will use most in this class!
# You should do the following at the beginning of every script.

############### THIS IS IMPORTANT !!!! ###############################################

library(devtools)
install_github("mlcollyer/chatham.bio532")

######################################################################################

# Let's load this library!
library(chatham.bio532)
?chatham.bio532

# This package will be updated regularly, so always start any R script by reinstalling it.

