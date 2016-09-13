#---------------------------------------------------------------------------------
#
#  BIO532 R INTRODUCTION, just some basics for using R
#
#---------------------------------------------------------------------------------


# First thing to note, if one wants to add notes to an R script
# one must use a # to type the note
# DO THIS OFTEN!!!!  IT HELPS OTHERS UNDERSTAND YOUR INTENTION
# IT ALSO REMINDS YOU WHAT YOU WERE ATTEMPTING TO DO LATER
# FINALLY, YOU WILL HAVE TO SUBMIT SOME HOMEWORK USING SCRIPTS
# IF YOU HAVE TROUBLE, IT'S EASIER TO COMMUNICATE WHAT YOU WERE
# ATTEMPTING TO DO WITH ANNOTATED SCRIPTS

### Typing in data or data files

a = 4

# To look at the value of an object, just type the object name

a

# Note: R is an open-source programming language that was modeled after
# the S programming language (yes, the name is a bit of a play on the name S, but
# the original authors had the first names Ross and Robert).  The creator of the S
# programming language is a member of the R core Development team (so no competition).
# R makes a point to work with all S programming.  In S, one must do the following,
# which also works perfectly fine in R

a <- 4

# Note that spaces are not needed and arrow direction does not matter
# Note that the arrow assigns an object a value, a string of values, or a function, 
# but this can be replaced by the '=' sign.
# Note that in R, as we will see later, an actual equality might need "==" (two equal signs)
# to differentiate it from an assignment.  

### Other types of assignments

b = c(4,2,5) # c means concatenate or combine
c = array(1:5)
c = 1:5
d = array(1,10)
e = array(1:3,15)
f = rep(seq(1,4,1),5) # seq means sequence (from 1 to 4 by 1s); rep = repeat (5 times)
g = c("I", "LOVE","R",4,"STATISTICS")
h = c("I love R for statistics")

# If one wants to assign a few variables he/she can type it all in
# and assemble a data frame

# E.g., 

x = c(10,11,14,15,9,12,8)
y = c(100,100,110,100,120,140,110)
z = factor(c(rep("A",4),rep("B",3))) # factor means a nominal/categorical variable

x
y
z

# Data Frames are the most common way to deal with data for statistical analysis
# They are matrices, but with the specific designation that columns are variables

Y = data.frame(x,y,z)
Y # each row is a subject with three values for three variables/variates

### Reading in files
# One will do this a lot, if one has data, say, in an Excel file
# or other type of spreadsheet.

# IT IS BEST TO SAVE EXCEL FILES AS TEXT DOCUMENTS.  FOR EXAMPLE, "SAVE.AS"
# THEN CHOOSE .csv or .txt

# There are different formats to use, but .csv (comma separated values) files
# work regularly and do not require knowing the difference
# between a space and a tab (values in text files can be separated or 
# 'delimited' by commas, spaces, or tabs)

# CHANGE YOUR DIRECTORY NOW!!!!!!!!

# When you want to work within a directory....

# E.g.,
# First, save the malaria.xlsx file as a csv file in a directory of your choice
# Second, change the directory in R to match that directory

malaria = read.csv("malaria.csv",header=T)

# look at the data

malaria

# 'read.csv' is a function.  It performs a task.  If you ever need help using a function
# use '?' followed by the function.  E.g., 

? read.csv
? factor
? seq

# Note that any spreadsheet in Excel can be saved as a text file.
# One must do this to read the file into R
# Other stats programs might be able to read Excel files
# R can do with an appropriate library (more on this later)

# When one is not sure about directory, but would like to search for a file....

malaria = read.csv(file.choose())

# file.choose() is a function that allows one to choose a file from a pop-up window

# Note in the example above that there is a function within another function.


# When one wants to read a file from a URL directly into R....

pupfish = read.csv("http://people.wku.edu/michael.collyer/biol.582/R_files/pupfish.parasite.csv")

# Note that quotes are used around the URL or file name.  They can be single or double quotes.

### Playing with the data

dim(malaria) # provides the dimensions of the "data frame"
str(malaria) # an overview of the structure of the object, malaria

malaria[33,2] # provides the value in the 33rd row and 2nd column
malaria[33,] # provides the entire 33rd row
malaria[,2] # provides the entire 2nd column

malaria[1:10,] # provides the first 10 rows
malaria[-(3:6),] # provides the entire data frame, minus the third through sixth rows

# When objects have "sublevels", the symbol,'$',is used to strictly
# pay attention to a sublevel

# E.g.,
str(malaria)
malaria$rate
malaria$year
