## Federal Reserve Board of Governors
## Howard University
## Data Analysis in R course ECON 314
## Introduction to visualization with ggplot2
## STUDENT LECTURE CODE

## Where does the computer think I am?
getwd()

## Where do I want the computer to think I am?
lecture_2_location <- ""

setwd(lecture_2_location)

## Read in the data




# Exercise 1: Examining our Data ------------------------------------------

# Ok, now it's your turn: using the `tail()` function, 
# I want to see the last 4 rows of treasury data





# Exercise 2: Checking types ----------------------------------------------

# Using the `class()` function and the $ notation check the type 
# for the rest of our columns.
# What is the class of your "DATE" column?
# What is the class of your UNRATE column?



# Attaching a package

library(ggplot2)





# Exercise 4: First Plot --------------------------------------------------

# Using the code from the previous slide, 
# make a chart showing the interest rate of 10 year bonds over time






# Exercise 5: GS5 ---------------------------------------------------------
# Make a scatterplot for the 2016 values of the `GS5` column 
# in the treasuries data.frame.








# Exercise 6: Line Plot ---------------------------------------------------
# Make a line plot for the 10 year treasury data for 2015 and 2016. 
# After creating your plot, save it as "GS10_2015_2016.pdf"
# Note that you should save it as a pdf






# Exercise 7 - Layering ---------------------------------------------------
# Create a layered line and point chart for the GS30 data for the year 2016. 









# Exercise 8 - Plotting multiple lines ------------------------------------
# Add the lines for `GS10` and `GS30`, they should be orange and blue respectively.










# Exercise 9: Plotting Multiple Lines -------------------------------------

# Now it's your turn, make a plot of the unemployment rate, 3, 
# 30, and 10 year treasury yields for 2015-2016.













# Exercise 10 - Data Distribution -----------------------------------------

# For the rest of the class work on creating 4 charts 
# like the ones we just went over: boxplot, histogram, density, and violin plots
# You should look at the following 4 values: GS3, GS10, GS30, and UNRATE
# Use the scale functions
# Your charts should look like the following:
# You should make yours separately, do not worry about displaying all 4 at once
# When you are finished save your plots to .png files

