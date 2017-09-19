## Tidying data homework
## Assigned: September 16, 2017
## Due: September 21, 2017

## Overview: This homework accompanies the lecture on tidying data. Please refer
## to the lecture slides if you run into any problems completing this assignment.
## Submission of this assignment will be done, as usual, through Piazza. The final
## deliverable will just be your R script. Please use the following file name format:
## YOURLASTNAME_tidydata_hw.R

#------------------------------------------------------------------------------#
# Set up directories ----
#------------------------------------------------------------------------------#
hw4_dir <- ""

setwd(hw4_dir)
#------------------------------------------------------------------------------#
# Load libraries ----
#------------------------------------------------------------------------------#
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

#------------------------------------------------------------------------------#
# Read in data ---
#------------------------------------------------------------------------------#
# You will need to load tidydata_data.Rdata, the file used in class
# If you don't already have it, find it in the lecture4 folder on github
# https://github.com/wampeh1/Ecog314_Fall2017/
load("tidydata_data.Rdata")

#------------------------------------------------------------------------------#
# Exercise 1 (10 points) ----
#------------------------------------------------------------------------------#
# Perform the following operations on the banks dataset:
# (If you followed along in class then you just need to write your code in here)
# 1. Update the date variable to be of Date type

# 2. Add the following variables:
#     yearonly - the year part of date
#     monthonly - the month part of date

# 3. Update the type variable to correct for all the typos. The only values that
#    should appear in type are: Commercial, Credit Union, and Savings

# 4. Add a variable called loantype by extracting only the P's and G's of loanID

#------------------------------------------------------------------------------#
# Exercise 2 (30 points) ----
#------------------------------------------------------------------------------#
# Using banks and loanDescriptions complete the following:
# 1. Calculate the aggregate revenue on loans for banks within each of the 
#    charter types. Revenue will be defined as intAnnual*length*principal*loan_num.
#    So your output at this step should be something like:
#                         type          aggRev
#                         Commercial      -
#                         Credit Union    -
#                         Savings         -

# 2. Calculate the proportions of the charter revenue held by each bank of that 
#    type

# 3. Return the largest contributor to revenue in each charter type


#------------------------------------------------------------------------------#
# Exercise 3 (30 points) ----
#------------------------------------------------------------------------------#
# 1. Using str_replace() and loanID make a new variable in banks that recodes loanID
#    in the following way: P1 should become Private-1, P2 should become Private-2,
#    G1 should be Gov-1, G2 should be Gov-2, and so on. Call this new variable
#    loanID_long.

# 2. Make a plot showing for each day of the week the average number of loans issued 
#    for each kind of loan. Be sure the legend for your plot uses loanID_long.

# 3. Explain what information is being presented in your plot and propose a research
#    idea that would improve our understanding of the information.


#------------------------------------------------------------------------------#
# Exercise 4 (30 points) ----
#------------------------------------------------------------------------------#
# 1. Add the length and intAnnual variables from loanDescriptions to banks and
#    assign the result of this join to banks_new.
#    banks_new should have the same numbers of rows as banks and should have two
#    more columns than banks.
#banks_new <- 

# 2. Reshape banks_new by combining the following variables into one column: 
#    principal, loan_num, intAnnual, and length. Call this new variable 'variable'
#    and call the resulting value column 'value'. Call this reshaped data banks_long.
#    Hint: use gather()
#banks_long <-

# 3. Explain how the format for our data has changed between banks_new and banks_long.
#    Use the total number of rows in banks_long to help justify your explanation.

