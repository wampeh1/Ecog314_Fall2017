## Dplyr Homework
## Assigned: September 8, 2017
## Due: September 14, 2017

## Overview: This homework assignment will cover the different ways in 
## which to use the dplyr package. 

## Datasets: For this homework you will need the lending_club_loans csv file. 
## Additionally, the lending_club_data_dic.xlsx file will be useful

## Before you begin I would recommend reading the Dplyr overview written
## by Hadley Wickham, the package creator located at: 
## http://r4ds.had.co.nz/transform.html


# ----------#
#  Libraries
# ----------#

# load the dplyr library
library(dplyr)
library(ggplot2)

#--------------#
# Exercise 1 (20 points)
# -------------#

########
# Part a (5 points)
# Read in the raw data and assign it to the variable lending_club. (2 points)
# HINT: Be sure to read characters in as characters and not as factors
# What are the dimensions of the lending club data frame? (3 points)


lending_club <- read.csv()

##########
# Part b (6 points)
# Take a look at your loans data frame by calling it in the console, 
# does the formatting look good, are the types correct?
# What is the type of the following variables (use the class() function): 
# id, term ,int_rate, loan_amnt, issue_d, and installment?




###########
# Part c (5 points)
# Create a new data frame called lending_club_small and keep the following columns:
# id, loan_amnt, funded_amnt, term, int_rate, installment, grade, emp_length, 
# home_ownership, annual_inc, issue_d, loan_status, total_acc, addr_state.

lending_club_small <- select()

###########
# Part d ( 4 points)
# What does the substr function do? (2 points)
# what arguments does it use? (2 points)

## ANSWER HERE


# ----------------#
# Exercise 2
# ----------------#

## Now that we've succesffully loaded the data we are interested in,
## we can begin to take a more in-depth look at it and clean as necessary

#########
# Part a. (5 points)
# Right now the term varaible is a character taking on the values " 36 months" " 60 months".
# using the mutate(), substr(), and as.numeric() functions, 
# Create a column, term_num that takes on the values 36 or 60 based on the term column.
# Be sure to re-assign the results back to lending_club_small

lending_club_small <- mutate()


#########
# Part b (8 points)
# In order to recode the emp_length variable we will first need to find the unique values in the 
# dataset and then decide how we want to encode them

# Find the unique values of the emp_length variable (2 points)


# Using the ifelse function as well as mutate() and other functions you need:
#     Create a new variable called emp_term which has numeric values for the
#     number of years worked. Treat "< 1 year" as 0 and "10+ years" as 10. Assign your results to lending_club_small
#     HINT: you can mutate the same variable multiple times within a mutate call. 
    

lending_club_small <- 

##########
# part c (12 points)

#     Check the data dictionary for a description of the total_acc variable
#     what does the variable show? 

## ANSWER HERE

#     Do you think it makes sense to leave it as an integer value
#     or would it be more useful to bin the data into character based buckets
#     such as small, medium, large? 
#     Use ggplot to plot a basic histogram of the total_acc variable with 30 bins
#     be sure that your plot has an appropriate title and y-axis label, you should 
#     turn off the x axis label 



##################
# Part d (10 points)

#  Use the cut() function to create a new column, crdt_lines. 
#      You will want to look up the syntax for this function. 
#      But generally cuts takes three arguments: a vector that you want to cut, 
#      a vector of the values to cut on and a vector of labels for each of the cuts.
#      In this case the values should be "small" if total acc [0, 10], 
#      "medium" if (10, 25] and "large" if (25, 100]. 
#      Be sure to assign your data frame to lending_club_small
#  Once you have cut the data into the three buckets, make an appropriately 
#      labeled bar chart.
#      Are the bins equally sized? Should they be?


lending_club_small <- 

# ------------#
# Exercise 3
# ------------#


# We have the loan amount, the interest rate, and the term,
# use those to create a column, ttl_val, showing the total amount paid, A,
# with the formula A = P(1 + r/t)^nt. 
# Assuming monthly payments and that interest compounds monthly we get the following:
# P = principle, r = rate, t = number of periods per year, n = number of years


lending_club_small <- lending_club_small %>%
    mutate(ttl_val = (funded_amnt) * (1 + (int_rate / 12)) ^ (term_num))

## Now that our data is set up nicely we can start to ask questions 
## of our data and utilize the real power of dplyr

######## 
# Part a (5 points)
# Find the maximum loan amount for each of the different terms using the 
# group_by() and summarise() function.



answer_3a <- 

#########
# Part b. (15 points)
# Do we see that as people have larger incomes they also take on larger loans?

#    First add a column to lending_club_small that is the leverage ratio 
#    (call it lev_ratio), the ratio of loans to income, so you will want to use the 
#    ttl_val column created at the end of question 2 and the annual income column.
#    Then aggregate by income level and find the mean (call it avg_lev_ratio). 
#    Once your data are set up, make a line plot of average annual income
#    vs. average leverage ratio. 
#    Does the plot give you additional information about how leveraged people are?
#    (do people take on the same level of debt as a percentage of income at all income levels?)

answer_3b <- 

answer_3b_plot <- 

## Answer the free response question below

#--------------#
# Exercise 4
#--------------#


############
# Part a (5 points)
# Take a look at the value in the loan_status variable, what are the possible
# values?
# HINT: take a look at the unique() function

answer_4a <- 

##############
# Part b (5 points)
# Let's now take a look at finding any differences between "good" loans
# and "bad" loans, we'll define "good" as loans that are charged off, fully paid,
# or current, and "bad" as the rest. Write two vectors corresponding to
# the correct loan statuses using the c() function

good <- 
bad  <- 


##########
# Part c (10 points)
# We want to know how many good and bad loans we have in our sample
# To do this we will need to add a column called "status" to our 
# lending_club_small data using the ifelse() function and the 
# vectors created above. 
# If the loan is good the status should be "good" otherwise it should be "bad".
# Once we do that we will need to use dplyr to figure out the count.
# You will want to group by the status column and call the dplyr n() function 
# within you call to summarize()
# Do you think this is a large enough sample size of both statuses to derive 
# meaningful conclusions from generate statistics?



# First mutate your data
lending_club_small <- 
# Create a new dataframe with your summary statistics
answer_3c <- 

# Answer the short answer question below

#############
# Part d (10 points)
# Let's take a deeper look at good vs bad loans
# First we want to know the difference in the average loan amount between good and bad loans
# Then we want to know if the interest rates on good and bad loans are similar
# aka, do bad loans have higher interest rates? How much greater is the interest rate on
# bad loans than good loans?
# Do you think that higher interest rates cause more laons to go bad or 
# do you think that people less likely to pay have higher interest rates to begin with?


#   Use dplyr to create a table showing the mean loan amount by status
answer_4d <- 

# Now find the difference between the two values

# HINT: difference of ~$6850

# ii. So far we've seen differences in loan amount by status, let's 
# take a look at interest rates, do bad loans tend to have higher average interest rates?
# Follow the same process as the above question
answer_4d2 <- 

# How much greater is the interest rate on the average bad loan
# than the interest rate on the average good loan?

# HINT: difference 5.37%

## Answer the free response question below
