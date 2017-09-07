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

path <- "/msu/home/m1skm01/Howard_ECOG_314/dplyr_2017F/Data/lending_club_loans.csv"

lending_club <- read.csv(path, stringsAsFactors = F)

dim(lending_club)

##########
# Part b (6 points)
# Take a look at your loans data frame by calling it in the console, 
# does the formatting look good, are the types correct?
# What is the type of the following variables (use the class() function): 
# id, term ,int_rate, loan_amnt, issue_d, and installment?

# One point for each call to class

class(lending_club$id)
class(lending_club$term)
class(lending_club$int_rate)
class(lending_club$loan_amnt)
class(lending_club$issue_d)
class(lending_club$installment)



###########
# Part c (5 points)
# Create a new data frame called lending_club_small and keep the following columns:
# id, loan_amnt, funded_amnt, term, int_rate, installment, grade, emp_length, 
# home_ownership, annual_inc, issue_d, loan_status, total_acc, addr_state.

# 3 points for creating a dataframe with all of the correct columns
# 2 points for using the select() function from dplyr
# 0 points for using the basic [,] subsetting method

lending_club_small <- select(lending_club, id, loan_amnt, funded_amnt, term, int_rate, 
                             installment, grade, emp_length, home_ownership, annual_inc, 
                             issue_d, loan_status, total_acc, addr_state)

###########
# Part d ( 4 points)
# What does the substr function do? (2 points)
# what arguments does it use? (2 points)

## ANSWER HERE
# Substr can be used to extract or replace substrings in a character vector.
# argument is x (a character vector), start, and stop with start and stop being
# numbers referring to the locations of elements within the string for the function
# to either extract or replace.

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

lending_club_small <- mutate(lending_club_small, 
                             term_num = as.numeric((substr(term,2,3))))


#########
# Part b (8 points)
# In order to recode the emp_length variable we will first need to find the unique values in the 
# dataset and then decide how we want to encode them

# i. Find the unique values of the emp_length variable (2 points)
answer_2ci <- unique(lending_club_small$emp_length)

# ii. Using the ifelse function as well as mutate() and other functions you need:
#     Create a new variable called emp_term which has numeric values for the
#     number of years worked. Treat "< 1 year" as 0 and "10+ years" as 10. Assign your results to lending_club_small
#     HINT: you can mutate the same variable multiple times within a mutate call. 
    
# 2 points for correctly creating/reassigning the correct variable
# 4 points for using ifelse correctly and updating the variable multiple times in the same ifelse call

lending_club_small <- lending_club_small %>%
    mutate(emp_length = ifelse(emp_length == "10+ years", 10, emp_length),
           emp_length = ifelse(emp_length == "< 1 year",1, emp_length),
           emp_length = ifelse(emp_length == "1 year", 1, emp_length),
           emp_length = ifelse(emp_length == "3 years", 3, emp_length),
           emp_length = ifelse(emp_length == "8 years", 8, emp_length),
           emp_length = ifelse(emp_length == "9 years", 9, emp_length), 
           emp_length = ifelse(emp_length == "4 years", 4, emp_length),
           emp_length = ifelse(emp_length == "5 years", 5, emp_length),
           emp_length = ifelse(emp_length == "6 years", 6, emp_length),
           emp_length = ifelse(emp_length == "2 years", 2, emp_length),
           emp_length = ifelse(emp_length == "7 years", 7, emp_length))

##########
# part c (12 points)

#     Check the data dictionary for a description of the total_acc variable
#     what does the variable show? 

## ANSWER HERE
# total_acc is the total number of credit lines currently in the borrower's credit file

#     Do you think it makes sense to leave it as an integer value
#     or would it be more useful to bin the data into character based buckets
#     such as small, medium, large? 
#     Use ggplot to plot a basic histogram of the total_acc variable with 30 bins
#     be sure that your plot has an appropriate title and y-axis label, you should 
#     turn off the x axis label 

# 4 points for looking up the value in the dictionary and writing the description
# 2 points for answering if it would be better to bin the data into buckets
# 6 points for appropriate histogram
lending_club_small %>% 
    ggplot(aes(x = total_acc)) +
    geom_histogram(bins = 30) +
    ggtitle("Histogram of number of credit lines") +
    labs(x = NULL, y = "Count")

# The difference between 77 lines of credit and 82 probably isn't 
# all that relevant, we should bin the data

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

# 4 points for using the cut function correctly
# 4 points for correctly labeled chart
# 2 points for answering the question about bin size

lending_club_small <- lending_club_small %>%
    mutate(crdt_lines = cut(total_acc, 
                            breaks = c(0, 10, 25, 100), 
                            labels = c("Small", "Medium", "Large")))


lending_club_small %>% 
    ggplot(aes(x = crdt_lines, fill = crdt_lines)) +
    geom_bar(stat = "count") +
    ggtitle("Size of our credit line bins") +
    labs(x = "Bin Size", fill = NULL,
         caption = "Small refers to 0-10, Medium 10-25, Large 25-100")
## Our bins do not need to be equally sized, we do not want to lose meaning 
## to our data by forcing equally sized bins when not necessary


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

# 3 points for using group by
# 2 for summarise

answer_3a <- lending_club_small %>%
    group_by(term) %>%
    summarise(max = max(loan_amnt))

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

#  5 points for correctly calculating leverage ratio column and setting up data
#  7 points for correctly labeled line plot (3 points for plot, 1 point each for title,
#                                            each axis title and caption)
#  3 points for seeing that high income people are less leveraged than lower income

answer_3c <- lending_club_small %>%
    mutate(lev_ratio = ttl_val/annual_inc) %>%
    group_by(annual_inc) %>%
    summarise(avg_lev_ratio = mean(lev_ratio)) %>%
    arrange(annual_inc)


answer_3cii <- answer_3c %>% 
    ggplot(aes(x = annual_inc, y = avg_lev_ratio)) +
    geom_line() +
    ggtitle("Income vs Leverage Ratio") +
    labs(x = "Annual Income", y = "Leverage Ratio",
         caption = "Data obtained from Lending Club")

# Yes. Very high income individuals are much less leveraged than low income individuals. But most of the data
# is cluseted around 0 so it is hard to see much else.

#--------------#
# Exercise 4
#--------------#


############
# Part a (5 points)
# Take a look at the value in the loan_status variable, what are the possible
# values?
# HINT: take a look at the unique() function

answer_3d <- unique(lending_club_small$loan_status)

##############
# Part b (5 points)
# Let's now take a look at finding any differences between "good" loans
# and "bad" loans, we'll define "good" as loans that are charged off, fully paid,
# or current, and "bad" as the rest. Write two vectors corresponding to
# the correct loan statuses using the c() function

good <- c("Charged Off", "Fully Paid", "Current")
bad  <- c("Default", "In Grace Period", "Late (31-120 days)", "Late (16-30 days)")


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

# 4 points for using ifelse to crrectly set up the status column
# 4 points for using group_by, summarize and n() to find the count
# 2 points for answering the short answer question

# First mutate your data
lending_club_small <- mutate(lending_club_small, 
                             status = ifelse(loan_status %in% good, "good", "bad"))

# Create a new dataframe with your summary statistics
answer_3g <- lending_club_small %>%
    group_by(status) %>%
    summarise(number_loans = n())


# Answer the short answer question below

# No there are very few bad loans, I do not think that this is a 
# large enough population to derive meaningful statistics on

#############
# Part d (10 points)
# Let's take a deeper look at good vs bad loans
# First we want to know the difference in the average loan amount between good and bad loans
# Then we want to know if the interest rates on good and bad loans are similar
# aka, do bad loans have higher interest rates? How much greater is the interest rate on
# bad loans than good loans?
# Do you think that higher interest rates cause more laons to go bad or 
# do you think that people less likely to pay have higher interest rates to begin with?

# 2 points for creating the table with different mean loan amounts
# 2 points for finding the difference in loans values
# 2 points for creating the table with difference mean interest rates
# 2 points for finding the difference in rates
# 2 points for answer to short-response question

#   Use dplyr to create a table showing the mean loan amount by status
answer_hi <- lending_club_small %>%
    group_by(status) %>%
    summarise(mean_loan_amnt = mean(loan_amnt))
# Now use data.frame slicing with [] to find the difference between the
# two values

answer_hi[answer_hi$status == "bad", "mean_loan_amnt"] - answer_hi[answer_hi$status == "good", "mean_loan_amnt"] 
# HINT: difference of ~6850

# ii. So far we've seen differences in loan amount by status, let's 
# take a look at interest rates, do bad loans tend to have higher average interest rates?
# Follow the same process as the above question
answer_hii <- lending_club_small %>%
    group_by(status) %>%
    summarise(mean_int = mean(int_rate))

# How much greater is the interest rate on the average bad loan
# than the interest rate on the average good loan?


answer_hii[answer_hii$status == "bad", "mean_int"] - answer_hii[answer_hii$status == "good", "mean_int"] 

# HINT: difference 5.37%

# Do you think that this higher interest rate is causative or corellative?
# (Do you think that having higher interest may cause people to default on 
# the loan or do you think that people more likely to default have to 
# pay higher interest and then, unsurprisingly, have bad loans in higher numbers)
