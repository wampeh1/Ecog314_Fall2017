## FRB/Howard University ECON 181
## Lecture 4: Tidying Data
## 9/14/2017

#------------------------------------------------------------------------------#
# Set up directory path ---
#------------------------------------------------------------------------------#
lecture4_dir <- ""

# Set working directory
setwd(lecture4_dir)

#------------------------------------------------------------------------------#
# Load packages ----
#------------------------------------------------------------------------------#
#install.packages("tidyverse")
library(tidyverse)
#install.packages("lubridate")
library(lubridate)
#install.packages("stringr")
library(stringr)

#------------------------------------------------------------------------------#
# Read in data ----
#------------------------------------------------------------------------------#
load("tidydata_data.Rdata")

#------------------------------------------------------------------------------#
# Begin in-class notes ----
#------------------------------------------------------------------------------#

# Browse through the objects in the Rdata file
## Look at their str's, dimensions, names





# In-class exercise #1
## Convert date_vec to a Date vector
date_vec <- c("Day 5 of March, 2017",
              "Day 23 of May, 2017")

#EX_1_sol <-









# In-class exercise #2
## Use the seq() command to generate a vector of every month-end date in 2010

#EX_2_sol <- 









# In-class exercise #3
## Convert the following vector, numbers, to numeric type
numbers <- c("2,100", "3,250,000")

#EX_3_sol <-








# In Class Exercise #4
## Use var_string and str_split to calculate the average value of loans by 
## loan type and date
var_string <- "principal, loan_num"

#EX_4_sol <- 







# In-class exercise #5
## Using banks and loanDescriptions complete the following:
## 1. Calculate the aggregate revenue on loans for banks within 
##    each of the charter types
## 2. Calculate the proportions of the charter revenue held by
##    each bank of that type
## 3. Return the largest contributor to revenue in each charter type
##    - By bank
##    - By loan type

#EX_5_sol <-