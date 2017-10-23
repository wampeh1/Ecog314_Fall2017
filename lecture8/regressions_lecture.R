# Regressions Lecture
# 
# DESCRIPTION: In this assignment we will learn how to run multiple types of regression
#
# INSTRUCTIONS: Read the instructions provided in the comments. Add your own
# code and comments in the space provided to complete the assignment. 
# 
#-------------------------------------------------------------------------------


#---------------#
# Load Packages #
#---------------#
library(dplyr)
library(ggplot2)


#------------#
# Load Data  #
#------------#

# Load 2015 ACS data
ipums_data <- read.csv("filepath", stringsAsFactors = FALSE)

glimpse(ipums_data)


#------------#
# Exercise 1 #
#------------#

# Part 1. What variables are in the data? What type are they?


# Part 2. Run summary on the dataset. 


# Part 3. Check out the codebook. What variables are we going to have to recode?


#------------#
# Exercise 2 #
#------------#

# Part 1. Let's start by recoding the variables EDUC, SEX, RACE, HISPAN
# rewrite this code using case_when instead of ifelse

ipums_data_recoded <- ipums_data %>%
                      mutate(new_educ = ifelse(EDUC == 1, 5, NA),
                             new_educ = ifelse(EDUC == 2, 9, new_educ),
                             new_educ = ifelse(EDUC == 3, 10, new_educ),
                             new_educ = ifelse(EDUC == 4, 11, new_educ),
                             new_educ = ifelse(EDUC == 5, 12, new_educ),
                             new_educ = ifelse(EDUC == 6, 13, new_educ),
                             new_educ = ifelse(EDUC == 7, 14, new_educ),
                             new_educ = ifelse(EDUC == 8, 15, new_educ),
                             new_educ = ifelse(EDUC == 9, 16, new_educ),
                             new_educ = ifelse(EDUC == 10, 17, new_educ),
                             new_educ = ifelse(EDUC == 11, 18, new_educ),
                             new_sex  = ifelse(SEX == 1, "Male", "Female"),
                             new_race = ifelse(RACE == 1, "White", NA),
                             new_race = ifelse(RACE == 2, "Black", new_race),
                             new_race = ifelse(RACE == 3, "American Indian or Alaska Native", new_race),
                             new_race = ifelse(RACE %in% c(4,5,6), "Asian or Pacific Islander", new_race),
                             new_race = ifelse(RACE == 7, "Other", new_race),
                             new_hispan = ifelse(HISPAN %in% c(1,2,3,4), "Hispanic", "Not Hispanic"))


# Part 2. Filter time! Please filter out all observations that are missing data for the variables: new_educ, 
#         AGE, SEX, MARST, EDUC


ipums_data_filtered <- ipums_data_recoded %>%
                       filter()


#------------#
# Exercise 3 #
#------------#

# Part 1. Please make a densiyt plot of wages by gender

ggplot(ipums_data_filtered) +
geom_density(aes(x = , color = , fill = ), alpha = 0.5, adjust = 2) +
labs(x = "Annual Income ($)",
     y = "Density",
     color = "",
     fill = "",
     title = "Distribution of Income by Gender",
     caption = "Source: Census") 

# Part 2. Please plot the distribution of wages by years of schooling.

ggplot(ipums_data_filtered) +
geom_density(aes(color = , fill = , x = ), alpha = 0.2) +
labs(x = "Annual Income ($)",
     y = "Density",
     color = "Years of Schooling",
     fill = "Years of Schooling",
     title = "Distribution of Income by Years of Schooling",
     caption = "Source: Census") 
  

#------------#
# Exercise 4 #
#------------#

# Part 1. Run a simple regression of salery on years of education. Check out the results

baseline_model <- lm(    , data = ipums_data_filtered, weights = PERWT)

summary(baseline_model)

# Part 2. Install and load the broom package

#install.packages("broom")
libary(broom)

# Part 3. Tidy up the baseline model

baseline_model_tidy <- tidy(baseline_model)

str(baseline_model_tidy)

# Part 4. Augment the baseline model


# Part 5. Glance the baseline model


#------------#
# Exercise 5 #
#------------#

# Part 1. Make a density plot of residuals by Gender.
ggplot(baseline_model_augmented) +
geom_density(aes(color = , x = )) +
labs(x = "Residual",
     y = "Density",
     title = "Distribution Residuals by Gender",
     color = "") +
theme_minimal()


# Part 2. Run a regression of salery on years of education and gender. What do the results mean?

educ_gender_model <-

#------------#
# Exercise 6 #
#------------#

# Part 1. Use stargazer to make a table of summary statistics for ipums_data_filtered 

#insall.packages("stargazer")
library(stargazer)


stargazer(ipums_data_filtered)

# Part 2. Use stargazer to make a table of model results

stargazer()


#------------#
# Exercise 7 #
#------------#

# Build your own model!

# What variables will you add why?

# A table showing some summary statistics for your cut of the data

# A chart showing some information for your cut of the data

# A regression testing your model

# What do these results mean?
