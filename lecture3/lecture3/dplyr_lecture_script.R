## FRB/Howard University ECON 181
## Lecture 3: Introduction to Dplyr
## 9/8/2017
## Instructor: Simeon Markind

## SET THIS UP BEFORE LECTURE
## Where R thinks I am in my computer:
getwd()

# Where I want R to be:
lecture3_dir <- ""

# Tell R to go where I want:
setwd(lecture3_dir)

## Install the dplyr package if necessary
## library the dplyr package
# library/install tidyr
# library/install ggplot2

# BEGIN LECTURE HERE
# Read in the data


# Examine the data
# column names? number of columns? data dimensions? data classes?

# Select function
# create income_data by selecting the AGEP, SEX, WAGP, WKHP, PINCP, ADJINC, PWGTP, PUMA10 columns from acs

# names of selected columns?

# Deselect columns

# In Class Exercise: Select -----------------------------------------------
# create a dataframe with the SEX, WKHP, ADJINC, PWGTP, and WAGP columns

# exercise_1_data <- 

#------------------------------------

# Filtering our data
# Return rows of income_data where either AGEP or PINCP are not NA, and WKHP is not NA

# Boolean logic: exercises
# Ages less than 30

# Ages less than 30 and male

# ages less than 25 or ages greater than 65

# Filtering on multiple condition, & is the same as ,


# In Class Exercise: Filter -----------------------------------------------

#  Re-assign your exercise_1_data data.frame to exercise_2_data and filter out any `NA` values in the columns
#  Additionally, make sure that your WKHP variable is greater or equal to 10 and less than or equal to 60
#  Your data should have the following dimensions:
#exercise_2_data <-

# -----------------------------------------------------------

# Workflow by creating new objects
object1 <- select(acs, AGEP, SEX, WAGP,
                  PINCP, ADJINC, PWGTP)

object2 <- filter(object1,
                  !(is.na(AGEP) | is.na(PINCP)))

# Workflow by nesting functions
income_data_nest <- filter(select(acs, AGEP, SEX, WAGP,
                                  PINCP, ADJINC, PWGTP),
                           !(is.na(AGEP) | is.na(PINCP)))

identical(object2, income_data_nest)

# workflow with pipe operator

# Pipes explained

# Using mutate to update the ADJINC column
income_data %>% 
    mutate(ADJINC = ADJINC/1000000) %>% 
    head(3)

# Updating income and wages
# Create new object, adjusted_data in which you update ADJINC and then multiply PINCP and WAGP by updated ADJINC

#adjusted_data <-

#head(adjusted_data, 3)

# Updating gender
# use the adjusted_data to create gender and Hours columns from SEX and WKHP columns
# use ifelse and plyr::round_any

#gender_hours_data <-

gender_hours_data %>% 
    select(SEX, gender, WKHP, Hours) %>% 
    head()

# Case_When function (ifelse for multiple criteria)
# Use the case_when function to update the SEX column of our adjusted_data


# In Class Exercise: Mutate -----------------------------------------------

# Now it's time to update your data: exercise_2_data
# Update SEX, ADJINC, and WKHP as seen previously
# Update WAGP to account for the adjustment factor

#exercise_3_data <-

# --------------------------------------------------------------------------

# Deleting columns with NULL
"SEX" %in% names(gender_hours_data)
# Set the SEX and WKHP column in gender_hours_data to NULL


"SEX" %in% names(gender_hours_data)


# Summarize
# Find the mean of income and wages for the gender_hours_data with the summarise function

# Grouping Find the mean of income and wages for each age/gender grouping (don't forget we have weighted data)
#income_data_summary <- 

#head(income_data_summary)

# In Class Exercise: group_by and summarize -------------------------------
# Using the `group_by()` and `summarize()` functions find the mean values for each gender/hour combination
# save your output as `exercise_4_data`
# Your data should look like the following:

#exercise_4_data <- exercise_3_data %>% 


# --------------------------------------------------------------------------

# use arrange to find the top and bottom of data

# Order income_data_summary by gender and descending age

# Show the top earners in our income_data_summary table


# In Class Exercise: arrange ----------------------------------------------
# * Using your `exercise_4_data` find the bottom five observations for average wages
# * You do not have to save your output to a variable
# * what is their gender, the hours worked?

#exercise_4_data %>% 

#--------------------------------------------------------------------------

# Answering our question
# Use tidyr::gather() to turn our wide data into long data


#income_data_plot <- income_data_summary %>% 
# gather()

## Use ggplot2 to create a line plot of our data


# inc_wages_plot <-  income_data_plot %>% 
#     ggplot(aes(x = , y = ,
#                linetype = ,
#                color = )) +
#     geom_line() +
#     labs(title = ,
#          linetype = ,
#          color = ,
#          subtitle = ,
#          x = , y = ,
#          caption = )
# inc_wages_plot


# Widening our age ranges
## mutate our gender_hours_data to have 5 year age ranges instead of 1-year and make a line plot like above

#income_data_wide_ages <- gender_hours_data %>% 

# Plot our wide age data
# (hint use the code above)

# In Class Exercise: Summarize --------------------------------------------
# * Using the `exercise_3_data` create a chart showing mean wages for men and women by hours worked

#hourly_line_plot <- exercise_3_data %>% 

# What message does this chart show about wage inequality compared with our previous chart? What more data would you want?

# -----------------------------------------------------------------------------

# Joining data
# read in the puma10_county_xwalk.csv file

#county_xwalk <- 

# Joining our data
# Use an inner join to merge our gender_hours_data with county_xwalk (what is the column to merge on?)

#joined_data <- 

# County level Statistics

# We want to know which counties have the highest wages for men and women
joined_data %>%
    group_by(gender, county) %>% # want data by county and gender
    dplyr::summarise(wages = weighted.mean(WAGP, PWGTP)) %>% #calculate our summary statistics
    arrange(gender, desc(wages)) %>% # order our data the way we want
    ungroup() %>% # need to eliminate our grouping by county
    group_by(gender) %>% # only want to group by gender
    slice(1:5) %>%
    spread(key = gender, value = wages)

# County Income Ratio
# Find the counties with the highest ratio of male/female income
joined_data %>%
    group_by(county, gender) %>%
    summarize(wages = weighted.mean(WAGP, PWGTP)) %>% 
    mutate(ratio = wages / lag(wages)) %>%
    filter(!is.na(ratio)) %>%
    arrange(desc(ratio)) %>%
    select(county, ratio) %>%
    head()


# Challenge exercise 1
# * Wages seem to increase dramatically with hours worked
# * Next step: bar chart showing the percentage of men and women in each 5 hour bucket
# 
# 1.  Find the total number of observations in each hour/gender combination
# + group_by() and dplyr::summarise()
# 2. Change your grouping to just the hour buckets 
# + use mutate to calculate the total number of observations in each bucket and the the percentage of men/women in each bucket
# 3. Plot with `ggplot()` and `geom_bar()` etc..., your `y` value should be the percentage
# + in the `geom_bar()` make sure to include `stat = "identity"`

# Challenge Exercise 1 Answer
# exercise_3_data %>% 
#     group_by() %>% 
#     dplyr::summarize() %>%
#     ungroup() %>% 
#     group_by() %>% 
#     mutate() %>% 
#     ggplot() +

# ----------------------------------------------------------------

# Challenge Exercise 2 ----------------------------------------------
# * Using dplyr and ggplot analyze how wage (WAGP) changes over age between men and women 
# * control for number of hours worked 
# * 10 hour buckets instead of 5
# * Limit to people who work between 20 and 60 hours per week
# * Bucket sizes for age should be 5 years
# * PWGTP
# * You will need to start with the original ACS data to answer this

# Challenge Exercise 2: Answer
# gender_hours_age <- acs %>% 
#     select() %>% 
#     filter() %>% 
#     mutate(Gender = ,
#            Wages = ,
#            Hours = ,
#            Age = ) %>% 
#     select() %>% 
#     group_by() %>% 
#     summarise()
# 
# gender_hours_age %>% 
#     ggplot() +


#Insights? Further Questions? Is this a "good" chart?


















