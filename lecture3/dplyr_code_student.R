## Federal Reserve Board of Governors
## Howard University
## Data Analysis in R Course
## Lecture 3: Introduction to Dplyr
## INSTRUCTOR CODE

## Set your directory
acs <- read.csv()


# Examining our Data

names(acs)
ncol(acs)

# Install Dplyr


# dplyr::select
income_data <- select(acs, AGEP, SEX, WAGP, WKHP,
                      PINCP, ADJINC, PWGTP, PUMA10)
names(income_data)

# In Class Exercise: Select -----------------------------------------------


# Filtering our data
income_data <- filter(income_data, 
!(is.na(AGEP) | is.na(PINCP)),
!is.na(WKHP))

# Boolean logic: exercises
filter(acs, AGEP < 30)
filter(acs, AGEP < 30 & SEX == 1)
filter(acs, AGEP < 25 | AGEP > 65)
filter(acs, Occ %in% c("Computer/Math", "Legal"))

# filtering on multiple conditions
identical(filter(income_data, AGEP == 24 & SEX == 1),
filter(income_data, AGEP == 24, SEX == 1))

# In Class Exercise: Filter -----------------------------------------------

#  Re-assign your exercise_1_data data.frame to exercise_2_data and filter out any `NA` values in the columns
#  Additionally, make sure that your WKHP variable is greater or equal to 10 and less than or equal to 60
#  Your data should have the following dimensions:



# New Object creation workflow
object1 <- select(acs, AGEP, SEX, WAGP,
PINCP, ADJINC, PWGTP)

object2 <- filter(object1,
!(is.na(AGEP) | is.na(PINCP)))

# Nested Functions example
income_data_nest <- filter(select(acs, AGEP, SEX, WAGP,
PINCP, ADJINC, PWGTP),
!(is.na(AGEP) | is.na(PINCP)))

identical(object2, income_data_nest)

# Pipe Operators
income_data_pipe <- acs %>% 
select(AGEP, SEX, WAGP, PINCP, ADJINC, PWGTP) %>% 
filter(!(is.na(AGEP) | is.na(PINCP)))

identical(income_data_nest, income_data_pipe)

# Pipes Explained
25 %>% seq(30, by = 1)
25 %>% seq(30, ., by = -1)

# Mutating our data
income_data %>% 
mutate(ADJINC = ADJINC/1000000) %>% 
head(3)

# Updating Income and Wages
adjusted_data <- income_data %>% 
    mutate(ADJINC = ADJINC/1000000,
           PINCP = PINCP*ADJINC,
           WAGP = WAGP*ADJINC)
head(adjusted_data, 3)

# Updating Gender
gender_hours_data <- adjusted_data %>% 
mutate(gender = ifelse(SEX == 1, "Male", "Female"),
Hours = plyr::round_any(WKHP, 5, ceiling))

gender_hours_data %>% 
select(SEX, gender, WKHP, Hours) %>% 
head()


# In Class Exercise: Mutate -----------------------------------------------

# Now it's time to update your data: exercise_2_data
# Update SEX, ADJINC, and WKHP as seen previously
# Update WAGP to account for the adjustment factor


# Deleting your columns
"SEX" %in% names(gender_hours_data)

gender_hours_data <- gender_hours_data %>% 
    mutate(SEX = NULL, 
           WKHP = NULL)

"SEX" %in% names(gender_hours_data)

# The fun part: Summarize
gender_hours_data %>% summarise(income = mean(PINCP),
                                wages = mean(WAGP))

# Grouping
income_data_summary <- gender_hours_data %>%
    group_by(AGEP, gender) %>%
    dplyr::summarise(income = weighted.mean(PINCP,
                                            PWGTP),
                     wages = weighted.mean(WAGP, PWGTP))
head(income_data_summary, 3)


# In Class Exercise: group_by and summarize -------------------------------
# Using the `group_by()` and `summarize()` functions find the mean and median values for each gender/hour combination
# save your output as `exercise_4_data`
# Your data should look like the following:


# Finding the top and bottom with arrange

# order by gender and descending age
income_data_summary %>%
    arrange(gender, desc(AGEP)) %>% head(3)

# Show the top earners
income_data_summary %>%
    arrange(desc(income)) %>%
    head(3)



# In Class Exercise: arrange
# * Using your `exercise_4_data` find the bottom five observations for average wages
# * You do not have to save your output to a variable
# * what is their gender, the hours worked?

exercise_4_data %>% 
arrange(wages) %>% 
head(5)


# Answering our Question
library(ggplot2)
library(tidyr)
inc_wages_plot <- income_data_summary %>%
    gather(key = payment, value = amount,-AGEP,-gender) %>%
    ggplot(aes(
        x = AGEP,
        y = amount,
        linetype = payment,
        color = gender
    )) +
    geom_line() +
    labs(
        title = "Income and Wages",
        linetype = "Payment\nType",
        color = "Gender",
        subtitle = "California 2010-2014 average",
        x = "Age",
        y = "2014 USD",
        caption = "Data obtained from Census Bureau"
    )
inc_wages_plot



# Widening our age ranges

income_data_wide_ages <- gender_hours_data %>%
    mutate(age = plyr::round_any(AGEP, 5)) %>%
    group_by(age, gender) %>%
    dplyr::summarise(wages = weighted.mean(WAGP, PWGTP),
                     income = weighted.mean(PINCP, PWGTP))

income_data_wide_ages %>%
    gather(key = payment, value = amount,-age,-gender) %>%
    ggplot(aes(
        x = age,
        y = amount,
        color = gender,
        linetype = payment
    )) +
    geom_line() +
    labs(
        title = "Income and Wages",
        subtitle = "5-year age buckets",
        x = "age",
        y = "2014 USD",
        color = "Gender",
        linetype = "Payment\nType",
        caption = "Data obtained from Census"
    )



# In Class Exercise: Summarize --------------------------------------------
# * Using the `exercise_3_data` create a chart showing mean wages for men and women by hours worked


# What message does this chart show about wage inequality compared with our previous chart? What more questions do you have?

# Joins
county_xwalk <- read.csv("Data/puma10_county_xwalk.csv", 
stringsAsFactors = F)
head(county_xwalk, 3)

# Joining our Data
joined_data <- gender_hours_data %>% 
inner_join(county_xwalk, by = c("PUMA10" = "puma12")) 
names(joined_data)

# County Level Statistics
joined_data %>%
    group_by(gender, county) %>%
    dplyr::summarise(wages = weighted.mean(WAGP, PWGTP)) %>%
    arrange(gender, desc(wages)) %>%
    ungroup() %>%
    group_by(gender) %>%
    slice(1:5) %>%
    spread(key = gender, value = wages)

# County Income Ratio
joined_data %>%
    group_by(county, gender) %>%
    summarize(wages = weighted.mean(WAGP, PWGTP),
              observations = sum(PWGTP)) %>%
    mutate(ratio = wages / lag(wages)) %>%
    filter(!is.na(ratio)) %>%
    arrange(desc(ratio)) %>%
    select(county, ratio, observations) %>%
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


# Challenge Exercise 2
# * Using dplyr and ggplot analyze how wage (WAGP) changes over age between men and women 
# * control for number of hours worked 
# * 10 hour buckets instead of 5
# * Limit to people who work between 20 and 60 hours per week
# * Bucket sizes for age should be 5 years
# * PWGTP
# * You will need to start with the original ACS data to answer this

# Challenge Exercise 2: Answer


#Insights? Further Questions? Is this a "good" chart?
