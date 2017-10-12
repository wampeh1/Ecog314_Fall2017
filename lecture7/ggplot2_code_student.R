## Federal Reserve Board of Governors
## Howard University
## Customizing the aesthetic in ggplot2 and mapmaking
## STUDENT CODE


# Library the following packages - install if needed
library(tidyverse)
library(scales)
library(maps)
library(viridis)

# Set working directory
setwd()

# Ready in data
acs <- read.csv("./Data/acs_large.csv", 
                stringsAsFactors = F)

# ACS data stats
dim(acs)
names(acs)


# Review of previous plotting  --------------------------------------------------------

# Let's create a plot of the weighted mean wage by education level and age 
# PWGTP is the column of weights, WAGP is the wage, AGEP is age, and eduStat is the education level
warm_up_plot <- acs %>% select(AGEP, eduStat, WAGP, PWGTP) %>% 
  group_by(AGEP, eduStat) %>% 
  summarise(wage = weighted.mean(WAGP, PWGTP)) %>% 
  ggplot(# Fill in to get appropriate plot
    ) +
  # What else do we need? +
  ggtitle("Weighted mean wage by education level") +
  labs(x = "Age", y = "2014 USD", caption = "Data obtained from ACS", color = "Education", linetype = "Education")
warm_up_plot

# Scale functions
scale_color_manual_plot <- warm_up_plot +
 #   scale_color_manual(set the title and specify the colors for the lines)

scale_color_manual_plot

# Scale Functions in Action
full_time_wages <- acs %>% filter(WKHP >= 35) %>%  # full time
    select(eduStat, WAGP, PWGTP, AGEP) %>%
    mutate(age = plyr::round_any(AGEP, 5)) %>%
    group_by(eduStat, age) %>%
    summarise(wages = weighted.mean(WAGP, PWGTP))

breaks_plot <- full_time_wages %>%
    ggplot(aes(
        x = age,
        y = wages,
        linetype = eduStat,
        color = eduStat
    )) +
    geom_line() +
    ## Add scale_x_continuous call here
    labs(
        title = "Weighted mean wages",
        color = "Highest\nDegree",
        linetype = "Highest\nDegree",
        caption = "Data obtained from census"
    )

breaks_plot +
    ## add scale_y_continuous call here

# Now we'll be using the "scales" package you installed earlier
# Let's put dollars on the axis
breaks_plot <- breaks_plot + scale_y_continuous("2014 USD",
                                 # set the breaks
                                 labels = dollar)
breaks_plot


# In Class Exercise: Color Guide ------------------------------------------

# * Now change color legend
# * Order by degree
# * We just saw a function used to change the tick marks on the x - axis
# * Using this information and the plots we already created, create a chart with a re -
#     ordered color guide

breaks_plot + 
  # adjust the color and linetype scales to have degrees in descending order:
  # Graduate, Bachelors, Associate, Some college, HS Diploma, No HS Diploma

# ------------------------------------------------------------------------

# How do Coders do?

coder_data <- acs %>%
    filter(WKHP >= 35,  #We want full-time employed
           Occ == "Computer/Math") %>%
    select(eduStat, AGEP, WAGP, PWGTP) %>%
    mutate(age = plyr::round_any(AGEP, 5)) %>%
    group_by(age, eduStat) %>%
    dplyr::summarise(wages = weighted.mean(WAGP, PWGTP))

coder_plot <- coder_data %>%
    ggplot(aes(x = age, y = wages, color = eduStat)) +
    geom_line() +
    labs(title = "Programmer wages",
         x = "Age",
         caption = "Data from ACS") +
    scale_color_discrete(
        "Highest\nDegree",
        breaks = c(
            "Graduate",
            "Bachelors",
            "Associates",
            "HS Diploma",
            "No HS Diploma"
        )
    ) +
    scale_y_continuous("2014 USD", labels = dollar)

coder_plot

# Centering the title
?theme
?element_text
coder_plot +
    theme(# adjust plot title appropriately
      )


# In Class Exercise: Element_text and element_rect -----------------------------------------
# make title green, bolded, and right-justified
# make your x axis text blue, vertical, and left justified
# make y axis text red
# make the panel lavender with a red outline
# make the plot background honeydew
?element_rect

coder_plot + 
  theme( # fill in theme arguments
    )

# -----------------------------------------------------------------------------------------

# Faceting
gender_educ <- acs %>%
    select(sex, PWGTP, AGEP, eduStat) %>%
    mutate(age = plyr::round_any(AGEP, 5)) %>%
    group_by(sex, age, eduStat) %>%
    summarise(count = sum(PWGTP)) %>%
    ungroup() %>% group_by(sex, age) %>%
    mutate(percent = count / sum(count))

# Faceting in Action
gender_educ_plot <- gender_educ %>%
    ggplot(aes(
        x = age,
        y = percent,
        linetype = eduStat,
        color = eduStat
    )) +
    geom_line() +
    labs(title = "Education Achievement for each Gender",
         x = "Age") +
    scale_y_continuous(NULL, labels = percent) +
    scale_color_discrete(
        "Highest\nDegree",
        breaks = c(
            "Graduate",
            "Bachelors",
            "Associates",
            "Some College",
            "HS Diploma",
            "No HS Diploma"
        )
    ) +
    scale_linetype_discrete(
        "Highest\nDegree",
        breaks = c(
            "Graduate",
            "Bachelors",
            "Associates",
            "Some College",
            "HS Diploma",
            "No HS Diploma"
        )
    )

gender_educ_plot

# Faceting by Gender
## use the facet wrap function to make separate plots for Male and Female
gender_educ_plot +
  facet_wrap("sex", nrow = 1)


# In Class Exercise: Faceting ---------------------------------------------

# What percent of the male / female populations have each degree level?
# Use faceting to show a different plot for each level of education
# Restrict our dataset to indivudals who are at least 25 years old

gender_educ %>%
  filter(# add restrictions here
         ) %>% 
  ggplot(# fill in
         ) +
  # add facet wrap call here +
  geom_line() +
  labs(title = "Education as percent of total gender population",
       color = "Gender", linetype = "Gender",
       caption = "Data from ACS") +
  scale_y_continuous(NULL, label = percent,
                     breaks = seq(0, 0.3, 0.05)) +
  scale_x_continuous("Age", breaks = seq(25, 75, 10))
  
# -------------------------------------------------------------------------

# Mapping! --------------------------------------------------------------------------------------------

# Setting up our geographic data

wage_data <- acs %>% select(PUMA10, WAGP, PWGTP) %>%
    filter(!is.na(PUMA10)) %>%
    group_by(PUMA10) %>%
    summarise(wage = weighted.mean(WAGP, PWGTP))

# Map data file
pumas.points <- read.csv("Data/pumas_points.csv",
                         stringsAsFactors = F)
wage_map_data <- pumas.points %>%
    left_join(wage_data,
              by = c("id" = "PUMA10"))

# Making the map

wage_map <- wage_map_data %>%
    ggplot(aes(
        #set up x, y, group, fill aesthetics
    )) +
    geom_polygon(color = "black", size = 0.05)
wage_map

# Improving our Map

improved_map <- wage_map +
    coord_map() +
    scale_fill_viridis("2014 USD", labels = dollar) +
    ggtitle("Wages for California",
            "2012-2014 average") +
    labs(caption = "Data from ACS") +
  theme(panel.background = element_blank())

improved_map


# In Class Exercise: Occupations ------------------------------------------

# Using our ACS data, find the 5 occupations with the highest full - time average salary
# aka working 35 + hours
# use weighted observations for count
# HINT: you may need the arrange() function to get the numbers in the right order

high_paying_occs <- acs %>% 
  # filter to only full time
  # select your wage, weighting and occupation columns
  # find the weighted mean wages by each occupation
  # then order and find the top 5
  
# -------------------------------------------------------------------------

# Data prep for our Occupation map
wealthy_worker_data <- acs %>%
    select(Occ, WKHP, PUMA10, PWGTP) %>%
    filter(WKHP >= 35, !is.na(PUMA10)) %>%
    group_by(PUMA10) %>%
    summarise(
        population = sum(PWGTP),
        wealthy_occ = sum(PWGTP * Occ %in% high_paying_occs$Occ),
        percentage = wealthy_occ / population
    ) %>%
    select(PUMA10, percentage) %>% 
  left_join(pumas.points, by = c("PUMA10" = "id"))


# In Class Exercise: Occupation Map ---------------------------------------

# * Using the code from our previous map as a template, and the `wealthy_worker_data` just created
# * make a heatmap showing the percentage of each PUMA's full-time employed population that works in a high-wage industry.

wealthy_worker_data %>% 
  ggplot(# fill in x, y, fill, and group aesthetics
    )
  # + what else did we have in our code for wage_map and improved_map?

# -------------------------------------------------------------------------

# Challenge Exercises
# * For the rest of class take a look at creating the following infographics:
# * We want to see how wages for men and women across the state of California
# * For your first chart show the average wages for full-time employed men and women in each PUMA for the entire state, graph these two maps side by side
# * Hint: What function did we look at earlier today that allows us to show multiple charts side by side?
# * For the second chart we want to look at the San Francisco area in particular and show the wage percentage that full-time employed women make compared to full-time employed men in each PUMA

# Challenge Exercise 1

# What differences do you notice between the maps?

# Challenge Exercise 2


# Hint: You will need the `xlim` and `ylim` arguments to `coord_map()`