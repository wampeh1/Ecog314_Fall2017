## Federal Reserve Board of Governors
## Howard University
## Customizing the aesthetic in ggplot2 and mapmaking
## INSTRUCTOR CODE


# Setup
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(maps)
library(viridis)

theme_set(theme_gray())

acs <-
    read.csv(
        "Data/acs_large.csv",
        stringsAsFactors = F
    )
# ACD data stats
dim(acs)
names(acs)


# Warm Up Exercise --------------------------------------------------------

# * As a warm up exercise create a plot of the weighted mean wage by education level and age, it should look like this:* PWGTP is the column of weights, WAGP is the wage, AGEP is age, and eduStat is the education level
warm_up_plot <- acs %>% select(AGEP, eduStat, WAGP, PWGTP) %>%
    group_by(AGEP, eduStat) %>%
    summarise(wage = weighted.mean(WAGP, PWGTP)) %>%
    ggplot(aes(
        x = AGEP,
        y = wage,
        linetype = eduStat,
        color = eduStat
    )) +
    geom_line() +
    ggtitle("Weighted mean wage by education level") +
    labs(
        x = "Age",
        y = "2014 USD",
        caption = "Data obtained from ACS",
        color = "Education",
        linetype = "Education"
    )
warm_up_plot

# ggplot: example data
smry_table <- acs %>% select(AGEP, eduStat, WAGP, PWGTP) %>%
    group_by(AGEP, eduStat) %>%
    summarise(wage = weighted.mean(WAGP, PWGTP))

data.frame(
    "x" = smry_table$AGEP,
    "y" = smry_table$wage,
    "color" = smry_table$eduStat,
    "size" = 1,
    "linetype" = smry_table$eduStat
) %>%
    head(4)

# ggplot: scale mapping
data.frame(
    "x" = smry_table$AGEP,
    "y" = smry_table$wage,
    "color" = c("red", "green", "orange", "blue", "black"),
    "size" = 1,
    "linetype" = smry_table$eduStat
) %>%
    head(4)

# Scale functions
scale_color_manual_plot <- warm_up_plot +
    scale_color_manual(
        "Education",
        values = c(
            "Graduate" = "red",
            "Bachelors" = "blue",
            "Associates" = "darkgoldenrod",
            "Some College" = "green4",
            "HS Diploma" = "blueviolet",
            "No HS Diploma" = "black"
        )
    )

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
   scale_x_continuous("Age", breaks = seq(20,75,5)) +
    labs(
        title = "Weighted mean wages",
        color = "Highest\nDegree",
        linetype = "Highest\nDegree",
        caption = "Data obtained from census"
    )

breaks_plot +
    scale_y_continuous("2014 USD", breaks = seq(25, 125, 25)*1000)

# The scales package
library(scales)

# Putting dollars on the axis
breaks_plot + scale_y_continuous("2014 USD",
                                 labels = dollar,
                                 breaks = seq(25, 125, 25)*1000)


# In Class Exercise: Color Guide ------------------------------------------

# * Now change color legend
# * Order by degree
# * We just saw a function used to change the tick marks on the x - axis
# * Using this information and the plots we already created, create a chart with a re -
#     ordered color guide

#

full_time_wages %>%
    ggplot(aes(
        x = age,
        y = wages,
        linetype = eduStat,
        color = eduStat
    )) +
    geom_line() +
    scale_x_continuous("Age", breaks = seq(20, 75, 5)) +
  scale_color_discrete(
    "Highest\nDegree",
    breaks = c(
      "Graduate",
      "Bachelors",
      "Associates",
      "Some College",
      "HS Diploma",
      "No HS Diploma"
    )) +
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
    ) +
    labs(title = "Weighted mean wages",
         caption = "Data obtained from ACS") +
    scale_y_continuous("2014 USD",
                       labels = dollar,
                       breaks = seq(0, 125, 25) * 1000)

# ---------------------------------------------------------------------------

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

# Centering the title
coder_plot +
    theme(plot.title = element_text(
        face = "italic",
        color = "blue",
        hjust = 0.5
    ))


# In Class Exercise: Element_text and element_rect -----------------------------------------
coder_plot +
  theme(plot.title = element_text(face = "bold",
                                  color = "green",
                                  hjust = 1),
        axis.title.x = element_text(color = "blue",
                                    angle = 90),
        axis.title.y = element_text(color = "red",
                                    size = 12),
        panel.background = element_rect(fill = "lavender", color = "red"),
        #panel.grid = element_blank(),
        plot.background = element_rect(fill = "honeydew"))

# -------------------------------------------------------------------------

# Occupation Breakdown
occupations <- c("Admin Support",
                 "Sales",
                 "Construction",
                 "Legal",
                 "Healthcare",
                 "Computer/Math")
six_occ_data <- acs %>% select(PWGTP, eduStat, Occ) %>%
    filter(!is.na(Occ)) %>%
    group_by(eduStat, Occ) %>%
    summarise(count = sum(PWGTP)) %>%
    ungroup() %>% group_by(eduStat) %>%
    mutate(perc = count / sum(count)) %>%
    filter(Occ %in% occupations)

# Occupation Plot
six_occ_plot <- six_occ_data %>%
    ggplot(aes(x = eduStat, y = perc, fill = Occ)) +
    geom_bar(stat = "identity", color = "black") +
    labs(
        title = "Occupation breakout by Education Level",
        caption = "Data from American Community Survey",
        x = NULL,
        fill = NULL
    ) +
    theme(legend.position = "bottom") +
    scale_y_continuous(NULL, labels = percent)
six_occ_plot

# Changing the Panel Background

six_occ_plot +
    theme(plot.background = element_rect(fill = "bisque",
                                         color = "black", size = 1))

# In Class Exercise: Panel ------------------------------------------------

# * Area where the data are plotted is called the "panel"
# * Using the above code as a guide, update the panel for `six_occ_plot` to look like the below, the blue color is "aquamarine"

six_occ_plot + theme(panel.background = element_rect(fill = "aquamarine",
                                                     color = "red", size = 2))

# What do the `color` and `size` arguments of `element_rect()` control?
# --------------------------------------------------------------------

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


# Faceting by Gender
gender_educ_plot +
    facet_wrap("sex", nrow = 1) +
  labs(caption = "Data from ACS")


# In Class Exercise: Faceting ---------------------------------------------

# * What percent of the male / female populations have each degree level?
# * Use faceting to show a different plot for each level of education
# * Restrict our dataset to indivudals who are at least 25 years old

gender_educ %>%
    filter(age >= 25) %>%
    ggplot(aes(
        x = age,
        y = percent,
        linetype = sex,
        color = sex
    )) +
    facet_wrap("eduStat", nrow = 2, ncol = 3) +
    geom_line() +
    labs(
        title = "Education as percent of total gender population",
        color = "Gender",
        linetype = "Gender",
        caption = "Data from ACS"
    ) +
    scale_y_continuous(NULL, label = percent,
                       breaks = seq(0, 0.3, 0.05)) +
    scale_x_continuous("Age", breaks = seq(25, 75, 10))


# Setting up our geographic data
library(maps)
library(viridis)

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
        x = long,
        y = lat,
        group = group,
        fill = wage
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

# * Using our ACS data, find the 5 occupations with the highest full - time average salary
# * aka working 35 + hours
# * use weighted observations for count
# * Call the data frame for your answer:`high_paying_occs`, it should look like this:```{
high_paying_occs <- acs %>%
    filter(WKHP >= 35) %>%
    select(Occ, WAGP, PWGTP) %>%
    group_by(Occ) %>%
    summarise(wages = weighted.mean(WAGP, PWGTP)) %>%
    arrange(-wages) %>%
    head(5)
high_paying_occs


# Data prep for our Occupations
wealthy_worker_data <- acs %>%
    select(Occ, WKHP, PUMA10, PWGTP) %>%
    filter(WKHP >= 35, !is.na(PUMA10)) %>%
    group_by(PUMA10) %>%
    summarise(
        population = sum(PWGTP),
        wealthy_occ = sum(PWGTP * Occ %in%
                              high_paying_occs$Occ),
        percentage = wealthy_occ / population
    ) %>%
    select(PUMA10, percentage)


# In Class Exercise: Occupation Map ---------------------------------------

# * Using the code from our previous map as a template, and the `wealthy_worker_data` just created
# * make a heatmap showing the percentage of each PUMA's full-time employed population that works in a high-wage industry.

wealthy_worker_data %>%
    left_join(pumas.points, by = c("PUMA10" = "id")) %>%
    ggplot(aes(
        x = long,
        y = lat,
        group = group,
        fill = percentage
    )) +
    geom_polygon(size = 0.05, color = "black") +
    coord_map() +
    scale_fill_viridis(name = NULL, labels = percent) +
    ggtitle("Percentage of full-time population\nin high wages occupations") +
    labs(x = NULL, y = NULL, caption = "Data from ACS")+
  theme(panel.background = element_blank())

# Challenge Exercises
# * For the rest of class take a look at creating the following infographics:
# * We want to see how wages for men and women across the state of California
# * For your first chart show the average wages for full-time employed men and women in each PUMA for the entire state, graph these two maps side by side
# * Hint: What function did we look at earlier today that allows us to show multiple charts side by side?
# * For the second chart we want to look at the San Francisco area in particular and show the wage percentage that full-time employed women make compared to full-time employed men in each PUMA

# Challenge Exercise 1
acs %>% select(PUMA10, WKHP, sex, WAGP, PWGTP) %>%
    filter(WKHP >= 35,!is.na(PUMA10)) %>%
    group_by(sex, PUMA10) %>%
    summarise(wages = weighted.mean(WAGP, PWGTP)) %>%
    left_join(pumas.points, by = c("PUMA10" = "id")) %>%
    ggplot(aes(
        x = long,
        y = lat,
        group = group,
        fill = wages
    )) +
    coord_map() +
    geom_polygon(size = 0.05, color = "black") +
    facet_wrap("sex", nrow = 1, ncol = 2) +
    scale_fill_viridis("2014 USD", labels = dollar) +
    labs(
        title = "Average Wages",
        subtitle = "Sample limited to working at least 35 hours per week",
        x = NULL,
        y = NULL,
        caption = "Data from ACS"
    )

# What differences do you notice between the maps?

# Challenge Exercise 2
acs %>% select(PUMA10, WKHP, sex, WAGP, PWGTP) %>%
    filter(WKHP >= 35,!is.na(PUMA10)) %>%
    group_by(sex, PUMA10) %>%
    summarise(wages = weighted.mean(WAGP, PWGTP)) %>%
    ungroup() %>% group_by(PUMA10) %>%
    mutate(percentage = wages / (sum(wages) - wages)) %>%
    filter(sex == "Male") %>%
    left_join(pumas.points, by = c("PUMA10" = "id")) %>%
    ggplot(aes(
        x = long,
        y = lat,
        group = group,
        fill = percentage
    )) +
    geom_polygon(size = 0.05, color = "black") +
    coord_map(xlim = c(-122.8,-121.75), ylim = c(37, 38.2)) +
    scale_fill_viridis(NULL, labels = percent) +
    labs(
        title = "Percentage Wage Male/Female",
        subtitle = "SF Bay Area, 2012-2014 Average",
        x = NULL,
        y = NULL,
        caption = "Full-time employed adults, Data from ACS"
    )

# Hint: You will need the `xlim` and `ylim` arguments to `coord_map()`