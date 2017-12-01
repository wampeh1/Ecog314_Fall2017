
# Load relevant libraries and set file path
library(data.table)
library(tidyverse)
library(lubridate)
library(viridis)
library(stringr)
library(stargazer)

path <- "your file path here"

## read in the necessary data
nyc_points <- read.csv(paste0(path, "nyc_points.csv"),header = T)
uber <- fread(paste0(path, "uber-raw-data-apr14.csv"))

ggplot() + 
  geom_polygon(data=nyc_points, aes(x=long, y=lat, group=group),
               color = "black",
               size = 0.15,
               fill = NA) +
  facet_wrap("Day", labeller = as_labeller(c("2014-04-08" = "Tuesday, April 8",
                                             "2014-04-12" = "Saturday, April 12"))) +
  geom_point(data = rush_hour, aes(x=Lon, y = Lat, size = count, color = Hour), 
             alpha = 0.25) +
  coord_map(xlim = c(-74.025, -73.89), ylim = c(40.67, 40.85)) +
  scale_color_viridis(direction = -1) +
  labs(title = "April 2014 Uber ride pick-ups",
       caption = "Darker color indicates later rides\nData from BetaNYC",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(color = "black", size = 0.5))

# -------------------------------
# Simple Example
# -------------------------------
DT = data.table(x=letters[1:10], y=1:10, z=rep( c("odd", "even"), 5))   # data table

#1.Basic aggregation 
DT[, sum(y), by = z]                                # data.table syntax
DF %>% group_by(z) %>% summarise(sum(y))            # dplyr syntax

#2.Basic update operation 
DT[, y := cumsum(y), by = z]
DF %>% group_by(z) %>% mutate(y = cumsum(y))

yellow.all <- fread(paste0(path, "yellow_ss_2014-04.csv"), na.strings = "")
head(yellow.all, n = 3)
yellow.all[, V1 := NULL]

# -------------------------------
# Time for efficiency
# -------------------------------

## Uber data --- 564,516 obs with 4 variables
system.time(read.csv(paste0(path, "uber-raw-data-apr14.csv")))
system.time(fread(paste0(path, "uber-raw-data-apr14.csv")))

## Taxi data --- 200,000 obs with 19 variables
system.time(read.csv(paste0(path, "yellow_ss_2014-04.csv"),stringsAsFactors = F))
system.time(yellow.all <- fread(paste0(path, "yellow_ss_2014-04.csv"), na.strings = "")) # blank spaces are NAs

## drop variable that keeps track of random samples
head(yellow.all, n = 3)
yellow.all[, V1 := NULL]

# -------------------------------
# Basic Subsetting with i
# -------------------------------
# only want first 1000 observations
yellow.all[1:1000,]

# only want people who paid with a credit card
yellow.all[payment_type=="CRD",]

# only want people who tipped with a credit card
yellow.all[payment_type=="CRD" & tip_amount > 0,] %>% ggplot()

# -------------------------------
# Using Keys
# -------------------------------
setkey(yellow.all,payment_type)
yellow.all[J("CRD")]

# -------------------------------
# Selecting Columns with j
# -------------------------------
# only keep payment type and fare amount
yellow.all[, .(payment_type, fare_amount)]

# -------------------------------
# In Class Exercise #1:
# -------------------------------
# Create a data.frame of only payment type and fare amount for rides that cost more than $10 in **both** data.table and dplyr


# -------------------------------
# Renaming and mutating variables with j
# -------------------------------
yellow.all[, .(pickup_time = ymd_hms(pickup_datetime, tz = "EST"), 
               dist = trip_distance, fare = fare_amount)] 

# -------------------------------
# Taxi Cabs at a Glance
# -------------------------------
summary(yellow.all[,.(passenger_count,trip_distance,pickup_longitude,pickup_latitude,
              dropoff_longitude,dropoff_latitude, fare_amount, tip_amount)])

# -------------------------------
# Final Sample Selection
# -------------------------------
## removing trips that were stored or were improrperly catalouged
yellow.ss <- yellow.all %>% 
  mutate(p_time = ymd_hms(pickup_datetime, tz = "EST"), 
         d_time = ymd_hms(dropoff_datetime, tz = "EST")) %>%
  filter(payment_type =="CRD" & 
           trip_distance > 0 &
           store_and_fwd_flag != "Y" &
           pickup_latitude != 0 & 
           dropoff_latitude != 0 &
           passenger_count > 0) %>% 
  rename(fare = fare_amount,
         tip = tip_amount,
         dist = trip_distance,
         p_lat = pickup_latitude,
         p_long = pickup_longitude,
         d_lat = dropoff_latitude,
         d_long = dropoff_longitude) %>%
  select(p_time, d_time, fare, tip, dist,
         passenger_count,
         p_lat, p_long, d_lat, d_long)

# -------------------------------
# In Class Exercise #2:
# -------------------------------
## Translate dplyr code for above into data.table syntax 
yellow.ss <- yellow.all[ ,.( )]


# -------------------------------
# Updating Data in Place
# -------------------------------
## Create a rounded tip variable
yellow.all[, tip_whole := round(tip_amount)]

## round both the fare and the total amount
yellow.all[, c("fare_whole","total_whole") := .(round(fare_amount),round(total_amount))]

# -------------------------------
# In Class Exercise #3:
# -------------------------------
## Fill in the code below and create four new columns:
    # A dummy indicating the hour of the day
    # A dummy indicating the week of the year
    # A variable for the total time of the trip
    # A dummy indicating the day of the week
## Hint: Recall the properties of date objects and the lubridate package


day_names <- c("Monday","Tuesday","Wednesday",
               "Thursday","Friday","Saturday","
               Sunday")
yellow.ss <- as.data.table(yellow.ss)
yellow.ss[, c( , , , ) := .( , , as.numeric( ,type = "secs"), factor( , levels = day_names, labels = day_names))]


# -------------------------------
# Summarizing the data
# -------------------------------
## what is the average distance (in miles) and time (in minutes) of trips in april?
yellow.ss[,.(mean(dist), mean(trip_time))]

## what is the average distance (in miles) and fare of trips in april by weekday?
setkey(yellow.ss, weekday, hour)  
yellow.ss[,.(avg_distance = mean(dist), avg_time = mean(trip_time)), by = .(weekday)]

# -------------------------------
# In Class Exercise #4:
# -------------------------------
## Perhaps people tip better later in the evening?
## Translate the following code INTO dplyr:

yellow.ss[hour >= 22 | hour < 2, .(mean(fare), mean(tip), mean(tip/fare)), by = .(weekday, hour)]

# -------------------------------
# The .N Arguement
# -------------------------------
## number of rides on each weekday
yellow.ss[,.(num_rides = .N), by=weekday]
## number of rides that did not tip
yellow.ss[tip == 0,.(.N), by=weekday]
## recreate the average hourly fare, tips,
yellow.ss[tip == 0,.(test = sum(fare)/.N), by=weekday]

# -------------------------------
# Chaining Arguments
# -------------------------------
## calculate the the average amount of gross earnings earnings per mile for trips over 2 miles
yellow.ss[,gross_earnings := fare + tip][, gepm := gross_earnings/dist][dist > 2,.(max(gepm))]


# -------------------------------
# In-Class Exercise #5:
# -------------------------------
gas_prices <- read.csv(paste0(path, "nyc_gas_prices.csv"))
gas_prices <- as.data.table(gas_prices)

yellow.mpg <- yellow.ss[gas_prices, on = "week"]

## Use merge data and data.table chains to:
  # Create a new variable net earnings (Hint: How should we calculate cost of gas per mile)
  # Create a new variable net earnings per mile (nepm)
  # Then find the average nepm after noon

## FINALLY THE MAPS!

# -------------------------------
# Prepratory Data Work
# -------------------------------

nyc_points <- fread(paste0(path, "nyc_points.csv"))
yellow.all <- fread(paste0(path, "yellow_ss_2014-04.csv"),
                    drop = "V1",
                    na.strings = "")

## Read and transform the Uber data
uber <- fread(paste0(path, "uber-raw-data-apr14.csv"),
              check.names = T)

uber_summary <- uber[, .(Date.Time = mdy_hms(Date.Time),
                         Lat = round(Lat, digits = 3),
                         Lon = round(Lon, digits = 3))][, .(Day = as.Date(Date.Time),
                                                            Hour = as.numeric(format(Date.Time, "%H")),
                                                            Company = "Uber",
                                                            Lat, Lon)]

days_vec <-  as.Date(c("2014-04-08", "2014-04-12"))
uber_summary <- uber_summary[Day %in% days_vec, .(.N, Company), by = .(Day, Hour, Lat, Lon)][order(Hour)]


## Transform cab data 
yellow_summary <- yellow.all[pickup_latitude != 0 & 
                            pickup_longitude != 0, .(pickup_datetime = ymd_hms(pickup_datetime), 
                                                     Lat = round(pickup_latitude, digits = 3),
                                                     Lon = round(pickup_longitude, digits = 3))][, 
                                                    .(Day = as.Date(pickup_datetime),
                                                      Hour = as.numeric(format(pickup_datetime, "%H")),
                                                      Company = "Taxi",Lat, Lon)]

yellow_summary <- yellow_summary[Day %in% days_vec, .(.N, Company), by = .(Day, Hour, Lat, Lon)]

## Merge the data
uber_taxi <- merge(yellow_summary, uber_summary, by = c("Lat", "Lon", "Day", "Hour", "Company", "N"), all = TRUE) 

# -------------------------------
# Uber vs Taxis: Saturday
# -------------------------------

## Plot
uber_taxi_sat <- uber_taxi[Day == days_vec[2], ][order(Hour)]

ggplot() + 
  geom_polygon(data=nyc_points, aes(x=long, y=lat, group=group),
               color = "black",
               size = 0.15,
               fill = NA) +
  facet_wrap("Company") +
  geom_point(data = uber_taxi_sat, aes(x=Lon, y = Lat, size = N, color = Hour), 
             alpha = 0.25) +
  coord_map(xlim = c(-74.025, -73.89), ylim = c(40.67, 40.85)) +
  scale_color_viridis(direction = -1) +
  labs(title = "Saturday ride pick-ups",
       subtitle = "April 12, 2014",
       caption = "Darker color indicates later rides\nData from NYC TLC and BetaNYC",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(color = "black", size = 0.5),
        plot.caption = element_text(size = 8),
        legend.key.size = unit(.25,"cm")) 

# -------------------------------
# Uber vs Taxis: Tuesday
# -------------------------------
uber_taxi_tues <- uber_taxi[Day == days_vec[1], ][order(Hour)]

ggplot() + 
  geom_polygon(data=nyc_points, aes(x=long, y=lat, group=group),
               color = "black",
               size = 0.15,
               fill = NA) +
  facet_wrap("Company") +
  geom_point(data = uber_taxi_tues, aes(x=Lon, y = Lat, size = N, color = Hour), 
             alpha = 0.25) +
  coord_map(xlim = c(-74.025, -73.89), ylim = c(40.67, 40.85)) +
  scale_color_viridis(direction = -1) +
  labs(title = "Tuesday ride pick-ups",
       subtitle = "April 8, 2014",
       caption = "Darker color indicates later rides\nData from NYC TLC and BetaNYC",
       x = NULL, y = NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(color = "black", size = 0.5),
        plot.caption = element_text(size = 8),
        legend.key.size = unit(.25,"cm"))

# -------------------------------
# In-Class Excercise 6:
# -------------------------------
## Create a map that compares the number and time of Taxi cab rides on the average Tuesday to the number versus the number of Taxi cab rides on the average Saturday


# -------------------------------
# In-Class Excercise 7:
# -------------------------------
## Create a map that plots the most profitable locations for Taxi cap pick up *Hint*: you will use one less aes option

