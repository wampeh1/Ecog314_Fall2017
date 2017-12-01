library(tidyverse)
library(lubridate)

path <- "//mqlx1/m1icd00/projects/Howard_TA/lecture10/Data/"

gas_data <- read.csv(paste0(path,"weekly_gas_prices.csv"))
clean_gas <- gas_data %>% mutate(time = mdy(Date)) %>% mutate(week = week(time)) %>% 
  rename(gas_price = Average.Price) %>% select(week, price)

write.csv(clean_gas, paste0(path,"nyc_gas_prices.csv"), row.names = F)
