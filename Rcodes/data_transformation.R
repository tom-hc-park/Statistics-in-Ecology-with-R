if (!require("lubridate")) install.packages("lubridate")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
# load packages
library(tidyverse)
library(lubridate)
library("dplyr")
library(tidyr)
## look at data in salinity/temperature data file
sal_temp <- read.csv("../data/salinity_temperature.csv")

## convert date into proper format readable by R
sal_temp$time <- as.POSIXct(sal_temp$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )

## parse out date, time, and hour into separate columns
sal_temp <- sal_temp %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE)

# let's keep the date variable for now
sal_temp$time <- as.POSIXct(sal_temp$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )

## convert date to preferred format 
sal_temp$date <- as.Date(sal_temp$date, "%Y-%m-%d")


write.csv(sal_temp, "../data/date_transformed_salinity_temperature.csv")
