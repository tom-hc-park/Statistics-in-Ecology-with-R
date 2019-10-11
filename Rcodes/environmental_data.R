if (!require("lubridate")) install.packages(lubridate)
if (!require("tidyverse")) install.packages(tidyverse)

# load packages
library(tidyverse)
library(lubridate)

## look at data in salinity/temperature data file
sal_temp <- read_csv("../data/salinity_temperature.csv")

## convert date into proper format readable by R
sal_temp$time <- as.POSIXct(sal_temp$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )

## parse out date, time, and hour into separate columns
sal_temp <- sal_temp %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE) %>% 
  select(-date_time)

## convert date to preferred format 
sal_temp$date <- as.Date(sal_temp$date, "%Y-%m-%d")

## calculate outplant times to match response data

sal_temp_outplant <- sal_temp %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (date - date[1])/7)

# summarize the average hourly temperature and salinity
sal_temp_hours <- sal_temp_outplant %>% 
  group_by(site, b_r, date, hour) %>% 
  dplyr::summarize(outplant_time = mean(outplant_time), temp_av = mean(temp, na.rm = TRUE), sal_av = mean(sal, na.rm = TRUE))

sal_temp <- sal_temp_hours

## need to calculate degree hours from environmental data

# first step is to create separate data frames for each period of data

# March-May 2017
sal_temp_may17 <- sal_temp %>% 
  filter(outplant_time <= 8.1)

# May - July 2017
sal_temp_july17 <- sal_temp %>% 
  filter(outplant_time <= 15.9 & outplant_time > 8.1)

# July - September 2017
sal_temp_sept17 <- sal_temp %>% 
  filter(outplant_time <= 24.1 & outplant_time > 15.9)

# September - November 2017
sal_temp_nov17 <- sal_temp %>% 
  filter(outplant_time <= 31 & outplant_time > 24.1)

# November 2017 - March 2018
sal_temp_mar18 <- sal_temp %>% 
  filter(outplant_time > 31)

# next step is to calculate degree hours (cumulative salinity/temperature stress) for each time period

# create function for degree hours above and below a certain threshold

# t is the threshold value, d = temperature data, dh = initial degree hours (0), 
# c = initial counter (0)
degree_hours_above <- function(t, d, dh, c){
  for (i in 1:length(d)) {
    if (d[i] >= t) {
      if (i == 1) {
        n = floor(d[i] - t) + 1
        dh = dh + n
        c = 1
      } else if (d[i-1] >= t){
          n = floor(d[i] - t) + 1
          dh = dh + c + n
          c = c + 1
        } else {
          n = floor(d[i] - t) + 1
          dh = dh + n
          c = 1
        } 
    } else {
        dh = dh
        c = 0
    }
  }
  return(dh)
}


degree_hours_below <- function(t, d, dh, c){
  for (i in 1:length(d)) {
    if (d[i] <= t) {
      if (i == 1) {
        n = floor(t - d[i]) + 1
        dh = dh + n
        c = 1
      } else if (d[i-1] <= t){
        n = floor(t - d[i]) + 1 
        dh = dh + c + n
        c = c + 1
      } else {
        n = floor(t - d[i]) + 1
        dh = dh + n
        c = 1
      } 
    } else {
      dh = dh
      c = 0
    }
  }
  return(dh)
}

# separate data by time point

# may
st_may <- sal_temp %>% 
  filter(date < "01-06-2017") %>% 
  group_by(site, b_r) %>% 
  summarize(mean_temp = mean(temp, na.rm=TRUE), mean_sal = mean(sal, na.rm = TRUE), 
            var_temp = sd(temp, na.rm=TRUE)^2, var_sal = sd(sal, na.rm=TRUE)^2,
            min_temp = min(temp, na.rm=TRUE), max_temp = max(temp, na.rm=TRUE), min_sal = min(sal),
            max_sal = max(sal))

# july
st_july <- sal_temp %>% 
  filter(date < "01-08-2017" & date >= "01-06-2017") %>% 
  group_by(site, b_r) %>% 
  summarize(mean_temp = mean(temp, na.rm=TRUE), mean_sal = mean(sal, na.rm = TRUE), 
            var_temp = sd(temp, na.rm=TRUE)^2, var_sal = sd(sal, na.rm=TRUE)^2,
            min_temp = min(temp, na.rm=TRUE), max_temp = max(temp, na.rm=TRUE), min_sal = min(sal),
            max_sal = max(sal))

# sept
st_sept <- sal_temp %>% 
  filter(date < "15-09-2017" & date >= "01-08-2017") %>% 
  group_by(site, b_r) %>% 
  summarize(mean_temp = mean(temp, na.rm=TRUE), mean_sal = mean(sal, na.rm = TRUE), 
            var_temp = sd(temp, na.rm=TRUE)^2, var_sal = sd(sal, na.rm=TRUE)^2,
            min_temp = min(temp, na.rm=TRUE), max_temp = max(temp, na.rm=TRUE))

# nov
st_nov <- sal_temp %>% 
  filter(date >= "15-09-2017" & date <= "15-11-2017") %>% 
  group_by(site, b_r) %>% 
  summarize(mean_temp = mean(temp, na.rm=TRUE), mean_sal = mean(sal, na.rm = TRUE), 
            var_temp = sd(temp, na.rm=TRUE)^2, var_sal = sd(sal, na.rm=TRUE)^2,
            min_temp = min(temp, na.rm=TRUE), max_temp = max(temp, na.rm=TRUE),
            se_temp = se(temp), se_sal = se(sal))

# march
st_march <- sal_temp %>% 
  filter(date >= "15-11-2017") %>% 
  group_by(site, b_r) %>% 
  summarize(mean_temp = mean(temp, na.rm=TRUE), mean_sal = mean(sal, na.rm = TRUE), 
            var_temp = sd(temp, na.rm=TRUE)^2, var_sal = sd(sal, na.rm=TRUE)^2,
            min_temp = min(temp, na.rm=TRUE), max_temp = max(temp, na.rm=TRUE),
            se_temp = se(temp), se_sal = se(sal))
