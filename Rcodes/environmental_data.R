#Script to get CT data summarized

library(tidyverse)
library(lubridate)

# need to set working directory to proper data folder

## look at data in edited environmental CT data file
sal_temp <- read_csv("./full_data.csv")

## convert date into proper format readable by R
sal_temp$time <- as.POSIXct(sal_temp$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )

## parse out date and time into separate columns
sal_temp <- sal_temp %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE) %>% 
  select(-date_time)

## convert date to preferred format 
sal_temp$date <- as.Date(sal_temp$date, "%Y-%m-%d")

sal_temp$date <- format(sal_temp$date, "%d-%m-%Y")

class(sal_temp$date)

# now create some summaries of variables across each time period

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
