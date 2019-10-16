if (!require("lubridate")) install.packages(lubridate)
if (!require("tidyverse")) install.packages(tidyverse)
if (!require("readr")) install.packages(readr)
# load packages
library(tidyverse)
library(lubridate)
library(readr)

## look at data in salinity/temperature data file
sal_temp <- read_csv("../data/date_transformed_salinity_temperature.csv")


# next step is to calculate degree hours (cumulative salinity/temperature stress) for each time period

# create function for degree hours above and below a certain threshold

# t is the threshold value, d = temperature data, 
# dh = cumulative degree hours

degree_hours_above <- function(t, d, na.rm = TRUE){
  dh = 0
  for (i in 1:length(d)) {
    if (d[i] >= t) {
      dh = dh + (d[i] - t) # if temperature is above threshold, then add that amount to the running total
    } else {
        dh = dh
    }
  }
  return(dh/4) # data loggers are in 15 minute increments, so divide by 4 to make hours.
}


degree_hours_below <- function(t, d, na.rm = TRUE){
  dh = 0
  for (i in 1:length(d)) {
    if (d[i] <= t) {
      dh = dh + (t - d[i])
    } else {
      dh = dh
    }
  }
  return(dh/4)
}

# calculate response metrics of interest for each outplant period
# 1) mean daily max & min
# 2) total degree/ppt hours
# 3) mean temp/sal

#daily degree/ppt hours for each beach or raft subsite using written functions

# convert grouping variables to factors
sal_temp$site <- as.factor(sal_temp$site)
sal_temp$b_r <- as.factor(sal_temp$b_r)
sal_temp$date <- as.factor(sal_temp$date)

sal_temp_dh <- sal_temp %>% 
  group_by(site, b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))

sal_temp_daily <- sal_temp %>%
  group_by(site, b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))

sal_temp_ddh <- sal_temp_daily %>% 
  full_join(sal_temp_dh) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

# now create dataframes for each time period, calculate overall means for these periods,
# and join with other summary response variables

## calculate outplant times to match response data

sal_temp_outplant <- sal_temp %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

# March-May 2017
sal_temp_may17 <- sal_temp_outplant %>% 
  filter(outplant_time <= 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   temp_se = sd(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE),
                   sal_se = sd(sal, na.rm = TRUE))

sal_temp_may17_2 <- sal_temp_ddh %>% 
  filter(outplant_time <= 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   dmax_sd = sd(dmax_t, na.rm= TRUE),
                   av_dmin_t = mean(dmin_t),
                   dmin_tse = sd(dmin_t, na.rm= TRUE),
                   av_dmin_s = mean(dmin_s),
                   dmin_sse = sd(dmin_s, na.rm= TRUE),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

sal_temp_may17_full <- sal_temp_may17 %>% 
  full_join(sal_temp_may17_2) %>% 
  mutate(month = 2)

# May - July 2017
sal_temp_july17 <- sal_temp_outplant %>% 
  filter(outplant_time <= 15.9 & outplant_time > 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   temp_sd = sd(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE),
                   sal_se = (sd(sal, na.rm = TRUE)/sqrt(length(sal))))

sal_temp_july17_2 <- sal_temp_ddh %>% 
  filter(outplant_time <= 15.9 & outplant_time > 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   dmax_tse = mean(sd(dmax_t, na.rm= TRUE)/sqrt(length(dmax_t))),
                   av_dmin_t = mean(dmin_t),
                   dmin_tse = mean(sd(dmin_t, na.rm= TRUE)/sqrt(length(dmin_t))),
                   av_dmin_s = mean(dmin_s),
                   dmin_sse = mean(sd(dmin_s, na.rm= TRUE)/sqrt(length(dmin_s))),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

sal_temp_july17_full <- sal_temp_july17 %>% 
  full_join(sal_temp_july17_2) %>% 
  mutate(month = 4)
 
# July - September 2017

sal_temp_sept17 <- sal_temp_outplant %>% 
  filter(outplant_time <= 24.1 & outplant_time > 15.9) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sal_temp_sept17_2 <- sal_temp_ddh %>% 
  filter(outplant_time <= 24.1 & outplant_time > 15.9) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

sal_temp_sept17_full <- sal_temp_sept17 %>% 
  full_join(sal_temp_sept17_2) %>% 
  mutate(month = 6)

# September - November 2017

sal_temp_nov17 <- sal_temp_outplant %>% 
  filter(outplant_time <= 31 & outplant_time > 24.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sal_temp_nov17_2 <- sal_temp_ddh %>% 
  filter(outplant_time <= 31 & outplant_time > 24.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

sal_temp_nov17_full <- sal_temp_nov17 %>% 
  full_join(sal_temp_nov17_2) %>% 
  mutate(month = 8)

# November 2017 - March 2018

sal_temp_mar18 <- sal_temp_outplant %>% 
  filter(outplant_time > 31) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sal_temp_mar18_2 <- sal_temp_ddh %>% 
  filter(outplant_time > 31) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

sal_temp_mar18_full <- sal_temp_mar18 %>% 
  full_join(sal_temp_mar18_2) %>% 
  mutate(month = 12)

# now join to make full response dataset

response_summary <- sal_temp_may17_full %>% 
  full_join(sal_temp_july17_full) %>% 
  full_join(sal_temp_sept17_full) %>% 
  full_join(sal_temp_nov17_full) %>% 
  full_join(sal_temp_mar18_full)

# now we just need to make a data frame for the change in growth between datapoints
# for the response, and we can join and model these data

write_csv(response_summary, "../data/summary.csv")

## write up example data table to demonstrate the effect of imputation on environmental paramaters
write.table(sal_temp_july17_full, "../data/cca_st.txt", quote = FALSE, sep = "\t", row.names = FALSE)
