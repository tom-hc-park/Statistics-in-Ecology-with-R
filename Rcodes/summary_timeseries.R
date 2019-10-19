library(tidyverse)

# look at the change in environmental parameters obtained with time series 

st_timeseries <- read.csv("../data/time_series_imputed.csv")
st_mice <- get(load("../data/mice_imputed_data.RData"))

# first do time series with code written in summary_of_salinity_temp.R
st_timeseries$site <- as.factor(st_timeseries$site)
st_timeseries$b_r <- as.factor(st_timeseries$b_r)
st_timeseries$date <- as.factor(st_timeseries$date)

st_dh_ts <- st_timeseries %>% 
  group_by(site, b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))

st_daily_ts <- st_timeseries %>%
  group_by(site, b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))

st_ddh_ts <- st_daily_ts %>% 
  full_join(st_dh_ts) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

st_outplant_ts <- st_timeseries %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

# March-May 2017
st_may17_ts <- st_outplant_ts %>% 
  filter(outplant_time <= 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   temp_se = (sd(temp, na.rm = TRUE)/sqrt(length(temp))),
                   sal_av = mean(sal, na.rm = TRUE),
                   sal_se = (sd(sal, na.rm = TRUE)/sqrt(length(sal))))

st_may17_ts_2 <- sal_temp_ddh %>% 
  filter(outplant_time <= 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   dmax_tse = mean(sd(dmax_t)/sqrt(length(dmax_t)),na.rm= TRUE),
                   av_dmin_t = mean(dmin_t),
                   dmin_tse = mean(sd(dmin_t)/sqrt(length(dmin_t)), na.rm= TRUE),
                   av_dmin_s = mean(dmin_s),
                   dmin_sse = mean(sd(dmin_s)/sqrt(length(dmin_s)), na.rm= TRUE),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

st_mayfull_ts <- st_may17_ts %>% 
  full_join(st_may17_ts_2) %>% 
  mutate(month = 2)

# May - July 2017
st_july17_ts <- st_outplant_ts %>% 
  filter(outplant_time <= 15.9 & outplant_time > 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   temp_se = sd(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE),
                   sal_se = sd(sal, na.rm = TRUE))

st_july17_ts_2 <- st_ddh_ts %>% 
  filter(outplant_time <= 15.9 & outplant_time > 8.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   dmax_sd = sd(dmax_t, na.rm= TRUE),
                   av_dmin_t = mean(dmin_t),
                   dmin_tse = sd(dmin_t, na.rm= TRUE),
                   av_dmin_s = mean(dmin_s),
                   dmin_sse = sd(dmin_s, na.rm= TRUE),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

st_julyfull_ts <- st_july17_ts %>% 
  full_join(st_july17_ts_2) %>% 
  mutate(month = 4)

# July - September 2017

st_sept17_ts <- st_outplant_ts %>% 
  filter(outplant_time <= 24.1 & outplant_time > 15.9) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

st_sept17_ts_2 <- st_ddh_ts %>% 
  filter(outplant_time <= 24.1 & outplant_time > 15.9) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

st_septfull_ts <- st_sept17_ts %>% 
  full_join(st_sept17_ts_2) %>% 
  mutate(month = 6)

# September - November 2017

st_nov17_ts <- st_outplant_ts %>% 
  filter(outplant_time <= 31 & outplant_time > 24.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

st_nov17_ts_2 <- st_ddh_ts %>% 
  filter(outplant_time <= 31 & outplant_time > 24.1) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

st_novfull_ts <- st_nov17_ts %>% 
  full_join(st_nov17_ts_2) %>% 
  mutate(month = 8)

# November 2017 - March 2018

st_mar18_ts <- st_outplant_ts %>% 
  filter(outplant_time > 31) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

st_mar18_ts_2 <- st_ddh_ts %>% 
  filter(outplant_time > 31) %>% 
  group_by(site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

st_marfull_ts <- st_mar18_ts %>% 
  full_join(st_mar18_ts_2) %>% 
  mutate(month = 12)

# now join to make full response dataset

summary_ts <- st_mayfull_ts %>% 
  full_join(st_septfull_ts) %>% 
  full_join(st_julyfull_ts) %>% 
  full_join(st_novfull_ts) %>% 
  full_join(st_marfull_ts)

write.csv(summary_ts,"../data/saltemp_summary_timeseries.csv")

write.table(st_julyfull_ts, "../data/ts_st.txt", quote = FALSE, sep = "\t", row.names = FALSE)
