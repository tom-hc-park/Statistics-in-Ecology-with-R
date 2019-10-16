## recycling code from cca in model.R for imputed datasets
## still a work in progress
st_summ_ts <- read.csv("../data/saltemp_summary_timeseries.csv")

oysters_full_ts <- growth_rate %>% 
  full_join(st_summ_ts) %>% 
  mutate(subsite = paste(site, b_r))

oysters_rs_ts <- oysters_full_ts %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

rafts_ts <- oysters_rs_ts %>% 
  filter(b_r == "r")

beach_ts <- oysters_rs_ts %>% 
  filter(b_r == "b")

M.ts <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
                     av_dmin_t + av_dmin_s + tot_dh_s +
                     outplant_time,
                   data = rafts_ts)
summary(M.ts)
anova(M.ts)

## now run the model on the mice imputed
st_mice <- get(load("../data/mice_imputed_data.RData"))

# convert output to five imputed dataframes
X1 <- complete(st_mice, action = 1L)
X2 <- complete(st_mice, action = 2L)
X3 <- complete(st_mice, action = 3L)
X4 <- complete(st_mice, action = 4L)
X5 <- complete(st_mice, action = 5L)

X1 <- X1 %>% 
  mutate(month = "NA")
X1$date <- as.Date(X1$date)

for (i in 1:length(X1$site)) {
  X1$date[i] <- as.Date(X1$date[i], format = "%Y-%m-%d")
  if (X1$date[i] < "2017-05-26") {
    X1$month[i] = 2
  }
  else if (X1$date[i] < "2017-07-19" & 
    X1$date[i] > "2017-05-26"){
    X1$month[i] = 4
  }
  else if (X1$date[i] > "2017-07-19" & 
      X1$date[i] < "2017-09-15"){
    X1$month[i] = 6
  }
  else if (X1$date[i] < "2017-11-02" & 
      X1$date[i] > "2017-09-15"){
    X1$month[i] = 8
  }
  else if (X1$date[i] > "2017-11-02"){
    X1$month[i] = 12
  }
}

# there's probably a way to iterate, but I can't figure it out

X2 <- X2 %>% 
  mutate(month = "NA")
X2$date <- as.Date(X2$date, format = "%Y-%m-%d")

for (i in 1:length(X2$site)) {
  if (X2$date[i] < "2017-05-26") {
    X2$month[i] = 2
  }
  if (X2$date[i] < "2017-07-19" & 
      X2$date[i] > "2017-05-26"){
    X2$month[i] = 4
  }
  if (X2$date[i] > "2017-07-19" & 
      X2$date[i] < "2017-09-15"){
    X2$month[i] = 6
  }
  if (X2$date[i] < "2017-11-02" & 
      X2$date[i] > "2017-09-15"){
    X2$month[i] = 8
  }
  if (X2$date[i] > "2017-11-02"){
    X2$month[i] = 12
  }
}

X3 <- X3 %>% 
  mutate(month = "NA")
X3$date <- as.Date(X3$date, format = "%Y-%m-%d")

for (i in 1:length(X3$site)) {
  if (X3$date[i] < "2017-05-26") {
    X3$month[i] = 2
  }
  if (X3$date[i] < "2017-07-19" & 
      X3$date[i] > "2017-05-26"){
    X3$month[i] = 4
  }
  if (X3$date[i] > "2017-07-19" & 
      X3$date[i] < "2017-09-15"){
    X3$month[i] = 6
  }
  if (X3$date[i] < "2017-11-02" & 
      X3$date[i] > "2017-09-15"){
    X3$month[i] = 8
  }
  if (X3$date[i] > "2017-11-02"){
    X3$month[i] = 12
  }
}

X4 <- X4 %>% 
  mutate(month = "NA")
X4$date <- as.Date(X4$date, format = "%Y-%m-%d")

for (i in 1:length(X4$site)) {
  if (X4$date[i] < "2017-05-26") {
    X4$month[i] = 2
  }
  if (X4$date[i] < "2017-07-19" & 
      X4$date[i] > "2017-05-26"){
    X4$month[i] = 4
  }
  if (X4$date[i] > "2017-07-19" & 
      X4$date[i] < "2017-09-15"){
    X4$month[i] = 6
  }
  if (X4$date[i] < "2017-11-02" & 
      X4$date[i] > "2017-09-15"){
    X4$month[i] = 8
  }
  if (X4$date[i] > "2017-11-02"){
    X4$month[i] = 12
  }
}

X5 <- X5 %>% 
  mutate(month = "NA")
X5$date <- as.Date(X5$date, format = "%Y-%m-%d")

for (i in 1:length(X5$site)) {
  if (X5$date[i] < "2017-05-26") {
    X5$month[i] = 2
  }
  if (X5$date[i] < "2017-07-19" & 
      X5$date[i] > "2017-05-26"){
    X5$month[i] = 4
  }
  if (X5$date[i] > "2017-07-19" & 
      X5$date[i] < "2017-09-15"){
    X5$month[i] = 6
  }
  if (X5$date[i] < "2017-11-02" & 
      X5$date[i] > "2017-09-15"){
    X5$month[i] = 8
  }
  if (X5$date[i] > "2017-11-02"){
    X5$month[i] = 12
  }
}

# save the results since that took so long

write.csv(X1, "../data/imp1.csv")
write.csv(X2, "../data/imp2.csv")
write.csv(X3, "../data/imp3.csv")
write.csv(X4, "../data/imp4.csv")
write.csv(X5, "../data/imp5.csv")

# need to get outplant time for imputed dataframes still...
X1 <- read.csv("../data/imp1.csv")
X2 <- read.csv("../data/imp2.csv")
X3 <- read.csv("../data/imp3.csv")
X4 <- read.csv("../data/imp4.csv")
X5 <- read.csv("../data/imp5.csv")

## compute those summaries
dh_X1 <- X1 %>% 
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))
daily_X1 <- X1 %>%
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))
ddh_X1 <- daily_X1 %>% 
  full_join(dh_X1) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

dh_X2 <- X2 %>% 
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))
daily_X2 <- X2 %>%
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))
ddh_X2 <- daily_X2 %>% 
  full_join(dh_X2) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

dh_X3 <- X3 %>% 
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))
daily_X3 <- X3 %>%
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))
ddh_X3 <- daily_X3 %>% 
  full_join(dh_X3) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)


dh_X4 <- X4 %>% 
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))
daily_X4 <- X4 %>%
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))
ddh_X4 <- daily_X4 %>% 
  full_join(dh_X4) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)


dh_X5 <- X5 %>% 
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))
daily_X5 <- X5 %>%
  group_by(site, b_r, date, month) %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))
ddh_X5 <- daily_X5 %>% 
  full_join(dh_X5) %>% 
  group_by(site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

# now create dataframes for each time period, calculate overall means for these periods,
# and join with other summary response variables

# March-May 2017

may_X1 <- X1 %>% 
  filter(month == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
may2_X1 <- ddh_X1 %>% 
  filter(outplant_time == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
mayf_X1 <- may_X1 %>% 
  full_join(may2_X1)

# May - July 2017
july_X1 <- X1 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

july2_X1 <- ddh_X1 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

julyf_X1 <- july_X1 %>% 
  full_join(july2_X1)

# July - September 2017
sept_X1 <- X1 %>% 
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sept2_X1 <- ddh_X1 %>%   
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

septf_X1 <- sept_X1 %>% 
  full_join(sept2_X1)

# September - November 2017

nov_X1 <- X1 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

nov2_X1 <- ddh_X1 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

novf_X1 <- nov_X1 %>% 
  full_join(nov2_X1)

novf_X1$month <- as.factor(novf_X1$month)

# November 2017 - March 2018

mar_X1 <- X1 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

mar2_X1 <- ddh_X1 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

marf_X1 <- mar_X1 %>% 
  full_join(mar2_X1) 

marf_X1$month <- as.factor(marf_X1$month)

# now join to make full response dataset

X1_s <- mayf_X1 %>% 
  full_join(julyf_X1) %>% 
  full_join(septf_X1) %>% 
  full_join(novf_X1) %>% 
  full_join(marf_X1)

X1_s$month <- as.factor(X1_s$month)


## now repeat for all the other df

may_X2 <- X2 %>% 
  filter(month == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
may2_X2 <- ddh_X2 %>% 
  filter(outplant_time == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
mayf_X2 <- may_X2 %>% 
  full_join(may2_X2)
mayf_X2$month <- as.factor(mayf_X2$month)

# May - July 2017
july_X2 <- X2 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

july2_X2 <- ddh_X2 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

julyf_X2 <- july_X2 %>% 
  full_join(july2_X2)
julyf_X2$month <- as.factor(julyf_X2$month)

# July - September 2017
sept_X2 <- X2 %>% 
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sept2_X2 <- ddh_X2 %>%   
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

septf_X2 <- sept_X2 %>% 
  full_join(sept2_X2)
septf_X2$month <- as.factor(septf_X2$month)

# September - November 2017

nov_X2 <- X2 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

nov2_X2 <- ddh_X2 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

novf_X2 <- nov_X2 %>% 
  full_join(nov2_X2)

novf_X2$month <- as.factor(novf_X2$month)

# November 2017 - March 2018

mar_X2 <- X2 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

mar2_X2 <- ddh_X2 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

marf_X2 <- mar_X2 %>% 
  full_join(mar2_X2) 

marf_X2$month <- as.factor(marf_X2$month)

X2_s <- mayf_X2 %>% 
  full_join(julyf_X2) %>% 
  full_join(septf_X2) %>% 
  full_join(novf_X2) %>% 
  full_join(marf_X2)
X2_s$month <- as.factor(X2_s$month)

## X3

may_X3 <- X3 %>% 
  filter(month == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
may2_X3 <- ddh_X3 %>% 
  filter(outplant_time == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
mayf_X3 <- may_X3 %>% 
  full_join(may2_X3)
mayf_X3$month <- as.factor(mayf_X3$month)

# May - July 2017
july_X3 <- X3 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
july2_X3 <- ddh_X3 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
julyf_X3 <- july_X3 %>% 
  full_join(july2_X3)
julyf_X3$month <- as.factor(julyf_X3$month)

# July - September 2017
sept_X3 <- X3 %>% 
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
sept2_X3 <- ddh_X3 %>%   
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
septf_X3 <- sept_X3 %>% 
  full_join(sept2_X3)
septf_X3$month <- as.factor(septf_X3$month)

# September - November 2017

nov_X3 <- X3 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
nov2_X3 <- ddh_X3 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
novf_X3 <- nov_X3 %>% 
  full_join(nov2_X3)

novf_X3$month <- as.factor(novf_X3$month)

# November 2017 - March 2018

mar_X3 <- X3 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
mar2_X3 <- ddh_X3 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
marf_X3 <- mar_X3 %>% 
  full_join(mar2_X3) 
marf_X3$month <- as.factor(marf_X3$month)

# now join to make full response dataset
X3_s <- mayf_X3 %>% 
  full_join(julyf_X3) %>% 
  full_join(septf_X3) %>% 
  full_join(novf_X3) %>% 
  full_join(marf_X3)
X3_s$month <- as.factor(X3_s$month)

## X4

may_X4 <- X4 %>% 
  filter(month == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
may2_X4 <- ddh_X4 %>% 
  filter(outplant_time == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
mayf_X4 <- may_X4 %>% 
  full_join(may2_X4)
mayf_X4$month <- as.factor(mayf_X4$month)

# May - July 2017
july_X4 <- X4 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

july2_X4 <- ddh_X4 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

julyf_X4 <- july_X4 %>% 
  full_join(july2_X4)
julyf_X4$month <- as.factor(julyf_X4$month)

# July - September 2017
sept_X4 <- X4 %>% 
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sept2_X4 <- ddh_X4 %>%   
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

septf_X4 <- sept_X4 %>% 
  full_join(sept2_X4)
septf_X4$month <- as.factor(septf_X4$month)

# September - November 2017

nov_X4 <- X4 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

nov2_X4 <- ddh_X4 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

novf_X4 <- nov_X4 %>% 
  full_join(nov2_X4)
novf_X4$month <- as.factor(novf_X4$month)

# November 2017 - March 2018

mar_X4 <- X4 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

mar2_X4 <- ddh_X4 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

marf_X4 <- mar_X4 %>% 
  full_join(mar2_X4) 
marf_X4$month <- as.factor(marf_X4$month)

# now join to make full response dataset

X4_s <- mayf_X4 %>% 
  full_join(julyf_X4) %>% 
  full_join(septf_X4) %>% 
  full_join(novf_X4) %>% 
  full_join(marf_X4)

X4_s$month <- as.factor(X4_s$month)

## X5
may_X5 <- X5 %>% 
  filter(month == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
may2_X5 <- ddh_X5 %>% 
  filter(outplant_time == 2) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
mayf_X5 <- may_X5 %>% 
  full_join(may2_X5)
mayf_X5$month <- as.factor(mayf_X5$month)

# May - July 2017
july_X5 <- X5 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

july2_X5 <- ddh_X5 %>% 
  filter(month == 4) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

julyf_X5 <- july_X5 %>% 
  full_join(july2_X5)
julyf_X5$month <- as.factor(julyf_X5$month)

# July - September 2017
sept_X5 <- X5 %>% 
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

sept2_X5 <- ddh_X5 %>%   
  filter(month == 6) %>% 
  group_by(site, b_r,month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

septf_X5 <- sept_X5 %>% 
  full_join(sept2_X5)
septf_X5$month <- as.factor(septf_X5$month)

# September - November 2017

nov_X5 <- X5 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

nov2_X5 <- ddh_X5 %>% 
  filter(month == 8) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

novf_X5 <- nov_X5 %>% 
  full_join(nov2_X5)

novf_X5$month <- as.factor(novf_X5$month)

# November 2017 - March 2018

mar_X5 <- X5 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))

mar2_X5 <- ddh_X5 %>% 
  filter(month == 12) %>% 
  group_by(site, b_r, month) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))

marf_X5 <- mar_X5 %>% 
  full_join(mar2_X5) 

marf_X5$month <- as.factor(marf_X5$month)

# now join to make full response dataset

X5_s <- mayf_X5 %>% 
  full_join(julyf_X5) %>% 
  full_join(septf_X5) %>% 
  full_join(novf_X5) %>% 
  full_join(marf_X5)
X5_s$month <- as.factor(X5_s$month)

## now we can finally join these with growth data and make a mira object
growth_rate$month <- as.factor(growth_rate$month)

X1_full <- X1_s %>% 
  full_join(growth_rate)
X1_full$month <- as.factor(X1_full$month)

X1_rs <- X1_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

X2_full <- X2_s %>% 
  full_join(growth_rate)
X2_full$month <- as.factor(X2_full$month)

X2_rs <- X2_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

X3_full <- X3_s %>% 
  full_join(growth_rate)
X3_full$month <- as.factor(X3_full$month)

X3_rs <- X3_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

X4_full <- X4_s %>% 
  full_join(growth_rate)
X4_full$month <- as.factor(X4_full$month)

X4_rs <- X4_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

X5_full <- X5_s %>% 
  full_join(growth_rate)
X5_full$month <- as.factor(X5_full$month)

X5_rs <- X5_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

# split into beach and raft

X1_b <- X1_rs %>% 
  filter(b_r == "b")
X2_b <- X2_rs %>% 
  filter(b_r == "b")
X3_b <- X3_rs %>% 
  filter(b_r == "b")
X4_b <- X4_rs %>% 
  filter(b_r == "b")
X5_b <- X5_rs %>% 
  filter(b_r == "b")

X1_r <- X1_rs %>% 
  filter(b_r == "r")
X2_r <- X2_rs %>% 
  filter(b_r == "r")
X3_r <- X3_rs %>% 
  filter(b_r == "r")
X4_r <- X4_rs %>% 
  filter(b_r == "r")
X5_r <- X5_rs %>% 
  filter(b_r == "r")

## run models
M.X1b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + tot_dh_t + tot_dh_s +
              outplant_time,
                   data = X1_b)

M.X2b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + tot_dh_t + tot_dh_s +
              outplant_time,
            data = X2_b)

M.X3b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + tot_dh_t + tot_dh_s +
              outplant_time,
            data = X3_b)

M.X4b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + tot_dh_t + tot_dh_s +
              outplant_time,
            data = X4_b)

M.X5b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + tot_dh_t + tot_dh_s +
              outplant_time,
            data = X5_b)

M.X1r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + av_dmin_s + tot_dh_s +
              outplant_time,
            data = X1_r)

M.X2r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + av_dmin_s + tot_dh_s +
              outplant_time,
            data = X2_r)

M.X3r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + av_dmin_s + tot_dh_s +
              outplant_time,
            data = X3_r)

M.X4r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + av_dmin_s + tot_dh_s +
              outplant_time,
            data = X4_r)

M.X5r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
              av_dmin_t + av_dmin_s + tot_dh_s +
              outplant_time,
            data = X5_r)

all_Mb <- as.mira(M.X1b,M.X2b, M.X3b, M.X4b, M.X5b)
all_Mr <- as.mira(M.X1r,M.X2r, M.X3r, M.X4r, M.X5r)
?as.mira
# now pool!

beach_mice <- pool(all_Mb$analyses)

# not working for some reason???
