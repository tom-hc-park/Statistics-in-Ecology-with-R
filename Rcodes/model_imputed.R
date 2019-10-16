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

M.ts.b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
                     av_dmin_t + tot_dh_t + tot_dh_s +
                     outplant_time,
                   data = beach_ts)
summary(M.ts)
anova(M.ts)

M.ts.r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
             av_dmin_t + av_dmin_s + tot_dh_t +
             outplant_time,
           data = rafts_ts)
summary(M.ts)
anova(M.ts)

## now run the model on the mice imputed
st_mice <- get(load("../data/mice_imputed_data.RData"))

## need to include original df in complete command...

X_long <- complete(st_mice, action = "long", include = TRUE )

X_long$site <- as.character(X_long$site)
X_long$b_r <- as.character(X_long$b_r)
X_long$date <- as.Date(X_long$date, format = "%Y-%m-%d")

dh_Xl <- X_long %>% 
  group_by(.imp, site, b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarize(dh_t = degree_hours_above(29, temp),
                   dh_s = degree_hours_below(20, sal))
daily_Xl <- X_long %>%
  group_by(.imp, site, b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarize(dmax_t = max(temp),
                   dmin_t = min(temp),
                   dmin_s = min(sal))
ddh_Xl <- daily_Xl %>% 
  full_join(dh_Xl) %>% 
  group_by(.imp, site, b_r) %>% 
  mutate(outplant_time = (as.Date(date) - as.Date(date[1]))/7)

may_Xl <- X_long %>% 
  filter(date < "2017-05-26") %>% 
  group_by(.imp, site, b_r) %>% 
  na.omit() %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   temp_se = (sd(temp, na.rm = TRUE)/sqrt(length(temp))),
                   sal_av = mean(sal, na.rm = TRUE),
                   sal_se = (sd(sal, na.rm = TRUE)/sqrt(length(sal))))

may2_Xl <- ddh_Xl %>% 
  filter(date < "2017-05-26") %>% 
  group_by(.imp, site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   dmax_tse = mean(sd(dmax_t)/sqrt(length(dmax_t)),na.rm= TRUE),
                   av_dmin_t = mean(dmin_t),
                   dmin_tse = mean(sd(dmin_t)/sqrt(length(dmin_t)), na.rm= TRUE),
                   av_dmin_s = mean(dmin_s),
                   dmin_sse = mean(sd(dmin_s)/sqrt(length(dmin_s)), na.rm= TRUE),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
mayf_Xl <- may_Xl %>% 
  full_join(may2_Xl) %>% 
  mutate(month = 2)

# May - July 2017
july_Xl <- X_long %>% 
  filter(date < "2017-07-19" & date >= "2017-05-26") %>% 
  group_by(.imp, site, b_r) %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   temp_se = sd(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE),
                   sal_se = sd(sal, na.rm = TRUE))
july2_Xl <- ddh_Xl %>% 
  filter(date < "2017-07-19" & date >= "2017-05-26") %>% 
  group_by(.imp, site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   dmax_sd = sd(dmax_t, na.rm= TRUE),
                   av_dmin_t = mean(dmin_t),
                   dmin_tse = sd(dmin_t, na.rm= TRUE),
                   av_dmin_s = mean(dmin_s),
                   dmin_sse = sd(dmin_s, na.rm= TRUE),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
julyf_Xl <- july_Xl %>% 
  full_join(july2_Xl) %>% 
  mutate(month = 4)

# July - September 2017
sept_Xl <- X_long %>% 
  filter(date < "2019-09-15" & date >= "2017-07-19") %>% 
  group_by(.imp, site, b_r) %>% 
  na.omit() %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
sept2_Xl <- ddh_Xl %>% 
  filter(date < "2019-09-15" & date >= "2017-07-19") %>% 
  group_by(.imp, site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
septf_Xl <- sept_Xl %>% 
  full_join(sept2_Xl) %>% 
  mutate(month = 6)

# September - November 2017
nov_Xl <- X_long %>% 
  filter(date < "2019-11-02" & date >= "2017-09-15") %>% 
  group_by(.imp, site, b_r) %>% 
  na.omit() %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
nov2_Xl <- ddh_Xl %>% 
  filter(date < "2019-11-02" & date >= "2017-09-15") %>% 
  group_by(.imp, site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
novf_Xl <- nov_Xl %>% 
  full_join(nov2_Xl) %>% 
  mutate(month = 8)

# November 2017 - March 2018
mar_Xl <- X_long %>% 
  filter(date > "2017-11-02") %>% 
  group_by(.imp, site, b_r) %>% 
  na.omit() %>% 
  dplyr::summarise(temp_av = mean(temp, na.rm = TRUE),
                   sal_av = mean(sal, na.rm = TRUE))
mar2_Xl <- ddh_Xl %>% 
  filter(date > "2017-11-02") %>% 
  group_by(.imp, site, b_r) %>% 
  dplyr::summarise(av_dmax_t = mean(dmax_t),
                   av_dmin_t = mean(dmin_t),
                   av_dmin_s = mean(dmin_s),
                   tot_dh_t = sum(dh_t),
                   tot_dh_s = sum(dh_s))
marf_Xl <- mar_Xl %>% 
  full_join(mar2_Xl) %>% 
  mutate(month = 12)

Xl_s <- mayf_Xl %>% 
  full_join(julyf_Xl) %>% 
  full_join(septf_Xl) %>% 
  full_join(novf_Xl) %>% 
  full_join(marf_Xl)
Xl_s$month <- as.factor(Xl_s$month)


Xl_full <- Xl_s %>% 
  full_join(growth_rate)

Xl_rs <- Xl_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100,
         .id = seq(1:length(site)))

Xl_b <- Xl_rs %>% 
  filter(b_r == "b")

Xl_r <- Xl_rs %>% 
  filter(b_r == "r")

mids_b <- as.mids(Xl_b)
mids_r <- as.mids(Xl_r)

model_b <- with(mids_b, lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
                             av_dmin_t + tot_dh_t + tot_dh_s +
                             outplant_time))
model_r <- with(mids_r, lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
                             av_dmin_t + av_dmin_s + tot_dh_s +
                             outplant_time))

mice_beach <- pool(model_b)
mice_raft <- pool(model_r)


write.table(summary(mice_beach), file = "../data/miceb.txt", quote = FALSE, sep = "\t", row.names = TRUE)
write.table(summary(mice_raft), file = "../data/micer.txt", quote = FALSE, sep = "\t", row.names = TRUE)
write.table(summary(M.ts.b)$coefficients, file = "../data/tsb.txt", quote = FALSE, sep = "\t", row.names = TRUE)
write.table(summary(M.ts.r)$coefficients, file = "../data/tsr.txt", quote = FALSE, sep = "\t", row.names = TRUE)
write.table(summary(lm_oysters_b)$coefficients, file = "../data/ccab.txt", quote = FALSE, sep = "\t", row.names = TRUE)
write.table(summary(lm_oysters_r)$coefficients, file = "../data/ccar.txt", quote = FALSE, sep = "\t", row.names = TRUE)


write.table(julyf_Xl, "../data/mice_st.txt", quote = FALSE, sep = "\t", row.names = FALSE)

