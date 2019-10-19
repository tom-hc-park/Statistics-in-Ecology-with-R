Er_complete <- read.csv("../data/Er_august_complete.csv") %>% 
  mutate(b_r = "r")
Eb_complete <- read.csv("../data/Eb_august_complete.csv") %>% 
  mutate(b_r = "b")

# remove 10% of missing data completely at random
missing_r <- sample_frac(Er_complete, 0.9)

Er_mcar <- Er_complete %>% 
  dplyr::select(date_time, b_r) %>% 
  full_join(missing_r)

missing_b <- sample_frac(Eb_complete, 0.9)

E_mcar <- Eb_complete %>% 
  dplyr::select(date_time, b_r) %>% 
  full_join(missing_b) %>%
  full_join(Er_mcar) %>% 
  dplyr::select(date_time, temp, sal, b_r) 

# remove 10% of data from early in the dataset

Er_mar <- Er_complete %>% 
  dplyr::select(date_time, temp, sal, b_r) %>% 
  filter(date_time > 42940.53)

E_mar <- Eb_complete %>% 
  dplyr::select(date_time, temp, sal, b_r) %>% 
  filter(date_time > 42940.5) %>% 
  full_join(Er_mar)

# remove 10% of data based on low salinity (MNAR)

Er_mnar <- Er_complete %>% 
  dplyr::select(date_time, temp, sal, b_r) %>% 
  filter(sal > 20.16)

E_mnar <- Eb_complete %>% 
  dplyr::select(date_time, temp, sal, b_r) %>% 
  filter(sal > 15.382) %>% 
  full_join(Er_mnar)

## now do the time series imputation

library(imputeTS)

# MCAR
E_mcar$b_r <- as.factor(E_mcar$b_r)
E_mcar_imp <- E_mcar

for (b_r.arg in 1:2) {
  for (target in 1:2) {
    b_r.value=levels(E_mcar_imp$b_r)[b_r.arg]
    target.value=if (target==1) "temp" else if (target==2) "sal"
    subset.df <- E_mcar_imp %>% filter(b_r == b_r.value) %>%  dplyr::select(target.value)
    ts.df <- ts(subset.df)
    # ts.df <- ts(subset.df, start = c(2017,4), frequency = 366*24*4) # make time series object
    #if (method=="kalman") imp <- na_kalman(ts.df)
    #imp.v <- as.numeric(imp)
    imp <- na_kalman(ts.df)
    plotNA.distribution(ts.df)
    plotNA.imputations(ts.df,imp)
    E_mcar_imp[E_mcar_imp$b_r == b_r.value,target.value] <- imp
    # newdf[site == site.value & b_r == b_r.value] <- newdf %>% filter(site==site.value & b_r ==b_r.value) %>% 
    #   mutate(sal=imp) %>% as.data.frame()
  }
}

# MAR
E_mar$b_r <- as.factor(E_mar$b_r)
E_mar_imp <- E_mar

for (b_r.arg in 1:2) {
  for (target in 1:2) {
    b_r.value=levels(E_mar_imp$b_r)[b_r.arg]
    target.value=if (target==1) "temp" else if (target==2) "sal"
    subset.df <- E_mar_imp %>% filter(b_r == b_r.value) %>%  dplyr::select(target.value)
    ts.df <- ts(subset.df)
    # ts.df <- ts(subset.df, start = c(2017,4), frequency = 366*24*4) # make time series object
    #if (method=="kalman") imp <- na_kalman(ts.df)
    #imp.v <- as.numeric(imp)
    imp <- na_kalman(ts.df)
    plotNA.distribution(ts.df)
    plotNA.imputations(ts.df,imp)
    E_mar_imp[E_mar_imp$b_r == b_r.value,target.value] <- imp
    # newdf[site == site.value & b_r == b_r.value] <- newdf %>% filter(site==site.value & b_r ==b_r.value) %>% 
    #   mutate(sal=imp) %>% as.data.frame()
  }
}

## MNAR
E_mnar$b_r <- as.factor(E_mnar$b_r)
E_mnar_imp <- E_mnar

for (b_r.arg in 1:2) {
  for (target in 1:2) {
    b_r.value=levels(E_mnar_imp$b_r)[b_r.arg]
    target.value=if (target==1) "temp" else if (target==2) "sal"
    subset.df <- E_mnar_imp %>% filter(b_r == b_r.value) %>%  dplyr::select(target.value)
    ts.df <- ts(subset.df)
    # ts.df <- ts(subset.df, start = c(2017,4), frequency = 366*24*4) # make time series object
    #if (method=="kalman") imp <- na_kalman(ts.df)
    #imp.v <- as.numeric(imp)
    imp <- na_kalman(ts.df)
    plotNA.distribution(ts.df)
    plotNA.imputations(ts.df,imp)
    E_mnar_imp[E_mnar_imp$b_r == b_r.value,target.value] <- imp
    # newdf[site == site.value & b_r == b_r.value] <- newdf %>% filter(site==site.value & b_r ==b_r.value) %>% 
    #   mutate(sal=imp) %>% as.data.frame()
  }
}


# now do the mice imputation

E_mcar$time <- as.POSIXct(E_mcar$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )
E_mcar <- E_mcar %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE) %>% 
  dplyr::select(-clocktime, -date_time)

mice_MCAR <- mice(E_mcar[,c("b_r","temp","sal","hour","date")], m=5, seed=1)

# MAR
E_mar$time <- as.POSIXct(E_mar$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )
E_mar <- E_mar %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE) %>% 
  dplyr::select(-clocktime, -date_time)

mice_MAR <- mice(E_mar[,c("b_r","temp","sal","hour","date")], m=5, seed=1)

# MNAR
E_mnar$time <- as.POSIXct(E_mnar$date_time*24*3600 + as.POSIXct("1899-12-29 23:00") )
E_mnar <- E_mnar %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE) %>% 
  dplyr::select(-clocktime, -date_time)

mice_MNAR <- mice(E_mnar[,c("b_r","temp","sal","hour","date")], m=5, seed=1)

## now summarize everything and see how means compare between true & imputed

## complete case first

Ejoint <- Er_complete %>% 
  full_join(Eb_complete) %>% 
  select(-cond, -s_v)

Ejoint <- Ejoint %>% 
  mutate(time = as.POSIXct(Ejoint$date_time*24*3600 + as.POSIXct("1899-12-29 23:00"))) %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE)

true_values_daily <- Ejoint %>% 
  dplyr::select(-clocktime, -date_time) %>% 
  group_by(b_r, date) %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

true_mean_dailies <- true_values_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

true_values_overall <- Ejoint %>% 
  group_by(b_r) %>%
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

true_values <- true_mean_dailies %>% 
  full_join(true_values_overall)%>% 
  mutate(missing.data = "no_missing") %>% 
  mutate(imputation.method = "none")

## now with missing df

ts_mcar_daily <- E_mcar %>% 
  group_by(b_r, date) %>% 
  na.omit() %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

ts_mcar_meandaily <- ts_mcar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

ts_mcar_overall <- E_mcar %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

ts_mcar <-  ts_mcar_meandaily %>% 
  full_join(ts_mcar_overall)%>% 
  mutate(missing.data = "MCAR")

## ts imputation mar

ts_mar_daily <- E_mar %>% 
  group_by(b_r, date) %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

ts_mar_meandaily <- ts_mar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

ts_mar_overall <- E_mar %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

ts_mar <-  ts_mar_meandaily %>% 
  full_join(ts_mar_overall)%>% 
  mutate(missing.data = "MAR")

## ts mnar

ts_mnar_daily <- E_mnar %>% 
  group_by(b_r, date) %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

ts_mnar_meandaily <- ts_mnar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

ts_mnar_overall <- E_mnar %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

ts_mnar <-  ts_mnar_meandaily %>% 
  full_join(ts_mnar_overall) %>% 
  mutate(missing.data = "MNAR")

# mice mcar

mice_MCAR_long <- mice::complete(mice_MCAR, action = "long")

mice_mcar_daily <- mice_MCAR_long %>% 
  group_by(b_r, date) %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

mice_mcar_meandaily <- mice_mcar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

mice_mcar_overall <- mice_MCAR_long %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

mice_mcar <-  mice_mcar_meandaily %>% 
  full_join(mice_mcar_overall) %>% 
  mutate(missing.data = "MCAR")

# mice mar 

mice_MAR_long <- mice::complete(mice_MAR, action = "long")

mice_mar_daily <- mice_MAR_long %>% 
  group_by(b_r, date) %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

mice_mar_meandaily <- mice_mar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

mice_mar_overall <- mice_MAR_long %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

mice_mar <-  mice_mar_meandaily %>% 
  full_join(mice_mar_overall) %>% 
  mutate(missing.data = "MAR")

# mice mnar
mice_MNAR_long <- mice::complete(mice_MNAR, action = "long")

mice_mnar_daily <- mice_MNAR_long %>% 
  group_by(b_r, date) %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

mice_mnar_meandaily <- mice_mnar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

mice_mnar_overall <- mice_MNAR_long %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

mice_mnar <-  mice_mnar_meandaily %>% 
  full_join(mice_mnar_overall) %>% 
  mutate(missing.data = "MNAR")

## now complete case analysis
E_mcar <- E_mcar %>% 
  mutate(time = as.POSIXct(E_mcar$date_time*24*3600 + as.POSIXct("1899-12-29 23:00"))) %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE)

E_mcar$b_r <- as.factor(E_mcar$b_r)

mcar_daily <- E_mcar %>% 
  group_by(b_r, date) %>%
  na.omit() %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

mcar_meandaily <- mcar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

mcar_overall <- E_mcar %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

cca_mcar <- mcar_overall %>% 
  full_join(mcar_meandaily) %>% 
  mutate(missing.data = "MCAR")

## cca mar data

E_mar <- E_mar %>% 
  mutate(time = as.POSIXct(E_mar$date_time*24*3600 + as.POSIXct("1899-12-29 23:00"))) %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE)

E_mar$b_r <- as.factor(E_mar$b_r)

mar_daily <- E_mar %>% 
  group_by(b_r, date) %>%
  na.omit() %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

mar_meandaily <- mar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

mar_overall <- E_mar %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

cca_mar <- mar_overall %>% 
  full_join(mar_meandaily) %>% 
  mutate(missing.data = "MAR")

## cca mnar data

E_mnar <- E_mnar %>% 
  mutate(time = as.POSIXct(E_mnar$date_time*24*3600 + as.POSIXct("1899-12-29 23:00"))) %>% 
  mutate(hour = as.POSIXlt(time)$hour) %>% 
  separate(col = "time", into = c("date","clocktime"),sep=" ", remove = TRUE)

E_mnar$b_r <- as.factor(E_mnar$b_r)

mnar_daily <- E_mnar %>% 
  group_by(b_r, date) %>%
  na.omit() %>% 
  dplyr::summarise(daily.max.t = max(temp),
                   daily.min.t = min(temp),
                   daily.min.s = min(sal))

mnar_meandaily <- mnar_daily %>% 
  group_by(b_r) %>% 
  dplyr::summarise(daily.mean.maxt = mean(daily.max.t),
                   daily.mean.mint = mean(daily.min.t),
                   daily.mean.mins = mean(daily.min.s))

mnar_overall <- E_mnar %>% 
  group_by(b_r) %>%
  na.omit() %>% 
  dplyr::summarise(average.temp = mean(temp),
                   average.sal = mean(sal),
                   total.dh = degree_hours_above(29, temp),
                   total.sh = degree_hours_below(20, sal))

cca_mnar <- mnar_overall %>% 
  full_join(mnar_meandaily) %>% 
  mutate(missing.data = "MNAR")

# join together analysis types

ts_summaries <- ts_mcar %>% 
  full_join(ts_mar) %>% 
  full_join(ts_mnar) %>% 
  mutate(imputation.method = "Time series")

mice_summaries <- mice_mcar %>% 
  full_join(mice_mar) %>% 
  full_join(mice_mnar) %>% 
  mutate(imputation.method = "Multiple imputation")

cca_summaries <- cca_mcar %>% 
  full_join(cca_mar) %>% 
  full_join(cca_mnar) %>% 
  mutate(imputation.method = "Complete case")
 
full_summary <- true_values %>% 
  full_join(ts_summaries) %>% 
  full_join(mice_summaries) %>% 
  full_join(cca_summaries)


full_summary

write.csv(full_summary, "imputation_method_test_results.csv")
