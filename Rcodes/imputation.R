
if (!require("mice")) install.packages("mice", repos="http://cran.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("VIM")) install.packages("VIM")
if (!require("Amelia")) install.packages("Amelia")
if (!require("mice")) install.packages("mice", dependencies = TRUE)
if (!require("lme4")) install.packages("lme4")

install.packages("broom", "mitml")

# load required libraries
library(VIM)
library(Amelia)
library(mice)
library(lme4)
library(mice)
library(tidyverse)

salt_temp <- read.csv("../data/date_transformed_salinity_temperature.csv")
mort<- read.csv("../data/mort_data.csv")
growth <- read.csv("../data/full_data.csv")

aggr_st <- aggr(salt_temp, numbers= TRUE, sortVars = TRUE)
# ~5% of the salinity data is missing and <1% of the temperature data

marginplot(salt_temp[,c("sal","date_time")])
# 0 salinity's are from when the tide is off
# when the sand blocks the logger, we have too low salinity values 
# the missingness seems to be MCAR because the missingness is not related to 
#the value of data variables. 

marginplot(salt_temp[,c("temp","date_time")])
# again, the date_time has nth to do with the temperature missingness
# It's just missing completely at random
# that means that we can use complete case analysis and that will give us unbiased estimate
# the red dot is when Amelia lost her logger. 
# it's MCAR and we cannot say a specific date value for date variable has effect on missingness
# on missing values of temp; she just randomly lost the logger on that day.
 
marginplot(salt_temp[,c("sal","temp")])
# revised: when temp is missing, salinity is not always missing
# can we infer that from this plot?
# summer: high temp, low salinity
# winter: low temp, high salinity
# we can say that the missingness happens when the weather was spring/summer

# I have to check that when temp is missing there's no way that sal alive.
x <- is.na(salt_temp$temp)
y <- is.na(salt_temp$sal)
df <- data.frame("temp"=x, "sal"=y)
n = nrow(salt_temp)
plot(salt_temp$date_time, salt_temp$sal)

df %>% filter(temp==T, sal==F)
# EMB requires multi normal distribuion and MAR assumption.
salt_temp %>% filter(is.na(temp)==T)
# so only temp missing and only sal missing is all possible

# checking for multinormality
hist(salt_temp$temp)
qqnorm(salt_temp$temp)

hist(salt_temp$sal)
qqnorm(salt_temp$sal)
# multi normality implies marginal normality.
# there is no way that multi normality holds.
# we bravely skip EMB part and just go for MCMC MICE

# *temp* missing situations
# programmed it wrong, or data logger missing(lost it)
# june part, programm was wrong
# one bag of oyster missing, oysters and the logger missing

# *salt* missing situations
# sand get the detect part, which leads false 0s, or really really low values
# ex. 25 is proper lower than 10 

# more salinity missing 
# imputing variables can include site,(beach, raft also location)
# no MAR: we cannot say that Amelia tends to lost her logger on some specific locations.
# temp/salinity ~ time + site + beach/raft

# EMB worst :transformation needed if we wanna use this 
# MCMC vs times series 
# fit(mcmc) vs fit(time series)

## time series imputation modeling

# univariate or multivariate? there is a relationship between temp and salinity
# as we discovered earlier. 

# univariate first
if (!require("imputeTS")) install.packages("imputeTS")
library(imputeTS)
str(salt_temp)
df <- salt_temp
df$date <- as.Date(df$date)
str(df)
(unique(df$date))
(min(df$date_time))
(max(df$date_time))
ts.df <- ts(salt_temp[c('sal','temp')], start = )
# we fill out missing times 
# maybe empty data frame and then join them with keys date_time, site and b_r

df$date_time[1]
df %>% select(site,b_r,date_time) %>% filter((date_time<=42824.54 & date_time >=42824.53))
df %>% filter(date_time==42824.53)
plotNA.distribution(salt_temp$temp)
plotNA.distribution(salt_temp$sal)
plotNA.distribution(ts.df)
## model part
# we may have to fit gam because the model assumption violation

# nonlinear regression

# nonparameteric model 
