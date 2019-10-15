
if (!require("mice")) install.packages("mice", repos="http://cran.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("VIM")) install.packages("VIM")
if (!require("Amelia")) install.packages("Amelia")
if (!require("mice")) install.packages("mice", dependencies = TRUE)
if (!require("lme4")) install.packages("lme4")
if (!require("imputeTS")) install.packages("imputeTS")

# load required libraries
library(VIM)
library(Amelia)
library(mice)
library(lme4)
library(mice)
library(tidyverse)
library(imputeTS)

salt_temp <- read.csv("../data/date_transformed_salinity_temperature.csv")

df <- salt_temp
str(df)
summary(df)
dfM <- mice(df[,c("site", "b_r","temp","sal","hour","date")], m=5, seed=1)
save(dfM, file = "../data/mice_imputed_data.RData")
#### please run codes until here and then use the dfM for your model fit


aggr_st <- aggr(salt_temp, numbers= TRUE, sortVars = TRUE)
# ~5% of the salinity data is missing and <1% of the temperature data

## possible model: temp/salinity ~ time + site + beach/raft 

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
# revised: when temp is missing, salinity is always missing
# can we infer that from this plot?
# summer: high temp, low salinity
# winter: low temp, high salinity
# we can say that the missingness happens when the weather was spring/summer

marginplot(salt_temp[,c("sal","site")])
# most of the missing values from site 3 and 4. 

marginplot(salt_temp[,c("sal","b_r")])
# missing values happening all locations.

# EMB requires multi normal distribuion and MAR assumption.
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


# EMB worst :transformation needed if we wanna use this 
# MCMC vs times series 
# fit(mcmc) vs fit(time series)  vs complete case

## time series imputation modeling

# univariate or multivariate time series imputation:
# multi is too advanced: there is a relationship between temp and salinity




