
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
# revised: when temp is missing, salinity is always missing
# 
# can we infer that from this plot?
# summer: high temp, low salinity
# winter: low temp, high salinity
# we can say that the missingness happens when the weather was spring/summer
# the plot is saying that 

# I have to check that when temp is missing there's no way that sal alive.


x <- is.na(salt_temp$temp)
y <- is.na(salt_temp$sal)
df <- data.frame("temp"=x, "sal"=y)
n = nrow(salt_temp)
plot(salt_temp$date_time, salt_temp$sal)

df
# EMB requires multi normal distribuion and MAR assumption.

# checking for multinormality
hist(salt_temp$temp)
qqnorm(salt_temp$temp)

qqnorm(salt_temp$sal)

# temp missing situations
# programmed it wrong, or data logger missing(lost it)
# june part, programm was wrong
# one bag of oyster missing, oysters and the logger missing

# salt missing situations
# sand get the detect part, which leads false 0s, or really really low values
# ex. 25 is proper lower than 10 

# more salinity missing 
# if temp missing, that impies salinity missing 
# imputing variables can include site,(beach, raft also location)

# temp/salinity ~ time + site + beach/raft

# EMB worst :transformation needed if we wanna use this 
# MCMC vs times series 
# fit(mcmc) vs fit(time series)

# we may have to fit gam because the model assumption violation

# nonlinear regression

# nonparameteric model 
