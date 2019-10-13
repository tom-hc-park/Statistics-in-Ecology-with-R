
if (!require("mice")) install.packages("mice", repos="http://cran.r-project.org")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("VIM")) install.packages("VIM")
if (!require("Amelia")) install.packages("Amelia")
if (!require("mice")) install.packages("mice", dependencies = TRUE)
install.packages("broom", "mitml")
library(mice)
library(tidyverse)

salt_temp <- read.csv("../data/date_transformed_salinity_temperature.csv")
mort<- read.csv("../data/mort_data.csv")
growth <- read.csv("../data/full_data.csv")
head(salt_temp)
if (!require("VIM")) install.packages("VIM")
if (!require("Amelia")) install.packages("Amelia")
if (!require("mice")) install.packages("mice")
if (!require("lme4")) install.packages("lme4")

# load required libraries
library(VIM)
library(Amelia)
library(mice)
library(lme4)

# check the missing case
salt_temp <- read.csv("../data/date_transformed_salinity_temperature.csv")

aggr_st <- aggr(salt_temp, numbers= TRUE, sortVars = TRUE)

# ~5% of the salinity data is missing and <1% of the temperature data

marginplot(salt_temp[,c("sal","date_time")])
marginplot(salt_temp[,c("temp","date_time")])
marginplot(salt_temp[,c("sal","temp")])

n = nrow(salt_temp)
plot(salt_temp$date_time, salt_temp$sal)

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
