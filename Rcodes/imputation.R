
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

# I have to check that when temp is missing there's no way that sal alive.
x <- is.na(salt_temp$temp)
y <- is.na(salt_temp$sal)
tf.df <- data.frame("temp"=x, "sal"=y)
tf.df %>% filter(temp==T, sal==T)
tf.df %>% filter(temp==T, sal==F) # 0 rows
# note that temp missing implies salt always missing
tf.df %>% filter(temp==F, sal==T) # only salt missing possible

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

# univariate first
df <- salt_temp

# time series imputation
# we just apply the time series imputation and see the result by plot
# subset of for each location and site, apply the imputation and then see the result

# new df for imputation
newdf <- df

#proto type: using kalman spline, giving us a complete data set (single imputation)
sum(is.na(newdf$temp)) # check number of na
sum(is.na(newdf$sal)) 
for (site.arg in 1:5) {
  for (b_r.arg in 1:2) {
    for (target in 1:2) {
      site.value=levels(newdf$site)[site.arg]
      b_r.value=levels(newdf$b_r)[b_r.arg]
      targer.value=if (target==1) "temp" else if (target==2) "sal"
      subset.df <- newdf %>% filter(site == site.value & b_r == b_r.value) %>% select(targer.value)
      
      ts.df <- ts(subset.df)
      # ts.df <- ts(subset.df, start = c(2017,4), frequency = 366*24*4) # make time series object
      #if (method=="kalman") imp <- na_kalman(ts.df)
      #imp.v <- as.numeric(imp)
      imp <- na_kalman(ts.df)
      plotNA.distribution(ts.df)
      plotNA.imputations(ts.df,imp)
      newdf[newdf$site == site.value & newdf$b_r == b_r.value,targer.value] <- imp
      # newdf[site == site.value & b_r == b_r.value] <- newdf %>% filter(site==site.value & b_r ==b_r.value) %>% 
      #   mutate(sal=imp) %>% as.data.frame()
    }
  }
} # warning: no justification of kalman spline yet.

sum(is.na(newdf$temp)) #verify all na is gone.
sum(is.na(newdf$sal)) #verify all na is gone.

# for choosing imputation values 

## we apply MCMC mice imputation:

write_csv(newdf, "../data/time_series_imputed.csv")




## model part

# we may have to fit gam because the model assumption violation

# nonlinear regression

# nonparameteric model 



























































































subset_ftn <- function(df, site.arg, b_r.arg, target) {
  site.value=levels(df$site)[site.arg]
  b_r.value=levels(df$b_r)[b_r.arg]
  targer.value=if (target==1) "temp" else if (target==2) "sal"
  subset.df <- df %>% filter(site == site.value & b_r == b_r.value) %>% select(targer.value)
  return(subset.df)
}


# we need to replace the original values of data frame with imputed values 
replace_NA <- function(df, site.arg, b_r.arg, targer) {
  temp <- subset_ftn(df,1,1,2) #generate a subset
  ts.df <- ts(temp) # make time series object
  imp <- na_kalman(ts.df) #apply a function 
}

subset_replace <- function(df, site.arg, b_r.arg, target, method="kalman") {
  site.value=levels(df$site)[site.arg]
  b_r.value=levels(df$b_r)[b_r.arg]
  targer.value=if (target==1) "temp" else if (target==2) "sal"
  subset.df <- df %>% filter(site == site.value & b_r == b_r.value) %>% select(targer.value)
  ts.df <- ts(subset.df) # make time series object
  if (method=="kalman") imp <- na_kalman(ts.df)
  #imp.v <- as.numeric(imp)
  newdf <- df %>% filter(site==site.value & b_r ==b_r.value) %>% 
    mutate(sal=imp) %>% as.data.frame()
  return(newdf)
}




## generate full data
min <- min(df$date_time)
max <- max(df$date_time)
date_time <- seq(min,max,0.01)
n=length(date_time)
date_time <- rep(date_time,10)
site <- c(rep("A", times = 2*n), rep("B", times = 2*n),
          rep("C", times = 2*n), rep("D", times = 2*n),
          rep("E", times = 2*n)) 
b_r <- rep(c(rep("b", times = n), rep("r", times = n)), times = 5)
empty <- data.frame("site"=site,"b_r"=b_r,"date_time"=date_time)
empty$site <- factor(empty$site, levels = c("A","B","C","D","E"))
empty$b_r <- factor(empty$b_r, levels = c("b","r"))

df$date_time<- as.integer(df$date_time*100)
empty$date_time<- as.integer(empty$date_time*100)

fulldf  <- merge(x=empty,y=df,by=c("date_time","site","b_r"), all = T)