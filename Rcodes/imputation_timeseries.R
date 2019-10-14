
if (!require("mice")) install.packages("mice", repos="http://cran.r-project.org")
if (!require("mice")) install.packages("mice", dependencies = TRUE)
if (!require("lme4")) install.packages("lme4")
if (!require("imputeTS")) install.packages("imputeTS")

# load required libraries

library(mice)
library(dplyr)
library(imputeTS)


salt_temp <- read.csv("../data/date_transformed_salinity_temperature.csv")

# new df for imputation
newdf <- salt_temp

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

write.csv(newdf, "../data/time_series_imputed.csv")




## model part

# we may have to fit gam because the model assumption violation

# nonlinear regression

# nonparameteric model 

























###############Trash box########################
# 
# 
# subset_ftn <- function(df, site.arg, b_r.arg, target) {
#   site.value=levels(df$site)[site.arg]
#   b_r.value=levels(df$b_r)[b_r.arg]
#   targer.value=if (target==1) "temp" else if (target==2) "sal"
#   subset.df <- df %>% filter(site == site.value & b_r == b_r.value) %>% select(targer.value)
#   return(subset.df)
# }
# 
# 
# # we need to replace the original values of data frame with imputed values 
# replace_NA <- function(df, site.arg, b_r.arg, targer) {
#   temp <- subset_ftn(df,1,1,2) #generate a subset
#   ts.df <- ts(temp) # make time series object
#   imp <- na_kalman(ts.df) #apply a function 
# }
# 
# subset_replace <- function(df, site.arg, b_r.arg, target, method="kalman") {
#   site.value=levels(df$site)[site.arg]
#   b_r.value=levels(df$b_r)[b_r.arg]
#   targer.value=if (target==1) "temp" else if (target==2) "sal"
#   subset.df <- df %>% filter(site == site.value & b_r == b_r.value) %>% select(targer.value)
#   ts.df <- ts(subset.df) # make time series object
#   if (method=="kalman") imp <- na_kalman(ts.df)
#   #imp.v <- as.numeric(imp)
#   newdf <- df %>% filter(site==site.value & b_r ==b_r.value) %>% 
#     mutate(sal=imp) %>% as.data.frame()
#   return(newdf)
# }
# 
# 
# 
# # asdf
# ## generate full data
# min <- min(df$date_time)
# max <- max(df$date_time)
# date_time <- seq(min,max,0.01)
# n=length(date_time)
# date_time <- rep(date_time,10)
# site <- c(rep("A", times = 2*n), rep("B", times = 2*n),
#           rep("C", times = 2*n), rep("D", times = 2*n),
#           rep("E", times = 2*n)) 
# b_r <- rep(c(rep("b", times = n), rep("r", times = n)), times = 5)
# empty <- data.frame("site"=site,"b_r"=b_r,"date_time"=date_time)
# empty$site <- factor(empty$site, levels = c("A","B","C","D","E"))
# empty$b_r <- factor(empty$b_r, levels = c("b","r"))
# 
# df$date_time<- as.integer(df$date_time*100)
# empty$date_time<- as.integer(empty$date_time*100)
# 
# fulldf  <- merge(x=empty,y=df,by=c("date_time","site","b_r"), all = T) 