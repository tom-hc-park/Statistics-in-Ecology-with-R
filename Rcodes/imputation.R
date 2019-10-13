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

# now we try a complete case analysis

# first, we need to make the data frame from which to create a model

salt_temp_summary <- read.csv("../data/summary.csv")

growth_rate <- read.csv("../data/growth_rate_response.csv")

oysters_full <- growth_rate %>% 
  full_join(salt_temp_summary)

head(oysters_full)

oysters_raft <- oysters_full %>% 
  filter(b_r == "r")

# need to rescale the predictors, particularly the degree hours
oysters_rescaled <- oysters_full %>% 
  mutate(tot_dh_t = (tot_dh_t - mean(tot_dh_t))/10,
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/10)

lmer_oysters <- lmer(growth_rate ~ temp_av + sal_av + av_dmax_t + 
                          av_dmin_t + av_dmin_s + tot_dh_t + tot_dh_s +
                          outplant_time + (1 | bag/site),
                        data = oysters_rescaled)

