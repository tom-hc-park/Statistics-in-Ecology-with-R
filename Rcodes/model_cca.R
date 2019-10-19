if (!require("mgcv")) install.packages("mgcv")
if (!require("tidyverse")) install.packages("tidyverse")

library(mgcv)
library(tidyverse)

salt_temp_summary <- read.csv("../data/summary.csv")
growth_rate <- read.csv("../data/growth_rate_response.csv")
df_full <- read.csv("../data/full_data.csv")

# create summary of growth rate data (don't need this part)
#growth_rate_summary <- growth_rate %>% 
#group_by(site, b_r, bag, month, outplant_time) %>% 
#dplyr::summarise(av_gr = mean(growth_rate)) %>% 
# make a new column for subsite (e.g. A r for raft at site A)
# we will use this for our nested random variable
#mutate(subsite = paste(site, b_r))

# now unite the oyster response data with the summarized environmental data
oysters_full <- growth_rate %>% 
  full_join(salt_temp_summary) %>% 
  mutate(subsite = paste(site, b_r))

# growth rate changes non-linearly with time
par(mar=c(4.5,4.4,2,2))
plot(growth_rate ~ outplant_time, data = oysters_full, ylab = "Growth rate (mm/week)",
     xlab = "Time since experiment start (weeks)", cex.lab = 1.5)

# but can transform it to fix
plot(growth_rate ~ exp(-1/outplant_time), data = oysters_full, ylab = "Growth rate (mm/week)",
     xlab = "Time since experiment start (weeks)", cex.lab = 1.5)

# create rescaled dataframe
oysters_rescaled <- oysters_full %>% 
  mutate(outplant_time = exp(-1/outplant_time)*10,
         tot_dh_t = tot_dh_t - mean(tot_dh_t),
         tot_dh_s = (tot_dh_s - mean(tot_dh_s))/100)

# split into raft and beach data
just_rafts <- oysters_rescaled %>% 
  filter(b_r == "r")

just_beach <- oysters_rescaled %>% 
  filter(b_r == "b")

# let's try a linear mixed model
lm_oysters_r <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
                     av_dmin_t + av_dmin_s + tot_dh_s +
                     outplant_time,
                   data = just_rafts)
plot(lm_oysters_r)
summary(lm_oysters_r)

vcov(lm_oysters_r)

or_res <- residuals(lm_oysters_r, type = "pearson")
acf(or_res)
# no real temporal autocorrelation

anova(lm_oysters_r)

lm_oysters_b <- lm(growth_rate ~ temp_av + sal_av + av_dmax_t +
                     av_dmin_t + tot_dh_t + tot_dh_s +
                     outplant_time,
                   data = just_beach)
plot(lm_oysters_b)
summary(lm_oysters_b)

ob_res <- residuals(lm_oysters_b, type = "pearson")
acf(ob_res)
# minor autocorrelation, but let's ignore for this analysis

anova(lm_oysters_b)

### follow this part of the script for gamm

gamm_oysters <- gamm(growth_rate ~ temp_av + sal_av + av_dmax_t + 
                       av_dmin_t + av_dmin_s + tot_dh_t + tot_dh_s +
                       s(outplant_time), random = list(subsite = ~1, bag = ~1),
                     data = oysters_full)
qq.gam(gamm_oysters$gam)

# qq plot is pretty straight - residuals look normal except for one point

# create normalized residuals
res_gamm <- residuals(gamm_oysters$lme, type = "normalized")
acf(res_gamm)
# not substantial autocorrelation, so don't need variance-covariance matrix

hist(res_gamm, breaks = 100)
# residuals are quite normally distributed

summary(gamm_oysters$gam)
summary(gamm_oysters$lme)

anova(gamm_oysters$gam)

## I talked to my supervisor, and he told me we should look at
# the beach oysters and raft oysters separately because they 
# are such different growing environments.
# same formula as before
gamm_oysters_raft <- gamm(growth_rate ~ temp_av + sal_av + av_dmax_t + 
                            av_dmin_t + av_dmin_s + tot_dh_t + tot_dh_s +
                            s(outplant_time), random = list(subsite = ~1, bag = ~1),
                          data = just_rafts)
summary(gamm_oysters_raft$gam)
anova(gamm_oysters_raft$gam)

or_res <- residuals(gamm_oysters_raft$lme, type = "normalized")
hist(or_res)

qq.gam(gamm_oysters_raft$gam)

gamm_oysters_beach <- gamm(growth_rate ~ temp_av + sal_av + av_dmax_t + 
                             av_dmin_t + av_dmin_s + tot_dh_t + tot_dh_s +
                             s(outplant_time), random = list(subsite = ~1, bag = ~1),
                           data = just_beach)
summary(gamm_oysters_beach$gam)
anova(gamm_oysters_beach$gam)

ob_res <- residuals(gamm_oysters_beach$lme, type = "normalized")
hist(ob_res)

qq.gam(gamm_oysters_beach$gam)

# Everything looks pretty good in terms of meeting assumptions