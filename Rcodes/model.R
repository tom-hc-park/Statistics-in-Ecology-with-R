if (!require("mgcv")) install.packages("mgcv")
if (!require("tidyverse")) install.packages("tidyverse")

library(mgcv)
library(tidyverse)

salt_temp_summary <- read.csv("../data/summary.csv")
growth_rate <- read.csv("../data/growth_rate_response.csv")

# create summary of growth rate data 
growth_rate_summary <- growth_rate %>% 
  group_by(site, b_r, bag, month, outplant_time) %>% 
  # make a new column for subsite (e.g. A r for raft at site A)
  # we will use this for our nested random variable
  mutate(subsite = paste(site, b_r))

# now unite the oyster response data with the summarized environmental data
oysters_full <- growth_rate_summary %>% 
  full_join(salt_temp_summary)

# growth rate changes non-linearly with time
plot(av_gr ~ outplant_time, data = oysters_rescaled)

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

# separate data frame into just raft oysters and just beach oysters
just_rafts <- oysters_full %>% 
  filter(b_r == "r")

just_beach <- oysters_full %>% 
  filter(b_r == "b")

# then make the models, same formula as before
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