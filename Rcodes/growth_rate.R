## changing growth response from absolute to proportional
library(tidyverse)

oysters_zero <- read.csv("../data/imputed_zero.csv")
df_full <- read.csv("../data/full_data.csv")

# add month and outplant times for the oysters_zero dataframe (from zero_point_imputation.R)
oysters_zero$month <- 0
oysters_zero$outplant_time <- 0

# define variables as appropriate classes for joining later
oysters_zero$bag <- as.factor(oysters_zero$bag)
oysters_zero$outplant_time <- as.numeric(oysters_zero$outplant_time)
df_full$bag <- as.factor(df_full$bag)

# now create a data frame for calculating growth rates
growth_rate <- df_full %>% 
  select(site, b_r, bag, outplant_time, shell_length, month) %>% 
  full_join(oysters_zero) %>% 
  select(-X)

# reclassify variables correctly
growth_rate$month <- as.factor(growth_rate$month)
growth_rate$site <- as.factor(growth_rate$site)
growth_rate$b_r <- as.factor(growth_rate$b_r)

# and create an empty column for growth rate. keep it numeric
growth_rate$growth_rate <- NA
growth_rate$growth_rate <- as.numeric(growth_rate$growth_rate)

# create summary data frame of average shell length at each oyster collection
growth_summary <- growth_rate %>% 
  group_by(site, b_r, bag, outplant_time, month) %>% 
  dplyr::summarise(av_sl = mean(shell_length), sd_sl = sd(shell_length))

# create new column where we subtract the mean shell length of the previous timepoint 
# from absolute shell length for each collection and divide by 
# the difference in outplant time to get a growth rate in millimetres per week

for (i in 2:6) {
  for (p in 1:length(growth_rate$shell_length)) {
      if (growth_rate$month[p] == levels(growth_summary$month)[i]) {
        # define the mean shell length (MSL) from the previous timepoint
        MSL = growth_summary$av_sl[i-1]
        # then get the growth rate in mm / week by subtracting the MSL from the 
        # shell length at the time point of interest, then divide by outplant time
        growth_rate$growth_rate[p] = 
          (growth_rate$shell_length[p] - MSL)/growth_rate$outplant_time[p]
      }
    }
  }

growth_rate <- na.omit(growth_rate)

# write up the dataframe
write_csv(growth_rate, "../data/growth_rate_response.csv")
