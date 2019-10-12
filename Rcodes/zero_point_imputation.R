library(mice)
library(tidyverse)

zero <- read.csv("../data/oyster_zeropoint.csv")

# create a character vector for site
site <- c(rep("A", times = 150), rep("B", times = 150),
          rep("C", times = 150), rep("D", times = 150),
          rep("E", times = 150)) 

# create a character vector for beach & raft subsites
b_r <- rep(c(rep("b", times = 75), rep("r", times = 75)), times = 5)

# create a vector for each of the five bags within each subsite & site
bag <- rep(c(rep("1", times = 15),rep("2", times = 15),
             rep("3", times = 15), rep("4", times = 15),
             rep("5", times = 15)), times = 10)

# create outplant time vector
outplant_time <- rep("0", times = 750)

# empty shell length vector
shell_length <- rep(NA, times = 750)

# create empty data frame to impute
empty_zero_data <- as.data.frame(cbind(site, b_r, bag, outplant_time))

# add relevant columns with dummy site, b_r, and bag columns to allow imputation
zero <- zero %>% 
  select(outplant_time, shell_length) %>% 
  mutate(site = "F", b_r = "b", bag = 1) 

# create appropriate classes to allow joining data
zero$site <- as.factor(zero$site)
zero$b_r <- as.factor(zero$b_r)
zero$bag <- as.factor(zero$bag)
zero$outplant_time <- as.factor(zero$outplant_time)
zero$shell_length <- as.numeric(zero$shell_length)

# join empty and real data
full_df <- zero %>% 
  full_join(empty_zero_data)

# use sample of 50 oysters to impute rest of data using EMB
zero_data <- mice(full_df, m = 5, method = "norm.nob",
                  seed = 1, maxit = 1)
regZd <- with(zero_data, lm(shell_length ~ 1))
regZdPool <- pool(regZd)

# create complete dataset
for (i in 1:zero_data$m) {
  oysters_zero <- complete(zero_data, i)
}

# remove real data
oysters_zero <- oysters_zero %>% 
  filter(site != "F")

# plot what the real and imputed data look like
plot(full_df$shell_length ~ 1, ylim = c(15,35))
points(oysters_zero$shell_length ~ 1, col = "green")

# write output
write.csv(oysters_zero, "../data/imputed_zero.csv")
