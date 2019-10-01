library(tidyverse)

setwd("/Users/Amelia/Documents/UBC/R_projects/STAT506H")

# read in data

growth_0 <- read_csv("growth_data_zero.csv")

growth_may <- read_csv("growth_data_may.csv")

growth_july <- read_csv("growth_data_july.csv")

growth_sept <- read_csv("growth_data_sept.csv")

growth_nov <- read_csv("growth_data_nov.csv")

growth_1 <- read_csv("growth_data_march.csv")

for (i in 1:6){
  growth_0 <- growth_0 %>% 
    dplyr::bind_rows(growth_0)
}

growth_0 <- growth_0 %>% 
  slice(1:2500)

growth_0 <- growth_0 %>% 
  rename(outplant_time = "outplant time")

site <- c(rep("A", times = 500), rep("B", times = 500),
           rep("C", times = 500), rep("D", times = 500),
           rep("E", times = 500)) 

b_r <- rep(c(rep("b", times = 250), rep("r", times = 250)), times = 5)

bag <- rep(c(rep("1", times = 50),rep("2", times = 50),
          rep("3", times = 50), rep("4", times = 50),
          rep("5", times = 50)), times = 10)

front_end <- cbind(site,b_r,bag)

front_end <- as.data.frame(front_end)

x <- cbind(front_end, growth_0)

growth_0 <- x %>% 
  dplyr::select(1:4, 7)

growth_0$site <- as.factor(growth_0$site)
growth_0$bag <- as.factor(growth_0$bag)
growth_0$b_r <- as.factor(growth_0$b_r)

## join rest of data

growth_july <- growth_july %>% 
  rename("site" = "Site")

growth_1 <- growth_1 %>% 
  rename("shell_length" = "length")

growth_sept <- growth_sept %>% 
  rename("shell_length" = "length")

growth_full <- growth_may %>% 
  full_join(growth_july) %>% 
  full_join(growth_sept) %>% 
  full_join(growth_nov) %>% 
  full_join(growth_1) %>% 
  select(1:3, 5, 10)

growth_full$site <- as.factor(growth_full$site)
growth_full$bag <- as.factor(growth_full$bag)
growth_full$b_r <- as.factor(growth_full$b_r)
growth_full$outplant_time <- as.numeric(growth_full$outplant_time)

growth <- growth_full %>% 
  full_join(growth_0)

growth <- na.omit(growth)
growth_summary <- growth %>% 
  group_by(site, b_r, outplant_time) %>% 
  summarize(av_length = mean(shell_length), se_length =
              sd(shell_length)/sqrt(length(shell_length)))

growth_plot <- ggplot(aes(x = outplant_time, y = av_length, colour = site, shape = b_r),
                      data = growth_summary) +
  geom_point(size = 3) +
  ylab("Mean length (mm)") +
  xlab("Time since outplant (weeks)") +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = b_r)) +
  geom_errorbar(aes(ymax = av_length + se_length, ymin = av_length - se_length))

growth_plot

enviro <- read_csv("salinity_temperature.csv")

low_sal <- enviro %>%
  filter(b_r == "r" & sal <= 20)
high_temp <- enviro %>% 
  filter(b_r == "b", temp > 29.22) #29.22 C is the ABT
