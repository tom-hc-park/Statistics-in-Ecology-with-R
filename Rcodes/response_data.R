library(tidyverse)



# read in data
growth_0 <- read.csv("../data/growth_data_zero.csv")

growth_may <- read.csv("../data/growth_data_may.csv")

growth_july <- read.csv("../data/growth_data_july.csv")

growth_sept <- read.csv("../data/growth_data_sept.csv")

growth_nov <- read.csv("../data/growth_data_nov.csv")

growth_1 <- read.csv("../data/growth_data_march.csv")

# concatenate the same data for fifty oysters into a longer data frame
for (i in 1:6){
  growth_0 <- growth_0 %>% 
    dplyr::bind_rows(growth_0)
}

# trim the data down to a length of 2500, since we to simulate data for each bag within subsite & site
growth_0 <- growth_0 %>% 
  slice(1:2500)

#
hist(growth_0$oyster_number)
#



# rename the variable outplant time to make it consistent for later merging
names(growth_0)
growth_0 <- growth_0 %>% 
  rename(outplant_time = "outplant.time")
names(growth_0)

# create a character vector for site
site <- c(rep("A", times = 500), rep("B", times = 500),
           rep("C", times = 500), rep("D", times = 500),
           rep("E", times = 500)) 

# create a character vector for beach & raft subsites
b_r <- rep(c(rep("b", times = 250), rep("r", times = 250)), times = 5)

# create a vector for each of the five bags within each subsite & site
bag <- rep(c(rep("1", times = 50),rep("2", times = 50),
          rep("3", times = 50), rep("4", times = 50),
          rep("5", times = 50)), times = 10)

# create the site, subsite, and bag columns by joining these three vectors
front_end <- cbind(site,b_r,bag)

# change to a dataframe
front_end <- as.data.frame(front_end)

# bind with the cloned response data
x <- cbind(front_end, growth_0)

# select the columns of interest (in this case, only shell length)
growth_0 <- x %>% 
  dplyr::select(1:4, 7)

# change variables to factors
growth_0$site <- as.factor(growth_0$site)
growth_0$bag <- as.factor(growth_0$bag)
growth_0$b_r <- as.factor(growth_0$b_r)

# join with the rest of the data

# first rename problematic columns
growth_july <- growth_july %>% 
  rename("site" = "Site")

growth_1 <- growth_1 %>% 
  rename("shell_length" = "length")

growth_sept <- growth_sept %>% 
  rename("shell_length" = "length")

# now join everything and select meaningful variables
growth_full <- growth_may %>% 
  full_join(growth_july) %>% 
  full_join(growth_sept) %>% 
  full_join(growth_nov) %>% 
  full_join(growth_1) %>% 
  select(1:3, 5, 10)

# change things to factors to allow further joining with zero data
growth_full$site <- as.factor(growth_full$site)
growth_full$bag <- as.factor(growth_full$bag)
growth_full$b_r <- as.factor(growth_full$b_r)
growth_full$outplant_time <- as.numeric(growth_full$outplant_time)

# join with zero data
growth <- growth_full %>% 
  full_join(growth_0)

# this is just to visualize data ...

# omit the NAs that made their way into the dataframe
growth <- na.omit(growth)

# summarize response data as mean and standard error
growth_summary <- growth %>% 
  group_by(site, b_r, outplant_time) %>% 
  summarize(av_length = mean(shell_length), se_length =
              sd(shell_length)/sqrt(length(shell_length)))

# create a plot to visualize growth data over time
growth_plot <- ggplot(aes(x = outplant_time, y = av_length, colour = site, shape = b_r),
                      data = growth_summary) +
  geom_point(size = 3) +
  ylab("Mean length (mm)") +
  xlab("Time since outplant (weeks)") +
  theme_classic() +
  geom_errorbar(aes(ymax = av_length + se_length, ymin = av_length - se_length))
growth_plot

# this is just me playing with the salinity temperature data to see how often
# salinity and temperature conditions were stressful for oysters

enviro <- read.csv("../data/salinity_temperature.csv")

low_sal <- enviro %>%
  filter(b_r == "r" & sal <= 20)
high_temp <- enviro %>% 
  filter(b_r == "b", temp > 29.22) #29.22 C is the ABT
