df_sep<- read.csv(file='../data/growth_data_sept.csv')
df_march<- read.csv(file='../data/growth_data_march.csv')
df_may<- read.csv(file='../data/growth_data_may.csv')
df_july<- read.csv(file='../data/growth_data_july.csv')
df_nov<- read.csv(file='../data/growth_data_nov.csv')

# Why we get arbitrary  variables such as X or X.1? It's because read.csv is a wrapper of read.table,
# The wrapper wraps, i.e. calls, another function that does the real work
# but provides a different or more convenient (or more convenient for a specific purpose) 
# interface to it or specific syntax


if (!require("plyr")) install.packages(plyr)



names(df_march)
names(df_july)
names(df_may)
names(df_sep)
names(df_nov)

df_zero <- read.csv("../data/oyster_zeropoint.csv")

temp <- rbind.fill(df_zero,df_march,df_may,df_july,df_sep,df_nov)

names(temp)

# we need to rename the variables...

