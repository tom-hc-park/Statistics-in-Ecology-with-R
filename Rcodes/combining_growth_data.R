# read data sets
df_sep<- read.csv(file='../data/growth_data_sept.csv')
df_march<- read.csv(file='../data/growth_data_march.csv')
df_may<- read.csv(file='../data/growth_data_may.csv')
df_july<- read.csv(file='../data/growth_data_july.csv')
df_nov<- read.csv(file='../data/growth_data_nov.csv')
df_zero <- read.csv("../data/growth_data_zero.csv")

# install packages
if (!require("plyr")) install.packages(plyr)
if (!require("dplyr")) install.packages(dplyr)

# function that tidy a data set
tidydata <- function(df) {
 # try catch can catch errors from invididual data
  if ("length" %in% names(df)) df$shell_length <- df$length
  if (!"wet_mass" %in% names(df)) {
    df$wet_mass = tryCatch(
      {
        unlist(df %>% select(whole_mass))
      }, error = function(e) {
        print(paste("whole mass doesn't exist:", e))
        unlist(df %>% select(whole_oyster))
      }, finally = {
        print("renaming as wet_mass done")
      })
  }
  
  if (!"dry_whole" %in% names(df)) {
    df$dry_whole = tryCatch(
      {
        unlist(df %>% select(whole_dry))
      }, error = function(e) {
        print(paste("whole dry doesn't exist:", e))
      }, finally = {
        print("renaming as dry_whole done")
      })
  }
  df <- df %>% select(site, b_r, bag, outplant_time, shell_length, 
                      wet_mass, dry_whole, dry_tissue)
  return(df)
}

# apply functions to data sets 
march <- tidydata(df_march)
july <- tidydata(df_july)
may <- tidydata(df_may)
sep <- tidydata(df_sep)
nov <- tidydata(df_nov)


# combine data
df_full <- rbind.fill(march, july, may, sep, nov)

# check names 
names(df_full)


# validation
nrow(df_full) ==  nrow(df_july) +nrow(df_march) +nrow(df_may) +nrow(df_sep) +nrow(df_nov)
# we can see no observation is missing.

write.csv(df_full,file = "../data/full_data.csv")
