# read data sets
df_sep<- read.csv(file='../data/growth_data_sept.csv')
df_march<- read.csv(file='../data/growth_data_march.csv')
df_may<- read.csv(file='../data/growth_data_may.csv')
df_july<- read.csv(file='../data/growth_data_july.csv')
df_nov<- read.csv(file='../data/growth_data_nov.csv')
df_zero <- read.csv("../data/growth_data_zero.csv")

# add source variable
df_sep$month <- 6
df_march$month <- 12
df_may$month <- 2
df_july$month <- 4
df_nov$month <- 8
df_zero$month <- 0 

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
        (df %>% pull(whole_mass))
      }, error = function(e) {
        print(paste("whole mass doesn't exist:", e))
        (df %>% pull(whole_oyster))
      }, finally = {
        print("renaming as wet_mass done")
      })
  }
  
  if (!"dry_whole" %in% names(df)) {
    df$dry_whole = tryCatch(
      {
        df %>% pull(whole_dry)
      }, error = function(e) {
        print(paste("whole dry doesn't exist:", e))
      }, finally = {
        print("renaming as dry_whole done")
      })
  }
  df <- df %>% select(site, b_r, bag, outplant_time, shell_length, 
                      wet_mass, dry_whole, dry_tissue,month)
  # adding the data frame name as a variable 
  return(df)
}



# apply functions to data sets 
march <- tidydata(df_march)
july <- tidydata(df_july)
may <- tidydata(df_may)
sep <- tidydata(df_sep)
nov <- tidydata(df_nov)

names(df_march)
temp <- df_march %>% pull(dry_whole)

# combine data
df_full <- rbind.fill(march, july, may, sep, nov)

# check names 
names(df_full)

# validation
# total number of rows
nrow(df_full) ==  nrow(df_july) +nrow(df_march) +nrow(df_may) +nrow(df_sep) +nrow(df_nov)
# tow number of missing values 
sum(is.na(df_full)) == sum(is.na(july)) +sum(is.na(march)) +sum(is.na(may)) +sum(is.na(sep)) +sum(is.na(nov)) 

write.csv(df_full,file = "../data/full_data.csv")
