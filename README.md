# STAT506H

## Process
1. combining the growth data seems to be done. 
2. For mortality, salinity_temp, I am having some difficulties of interpreting the time/week variable so that I 
stopped at here. I am hoping to combine the growth data and the two data sets for imputation since our growth data has 'month' information, thus the info from the data could help our imputing of salinity and temperature.
3. The EDA from your code seems alright :) but I was a little bit not sure about the duplicating same data sets and put them into each site. duplicated data could make n larger than actual number, hence it could lead a wrong p value. (it may lead nonsignificant result to look like significant). So I was wondering how about just randomly split the data from 'zero.csv' and randomly split them into different locations. 

## Todo
1. Create response variable for change in shell length (growth rate)
2. Generate a good model for complete case analysis
3. Perform imputation


## Data description

You can find the salinity and temperature data in the salinity_temperature.csv file for each site (A, B, C, D, and E) and each subsite within site (beach, raft).

Shell length/weight and tissue weight data are separate for each time point that data were collected. The initial measurements of oysters made before the outplant began were made for 50 individual oysters. Their shell length/weight and tissue weight are in the growth_data_zero.csv file.
Data were collected every two months from March 2017 to November 2017, and then were not collected again until March 2018 since growth is minimal over the wintertime. Some oyster bags were lost during the year (e.g. subsite C raft in spring 2017, subsite B raft, which lost three bags between November 2017 - March 2018).

Mortality data are in the mort_data.csv file.
