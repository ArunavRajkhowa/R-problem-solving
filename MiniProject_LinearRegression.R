####importing libraries
library(tidymodels)
library(visdat) 
library(tidyr)
library(car)
library(lubridate)

#### Reading the dataset
setwd("D:\\IITK Data Analytics\\R\\R-problem-solving\\")
file=read.csv("Cycle_Shared (1).csv",stringsAsFactors = F)
glimpse(file)
vis_dat(file)

### Data Preparation and cleaning

## first we seperate day and then drop dteday......month and weekday already given in data
file$date=parse_date_time(file$dteday,'ymd')
#file$year=year(file$date)
#$month=month(file$date)
file$day=day(file$date)
file$date=NULL
file$dteday=NULL
file$year=NULL
file$month=NULL


## dropping instant
file$instant=NULL



## Everything is numeric now and we dont have to worry about creating dummies 
## we can proceed to the fitting part straight away
## first let us split the dataset into train,val and test part

set.seed(2)
s=sample(1:nrow(file),0.8 * nrow(file))
df_trainval=file[s,]
df_test=file[-s,]

s=sample(1:nrow(df_trainval),0.8 * nrow(df_trainval))
df_train=df_trainval[s,]
df_val=df_trainval[-s,]

rm(s,df_trainval)
