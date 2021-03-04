# Shortcut for "<-"  is alt + "-"


#################################################################
# Install Packages & Load Libraries #
install.packages(c("tidyr", "devtools"))
devtools::install_github("garrettgman/DSR")
install.packages("tibble")
install.packages("RMySQL")
install.packages("lubridate")
install.packages("dplR")
install.packages("imputeTS")
install.packages("ggfortify")
install.packages("forecast")

library(lubridate)
library(RMySQL)
library(readr)
library(DSR)
library(tidyr)
library(tibble)
library(tidyverse)
library(openxlsx)
library(knitr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(scales)
library(dplyr)
library(imputeTS)
library(ggfortify)
library(forecast)

#-----------------------------------------------------------------------------
## Load Data SQL database connection
con = dbConnect(MySQL(), user='deepAnalytics', password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database  DList, a data type for representing elements of the same type with constant time append/prepend operations.
dbListTables(con)
# [1] "iris"    "yr_2006" "yr_2007" "yr_2008" "yr_2009" "yr_2010"

## list attributes in 'yr_2006' table
dbListFields(con, 'yr_2006')
# [1] "id"                    "Date"                  "Time"                 
# [4] "Global_active_power"   "Global_reactive_power" "Global_intensity"     
# [7] "Voltage"               "Sub_metering_1"        "Sub_metering_2"       
# [10] "Sub_metering_3" 

#-----------------------------------------------------------------------------
## Select attributes needed for analysis
yr_2006 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006')
yr_2007 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007')
yr_2008 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008')
yr_2009 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009')
yr_2010 <- dbGetQuery(con, 'SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010')

#-----------------------------------------------------------------------------
# check structure
str(yr_2006) #21992 obs of 5 variables  
str(yr_2007) #521669 obs. of  5 variables:
str(yr_2008) #526905 obs. of  5 variables:   
str(yr_2009) #521320 obs. of  5 variables: 
str(yr_2010) #457394 obs. of  5 variables:

## check head and tail (Only years 2007,2008, & 2009 have full years of data)
head(yr_2006) 
tail(yr_2006) # contains 2 weeks of data (12/16 - 12/31)
head(yr_2007) 
tail(yr_2007) # contains 1 full year of data (01/07 - 12/07)
head(yr_2008) 
tail(yr_2008) # contains 1 full year of data (01/08 - 12/08)
head(yr_2009) 
tail(yr_2009) # contains 1 full year of data (01/09 - 12/09)
head(yr_2010) 
tail(yr_2010) # contains 11 months of data (01/10 - 11/10)

#-----------------------------------------------------------------------------
## Combine the only needed Data Sets (include only full years)
subMeters <- bind_rows(yr_2007, yr_2008, yr_2009)
subMeters

## check structure
str(subMeters) # 1569894 obs. of  5 variables:
# $ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

## check head and tail, ensuring dates are in correct order
head(subMeters)
# Date     Time Sub_metering_1 Sub_metering_2 Sub_metering_3
# 1 2007-01-01 00:00:00              0              0              0
tail(subMeters)

#-----------------------------------------------------------------------------
# Preprocessing
## combine Date and Time attributes into a new attribute column
subMeters <- cbind(subMeters, paste(subMeters$Date, subMeters$Time), stringsAsFactors = FALSE)
colnames(subMeters)
# [1] "Date"                                  "Time"                                  "Sub_metering_1"                       
# [4] "Sub_metering_2"                        "Sub_metering_3"                        "paste(subMeters$Date, subMeters$Time)"                

## change column name
colnames(subMeters)[6] <- 'DateTime'
names(subMeters)
# [1] "Date"           "Time"           "Sub_metering_1" "Sub_metering_2" "Sub_metering_3" "DateTime" 

## move DateTime closer to front of data frame
subMeters <- subMeters %>% relocate(DateTime, .before = Sub_metering_1)

names(subMeters) # Confirm it was moved
# [1] "Date"           "Time"           "DateTime"       "Sub_metering_1" "Sub_metering_2" "Sub_metering_3"


#################################################################
## convert DateTime from character to POSIXct 
#################################################################
subMeters$DateTime <- as.POSIXct(subMeters$DateTime, '%Y/%m/%d %H:%M:%S')
str(subMeters) # Check Structure
# $ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ DateTime      : POSIXct, format: "2007-01-01 00:00:00" "2007-01-01 00:01:00" "2007-01-01 00:02:00" "2007-01-01 00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

#-----------------------------------------------------------------------------
## add time zone from France
attr(subMeters$DateTime, 'tzone') <- 'Europe/Paris'
names(subMeters)
#-----------------------------------------------------------------------------

## Inspect the data types
str(subMeters)
# 'data.frame':	1569894 obs. of  5 variables:
# $ DateTime      : POSIXct, format: "2007-01-01 00:00:00" "2007-01-01 00:01:00" "2007-01-01 00:02:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Date          : Date, format: "2007-01-01" "2007-01-01" "2007-01-01" ...

summary(subMeters) #Sub 1 using the most power at 82  Sub3 has the largest mean
str(subMeters)
## ?? What is the data type for DateTime? What do the values look like?   
###ANSWER: "POSIXct, and the values look like this 2007-01-01 00:00:00

#-----------------------------------------------------------------------------
names(subMeters)
# [1] "DateTime"       "Date"           "Time"           "Sub_metering_1" "Sub_metering_2" "Sub_metering_3"
#-----------------------------------------------------------------------------

## move Date and Time to more strategic location
subMeters <- subMeters %>% relocate(Date, .before = Sub_metering_1)
subMeters <- subMeters %>% relocate(Time, .before = Sub_metering_1)

## change name of certain columns
subMeters <- subMeters %>% rename(sub1 = Sub_metering_1)
subMeters <- subMeters %>% rename(sub2 = Sub_metering_2)
subMeters <- subMeters %>% rename(sub3 = Sub_metering_3)

#################################################################
## lubridate to create new attributes from 'DateTime' for analysis
subMeters$year <- year(subMeters$DateTime)
subMeters$quarter <- quarter(subMeters$DateTime)
subMeters$month <- month(subMeters$DateTime)
subMeters$week <- isoweek(subMeters$DateTime)
subMeters$wday <- wday(subMeters$DateTime)
subMeters$day <- day(subMeters$DateTime)
subMeters$hour <- hour(subMeters$DateTime)
subMeters$minute <- minute(subMeters$DateTime)
#################################################################

## Check Structure
str(subMeters)
# data.frame':	1569894 obs. of  14 variables:
#  $ DateTime: POSIXct, format: "2007-01-01 01:00:00" "2007-01-01 01:01:00" "2007-01-01 01:02:00" "2007-01-01 01:03:00" ...
#  $ Date    : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
#  $ Time    : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
#  $ sub1    : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ sub2    : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ sub3    : num  0 0 0 0 0 0 0 0 0 0 ...
#  $ year    : num  2007 2007 2007 2007 2007 ...
#  $ quarter : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ month   : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ week    : num  1 1 1 1 1 1 1 1 1 1 ...
#  $ wday    : num  2 2 2 2 2 2 2 2 2 2 ...
#  $ day     : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ hour    : int  1 1 1 1 1 1 1 1 1 1 ...
#  $ minute  : int  0 1 2 3 4 5 6 7 8 9 ...

## move Date and Time to more strategic location
subMeters <- subMeters %>% relocate(sub1, .after = minute)
subMeters <- subMeters %>% relocate(sub2, .after = sub1)
subMeters <- subMeters %>% relocate(sub3, .after = sub2)

## check structure
str(subMeters) # 'data.frame':	1569894 obs. of  14 variables:
# $ DateTime: POSIXct, format: "2007-01-01 01:00:00" "2007-01-01 01:01:00" "2007-01-01 01:02:00" "2007-01-01 01:03:00" ...
# $ Date    : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time    : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ year    : num  2007 2007 2007 2007 2007 ...
# $ quarter : int  1 1 1 1 1 1 1 1 1 1 ...
# $ month   : num  1 1 1 1 1 1 1 1 1 1 ...
# $ week    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ wday    : num  2 2 2 2 2 2 2 2 2 2 ...
# $ day     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ hour    : int  1 1 1 1 1 1 1 1 1 1 ...
# $ minute  : int  0 1 2 3 4 5 6 7 8 9 ...
# $ sub1    : num  0 0 0 0 0 0 0 0 0 0 ...
# $ sub2    : num  0 0 0 0 0 0 0 0 0 0 ...
# $ sub3    : num  0 0 0 0 0 0 0 0 0 0 ...

# Review your primary data frame. Was the year attribute created? What values are found in the year attribute? 
## ANSWER: Yes, 2007-2010
subMeters 
head(subMeters)
tail(subMeters)

#-----------------------------------------------------------------------------
## Check for NAs (Missing Data)
## group by date, obtain the count, and turn into data frame
missing_datetime <- subMeters %>% count(Date)
incomplete_data <- data.frame(table(missing_datetime$n))
incomplete_data
# Var1 Freq
# 1    21    1
# 2    30    1
# 3   549    1
# 4   576    1
# 5   985    1
# 6  1310    1
# 7  1370    1
# 8  1397    1
# 9  1402    2
# 10 1416    1
# 11 1419    1
# 12 1434    1
# 13 1436    1
# 14 1437    1
# 15 1438   12
# 16 1439   26
# 17 1440 1041

sum(is.na(incomplete_data)) # Number of missing values: 0 NAs

## filter for all days that do not have 1440 minutes
missing_time <- missing_datetime %>% filter(n !=1440) 
missing_time
# 26 2008-06-13 1439
# 27 2008-07-13 1438
# 28 2008-08-04 1439
# 29 2008-08-31 1439

names(missing_time) # Check Columns 
# [1] "Date" "n"   

# Observations: During time period of concern (July-Sept 2008), only 5 total minutes are missing on 5 separate days. Due to insignificant number of missing minutes during primary time focus, missing values were not analyzed in data set

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
## Viewing summary statistics
sum(subMeters$sub1) # 1,819,989 total kilowatts used
sum(subMeters$sub2) # 2,108,410 total kilowatts used
sum(subMeters$sub3) # 9,758,843 total kilowatts used
summary(subMeters)

# Sub1: Kitchen
# Least total energy used (1,819,989 Watts)
# Average 1.16 Watts per minute
# Largest energy range (0-82 Watts)

# Sub2: Laundry
# Total energy used (2,108,410 Watts)
# Average 1.34 Watts per minute
# Energy range (0-78 Watts)

# Sub3:` Water Heater & AC`
# Most total energy used (9,758,843 Watts)
# Average 6.21 Watts per minute
# Smallest energy range (0-32 Watts)

#-----------------------------------------------------------------------------

## 1 Data Documentation

# If you haven't done so already, review the Electric Power Consumption Data Set documentation in the resources tab (UCI Machine Learning Repository). Documentation includes an overview, percent of missing data and much more. You should pay special attention to attribute information and what is associated with each of the sub-meters? 

##REFERENCE##
# sub1 = kitchen, containing mainly a dishwasher, an oven and a microwave 
# sub2 = laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# sub3 = electric water-heater and an air-conditioner

################################################################## 
################################################################# 
######################### TASK 2 Deep Analytics and Visualization
################################################################# 
################################################################# 

# EDA
## summarizing in groups by year, quarter, month, week, day
QtrlySum <- subMeters %>%
  group_by(year, quarter) %>% 
  summarize(across(starts_with('sub'), sum))

## Subset the second week of 2008 - All Observations
houseWeek <- filter(subMeters, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$sub1)

## Subset the 9th day of January 2008 - All observations
houseDay <- filter(subMeters, year == 2008 & month == 1 & day == 9)
## Plot sub-meter 1
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$sub1, type = 'scatter', mode = 'lines')

# As you can see, line plots and Plotly make much better visualizations than the basic plot function. But, does this plot make sense? What does sub-meter 1 correspond with in this home?
#   
#   Perhaps the best way to understand the power usage on this day is to plot all three sub-meters to gain a perspective on all of the power being consumed. While we're at it let's add a legend, title and axis labels.  

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
################################ Reducing Granularity
# The plot above is pretty grainy given that 1440 points have been plotted. Let’s experiment and see if we can get a better plot by reducing the granularity from one observation per minute to one observation every 10 minutes. To do this we’ll need to use filter again and create a new sub-set.

## Subset the 9th day of January 2008 - 10 Minute frequency
# houseDay10 <- filter(subMeters, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))
#-----------------------------------------------------------------------------
## Subset the 9th day of January 2008 - 50 Minute frequency
houseDay10 <- filter(subMeters, year == 2008 & month == 1 & day == 9 & (minute == 50))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute 
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
# With the granularity adjusted we get a much more clear picture of the power consumption on January 9th. So what can you learn from this visualization? Your analysis could include answers to the following. What peaks might represent the water heater? How about the AC? What could be happening in the laundry room? How many times during this day are kitchen appliances being used? Lastly, in your opinion, does the data from these three sub-meters contain useful information for the homeowner? #-----------------------------------------------------------------------------

## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- subset(subMeters, wday == 2 & hour == 20 & minute == 60)
names(subMeters)
##+++
houseMonth8 <- filter(subMeters, year == 2008 & month == 8 & (minute == 50))
##+++
plot_ly(houseMonth8, x = ~houseMonth8$DateTime, y = ~houseMonth8$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth8$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth8$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##############################################################################
##############################################################################
## 1. Visualize the data
##############################################################################
##############################################################################
## Subset the 9th day of January 2008 - All observations
houseDay10 <- filter(subMeters, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency ?? Why is is 10minute when all minutes are shown?
houseDay10 <- filter(subMeters, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub2, name = 'Laundry Room(2)', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub3, name = 'Water Heater & AC(3)', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## OBSERVATION: Kitchen usage (Indicates Occupancy)
#-----------------------------------------------------------------------------
## Experimenting August 9th 
houseDayAug9 <- filter(subMeters, year == 2008 & month == 8 & day == 9)

## Plot sub-meter 1
plot_ly(houseDayAug9, x = ~houseDayAug9$DateTime, y = ~houseDayAug9$sub1, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseDayAug9, x = ~houseDayAug9$DateTime, y = ~houseDayAug9$sub1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDayAug9$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDayAug9$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Experimenting August 9th 10 Minute frequency
houseDayAug9 <- filter(subMeters, year == 2008 & month == 8 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))


## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDayAug9, x = ~houseDayAug9$DateTime, y = ~houseDayAug9$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDayAug9$sub2, name = 'Laundry Room(2)', mode = 'lines') %>%
  add_trace(y = ~houseDayAug9$sub3, name = 'Water Heater & AC(3)', mode = 'lines') %>%
  layout(title = "Power Consumption August 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))



## REFERENCE ##
# sub1 = kitchen, containing mainly a dishwasher, an oven and a microwave 
# sub2 = laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# sub3 = electric water-heater and an air-conditioner

#########################################################################
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 50 Minute Intervals
#########################################################################
## EXPERIMENTING MONTH USAGE
## Month of July
houseJuly <- filter(subMeters, year == 2008 & month == 7 & (minute == 50))

plot_ly(houseJuly, x = ~houseJuly$DateTime, y = ~houseJuly$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseJuly$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseJuly$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption, Month of July 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## OBSERVATION:  Usage of all Submeters (Indicates Occupancy)
#-----------------------------------------------------------------------------
## Month of August
houseAugust <- filter(subMeters, year == 2008 & month == 8 & (minute == 50))

plot_ly(houseAugust, x = ~houseAugust$DateTime, y = ~houseAugust$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseAugust$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseAugust$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption, Month of August 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## OBSERVATION: Little to No usage of Laundry Aug 3-30 and no usage of Kitchen (Indicates Vacancy)
#-----------------------------------------------------------------------------
## Month of September
houseSept <- filter(subMeters, year == 2008 & month == 9 & (minute == 50))

plot_ly(houseSept, x = ~houseSept$DateTime, y = ~houseSept$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseSept$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseSept$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption, Month of Sept. 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## OBSERVATION: Usage of all submeters (Indicates Occupancy)
#-----------------------------------------------------------------------------
## Power usage indicates possible vacancies in the month of August, Analyze days in August
## July 31st
houseJuly31 <- filter(subMeters, year == 2008 & month == 7 & day == 31 & (minute == 50))

plot_ly(houseJuly31, x = ~houseJuly31$DateTime, y = ~houseJuly31$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseJuly31$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseJuly31$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption Sept 31st, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## Observation: Usage of Water & kitchen (Indicates Occupancy)
#-----------------------------------------------------------------------------
## August 1st
houseAug1 <- filter(subMeters, year == 2008 & month == 8 & day == 1 & (minute == 50))

plot_ly(houseAug1, x = ~houseAug1$DateTime, y = ~houseAug1$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseAug1$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseAug1$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 1st, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## OBSERVATION:  Little to know usage of Laundry or Kitchen (Indicates Vacancy)
#-----------------------------------------------------------------------------
## August 3rd
houseAug3 <- filter(subMeters, year == 2008 & month == 8 & day == 3 & (minute == 50))
 
plot_ly(houseAug3, x = ~houseAug3$DateTime, y = ~houseAug3$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseAug3$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseAug3$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 3rd, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
#-----------------------------------------------------------------------------
## August 5th 
houseAug5 <- filter(subMeters, year == 2008 & month == 8 & day == 5 & (minute == 50))

plot_ly(houseAug5, x = ~houseAug5$DateTime, y = ~houseAug5$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseAug5$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseAug5$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 5th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
#-----------------------------------------------------------------------------
## August 6th
houseAug6 <- filter(subMeters, year == 2008 & month == 8 & day == 5 & (minute == 50))

plot_ly(houseAug6, x = ~houseAug6$DateTime, y = ~houseAug6$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseAug6$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseAug6$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August 6th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

##############################################################################
##############################################################################
## 2. Prepare to analyze the data
##############################################################################
##############################################################################
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(subMeters, wday == 2 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$sub3, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)
#-----------------------------------------------------------------------------
## EXPERIMENTING with sub1 
## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(subMeters, wday == 2 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM1_070809weekly <- ts(house070809weekly$sub1, frequency=52, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM1_070809weekly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM1_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")

## Plot sub-meter 1 with plot.ts
plot.ts(tsSM1_070809weekly)
#-----------------------------------------------------------------------------
## EXPERIMENTING - Subset to one observation at 8:00pm for 2007, 2008 and 2009
house070809monthly <- filter(subMeters, month == 1 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_house070809monthly <- ts(house070809monthly$sub3, frequency=12, start=c(2007,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_house070809monthly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_house070809monthly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_house070809monthly)
## ?? It shows until 2014?  How do I set it to end at 2010.
#-----------------------------------------------------------------------------
## EXPERIMENTING - Daily Subset to one observation at 8:00pm for 2008
house08daily <- filter(subMeters, day == 1 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_house08daily <- ts(house08daily$sub3, frequency=365.25, start=c(2008,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_house08daily)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_house08daily, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_house08daily)
#-----------------------------------------------------------------------------
## EXPERIMENTING - Subset Monthly to one observation at 8:00pm for 2007, 2008 and 2009
house070809monthly <- filter(subMeters, month == 1 & week == 1 & hour == 20 & minute == 1)

## Create TS object with SubMeter3
tsSM3_house070809monthly <- ts(house070809monthly$sub3, frequency=12, start=c(2008,1))

## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_house070809monthly)

## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_house070809monthly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")

## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_house070809monthly)
## OBSERVATION Sub 3 still shows usage in Summer Months (Indicates Occupancy)

##############################################################################
##############################################################################
# 1 Visualize the Data ADDRESSING GRANULARITY 
##############################################################################
##############################################################################


## **** Subsetting instead of Filtering
## Subset the second week of 2008 - All Observations
houseWeek <- subset(subMeters, year == 2008 & week == 2)
## Plot subset houseWeek
plot(houseWeek$Sub_metering_1)

## Subset the 9th day of January 2008 - 60 Minute frequency (Instead of 10)
houseDay0109 <- subset(subMeters, year == 2008 & month == 1 & day == 9 & (minute == 10))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 60 Minute frequency (Instead of 10)
plot_ly(houseDay0109, x = ~houseDay0109$DateTime, y = ~houseDay0109$sub1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay0109$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay0109$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
##################################################################
##################################################################
##################################################################
##################################################################
## TASK 2 ********
##################################################################
##################################################################
##################################################################
##################################################################
## summarizing in groups by year, quarter, month, week, day
QtrlySum <- subMeters %>%
  group_by(year, quarter) %>% 
  summarise(across(starts_with('sub'), sum))

QtrlySumGather <- gather(QtrlySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

MonthlySum <- subMeters %>%
  group_by(year, month) %>% 
  summarise(across(starts_with('sub'), sum))

MonthSumGather <- gather(MonthlySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

DailySum <- subMeters %>%
  group_by(Date, year, quarter, day) %>% 
  summarise(across(starts_with('sub'), sum))

DailySumGather <- gather(DailySum, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')

HourlySum <- subMeters %>% 
  group_by(Date, month, week, day, hour) %>% 
  summarise(across(starts_with('sub'), sum))

HourlySumGather <- gather(HourlySum, 'sub1', 'sub2', 'sub3',
                          key = 'submeter', value = 'amount')


AllSumGathered <- gather(subMeters, 'sub1', 'sub2', 'sub3',
                         key = 'submeter', value = 'amount')
#-----------------------------------------------------------------------------
## Monthly Sum by Year
subset(MonthSumGather, year != 2010) %>%
  ggplot(aes(month, amount, color=submeter)) +
  geom_line(size = 1) +
  facet_grid(year~.) +
  theme_bw() +
  theme(aspect.ratio = .25,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(0,0,0,6),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter: ', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  scale_x_discrete(limits=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab('\nMonths') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Monthly Energy Use by Year')

# 1st
## OBSERVATIONS 
##Water heater and AC consistently use more energy than other submeters across years
# Seasonal patterns show peak energy use for Water Heater & AC in winter months, with a steady decline in the summer months
# Sharp decline for all submeters is seen in August 2008 for Water Heater & AC
#-----------------------------------------------------------------------------
## Daily Sum by Month in Summer 2008
names(DailySumGather)
# 1] "Date"     "year"     "quarter"  "day"      "submeter" "amount" 

subset(DailySumGather, year==2008 & quarter == 3) %>%
  ggplot(aes(as.Date(Date), amount, color=submeter)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:  ', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  scale_x_date(labels = date_format('%b %d'), breaks = date_breaks('1 week')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8)) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Daily Energy Use July - September 2008')
## OBSERVATIONS 
# Steep drop in energy use in all submeters from Aug 5-29, 2008
# Water Heater & AC consistently use more energy than other submeters, even during August time period
#-----------------------------------------------------------------------------
## Comparing summers across years 
## Daily Sum each Summer by year
subset(DailySumGather, quarter==3) %>%
  ggplot(aes(as.Date(Date), amount, color=submeter)) +
  geom_line(size = .8) +
  facet_wrap(~year, scales = 'free', nrow = 3, strip.position = 'right') +
  theme_bw() +
  theme(aspect.ratio = .25,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,4),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  scale_x_date(labels = date_format('%b %d'), breaks = date_breaks('2 weeks')) +
  theme(axis.text.x = element_text(hjust = 1, vjust = 1, size = 8)) +
  xlab('\nMonths') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Daily Power Use July - September by Year')
## OBSERVATIONS:
# Steep, extended drop in energy use is seen in August 2008 only
# Water heater & AC consistently use more energy than other submeters across years
# Less steep drop is seen for short duration beginning of August 2009, but only for few days

#-----------------------------------------------------------------------------
## create hourly use DF for week 30 in 2008 (July 21 - July 27)
hourWeek30 <- data.frame(filter(AllSumGathered, year==2008 & week==30 & minute==50))

## hourly Sum by Week
hourWeek30 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = .8) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\n') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('50-minute Power Use July 21 - July 27 2008')
## OBSERVATIONS:
# Kitchen: used 2 times in 1 week period, 19-37
# Laundry: 2-3 Watt Usage
# Sub3 (Water Heater & AC) Usage Range of 2-30 Watts - used more frequently throughout the week
#-----------------------------------------------------------------------------
## Investigating energy use on microscopic level each week and within days in August 2008
## create hourly use by week 31 in 2008 (July 28 - August 4)
houseWeek31 <- data.frame(subset(AllSumGathered, year==2008 & week==31 & minute==0))


## hourly Sum by Week
houseWeek31 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = .8) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nMonths') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Use July 28 - Aug 4 2008')
## OBSERVATIONS:
# Kitchen: used 4 times in between July 28 - Aug 4,  19-37 Watts/time
# Laundry: 1 higher use of 35 Watts July 31, otherwise 2-3 Watts used at regular intervals
# Water Heater & AC: Range of 2-30 Watts used more frequently throughout the week
#-----------------------------------------------------------------------------
# Let’s explore hourly use the following week. We will do this throughout the rest of August to determine energy use and occupancy status of residence.
## create hourly use by week 32 in 2008 (August 4-11)
houseWeek32 <- data.frame(filter(AllSumGathered, year==2008 & week==32 & minute==0))

## hourly Sum by Week
houseWeek32 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = .8) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Use Aug 5 - Aug 11 2008')
##OBSERVATIONS:
# Noticeable different in energy use August 5th on.
# Kitchen: reveals 1 spike of 39 Watts on Aug 5, with no more use
# Laundry: 1 spike of 71 Watts late Aug 4. We can see 2-3 Watt intervals rest of week.
# Water heater & AC: High of 30 Watts occurred on Aug 5, followed by 1-2 Watt regular intervals with 12 Watt spikes about once/day.
#-----------------------------------------------------------------------------
## Investigating hourly use the following week in August
## create hourly use by week 33 in 2008 (August 11-18)
houseWeek33 <- data.frame(filter(AllSumGathered, year==2008 & week==33 & minute==50))

## hourly Sum by Week
houseWeek33 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = .8) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Use Aug 11 - Aug 18 2008')
## OBSERVATIONS:
# Kitchen: No energy use during this time frame
# Laundry: 1-2 Watt daily intervals, however, also notice time periods of seemingly no energy use
# Water heater & AC: 1-Watt regular intervals with 12-Watt spikes about once/day.
#-----------------------------------------------------------------------------
## INVESTIGATE HOURLY POWER CONSUMPTION FOR WEEK 34 IN AUGUST 08'
## create Data Frame for hourly use by week 34 in 2008 (August 18-25),
hourWeek34 <- data.frame(filter(AllSumGathered, year==2008 & week==34 & minute==0))
## hourly use for Week 34, August 08'
hourWeek34 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = .8) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Use Aug 18 - 25 2008')
## OBSERVATIONS:
# Kitchen: No energy use
# Laundry: Appears to be no energy used Aug 19, but used other days
# Water heater & AC: 1 Watt regular daily intervals, with some spikes
#-----------------------------------------------------------------------------
## Explore week 35 in August 08', then explore hourly usage August 19th to deduce if Laundry used or not
## create hourly use by week 35 in 2008 (August 25-Sept 1)
hourWeek35 <- data.frame(filter(AllSumGathered, year==2008 & week==35 & minute==0))

## hourly Sum by Week for Week 35, 
hourWeek35 %>%
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size = .8) +
  theme_bw() +
  theme(aspect.ratio = .4,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,10)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Use Aug 25 - Sept 1 2008')
## OBSERVATIONS:
# Kitchen: No usage until September 1, 2008
# Laundry & Water heater/AC: Similar pattern from prior week
#-----------------------------------------------------------------------------
## Let analyze energy use in 50-min. intervals on August 18, to verify if laundry used or not
## create 50-min. interval DF for August 18. 
hourDay18 <- data.frame(subset(AllSumGathered, year==2008 & month==8 & day==18 & (minute==50)))

## 60 Minuter interval doesn't work?

hourDay18 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 18th, 2008')
## OBSERVATIONS:
#  Laundry is in use, but very low usage (1-2 Watts), same as other days in August. Fridge is connected to laundry submeter (sub2), which can easily explain low use at regular intervals
# Kitchen is not used
#-----------------------------------------------------------------------------
hourAug02 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==2 & (minute==50)))

hourAug02 %>%  
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .45,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter: ', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption August 2nd, 2008')
## Only Water & AC usage Max of 8watts (Indicates possible vacancy)
#-----------------------------------------------------------------------------
hourDay4 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==4 & (minute==50)))

hourDay4 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 5th, 2008 test')
## Laundry usage Indicates Occupancy
#-----------------------------------------------------------------------------
hourDay5 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==5 & (minute==50)))

hourDay5 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 5th, 2008 test')
## Only Water & AC usage (Indicates vacancy)
#-----------------------------------------------------------------------------
hourAug6 <- data.frame(subset(AllSumGathered, year==2008 & month==8 & day==6 & (minute==50)))

hourAug6 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 6th, 2008 test')
## Only Water & AC usage (Indicates possible vacancy)
#-----------------------------------------------------------------------------
hourAug28 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==28 & (minute==50)))

hourAug28%>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 28th, 2008')
## OBSERVATION: Only AC usage - (Possible Vacancy)
#-----------------------------------------------------------------------------
hourAug29 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==07 & (minute==50)))

hourAug29 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 7th, 2008')
## OBSERVATION: No power usage almost guarantees no occupancy
#-----------------------------------------------------------------------------
hourAug31 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==31 & (minute==50)))

hourAug31 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals August 31st, 2008 test')
## OBSERVATION: August 31st Laundry (sub2) usage (Indicates occupancy)
#-----------------------------------------------------------------------------
hourSept01 <- data.frame(filter(AllSumGathered, year==2008 & month==9 & day==20 & (minute==50)))

hourSept01 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals September 1st, 2008 test') 
## OBSERVATION: No usage of kitchen or laundry ??? indicates possible vacancy
#-----------------------------------------------------------------------------
hourSept02 <- data.frame(filter(AllSumGathered, year==2008 & month==9 & day==02 & (minute==50)))

hourSept02 %>% 
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .5,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter:', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Power Use in 50-minute intervals September 2nd, 2008 test') 
# OBSERVATION: Kitchen and Laundry usage (Indicates Occupancy)
#-----------------------------------------------------------------------------
## Compare this to another day outside time period in question in order to compare & contrast
## create 10 minute interval DF use for one day outside of time period in question 
## Set 50-min. interval for February, 2 2008
hourFeb2 <- data.frame(filter(AllSumGathered, year==2008 & month==2 & day==2 & (minute==50)))

hourFeb2 %>%  
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .45,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter: ', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption February 2nd, 2008')
## OBSERVATIONS:
# Kitchen shows that it in in use during lunch & dinner time frame (Indicates Occupancy)
# Water heater & AC is in much higher use throughout the day 
#-----------------------------------------------------------------------------
hourFeb3 <- data.frame(filter(AllSumGathered, year==2008 & month==2 & day==3 & (minute==50)))

hourFeb3 %>%  
  ggplot(aes(DateTime, amount, color=submeter)) +
  geom_line(size=.8) +
  theme_bw() +
  theme(aspect.ratio = .45,
        legend.position = 'top',
        legend.justification = 'left',
        legend.margin=margin(2,0,0,0),
        legend.box.margin=margin(0,-10,-10,0)) +
  scale_color_brewer(palette = 'Dark2', name = 'Submeter: ', labels = c('Kitchen(1)', 'Laundry(2)', 'Water Heater & AC(3)')) +
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Hourly Power Consumption February 3rd, 2008')
## OBSERVATIONS:
# Kitchen usage shows that it is in use during lunch & dinner time frame (Indicates Occupancyoner

