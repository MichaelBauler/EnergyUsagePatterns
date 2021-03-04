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
# Understand the data 
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

## check structure
str(subMeters) # 1569894 obs. of  6 variables:
# $ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ DateTime      : chr  "2007-01-01 00:00:00" "2007-01-01 00:01:00" "2007-01-01 00:02:00" "2007-01-01 00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
## convert DateTime from character to POSIXct 
subMeters$DateTime <- as.POSIXct(subMeters$DateTime, '%Y/%m/%d %H:%M:%S')
str(subMeters) # Check Structure
# $ Date          : chr  "2007-01-01" "2007-01-01" "2007-01-01" "2007-01-01" ...
# $ Time          : chr  "00:00:00" "00:01:00" "00:02:00" "00:03:00" ...
# $ DateTime      : POSIXct, format: "2007-01-01 00:00:00" "2007-01-01 00:01:00" "2007-01-01 00:02:00" "2007-01-01 00:03:00" ...
# $ Sub_metering_1: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_2: num  0 0 0 0 0 0 0 0 0 0 ...
# $ Sub_metering_3: num  0 0 0 0 0 0 0 0 0 0 ...

## add time zone from France
attr(subMeters$DateTime, 'tzone') <- 'Europe/Paris'
names(subMeters)

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

names(subMeters)
# [1] "DateTime"       "Date"           "Time"           "Sub_metering_1" "Sub_metering_2" "Sub_metering_3"

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
## move Date and Time to more strategic location
subMeters <- subMeters %>% relocate(Date, .before = Sub_metering_1)
subMeters <- subMeters %>% relocate(Time, .before = Sub_metering_1)

## change name of certain columns
subMeters <- subMeters %>% rename(sub1 = Sub_metering_1)
subMeters <- subMeters %>% rename(sub2 = Sub_metering_2)
subMeters <- subMeters %>% rename(sub3 = Sub_metering_3)

## lubridate to create new attributes from 'DateTime' for analysis
subMeters$year <- year(subMeters$DateTime)
subMeters$quarter <- quarter(subMeters$DateTime)
subMeters$month <- month(subMeters$DateTime)
subMeters$week <- isoweek(subMeters$DateTime)
subMeters$wday <- wday(subMeters$DateTime)
subMeters$day <- day(subMeters$DateTime)
subMeters$hour <- hour(subMeters$DateTime)
subMeters$minute <- minute(subMeters$DateTime)

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
#-----------------------------------------------------------------------------
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
# DateTime                       Date               Time                sub1             sub2             sub3             year         quarter    
# Min.   :2007-01-01 01:00:00   Length:1569894     Length:1569894     Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   :2007   Min.   :1.00  
# 1st Qu.:2007-10-03 08:39:15   Class :character   Class :character   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.:2007   1st Qu.:2.00  
# Median :2008-07-01 22:05:30   Mode  :character   Mode  :character   Median : 0.000   Median : 0.000   Median : 1.000   Median :2008   Median :3.00  
# Mean   :2008-07-02 03:54:14                                         Mean   : 1.159   Mean   : 1.343   Mean   : 6.216   Mean   :2008   Mean   :2.51  
# 3rd Qu.:2009-03-31 14:32:45                                         3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000   3rd Qu.:2009   3rd Qu.:4.00  
# Max.   :2010-01-01 00:59:00                                         Max.   :82.000   Max.   :78.000   Max.   :31.000   Max.   :2010   Max.   :4.00  
# month             week            wday        day             hour          minute     
# Min.   : 1.000   Min.   : 1.00   Min.   :1   Min.   : 1.00   Min.   : 0.0   Min.   : 0.00  
# 1st Qu.: 4.000   1st Qu.:13.00   1st Qu.:2   1st Qu.: 8.00   1st Qu.: 5.0   1st Qu.:14.25  
# Median : 7.000   Median :27.00   Median :4   Median :16.00   Median :12.0   Median :30.00  
# Mean   : 6.529   Mean   :26.62   Mean   :4   Mean   :15.71   Mean   :11.5   Mean   :29.50  
# 3rd Qu.:10.000   3rd Qu.:40.00   3rd Qu.:6   3rd Qu.:23.00   3rd Qu.:18.0   3rd Qu.:44.00  
# Max.   :12.000   Max.   :53.00   Max.   :7   Max.   :31.00   Max.   :23.0   Max.   :59.00 

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

# ?? How Summary statistics of June - August 2007, 2008, and 2009 right track?

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# Step 5 - Perform initial exploration of the data

## 1 Data Documentation

# If you haven't done so already, review the Electric Power Consumption Data Set documentation in the resources tab (UCI Machine Learning Repository). Documentation includes an overview, percent of missing data and much more. You should pay special attention to attribute information and what is associated with each of the sub-meters? 


# sub1 = kitchen, containing mainly a dishwasher, an oven and a microwave 
# sub2 = laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# sub3 = electric water-heater and an air-conditioner


#-----------------------------------------------------------------------------
## 2 Gather summary statistics

# * Using the summary() command calculate the mean, mode, standard deviation, quartiles & characterization of the distribution and maybe more.

# ? Which SubMeter is using the most? Which is using the least? 
# ANSWER:: sub2(laundry room) is using the least @ 1.343 - sub3(water heater and AC) is using the most @ 6.216

# ? Is there anything to learn from the max and min? 
# ANSWER:: ???


#-----------------------------------------------------------------------------
## 3 Propose three high-level recommendations you can suggest based on your initial exploration of the power consumption data
# # ANSWER::

# * Thought Starter: If you could add more information to the data set, what kinds of attributes would you add? What would be important to understanding the power usage in this home?
# ANSWER:: Normal consumption of other residents?


#   * Thought Starter: Should the appliances on the sub-meters be grouped the way they are currently grouped? Could more information be gained if some were separated? 

#   * Your mentor will assist you with this objective during your team meetings.




 
################################################################# ################################################################# ################################################################# 
################################################################# 
################################  TASK 2 Deep Analytics and Visualization
################################################################# 
## Plot all of sub-meter 1
plot(subMeters$sub1)
## Toooo many for Plot to be useful
plot

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
## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(subMeters, year == 2008 & month == 1 & day == 9 & (minute == 10))

## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute 
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
# With the granularity adjusted we get a much more clear picture of the power consumption on January 9th. So what can you learn from this visualization? Your analysis could include answers to the following. What peaks might represent the water heater? How about the AC? What could be happening in the laundry room? How many times during this day are kitchen appliances being used? Lastly, in your opinion, does the data from these three sub-meters contain useful information for the homeowner? #-----------------------------------------------------------------------------

##+++
houseMonth8 <- filter(subMeters, year == 2008 & month == 8 & (minute == 50))
##+++
plot_ly(houseMonth8, x = ~houseMonth8$DateTime, y = ~houseMonth8$sub1, name = 'Kitchen(1)', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseMonth8$sub2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseMonth8$sub3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption August, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))








#########################################################################
# From Reference D

## Subset the second week of 2008 - All Observations
houseweek2 <- filter(subMeters, Year == 2008 & Week == 2)
## Plot subset houseWeek
plot(houseweek2$`sub1`)
## Subset the 9th day of January 2008 - All observations
houseday <- filter(subMeters, Year == 2008 & Month == 1 & Day == 9)
## Plot sub-meter 1
plot_ly(houseday, x = ~houseday$DateTime,
        y = ~houseday$`Kitchen(Wh)`, type = 'scatter', mode = 'lines')
## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations
plot_ly(houseday, x = ~houseday$DateTime, y = ~houseday$`Kitchen(Wh)`,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday$`Laundry.Room(Wh)`, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseday$`Heater.&.A/C(Wh)`, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
## Subset the 9th day of January 2008 - 10 Minute frequency
houseDay10 <- filter(subMeters, Year == 2008 & Month == 1 & Day == 9
                     & (Minute == 0 | Minute == 10 | Minute == 20 |
                          Minute == 30 | Minute == 40 | Minute == 50))
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 10 Minute frequency
plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$`Kitchen(Wh)`,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$`Laundry.Room(Wh)`, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$`Heater.&.A/C(Wh)`, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
plot_ly(houseweek, x = ~houseweek$DateTime, y = ~houseweek$`Kitchen(Wh)`,
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek$`Laundry.Room(Wh)`, name = 'Laundry Room', mode = 'lines')%>%
  add_trace(y = ~houseweek$`Heater.&.A/C(Wh)`, name = "Water Heater & AC", mode = 'lines')

#########################################################################


















#########################################################################
## Plot sub-meter 1, 2 and 3 with title, legend and labels - 50 Minute Intervals
#########################################################################
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
#-----------------------------------------------------------------------------



## Subset to one observation per week on Mondays at 8:00pm for 2007, 2008 and 2009
house070809weekly <- filter(subMeters, wday == 2 & hour == 20 & minute == 1)
##+++
house070809quarterly <- filter(subMeters, quarter == 3 & hour == 18 & minute == 1)


## Create TS object with SubMeter3
tsSM3_070809weekly <- ts(house070809weekly$sub3, frequency=52, start=c(2007,1))
##+++
tsSM3_house070809quarterly <- ts(house070809quarterly$sub3, frequency=4, start=c(2008,6))


## Plot sub-meter 3 with autoplot (you may need to install these packages)
autoplot(tsSM3_070809weekly)
##+++
autoplot(tsSM3_house070809quarterly)


## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")


## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)

##+++
autoplot(tsSM3_070809weekly)
## Plot sub-meter 3 with autoplot - add labels, color
autoplot(tsSM3_070809weekly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "sub3")
## Plot sub-meter 3 with plot.ts
plot.ts(tsSM3_070809weekly)




## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
fitSM3 <- tslm(tsSM3_070809weekly ~ trend + season) 
summary(fitSM3)
# Residual standard error: 6.871 on 104 degrees of freedom
# Multiple R-squared:  0.3831,	Adjusted R-squared:  0.07461 
# F-statistic: 1.242 on 52 and 104 DF,  p-value: 0.1747

## Create the forecast for sub-meter 3. Forecast ahead 20 time periods 
forecastfitSM3 <- forecast(fitSM3, h=20)
## Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)



























##################################################################
##################################################################
##################################################################
##################################################################
## TASK 2 ****site
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
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('Monthly Energy Use by Year')


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
  xlab('\nTime') +
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
  xlab('\nTime') +
  ylab('Power (Watt-hours)\n') +
  ggtitle('50-minute Power Use July 21 - July 27 2008')
## OBSERVATIONS:
# Kitchen: used 2 times in 1 week period, 19-37
# Laundry: 2-3 Watt Usage
# Sub3 (Water Heater & AC) Usage Range of 2-30 Watts - used more frequently throughout the week











#-----------------------------------------------------------------------------
## Investigating energy use on microscopic level each week and within days in August 2008
## create hourly use by week 31 in 2008 (July 28 - August 4)
houseWeek31 <- data.frame(filter(AllSumGathered, year==2008 & week==31 & minute==0))


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
  xlab('\nTime') +
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
hourDay18 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==18 & (minute==50)))

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
hourAug6 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==6 & (minute==50)))

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
hourAug29 <- data.frame(filter(AllSumGathered, year==2008 & month==8 & day==29 & (minute==50)))

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
  ggtitle('Power Use in 50-minute intervals August 29th, 2008')
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
hourSept01 <- data.frame(filter(AllSumGathered, year==2008 & month==9 & day==01 & (minute==50)))

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
## OBSERVATION: Kitchen and Laundry usage (Indicates Occupancy)
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
# Kitchen usage shows that it is in use during lunch & dinner time frame (Indicates Occupancy)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
## CONCLUSIONS
## Peak energy usage was higher in the winter compared to summer months. 
## The data shows that there was a sharp decline of power usage all sub-meters from Aug 5th - 31st, 2008
## This decline is atypical compared to all other time periods time periods
## Sub1 (Kitchen) shows no usage from Aug 6th - 31st, 2008
# Recommendation:  
# The data shows that the residence was not not occupied from August 6th - August 31st, 2008.  However, it  was occupied all other days within July-September 2008 time frame.



# REFERENCE
# sub1 = kitchen, containing mainly a dishwasher, an oven and a microwave 
# sub2 = laundry room, containing a washing-machine, a tumble-drier, a refrigerator and a light.
# sub3 = electric water-heater and an air-conditioner




##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
## Summmer of 07-09 hmmm testing 
names(subMeters)


summer07 <- subMeters %>%
  select(Date, sub1, sub2, sub3)
summer08 <- subMeters %>%
  select(Date, sub1, sub2, sub3)
summer09 <- subMeters %>%
  select(Date, sub1, sub2, sub3)
names(summer07)
tail(summer07)

newSummer07 <- summer07 %>%
  select(Date, sub1, sub2, sub3) %>%
  filter(Date >= "2007-06-21" & Date <= "2007-09-22")
newSummer07[order(as.Date(newSummer07$Date, format="%d/%m/%Y")),]

newSummer08 <- summer08 %>%
  select(Date, sub1, sub2, sub3) %>%
  filter(Date >= "2008-06-21" & Date <= "2008-09-22")
newSummer08[order(as.Date(newSummer07$Date, format="%d/%m/%Y")),]

newSummer09 <- summer09 %>%
  select(Date, sub1, sub2, sub3) %>%
  filter(Date >= "2009-06-21" & Date <= "2009-09-22")
newSummer08[order(as.Date(newSummer07$Date, format="%d/%m/%Y")),]


head(newSummer07)
tail(newSummer07)

head(newSummer08)
tail(newSummer08)

head(newSummer09)
tail(newSummer09)


summary(newSummer07)
# Date                sub1              sub2             sub3       
# Length:135206      Min.   : 0.0000   Min.   : 0.000   Min.   : 0.000  
# Class :character   1st Qu.: 0.0000   1st Qu.: 0.000   1st Qu.: 0.000  
# Mode  :character   Median : 0.0000   Median : 0.000   Median : 0.000  
# Mean   : 0.9819   Mean   : 1.279   Mean   : 4.402  
# 3rd Qu.: 0.0000   3rd Qu.: 1.000   3rd Qu.:16.000  
# Max.   :78.0000   Max.   :74.000   Max.   :19.000
summary(newSummer08)
summary(newSummer09)





