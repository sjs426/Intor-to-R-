library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr) # gather() and spread()
library(zoo)
library(tidyverse)

# Time Series Analysis
#as.Date <- year, month, day, time zone  
#as.POSIXct <- above + hour, min., sec.

# If you attempt to employ time series analysis,
# you have to transform an object into time format.


date_1 <- as.Date("2015-4-7")
date_1 # we get 04 and 07, which wasn't above

date_2 <- as.Date("2015-4-7 10:15")
date_2 # we only look for date. so it will omit the time

date_3 <- as.Date("2015/4/07 10:15")
date_3 # it will stick to - instead of / 

# Create a plot to show the daily number of Trump
# tweets from June 1, 2017 to June 30, 2017.

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

trumptweets <- read_xlsx('trumptweets.xlsx')
trumptweets$created_at[1:10]

trumptweets$date <- as.Date(trumptweets$created_at)
trumptweets$date[1:10]

trump_june17 <- trumptweets %>%
  filter(date >= "2017-06-01" & date <= "2017-06-30" ) # ???? ??????(binary operator) 여기에 포함 돼있는 것들만 보여줘줘

trump_date <- trump_june17 %>%
  group_by(date) %>%
  summarise(post = n()) # the number of data of date. n = number  

ggplot(trump_date, aes(x = date, y = post)) + geom_line()


# practice 1
# We can get mobility data from Google
# Does the soft-lockdown order have influences on Taiwanese mobility?
# Create a plot to show Taiwan??s transit.stations?? changes from Jan. 1, 2021 to May 18, 2021

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
google_mobility_data <- read.csv('google_mobility_data.csv')

colnames(google_mobility_data)

unique(google_mobility_data$country_region) # Taiwan

google_mobility_data$date <- as.Date(google_mobility_data$date) # change the data with / into - (R default)

GGL_JAN_MAY <- google_mobility_data %>%
  filter(country_region == "Taiwan") %>% # DO NOT FOR GET TO USE DOUBLE = (==)
  filter(date >= "2021-01-01" & date <= "2021-05-18") # Have to as.Date first above since it was using / such as 2021/05/18
View(GGL_JAN_MAY)

# We are very lucky that Google Mobility and Trump tweets datasets provide default R time format.
# However, we usually get diverse time data formats.

ggplot(GGL_JAN_MAY, aes(x = date, y = transit.stations, group = 1)) + geom_line()
# We don't need group_by but we need to put group = 1 here to plot WHY?
# It means put it one line

date_4 <- as.Date("20150407") # Error occurred

# In this case, we need to tell R that the structure of the time data.
# Y for whole year 2022, y for two digit year 22

date_4 <- as.Date("20150407", "%Y%m%d") # In this case, we need to tell R that the structure of the time data.
date_4

date_5 <- as.Date("150407", "%y%m%d") # lower case y
date_5

date_6 <- as.Date("11022012") # Error occurred

date_6 <- as.Date("11022012", "%m%d%Y") # some countries using month, date, year 
date_6

date_7 <- as.Date("11-02-2012") # "0011-02-20" default is year- month -date so.. 
date_7

date_7 <- as.Date("11-02-2012","%m%d%Y") # NA
date_7

date_7 <- as.Date("11-02-2012","%m-%d-%Y") # "2012-11-02" have to put '-' next to the letter
date_7

date_8 <- as.Date("Dec022012") # Error occurred
date_8

date_8 <- as.Date("Dec022012", "%m%d%Y") # NA
date_8

date_8 <- as.Date("Dec022012", "%b%d%Y") # NOT WORKING. It will work after below code. By the way, %b is for JAN, FEB.. 
date_8

# Why %b does not work?
# Because R only recognizes abbreviated month name in the language your OS uses. Here mine Korean.
format(Sys.Date(), "%b") # 4
Sys.setlocale() # Korean was my default lang

Sys.setlocale(locale = "English") # Chanage to English 
format(Sys.Date(), "%b") # Apr

date_10 <- as.Date("Dec102012", format = "%b%d%Y") # 2012-12-10
date_10

# If Y is y, it will show you "2020-12-02". So be careful
Sys.setenv(TZ = "GMT")


# WHAT LIBRARY WE USE SO FAR HERE? -> default 




# 여기 어려운 것들 gather and spread
# From now, tidyr

g_w <- data.frame(country = c("Afghanistan", "Brazil", "China"),
                  `1999` = c(745, 37737, 212258),
                  `2000` = c(2666, 80488, 213766), check.names=FALSE)

s_l <- data.frame(country = c("Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", 
                              "Brazil", "Brazil","Brazil","Brazil", 
                              "China","China","China","China"),
                  year = c(1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000, 1999, 1999, 2000, 2000),
                  key = c("cases", "population", "cases", "population",
                          "cases", "population", "cases", "population",
                          "cases", "population", "cases", "population"),
                  value = c(745, 19987071, 2666, 20595360, 37737, 172006362,
                            80488, 174504898, 212258, 1272915272, 213766, 1280428583))
g_l <- g_w %>%
  gather("year", "cases", 2:3) 
# 1) A new column name to store df_w's column names  
# 2) A new column name to store the values of df_w's columns 
# 3) which columns you want to gather
g_l

ggplot(g_l, aes(x = year, y= cases, color = country, group = country)) + geom_line()


s_w <- s_l %>%
  spread(key, value)
# 1) s_l's column name's contents become new column names.
# 2) s_l's column name that is value.
s_w


# We want to compare all mobility ways?? changes
# The dataframe that ggplot2 can draw multilines in a plot

tw_google_l <- GGL_JAN_MAY[, -c(1:4)] %>% # remove column from 1 to 4 [ , ] ?̴?.
                             gather('move', 'value', 2:7) 
# gather the column data from 2 to 7 into new var called move and value
# move is for all column name, value is for the data inside

ggplot(tw_google_l, aes(x = date, y = value, group = move, color = move)) + geom_line()

# additional hour + mins + secs for as.POSIXct

time_date_1 <- as.POSIXct("2015-04-07 10:15:30")
time_date_1 # the standard R date and time => "2015-04-07 10:15:30 GMT"

time_date_2 <- as.POSIXct("2015-04-07 101530")
time_date_2 # "2015-04-07 GMT" we better put time properly

time_date_3 <- as.POSIXct("20150407 101530") # error occurred

time_date_3 <- as.POSIXct("20150407 101530", "%Y%m%D %H%M%S") # error occurred
time_date_3

Sys.timezone()
time_date_4 <- as.POSIXct("2015-04-07 10:15:30", tz = 'UTC')

time_date_1 - time_date_4 # Time difference of 0 secs

# Create a plot to show the hourly number of Trump tweets
trumptweets$time <- as.POSIXct(trumptweets$created_at)

trumptweets$hour <- format(trumptweets$time, "%H") # only take hour data to know what time he uses

trump_hour <- trumptweets %>%
  group_by(hour) %>%
  summarise(post = n()) %>%
  mutate(hour_rate = post / sum(post))

ggplot(trump_hour, aes(x = hour, y = post, group = 1)) + geom_line()
