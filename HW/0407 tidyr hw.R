#Class: Week 08
#Course: Big Data and Social Analysis
#Semester: Spring 2021
#Lesson: Time Series Analysis
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: ??????
#English First Name: Jongsung
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com

### Questions --------

#Please read JoeBidenTweets.csv which records Biden's tweets from 2007 to 2020.
#The column tweet is the context of Biden's tweets.
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr) # gather() and spread()
library(zoo)
library(tidyverse)
setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
JBT <- read.csv('JoeBidenTweets.csv')

#Question 1 (5 Points): 

#Please create a new variable called trump, If Biden mentioned Trump in his tweets, please label 1 otherwise 0.

length(JBT$tweet) # total 5955 tweets

for (i in 1:length(JBT$tweet)){
  if(grepl('Trump',JBT$tweet[i], ignore.case = TRUE) == TRUE){ # grepl => TRUE /  FALSE 판별. 맞으면 1 아니면 - 
    JBT$trump[i] <- 1
  } else {
    JBT$trump[i] <- 0
  }
}
View(JBT) # check if it worked or not. ignore.case does not care either lower case or upper case
unique(JBT$trump) # I successfully got 0 and 1 


#Question 2 (5 Points):

#Please create a new variable called obama, If Biden mentioned Obama in his tweets, please label 1 otherwise 0.

for (i in 1:length(JBT$tweet)){
  if(grepl('Obama',JBT$tweet[i], ignore.case = TRUE) == TRUE){
    JBT$obama[i] <- 1
  } else {
    JBT$obama[i] <- 0
  }
}

View(JBT) # check if it worked or not. ignore.case does not care either lower case or upper case
unique(JBT$obama) # I successfully got 0 and 1 


#Question 3 (8 Points):

#Please create and reshape your table in order to create a two-line plot: monthly number of tweets which mentioned Trump and Obama.

colnames(JBT) # Get exact name of timestamp 

JBT$timestamp <- as.POSIXct(JBT$timestamp) # change the date into R default with the time such as "2017-10-24 22:45 GMT"

aa <- regexpr('-', JBT$timestamp) # when '-' is appeared first on JBT$timestamp? => from 5th
aa 

unique(aa) # Make sure that aa indicates only 5 

JBT$month <- substr(JBT$timestamp, aa + 1, aa + 2) # create a new variable called month

MT_T_O <- JBT %>%
  group_by(month) %>%
  summarise_at( # summarize two columns by using summarise_at 
    .vars= vars(trump, obama), # 어떤 거를 summarize 할 거니? 
    .funs =  sum # .funs = function 인듯
    ) %>%
  mutate(month_rate_T = trump / sum(trump)) %>% # check the ratio of each month 
  mutate(month_rate_O = obama / sum(obama))


MT_T_O_P <- MT_T_O %>%
  gather("Who", "Cases", 2:3) 
# 1) A new column name called 'Who' to store MT_T_O's column names  
# 2) A new column name called 'Cases' to store the values of MT_T_O's columns 
# 3) columns I want to gather

ggplot(MT_T_O_P, aes(x = month, y= Cases, color = Who, group = Who)) + geom_line() # show the plot

#Question 4 (5 Points):

#Create a plot to show the hourly number of Biden's tweets.

JBT$hour <- format(JBT$timestamp, "%H") # only take hour data to know what time he uses

B_H <- JBT %>%
  group_by(hour) %>%
  summarise(post = n()) %>% 
  mutate(hour_rate = post / sum(post))

ggplot(B_H, aes(x = hour, y = post, group = 1)) + geom_line()
# we found out Biden does not post from 7 to 8am

#Question 5 (7 Points):

#Create a two-line plot to compare the hourly number of Biden and Trump's tweets (You should merge trump_hour created in the class).

trumptweets <- read_xlsx('trumptweets.xlsx')
trumptweets$time <- as.POSIXct(trumptweets$created_at)
trumptweets$hour <- format(trumptweets$time, "%H") # only take hour data to know what time he uses
trump_hour <- trumptweets %>%
  group_by(hour) %>%
  summarise(post = n()) %>%
  mutate(hour_rate = post / sum(post))

BTW <- merge(B_H, trump_hour, by = "hour", all = TRUE)

BTW <- rename(BTW, Biden = post.x, Trump = post.y)

BTW1 <- BTW %>%
  gather("Who", "Cases", 2,4, na.rm = TRUE) # make NA to 0
# 1) A new column name called 'Who' to store BTW's column names  
# 2) A new column name called 'Cases' to store the values of BTW's columns 
# 3) columns I want to gather

ggplot(BTW1, aes(x = hour, y= Cases, color = Who, group = Who)) + geom_line() # show the plot with 2 lines

# if they asks about hourly ratio, then change little bit.
BTW2 <- BTW %>%
  gather("Who", "ratio", 3,5, na.rm = TRUE) # make NA to 0
# 1) A new column name called 'Who' to store BTW's column names  
# 2) A new column name called 'Cases' to store the values of BTW's columns 
# 3) columns I want to gather

ggplot(BTW2, aes(x = hour, y= ratio, color = Who, group = Who)) + geom_line() # show the plot with 2 lines