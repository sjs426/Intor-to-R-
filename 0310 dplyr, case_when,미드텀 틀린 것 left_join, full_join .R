# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("censusapi")
# install.packages("WDI")

library(tidyr)
library(dplyr)
library(censusapi)
library(WDI)


ici <- 'best'
b <- length(ici)

a <- b
a
a <- b %>%
  length()

donate_df <- data.frame(code = c("001", "002", "003", "004", "005"),
                        gender = c("M", "F", "F", "M", "F"),
                        value = c(500, 300, 1000, 500, 300))

male_donate <- donate_df[donate_df$gender == 'M',] # this is too long, we can make it short via dplyr
male_donate

male_donate <- donate_df %>% # if we use  %>%, we don't need to write donate_df and donate_df$ things.
  filter(gender == 'M') # find only gender = M
male_donate

value_500 <- donate_df %>%
  filter(value>=500)
value_500

donate_1 <- donate_df %>%
  select(-code) # remove the code column
donate_1

donate_2 <- donate_df %>%
  filter(value >= 500) %>% # filter only the value over 500
  select(-code) # remove the code column
donate_2

donate_df$usd <- donate_df$value / 28 # create a new variable without using %>%

donate_df <- donate_df %>% # if we use %>%, we can omit donate_df and donate_df$
  mutate(usd = value / 28, 
         jpn = value * 4.12,
         ddefrefs = usd*jpn) # we can create a variable just like this. mutate?
donate_df

donate_df$sex <- 0 # old way. put every sex 0 first and then 
donate_df$sex[donate_df$gender == 'F'] <- 1 # and then only F change to 1




# IMP R's if = case_when 

# my_basket %>% 
#   mutate(Price_band = 
#            case_when
#          (Price>=50 & Price <=70   ~ "Medium", 
#            Price > 70 ~ "High", 
#            TRUE ~ "Low"))
# 
# case_when(
#   x %% 5 == 0 ~ "5X",
#   x %% 7 == 0 ~ "7X",
#   x %% 9 == 0 ~ "9X",
#   TRUE ~ as.character(x)
# )
# ¿¹½Ã

donate_df <- donate_df %>% 
  mutate(sex = case_when(gender == 'M' ~ 0, TRUE ~ 1)) # '~' means '==' in other lang. Also, we used = for new var called 'sex'
donate_df

donate_df <- donate_df %>%
  mutate(donate = case_when(value >= 1000 ~ 3,
                            value >= 500 & value < 1000 ~ 2,
                            TRUE ~ 1)) # if more than 3 variables, we can do like this 
donate_df # if we use FALSE instead of TRUE 300 is NA not 1


male_donate <- donate_df[donate_df$gender == 'M',] # Without dplyr, we have to write down like this
f_donate <- donate_df[donate_df$gender == 'F', -1] # without the first column 

gender_df <- donate_df %>%
  group_by(gender) %>% # each gender group
  summarise(donate = mean(value)) # summaries it as a new var called donate which shows the mean of value 
gender_df

# bring house congress folder 

house_115_2016 <- house_115[!(house_115$successor == 1) & !(house_115$non_voting == 1),] # it is ([i,j])
house_115_2018 <- house_115[!(house_115$vacate == 1) & !(house_115$non_voting == 1),] # we don't need to do this

house_115_2016 <- house_115 %>%
  filter(successor != 1) %>% 
  filter(non_voting != 1)
house_115_2016

house_115_2018 <- house_115 %>%
  filter(vacate != 1) %>%
  filter(non_voting != 1)
house_115_2018


setwd("C:/Users/sung/Desktop/Big data with R/0303 US HOUSE - 1, 2, 3, 4") # set the working directory so that I can bring two files

# Using dplyr's filter() to create table objects house_116_2019, house_116_2020, house_117_2021, house_117_2022
house_116 <- read.csv("house_116.csv") # read csv file 
house_117 <- read.csv("house_117.csv")


house_116_2019 <- house_116 %>%
  slice(-c(8, 283, 418))
  filter(successor != 1) %>%
  filter(non_voting != 1)
house_116_2019

house_116_2020 <- house_116 %>%
  filter(vacate != 1) %>% # only leave vacate is not 1, which means leaving 0 
  filter(non_voting != 1)
house_116_2020

house_117_2021 <- house_117 %>%
  filter(successor != 1) %>%
  filter(non_voting != 1)
house_117_2021

house_117_2022 <- house_117 %>%
  filter(vacate != 1) %>%
  filter(non_voting != 1)
house_117_2022

#Week 04

#Table Merge

#cbind vs merge

set.seed(123) # we need to use the same set.seed number so that we can fairly get data

df1 <- data.frame(id = 1:10, math = rpois(10,50)) # generate random number based on standard deviation 50 at middle and range is left, right 10 
df1

set.seed(123)
df2 <- data.frame(id = 10:1, art = rpois(10,70))
df2

# Don't use cbind!
# cbind works only when two tables observations are arranged in the same order
# cbind shows us id 1~ 10 for df1, and id 10 ~ 1 for df2, which looks ugly
# So, use 'merge' and by = "id". Then we can get id 1 ~ 10 equally even if it is df2.
# It automatically is organized

df <- cbind(df1, df2)
df

df <- merge(df1, df2, by = "id") # combine together id 1 ~ 10
df

#Different size of observations

set.seed(123)
df3 <- data.frame(id = 1:9, reading = rpois(9,65))
df3

df_a <- merge(df, df3, by = "id")
df_a # it shows only by 9th since df3 has only 9 not 10

df_b <- merge(df, df3, by = "id", all = TRUE)
df_b # by using all = TRUE, it shows by 10th students although we don't have a 10th student that has reading score 

# if there are over millions of observations, we need to find out what's wrong.

#merge vs dplyr

# Basic R¡¯s merge:
df <- merge(df1, df2, by = "id")
df

# dplyr¡¯s merge:
df <- df1 %>%
  left_join(df2, by = "id") # put df2 next to df1 as left join
df

df <- df %>%
  full_join(df3, by = "id") # like merge and all = TRUE, it shows all the value although one of them is NA  / put df3 next to df1
df

#The names of by column are different

set.seed(123)

df4 <- data.frame(sid = 1:11, writing = rpois(11, 75))
df4

df_e <- df %>% # left_join's standard is df which has 1~10th rows. 
  left_join(df4, by = c("id" = "sid")) # it only shows 10th since the standard is df. R will drop after 10th although it has 11th
df_e # change df4's sid into id 

df_f <- df %>%
  full_join(df4, by = c("id" = "sid")) # full_join accepts every thing rows although some does not have 11th row data.
df_f

#Merge tables by two columns

set.seed(123)
df5 <- data.frame(id = rep(1:10, 2), year = rep(c("2020", "2021"), each = 10) , math = rpois(20, 70))
df5 # rep(1:10, 2) 2 means repeat id(1~10) 2 times / 2020 and 2021 - 10 times each / sd = 70, range is 20 


set.seed(123)
df6 <- data.frame(id = rep(1:10, each = 2), year = rep(c("2020", "2021"), time = 10) , reading = rpois(20, 70))
df6 # rep(1:10, each = 2 repeat id(1,1 2,2 3,3 ...) like this 2 times each / 2020 - 1,3,5,7... , 2021 - 2,4,6,8..
# actually we can skip that time = 10

df_new <- df5 %>%
  left_join(df6, by = c("id", "year"))
df_new
