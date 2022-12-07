#Class: Week 07
#Course: Big Data and Social Analysis
#Semester: Spring 2021
#Lesson: Loop and Regular Expression
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: ??????
#English First Name: Jongsung
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com

### Questions --------

library(dplyr)
library(ggplot2)
library(readxl)

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
kh_recall <- read_xlsx('kh_recall.xlsx')

#Last week, you used a loop to create a dataframe called kh_recall.
#You also created the following two variables:

#1. DPP absolute rate: ddp_rate (DPP_2020 / num_voter_2020)
kh_recall <- kh_recall %>%
  mutate(ddp_rate = DPP_2020 / num_voter_2020) # create a new variable called DAR by using mutate for DPP absolute rate 
View(kh_recall) # check the result 

#2. absolute rate of signatures in the second phase: sign_rate (sign_recall_sum / num_voter_recall)
kh_recall <- kh_recall %>%
  mutate(sign_rate = sign_recall_sum / num_voter_recall) # create a new variable called ARS by using mutate for absolute rate of signatures in the second phase
View(kh_recall) # check the result

#Question 1 (5 points):

#Produce a point plot of ddp_rate (x) and sign_rate (y) with a regression line.
#Tell me the relationship between the x and y is positive or negative.

kh_recall_plot <- ggplot(kh_recall, aes(ddp_rate, sign_rate)) + 
  geom_point() + # get plot point
  geom_smooth(method = lm) +  # lm is regression. so I add regression line
  ggtitle('The relationship') # title 
kh_recall_plot

# the relationship between the x and y is positive since when the x increase, y tends to increase too.

#Question 2 (2 points):

#We attempt to create all towns?? plots (x = ddp_rate and y = sign_rate) to check individual town??s results.

#Please create a vector called town_list to get all towns' name:

town_list <- unique(kh_recall$town) # use unique to get each town's name  
town_list

#Question 3 (3 points):

#Using filter() to slice data of the first town in your town_list(town_list[1]) from kh_recall

town_list_sliced <- kh_recall %>%
  filter(town == town_list[1]) # we can get the first town's whole data
View(town_list_sliced)

#Question 4 (10 points):

#Using a loop to produce every town's plot of ddp_rate (x) and sign_rate (y) with a regression line.

for (i in 1:length(town_list)) {
  tl <- kh_recall %>%
    filter(town == town_list[i]) # we can get whole town's data
  
  kh_recall_e_plot <- ggplot(tl, aes(ddp_rate, sign_rate)) + # I need to put ggplot for tl not kh_recall
    geom_point() + # dots
    geom_smooth(method = lm) + # regression 
    ggtitle(town_list[i]) # change the name for each graph
  print(kh_recall_e_plot)
# ggsave(kh_recall_e_plot, file = paste0('kh_recall_e_plot/',town_list[i],'.png'), width = 14, height = 10, units = 'cm')
# Error occured since my laptop does not support Chinese? so I just printed those and checked 
  }

#Question 5 (5 Bonus points): 
#Please review all plots, tell me which towns have an opposite relationship between ddp_rate (x) and sign_rate(y) to whole Kaohsiung City.

# It seems '????ϡ' has the only negative relationship among cities since it shows the descending line 
# to know exact data, I will add more info next to the title
# ???? is chinses 

match('????ϡ',town_list) # use match to find out the index of the city. It turns out it is 37 th 

N_city <- kh_recall %>% # get a new variable with 37th data
  filter(town == town_list[37])

lm_city <- lm(sign_rate ~ ddp_rate, data = N_city)

tamp_plot <- ggplot(N_city, aes(ddp_rate, sign_rate)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle(paste(town_list[37], 
                "slope:", 
                signif(lm_city$coefficients[2], 3), # get what we need only
                "pvalue:", 
                signif(summary(lm_city)$coef[2, 4], 3)
  ))
tamp_plot

# it shows slope: -0.00205 and p-value: 0.957
# since the p_value is over than 0.05, the result is not likely to be significant.

#Question 6 (5 points):

#Please read trumptweets.xlsx which records Trump's tweets from Feb. 2017 to May 2018.
#The column text is the context of Trump's tweets.
#Please use regular expression (regexpr(), grep(), or grepl()) and other R skills to create a new column called china
#if Trump mentioned China, please label 1 otherwise 0

trumptweets <- read_xlsx('trumptweets.xlsx')

length(trumptweets$text)

for (i in 1:100) {
  print(grepl('china',trumptweets$text[i], ignore.case = TRUE)) # grepl shows us TRUE / FALSE 
} # check if it is working right or not. ignore.case does not care either lower case or upper case

for (i in 1:length(trumptweets$text)){
  if(grepl('china',trumptweets$text[i] , ignore.case = TRUE) == TRUE){
    trumptweets$china[i] <- 1
    } else {
      trumptweets$china[i] <- 0
    }
  }
View(trumptweets)
unique(trumptweets$china) # succssfully I got 0 and 1 

# for (i in 1:length(trumptweets$text)) {
#   trumptweets <- trumptweets %>%
#     mutate(china =  case_when
#            (grepl('china',text[i], ignore.case = TRUE) == TRUE ~ '1', TRUE ~ '0'))}
# I tried this with diverse ways, but I eventually failed to get the what I want so I looked up the if function and works. what can I fix here?


#Question 7 (2 points):
           
#The column created_at represent when Trump posted his tweets.
#Use created_at to create a column called year which shows Trump's tweet year.

yp <- regexpr('-', trumptweets$created_at) # - is located on 5th 
yp

trumptweets$year <- substr(trumptweets$created_at, 1, yp - 1) # yp = 5 
unique(trumptweets$year) # check if there is wrong data. It shows 2017, 2018. It worked

#Question 8 (3 points):

#Use group_by() and summarise() to tell me the number of Trump's tweets which mentions China in 2017 and 2018.
trumptweets_m_c <- trumptweets %>%
  group_by(year) %>% #group_by year to separate it 2017 and 2018
  summarise(MC = sum(china)) # sum all the 1 into two groups

View(trumptweets_m_c) # 2017 -> 43 / 2018 -> 30 

# I think it would be great to see it on the graph
ggplot(trumptweets_m_c, aes(year, MC, fill = year)) + 
  geom_bar(stat = 'identity', position = position_dodge())
