#Class: Week 04
#Course: Big Data and Social Analysis
#Semester: Spring 2021
#Lesson: Advanced Dataframe Manipulation
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: ??????
#English First Name: Jongsung
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com

### Questions --------
#Read ntc_ref_2018.xlsx and ntc_ref_2021.xlsx.
# list the library we need
library(tidyr)
library(dplyr)
library(censusapi)
library(WDI)
library(readxl)

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
NTC2018 <- read_xlsx("ntc_ref_2018.xlsx") 
NTC2021 <- read_xlsx("ntc_ref_2021.xlsx") 

#These two files are the results of New Taipei City's 2018 and 2021 pro-nuclear power referendums (Case no. 16 and 17) at li level. 
#These two referendums' had totally different results: the pro-nuclear side won in 2018, but lost in 2021.
#We are curious about the difference between the two referendums at district and li level.
#Please answer the following questions. Remember, No Comments, No Points!!!!!!!
#Question 1: 

#Merge these two data
colnames(NTC2018) # I used colnames to get the exact value
colnames(NTC2018)[c(2,3)] # I used this code to know those two data's name
NTC <- merge(NTC2018, NTC2021, by = c("li_id", "district")) # I combined the two data together stating from li_id followed by district 

# 21+(8-2) = 27 variables


# Prof answer
NTC_D <- NTC2018 %>%
  left_join(NTC2021[, -1], by = "li_id") # 18년에 맨 첫 번째 column 이 빠진 21년이 왼쪽으로 들어오는데 li_id 기준으로 통합한다.

# ntc_ref <- ntc_ref_2018 %>% 
#   left_join(ntc_ref_2021, by = "li_id") # -1 안 해도 가능하긴 하다.

# #Two district names
# ntc_ref <- ntc_ref_2018 %>%
#   left_join(ntc_ref_2021, by = c("district", "li_id")
#             #If two tables?? districts have different content?!
# ctrl + shift + c can make it all #


nrow(NTC) # 1032
nrow(NTC2018) # check the nrow if it has more than 1032 rows
nrow(NTC2021) # check the nrow if it has more than 1032 rows

#Question 2:
#Using dplyr's mutate to calculate every li's yea rate in 2018 and 2021 (number of yea / number of valid vote)

colnames(NTC) # check the index of column
NTC1 <- select(NTC, c(1,15,17,22,24)) # select columns that we need to see properly
colnames(NTC1)

NTC2018 <- NTC1 %>%
  mutate(NTC2018 = rf_16_yea/rf_16_valid_vote) # assign NTC2018 with a new column called NTC2018
NTC2021 <- NTC1 %>%
  mutate(NTC2021 = rf_17_yea/rf_17_valid_vote) # assign NTC2021 with a new column called NTC2021
# prof comments that why don't I use NTC that is merged already. 

# Prof answer 
NTC_D <- NTC_D %>%
  mutate(yearate_2018 = rf_16_yea / rf_16_valid_vote,
         yearate_2021 = rf_17_yea / rf_17_valid_vote,
         gap = yearate_2021 - yearate_2018,
         gap_rate = (rf_17_yea - rf_16_yea) / rf_17_yea)

#Question 3:
#Which li's yea rate has largest decrease? (Give me the li_id)

LD <- NTC2018 %>%
  mutate(NTC2018,NTC2021) # add these new columns into a new variable called LD

LD <- LD %>%
  select(-c(2:5)) # leave the only data we need

LD <- LD %>%
  mutate(LDD = NTC2021-NTC2018) # get new column which shows the difference between these two data

# find the minimum row by which.min.
LD[which.min(LD$LDD),] #  -0.379371 -> no.505, li_id: 6500600-021

# Prof answer 
NTC_D # 이 차트 보고 파악. 밑에도 같음 

#Question 4:
#Which li's yea rate has largest increase? (Give me the li_id)

# find the maximum row by which.max.
LD[which.max(LD$LDD),] # 0.2155448 -> no.384, li_id: 6500400-010

#Question 5:
#Using dplyr's group_by() and summarise() to calculate every district's yea rate

DYR <- NTC %>%
  group_by(district) %>% # grouping district  
  summarise(sum_rf_16 = sum(rf_16_yea), # shows the sum of rf_xx_yea and vaild_vote and each year's ratio
            sum_rf_16_vote = sum(rf_16_valid_vote), 
            sum_rf_17 = sum(rf_17_yea),
            sum_rf_17_vote = sum(rf_17_valid_vote),
            sum_rf_16_ratio = sum_rf_16/sum_rf_16_vote,
            sum_rf_17_ratio = sum_rf_17/sum_rf_17_vote,
            Difference = sum_rf_17_ratio - sum_rf_16_ratio)
            # summaries every district's year rate by making a new variable with a calculation

# 다른방법 summarise_at(vars)
ntc_dist <- DYR %>%
  group_by(district) %>%
  summarise_at(vars(sum_rf_16_ratio, sum_rf_17_ratio), list(name = mean))

# Prof answer 
 ntc_dist <- NTC_D %>%
   group_by(district) %>%
   summarise(rf_16_yea = sum(rf_16_yea),
             rf_16_valid_vote = sum(rf_16_valid_vote),
             rf_17_yea = sum(rf_17_yea),
             rf_17_valid_vote = sum(rf_17_valid_vote),
             yearate_2018 = rf_16_yea /
              rf_16_valid_vote,
            yearate_2021 = rf_17_yea /
               rf_17_valid_vote,
             gap = yearate_2021 - yearate_2018,
             gap_rate = (rf_17_yea - rf_16_yea) /
               rf_17_valid_vote)
 
 ntc_dist1 <- NTC_D %>%
   group_by(district) %>%
   summarise_at(vars(yearate_2018, yearate_2021), list(name = mean))


#Question 6:
#Which district's yea rate has largest decrease?

# same logic above, I use which.min to determine the location
DYR[which.min(DYR$Difference),] #  Pinglin has the largest decrease by -0.253

#Question 7:
#Which district's yea rate has largest increase?

# same logic above, we use which.max to determines the location
DYR[which.max(DYR$Difference),] # Xindian has the least decrease by -0.014

#Now you have 30 points! Question 5 is a bonus!
#Question 8: (5 points)

#Column closest_plant represents the li's distance from a nuclear power plant. 
#Do you think a li's distance from a nuclear power plant affect the changes of results in the two referendums?

# I created a new variable called CP_mean and used mean for closet_plant 
# so that I am able to know each district's mean and compare each other
CP <- NTC %>%
  group_by(district) %>%
  summarise(CP_mean = mean(closest_plant), 
          sum_rf_16 = sum(rf_16_yea),
          sum_rf_16_vote = sum(rf_16_valid_vote),
          sum_rf_17 = sum(rf_17_yea),
          sum_rf_17_vote = sum(rf_17_valid_vote),
          sum_rf_16_ratio = sum_rf_16/sum_rf_16_vote,
          sum_rf_17_ratio = sum_rf_17/sum_rf_17_vote,
          Difference = sum_rf_17_ratio - sum_rf_16_ratio)

# I made the CP_mean descending order to see if the mean is smaller while the difference for the two referendums is larger or not.
CP <- CP[(order(CP$CP_mean)),] 
CP # the result shows there is no certain pattern for this. 
# Therefore, I do not think a li's distrance from a nuclear power plant affect the changes of results in the two referendums.


# prof answer

summary(lm(yearate_2018 ~ closest_plant, data = NTC_D))

#closest_plant 0.0004059  0.0001126   3.606 0.000326 ***

summary(lm(yearate_2021 ~ closest_plant, data = NTC_D))

#closest_plant 0.0010646  0.0003052   3.488 0.000507 ***

summary(lm(gap ~ closest_plant, data = NTC_D))

#closest_plant  0.0006587  0.0002608   2.526   0.0117 * 