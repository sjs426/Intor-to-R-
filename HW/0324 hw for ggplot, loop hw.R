#Class: Week 06
#Course: Big Data and Social Analysis
#Semester: Spring 2021
#Lesson: Loop
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: ??????
#English First Name: Jongsung
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com

### Questions --------

#In June 2020, Kaohsiung held a mayoral recall election. Finally, the recall was successful. 
#Han Kuo-yu, who was elected as mayor in 2018, was recalled.

#Unzip kh_recall.zip, which involve all districts/towns?? recall data.

#Please answer the following questions. Remember, No Comments, No Points!!!!!!!

#Question 1: (8 Points) 
#Use loop to rbind all towns' data into one dataframe called kh_recall

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
library(readxl) # for xlsx file
library(jsonlite) # JSON can store complicated data such as nested data.
kh_recall_list <- list.files('kh_recall')
kh_recall_list

kh_recall <- data.frame() # create a new empty dataframe -> data.frame()
for(i in 1:length(kh_recall_list)){ # by using length, we can get the number of kh_recall_list
  kh_recall_df <- read_excel(paste0('kh_recall/', kh_recall_list[i])) # We will read from file 1 to the end (38). 
  kh_recall <- rbind(kh_recall, kh_recall_df) # combine kh_recall_df to kh_recall
}
print(i) # 38. I was wondering why kh_recall_df has only 3 obs of 38 variables. It turns out it will stay as kh_recall_list[38], which is the last loop 
# After loop, kh_recall has 888 obs with 38 variables
  
#Question 2: (5 Points)

#Using dplyr's mutate to calculate every li's:
library(dplyr) # dataframe package

#1. DPP absolute rate (DPP_2020 / num_voter_2020)
kh_recall <- kh_recall %>%
  mutate(DAR = DPP_2020 / num_voter_2020) # create a new variable called DAR by using mutate for DPP absolute rate 
View(kh_recall) # check the result 

#2. college rate (college_2018 / P_CNT_2018)
kh_recall <- kh_recall %>%
  mutate(CR = college_2018 / P_CNT_2018) # create a new variable called CR by using mutate for college rate  
View(kh_recall) # check the result

#3. absolute rate of signatures in the second phase (sign_recall_sum / num_voter_recall)
kh_recall <- kh_recall %>%
  mutate(ARS = sign_recall_sum / num_voter_recall) # create a new variable called ARS by using mutate for absolute rate of signatures in the second phase
View(kh_recall) # check the result

# prof comments that I can combine 1~3 altogether. below is his answer 
# kh_recall <- kh_recall %>%
#   mutate(DPP_rate = DPP_2020 / num_voter_2020,
#          highedu_rate = college_2018 / P_CNT_2018,
#          m_rate = M_CNT_2020 / P_CNT_2020,
#          sign_recall_rate = sign_recall_sum / num_voter_recall)

#Question 3: (3 Points)

#Produce a point plot of college rate (x) and recall absolute yea rate (y, recall_yea_rate).
library(ggplot2) # package for chart
ggplot(kh_recall, aes(x = CR, y = recall_yea_rate, color = town)) + geom_point() # a point plot with colors to recognize easily

# prof answer 
ggplot(kh_recall, aes(CR, recall_yea_rate)) +
  geom_point() +
  geom_smooth(method = lm)

#Question 4: (2 Points)

#Produce a point plot of DPP absolute rate (x) and recall absolute yea rate (y, recall_yea_rate).
ggplot(kh_recall, aes(x = DAR, y = recall_yea_rate, color = town)) + geom_point() # a point plot with colors to recognize easily

#Question 5: (5 Points)

#Using dplyr's group_by() and summarise() to calculate every town's DPP absolute rate and recall absolute yea rate
Towns <- kh_recall %>%
  group_by(town) %>% # group by town
  summarise(DAR_town = sum(DPP_2020) / sum(num_voter_2020), # sum each data and divde to get everey town's DDP rate and recall absolute yea rate
            recall_yea_rate_town = sum(agreeTks) / sum(num_voter_recall)) %>%
  mutate(gap = DAR_town - recall_yea_rate_town) %>% # from prof ansewr 6번을 같이 해버리는 작업
  arrange(desc(gap)) # descending 내림차순 큰 수 먼저 작은 수 아래.
Towns

# prof ansewr
# kh_town <- kh_recall %>%
#   group_by(town) %>%
#   summarise(DPP_rate = sum(DPP_2020) / sum(num_voter_2020),
#             sign_recall_rate = sum(sign_recall_sum) / sum(num_voter_recall)) %>%
#   mutate(gap = DPP_rate - sign_recall_rate) %>%
#   arrange(desc(gap))

#Question 6: (2 Points)

#Using dplyr's mutate() to calculate the difference between DPP absolute rate and recall absolute yea rate at town level.
Towns <- Towns %>%
  mutate(D_DPP_YR = DAR_town - recall_yea_rate_town) # create a new column for the difference between DPP absolute rate and recall absolute yea rate
Towns

#Question 7: (5 Points)

#Plot a bar chart to show the top ten difference between DPP absolute rate and recall absolute yea rate at town level 
#(X is town names, and Y is difference values)

# sort the Towns using arrange for descending order
Towns_Top <- Towns %>%
  arrange(desc(D_DPP_YR)) %>% # If I do not use arrange(), top_n will not be in descending order since arrange() and top_n ordering function is repeated
  top_n(10) # we can select top 10 by top_n by using dplyr package. 
View(Towns_Top)

ggplot(Towns_Top, aes(town, D_DPP_YR, fill = town)) + geom_bar(stat = 'identity')
# I use fill to color each town 

ggplot(Towns_Top, aes(x = reorder(town, -D_DPP_YR), y = D_DPP_YR, fill = town)) + geom_bar(stat = 'identity')
# reorder (차트 내림차순) from prof answer 


# prof answer 
# use reorder(town,-gap) for bar graph

# kh_town_10 <- Towns[1:10,]
# 
# ggplot(kh_town_10, aes(x = town, y = gap)) +
#   geom_bar(stat = "identity")
# 
# ggplot(kh_town_10, aes(x = reorder(town, -gap), y = gap)) +
#   geom_bar(stat = "identity")
