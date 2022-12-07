#Document: Midterm Exam
#Course: Big Data and Social Analysis
#Semester: Spring 2022
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: 辛鐘成
#English First Name: Jongsung
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com 

### Answers --------

library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr) # gather() and spread()
library(tidyverse)
library(zoo) # today


#1. (5 points)

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
tw_2018 <- read_excel("result_2018.xlsx")
tw_2020 <- read_excel("result_2020.xlsx")


#2. 

#(1) (5 points)
colnames(tw_2018)

tw_2018_rate <- tw_2018 %>% 
  mutate(kmt_rate_2018 = KMT / valid_vote) %>%
  mutate(ddp_rate_2018 = DPP / valid_vote)

tw_2020_rate <- tw_2020 %>%
  mutate(kmt_rate_2020 = KMT / valid_vote) %>%
  mutate(ddp_rate_2020 = DPP / valid_vote)


#(2) (15 points)
colnames(tw_2018)
colnames(tw_2020)

kmt_rate_a <- tw_2018_rate %>%
  select(kmt_rate_2018)

kmt_rate_b <- tw_2020_rate %>%
  select(kmt_rate_2020)

ab <- merge(kmt_rate_a,kmt_rate_b)

ggplot(ab, aes(tw_2020_rate, tw_2018_rate) +
  geom_line())


tw_2018$Third <- NULL
C_kmt_rate <- rbind(tw_2018, tw_2020)

C_kmt_rate <- C_kmt_rate %>%
  group_by(county_e) %>%
  summarise(kmt_rate = )

ggplot(C_kmt_rate, aes(x = kmt_rate, y= ddp_rate)) + geom_line() # show the plot with 2 lines


#3. (20 points)

tw_2018_R <- tw_2018 %>%
  group_by(district_e) %>%
  summarise(KMT = sum(KMT), DPP = sum(DPP), valid_vote = sum(valid_vote))%>%
  mutate(kmt_rate = KMT / valid_vote) %>%
  mutate(ddp_rate = DPP / valid_vote) 

tw_2020_R <- tw_2020 %>%
  group_by(district_e) %>%
  summarise(KMT = sum(KMT), DPP = sum(DPP), valid_vote = sum(valid_vote))%>%
  mutate(kmt_rate = KMT / valid_vote) %>%
  mutate(ddp_rate = DPP / valid_vote)

two_gap <- merge(tw_2018,tw_2020, all= TRUE)
colnames(two_gap)

T_R < 
gap <- data.frame()
gap <- (tw_2020_R$kmt_rate - tw_2018_R$kmt_rate)
gap

tw_2018 <- tw_2018 %>%
  merge(gap) %>%
  arrange(desc(gap))
# tw_2020_R <- tw_2020 %>%
#   group_by(district_e) %>%
#   summarise_at(.vars = vars(KMT, DPP,valid_vote),
#                .funs = sum) %>%
#   mutate(kmt_rate = KMT / valid_vote) %>%
#   mutate(ddp_rate = DPP / valid_vote)

tw_k_R <- mutate(tw_2018_R$kmt_rate, tw_2020_R$kmt_rate)

tw_R <- 
  mutate(tw_2020_R$kmt_rate - tw_2018_R$kmt_rate)
Kmt_Gap <- rbind(Kmt_Gap,Gap)
arrange(desc(Kmt_Gap))
top_n(10)

# for(i in 1:302) {
#   tw_R <- tw_2020_R$kmt_rate[i] - tw_2018_R$kmt_rate[i]
#   Kmt_Gap <- rbind(Kmt_Gap,Gap)
#   arrange(desc(Kmt_Gap))
#   top_n(10)
# }



#4. (25 points)

library(jsonlite)

popu_2018_list <- list.files("popu_2018")
popu_2018_list

popu_2018 <- data.frame()
for(i in 1:length(popu_2018_list)) {
  popu_2018_df <- read_excel(paste0('popu_2018/',popu_2018_list[i]))
  popu_2018 <- rbind(popu_2018,popu_2018_df)
}
popu_2018


#5. (15 points)

election_2018_2020 <- data.frame()
election_2018_2020 <- merge(tw_2018,tw_2020, all= TRUE) %>%
  merge(popu_2018)


#6. (10 points)
colnames(election_2018_2020)
election_2018_2020 <- election_2018_2020 %>%
 mutate(kmt_gap = kmt_rate_2020 - kmt_rate_2018,
        college_rate = college / P_CNT,
        aged_rate = A60UP_CNT / P_CNT,
        working_rate = A15A64_CNT / P_CNT)

#7. (15 points)

election_2018_2020_plot <- ggplot(election_2018_2020, aes(college_rate, kmt_gap)) + 
  geom_point(na.rm = TRUE) + # get plot point
  geom_smooth(method = lm) +  # lm is regression. so I add regression line
  ggtitle('The relationship') # title 
election_2018_2020_plot

election_2018_2020_plot <- ggplot(election_2018_2020, aes(aged_rate, kmt_gap)) + 
  geom_point(na.rm = TRUE) + # get plot point
  geom_smooth(method = lm) +  # lm is regression. so I add regression line
  ggtitle('The relationship') # title 
election_2018_2020_plot

election_2018_2020_plot <- ggplot(election_2018_2020, aes(working_rate, kmt_gap)) + 
  geom_point(na.rm = TRUE) + # get plot point
  geom_smooth(method = lm) +  # lm is regression. so I add regression line
  ggtitle('The relationship') # title 
election_2018_2020_plot

election_2018_2020_plot <- ggplot(election_2018_2020, aes(income_mid, kmt_gap)) + 
  geom_point(na.rm = TRUE) + # get plot point
  geom_smooth(method = lm) +  # lm is regression. so I add regression line
  ggtitle('The relationship') # title 
election_2018_2020_plot

#8. (15 points)

# college_rate has tiny impact on y
# working_rate has decrease impact on y
# income_mid has positive impact on y


#9. (10 bonus points)



#10. (10 points)

fake_tweet <- read_excel("fake_tweets_election.xlsx")


fake_tweet$date<- as.Date(fake_tweet$date)

fake_tweet <- fake_tweet %>%
  filter(date > "2020-09-27")



#11. (10 points)

length(fake_tweet$text) # 7661
for ( i in 1:length(fake_tweet$text)) {
  if(grepl('biden',fake_tweet$text[i], ignore.case = TRUE) == TRUE)
    fake_tweet$biden[i]  <- 1
  } else {
    fake_tweet$biden[i]  <- 0
  }


for ( i in 1:length(fake_tweet$text)) {
  if(grepl('trump',fake_tweet$text[i], ignore.case = TRUE) == TRUE)
    fake_tweet$trump[i]  <- 1
  } else {
    fake_tweet$trump[i]  <- 0
  }


#12. (25 points) # biden

fake_trump <- fake_tweet %>%
  group_by(date) %>%
  summarise(trump = sum(trump))

fake_biden <- fake_tweet %>%
  group_by(date) %>%
  summarise(biden = sum(biden))   

fake_tweet_plot <- data.frame()
fake_tweet_plot <- merge(fake_trump, fake_biden, by = c('date'))

fake_tweet_plot <- fake_tweet_plot %>%
  gather('who','cases',2:3)

ggplot(fake_tweet_plot, aes(x = date, y = cases, fill = who)) +
  geom_bar(stat = "identity", position = position_dodge())

#13. (25 points)  # biden

fake_tweet <- read_excel("fake_tweets_election.xlsx")

fake_tweet$hour <- format(fake_tweet$date, "%H") # only take hour data to know what time he uses

length(fake_tweet$text) # 7661
for ( i in 1:length(fake_tweet$text)) {
  if(grepl('biden',fake_tweet$text[i], ignore.case = TRUE) == TRUE){ # grepl => TRUE /  FALSE 판별. 맞으면 1 아니면 - 
    fake_tweet$biden[i]  <- 1
  } else {
    fake_tweet$biden[i]  <- 0
  }
}

for ( i in 1:length(fake_tweet$text)) {
  if(grepl('trump',fake_tweet$text[i], ignore.case = TRUE) == TRUE){ # grepl => TRUE /  FALSE 판별. 맞으면 1 아니면 - 
    fake_tweet$trump[i]  <- 1
  } else {
    fake_tweet$trump[i]  <- 0
  }
}

fake_tweet_b_h <- fake_tweet %>%
  group_by(hour) %>%
  summarise(biden = sum(biden)) 

fake_tweet_t_h <- fake_tweet %>%
  group_by(hour) %>%
  summarise(trump = sum(trump)) 

fake_tweet_h <- merge(fake_tweet_b_h, fake_tweet_t_h, by = "hour", all = TRUE)

fake_tweet_h <- fake_tweet_h %>%
  gather("Who", "Cases", 2,3, na.rm = TRUE) # make NA to 0

ggplot(fake_tweet_h, aes(x = hour, y= Cases, color = Who, group = Who)) + geom_line()




# 2. -15 The way you merge is wrong.
# 3. -10
# 5. -10 merge by what?
# 6. -5 Your merge and mutate have programs. As a result, your plots also wrong, although codes are correct.
# 8. -3
# 200-43-5(delay)=152 point
