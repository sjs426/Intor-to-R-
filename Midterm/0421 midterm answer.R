#Document: Midterm Exam
#Course: Big Data and Social Analysis
#Semester: Spring 2022
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: 
#English First Name:
#UID: 
#E-mail: 

### Answers --------

library(plyr)
library(dplyr)
library(tidyr)
library(utils)
library(stringr)
library(readr)
library(readxl)
library(writexl)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#1. (5 points)

tw_2018 <- read_xlsx("result_2018.xlsx")

tw_2020 <- read_xlsx("result_2020.xlsx")

#2. 

#(1) (5 points)

kmt_rate_2018 <- sum(tw_2018$KMT) / sum(tw_2018$valid_vote)
dpp_rate_2018 <- sum(tw_2018$DPP) / sum(tw_2018$valid_vote)

kmt_rate_2020 <- sum(tw_2020$KMT) / sum(tw_2020$valid_vote)
dpp_rate_2020 <- sum(tw_2020$DPP) / sum(tw_2020$valid_vote)

#(2) (15 points)

rate <- data.frame(year = c("2018", "2018", "2020", "2020"), 
                   party = c("KMT", "DPP", "KMT", "DPP"),
                   rate = c(kmt_rate_2018, dpp_rate_2018, kmt_rate_2020, dpp_rate_2020))

ggplot(rate, aes(x = year, y = rate, color = party, group = party)) +
  geom_line()

#3. (20 points)

district_2018 <- tw_2018 %>%
  group_by(district_e) %>%
  summarise(KMT_rate_2018 = sum(KMT) / sum(valid_vote))

district_2020 <- tw_2020 %>%
  group_by(district_e) %>%
  summarise(KMT_rate_2020 = sum(KMT) / sum(valid_vote))

district <- district_2018 %>%
  left_join(district_2020, by = "district_e") %>%
  mutate(kmt_desc = KMT_rate_2020 - KMT_rate_2018) %>%
  arrange(kmt_desc)

ggplot(district[1:10,], aes(district_e, kmt_desc)) +
  geom_bar(stat = "identity")

#4. (25 points)

filelist <- list.files("popu_2018")

popu_2018 <- data.frame()

for(i in 1:length(filelist)) {
  
  district_temp <- read_xlsx(paste0("popu_2018/", filelist[i]))
  
  popu_2018 <- rbind(popu_2018, district_temp)
  
}

#5. (15 points)


tw_2018 <- tw_2018 %>%
  mutate(kmt_rate_2018 = KMT / valid_vote)

tw_2020 <- tw_2020 %>%
  mutate(kmt_rate_2020 = KMT / valid_vote)

election_2018_2020 <- tw_2018[, c(1:7, 16)] %>%
  left_join(tw_2020[, c(7, 15)], by = "li_id") %>%
  mutate(kmt_desc = kmt_rate_2020 - kmt_rate_2018)

election_2018_2020 <- election_2018_2020 %>%
  left_join(popu_2018[, c(7:35)], by = "li_id")



#6. (10 points)

election_2018_2020 <- election_2018_2020 %>%
  mutate(college_rate = college / P_CNT,
         aged_rate = A60UP_CNT / P_CNT,
         working_rate = A15A64_CNT / P_CNT)

#7. (15 points)

ggplot(election_2018_2020, aes(college_rate, kmt_desc)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(election_2018_2020, aes(aged_rate, kmt_desc)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(election_2018_2020, aes(income_mid, kmt_desc)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(election_2018_2020, aes(working_rate, kmt_desc)) +
  geom_point() +
  geom_smooth(method = "lm")

#8. (15 points)

#Tiny impact: college_rate and aged_rate
#Negative impact: working_rate
#Positive impact: income_mid

#9. (10 bonus points)

kmt_lm <- lm(kmt_desc ~ working_rate, data = election_2018_2020)

townlist <- unique(election_2018_2020$TownID)

for (i in 1:length(townlist)) {
  
  towndf <- election_2018_2020 %>%
    filter(TownID == townlist[i])
  
  town_lm <- lm(kmt_desc ~ working_rate, data = towndf)
  
  slope <- town_lm$coefficients[2]
  
  if (slope < kmt_lm$coefficients[2]) {
    
    temp_plot <- ggplot(towndf, aes(working_rate, kmt_desc)) +
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle(paste(unique(towndf$county), unique(towndf$district), "slope:", slope, "p-value:", signif(summary(town_lm)$coef[2, 4], 3)))
    
    ggsave(temp_plot, file = paste0("impact_plot/", townlist[i], ".png"), width = 14, height = 10, units = "cm") 
  }  
}

#There are 84 plots.

#or

impact <- data.frame()

for (i in 1:length(townlist)) {
  
  towndf <- election_2018_2020 %>%
    filter(TownID == townlist[i])
  
  town_lm <- lm(kmt_desc ~ working_rate, data = towndf)
  
  slope <- town_lm$coefficients[2]
  
  if (slope < kmt_lm$coefficients[2]) {
    
    temp <- data.frame(district = unique(towndf$district_e), slope = slope)
    
    impact <- rbind(impact, temp)
    
  }  
}

#There are 84 observations.

#10. (10 points)


fake_tweets <- read_xlsx("fake_tweets_debate.xlsx")

fake_tweets <- fake_tweets %>%
  filter(date >= "2020-09-27")



#11. (10 points)

fake_tweets$biden <- 0

fake_tweets$biden[grepl("Biden|biden", fake_tweets$text) == TRUE] <- 1


fake_tweets$trump <- 0

fake_tweets$trump[grepl("trump|Trump", fake_tweets$text) == TRUE] <- 1


#12. (25 points)

fake_tweets$date <- as.POSIXct(fake_tweets$date)

fake_tweets$day <- as.Date(fake_tweets$date)

compare <- fake_tweets %>%
  group_by(day) %>%
  summarise(trump = sum(trump),
            biden = sum(biden))

compare <- compare %>%
  gather(name, post, 2:3)

ggplot(compare, aes(day, post, fill = name)) +
  geom_bar(stat = "identity", position = position_dodge())

#Answer: Biden

#13. (25 points)

fake_tweets$hour <- format(fake_tweets$date, "%H")

hour <- fake_tweets %>%
  group_by(hour) %>%
  summarise(trump = sum(trump),
            biden = sum(biden))

hour <- hour %>%
  gather(name, post, 2:3)

ggplot(hour, aes(hour, post, group = name, color = name)) +
  geom_line()

#Answer: Biden