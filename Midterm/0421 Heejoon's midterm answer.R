#Document: Midterm Exam
#Course: Big Data and Social Analysis
#Semester: Spring 2022
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

##Library
library(readxl) #For reading excel files
library(dplyr) #Used for data frame manipulation
library(ggplot2) #Used for visualization
library(extrafont) #To prevent error for when plotting x and y valueslibrary(tidyr) #For gathering or spreading
library(zoo) # Import Zoo Since as.Data and as.POSIXct can't produce monthly or quarterly data

### Answers --------

#1. (5 points)
setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

tw_2018 <- read_xlsx("result_2018.xlsx")
tw_2020 <- read_xlsx("result_2020.xlsx")

#2. 

#(1) (5 points)
sum(tw_2018$DPP)/sum(tw_2018$valid_vote)
#DPP's percentage of votes in 2018 was 0.4390412 or 43.9%
sum(tw_2018$KMT)/sum(tw_2018$valid_vote)
#KMT's percentage of votes in 2018 was 0.5065019 or 50.65%
sum(tw_2020$DPP)/sum(tw_2020$valid_vote)
#DPP's percentage of votes in 2020 was 0.6404395 or 64.04%
sum(tw_2020$KMT)/sum(tw_2020$valid_vote)
#KMT's percentage of votes in 2020 was 0.4461688 or 44.62%


#(2) (15 points)

change_rate <- data.frame(Year = c("2018", "2018", "2020", "2020"),
                          rate = c(0.4390412, 0.5065019, 0.563992, 0.3929108),
                          name = c("DPP", "KMT", "DPP", "KMT"))
ggplot(change_rate, aes(x=Year, y=rate, group = name, color = name))+
  geom_line()



#3. (20 points)
tw_2018_simple <- tw_2018 %>%
  select(district_e, 9, 12)
colnames(tw_2018_simple)[2] <- c("KMT_2018")
colnames(tw_2018_simple)[3] <- c("valid_vote_2018")

tw_2020_simple <- tw_2020 %>%
  select(district_e, 9, 11)
colnames(tw_2020_simple)[2] <- c("KMT_2020")
colnames(tw_2020_simple)[3] <- c("valid_vote_2020")


tw_2018_simple_town <- tw_2018_simple %>%
  group_by(district_e) %>%
  summarise(kmt_rate_2018 = sum(KMT_2018)/sum(valid_vote_2018))

tw_2020_simple_town <- tw_2020_simple %>%
  group_by(district_e) %>%
  summarise(kmt_rate_2020 = sum(KMT_2020)/sum(valid_vote_2020))

tw_voting_change <- merge(tw_2018_simple_town, tw_2020_simple_town, by = 'district_e')

tw_voting_change <- tw_voting_change %>%
  mutate(difference = kmt_rate_2018 - kmt_rate_2020)

tw_voting_change %>% arrange(desc(difference))

top_n(tw_voting_change, n=10) %>% 
  ggplot(., aes(x = district_e, y = difference))+
  geom_bar(stat = 'identity')



#4. (25 points)
popu_2018_list <- list.files('popu_2018')

popu_2018 <- data.frame()
for (i in 1:312){ 
  temp_df<- read_excel (paste0("popu_2018/", popu_2018_list[i]))  
  popu_2018<- rbind(popu_2018,temp_df)
}


#5. (15 points)
df <- full_join(tw_2018, tw_2020, by = 'li_id')

election_2018_2020 <- full_join(df, popu_2018, by = 'li_id')


#6. (10 points)
election_2018_2020 <- election_2018_2020 %>%
  mutate(kmt_gap = KMT.y - KMT.x,
         college_rate = college/P_CNT,
         aged_rate = A60UP_CNT/P_CNT,
         working_rate = A15A64_CNT/P_CNT)



#7. (15 points)

#(1)
ggplot(election_2018_2020, aes(x= college_rate, y = kmt_gap))+
  geom_point()

#(2)
ggplot(election_2018_2020, aes(x= aged_rate, y = kmt_gap))+
  geom_point()

#(3)
ggplot(election_2018_2020, aes(x= working_rate, y = kmt_gap))+
  geom_point()

#(4)
ggplot(election_2018_2020, aes(x= income_mid, y = kmt_gap))+
  geom_point()

#8. (15 points)
#1
x <- c(election_2018_2020$college_rate)
y <- c(election_2018_2020$kmt_gap)
mod = lm(y~x)
coef(mod) 
#The slope is 397.66

#2
x <- c(election_2018_2020$aged_rate)
y <- c(election_2018_2020$kmt_gap)
mod = lm(y~x)
coef(mod) 
#The slope is 449.48

#3
x <- c(election_2018_2020$working_rate)
y <- c(election_2018_2020$kmt_gap)
mod = lm(y~x)
coef(mod) 
#The slope is -1272.36

#4
x <- c(election_2018_2020$income_mid)
y <- c(election_2018_2020$kmt_gap)
mod = lm(y~x)
coef(mod) 
#The slope is 0.65

#We can conclude that income_mid has tiny effect on kmt_gap while working_rate promotes the decrease in kmt_rate.
#Both college_rate and aged_rate have positive effects of kmt_gap.


#9. (10 bonus points)






#10. (10 points)

tweets <- read_excel('fake_tweets_election.xlsx')
tweets$time <- as.POSIXct(tweets$date, format = "%Y%m%d %H%M%S")
tweets$a <- as.Date(tweets$time)

new_tweets <- tweets %>%
  filter(a > '2020-09-27')

unique(new_tweets$a)



#11. (10 points)
grep('biden', new_tweets$text, ignore.case = TRUE)
grepl('biden', new_tweets$text, ignore.case = TRUE)

for (i in 1:7661) {
  if(grepl('biden', new_tweets$text[i], ignore.case = TRUE) == TRUE)
    new_tweets$biden[i] <- 1
  else
    new_tweets$biden[i] <- 0 
}

grep('trump', new_tweets$text, ignore.case = TRUE)
grepl('trump', new_tweets$text, ignore.case = TRUE)

for (i in 1:7661) {
  if(grepl('trump', new_tweets$text[i], ignore.case = TRUE) == TRUE)
    new_tweets$trump[i] <- 1
  else
    new_tweets$trump[i] <- 0 
}


#12. (25 points)

new_tweets$b <- as.yearmon(as.Date(new_tweets$a)) #Using 'as.yearmon()' function and 'as.Date()' we make a new variable called month that shows just the month and date the tweets were posted.

tweets_mis <- new_tweets %>% 
  summarise(Biden = sum(biden), Trump = sum(trump))

tweets_mis_gather <- tweets_mis %>%
  gather("Name", "Posts") 

ggplot(tweets_mis_gather, aes(Name, Posts, fill = Name)) +
  geom_bar(stat = "identity", position = position_dodge())

#We can clearly see that Biden suffered more from misinformation attacks



#13. (25 points)
new_tweets$hour <- format(new_tweets$time, "%H")

new_tweets_hour <- new_tweets %>%
  group_by(hour) %>%
  summarise(Biden = sum(biden), 
            Trump = sum(trump))

final_tweets <- new_tweets_hour %>%
  gather("Name", "Posts", 2:3)

ggplot(final_tweets, aes(x=hour, y=Posts, group = Name, color = Name))+
  geom_line()

#We can observe that Biden suffered more from paid cyber warriors' misinformation.


# 10. I suggest a >= '2020-09-27'

# 12. -2 Your gather() missed "2:3"