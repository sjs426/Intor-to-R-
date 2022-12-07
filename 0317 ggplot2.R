library(ggplot2)
library(readr)
library(dplyr)
gpa <- read_csv("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE/gpa.csv")

# data, geometric, aesthetics are the main point

ggplot(gpa, aes(x = studyHr, y = GPA, color = gender)) + geom_point()
# aes is aesthetics which means colorful? art?
# by using color = gender, we can color them
ggplot(gpa, aes(gender)) + geom_bar()

gender <- gpa %>%
  group_by(gender) %>%
  summarise(GPA = mean(GPA))
gender

ggplot(gender, aes(gender, GPA)) + geom_bar(stat = 'identity')
# identity means that you have Y value

# house_115_2016's gender distribution
ggplot(house_115_2016, aes(gender)) + geom_bar()

# create a new df that includes 3 term's gender info, bring us congress file and hw 0303 house 116 file
house_gender_115 <- house_115_2016 %>%
  group_by(gender) %>%
  summarise(number = n()) %>% # utilize the existed data to make a new variable and show nrow by gender
  mutate(term = '115')

house_gender_115

house_gender_116 <- house_116_2019 %>%
  group_by(gender) %>%
  summarise(number = n()) %>% # utilize the existed data to make a new variable and show nrow by gender
  mutate(term = '116') # term is 116 
house_gender_116

house_gender_117 <- house_117_2021 %>%
  group_by(gender) %>%
  summarise(number = n()) %>% # utilize the existed data to make a new variable and show nrow by gender
  mutate(term = '117')
house_gender_117

house_gender <- rbind(house_gender_115, house_gender_116, house_gender_117)
house_gender

ggplot(house_gender, aes(term, number, fill = gender)) + geom_bar(stat = 'identity', position = position_dodge())
# term = x, number = y
# if we use color, only border has a slight color
# if we use fill = gender => we can separate the male and female colorfully in a bar
# with position = position_dodge() => we can separate those two in each bar

ggplot(house_gender, aes(term, number, group = gender)) + geom_line(aes(color = gender))
# we can use geom_line for a line graph and put color 

# 0324 ~

# Practice 4 - draw a plot to show the degree of party controls on lawmakers change from 115-117 temrs 
party_vote <- data.frame(term = c('115', '116','117'),
                         against_party = 
                           c(mean(house_115_2016$votes_against_party_pct),
                             mean(house_116_2019$votes_against_party_pct, na.rm = TRUE),
                             mean(house_117_2021$votes_against_party_pct)
                           ))


ggplot(party_vote,aes(x= term, y = against_party, group = 1)) + geom_line() # x=, y= can be omitted

# create a new df that includes 3 term's gender info, 
# execute congress file and hw 0303 house 116 file

# we need to put new variable's name first and then = mean(a column you want to get mean)
house_gender_115 <- house_115_2016 %>%
  group_by(gender) %>%
  summarise(against_party = mean(votes_against_party_pct)) %>% 
  mutate(term = '115')
house_gender_115

# some lawmakers has NA. so we need to use na.rm = TRUE
house_gender_116 <- house_116_2019 %>%
  group_by(gender) %>%
  summarise(against_party = mean(votes_against_party_pct, na.rm = TRUE)) %>% 
  mutate(term = '116')
house_gender_116

house_gender_117 <- house_117_2021 %>%
  group_by(gender) %>%
  summarise(against_party = mean(votes_against_party_pct)) %>% 
  mutate(term = '117')
house_gender_117

party_vote_all <- rbind(house_gender_115, house_gender_116, house_gender_117) # combine all together

# create a variable called gender and put 'all'
party_vote$gender = 'all'

# combine party_vote_all that has female and male data and party_vote for all
party_vote_all <- rbind(party_vote_all, party_vote)

ggplot(party_vote_all,aes(x= term, y = against_party, group = gender)) + geom_line(aes(color = gender))

