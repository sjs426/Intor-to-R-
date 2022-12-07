library(readxl)
setwd("C:/Users/sung/Desktop/Big data with R/0303 US HOUSE - 1, 2, 3, 4")
library(dplyr)
house_115 <- read.csv('house_115.csv')
house_116 <- read.csv('house_116.csv')
house_117 <- read.csv('house_117.csv')
trumpscore <- read_xlsx('trumpscore.xlsx')

# bring US_congress file too

house_115_gp <- house_115_2016 %>%
  group_by(gender, party) %>%
  summarise(sex = n(), # utilize the existed data to make a new variable called sex and show the nrow by gender and party
            ratio = n() / nrow(.)) # count the observation of gender and party, the previous data of nrow = . is 435
View(house_115_gp)

house_115_state <- house_115_2016 %>%
  group_by(state, gender, party) %>%
  summarise(sex = n()) # show the nrow by state, gender and party
View(house_115_state)

house_115_sl <- house_115_2016 %>%
  group_by(state) %>%
  summarise(rep = n()) # show the nrow by state
View(house_115_sl)

house_115_state <- house_115_sl %>%
  left_join(house_115_state, by = 'state') # merge the data based on 'state' as a key
View(house_115_state)

house_115_state <- house_115_state %>%
  mutate(ratio = sex/rep)
View(house_115_state)

# practice2 by myself - 2. Were female lawmakers less likely to support Trump?
  
colnames(house_115)
colnames(trumpscore)
house_115_female <- select(house_115_2016,c(1,8)) # only select column 1 and 8

trump_f <- trumpscore %>%
  group_by(bioguide, agree_pct)

names(house_115_female)[1] <- 'bioguide'
# like this we can change the name. In here, 'id' -> 'bioguide'

house_115_female <- house_115_female %>%
  left_join(trump_f, by = 'bioguide') 
# we use the house as a base and mutate trump to the house since trump does not have gender column while house has 

house_115_female <- house_115_female %>%
  group_by(gender) %>%
  summarise(ratio = mean(agree_pct, na.rm = TRUE)) %>%
  mutate(ratio2 = ratio * 100) #na.rm = TRUE means if i have any missing data, please omit it 
house_115_female

# Jasmine way 
temp <- house_115_2016 %>%
  left_join(trumpscore,by=c("id"="bioguide"))
# we use the house as a base and mutate trump to the house since trump does not have gender column while house has 

trump_mean <- temp %>%
  group_by(gender)%>%
  summarise(ratio = mean(agree_pct, na.rm = TRUE))
trump_mean 

# professor code
house_115_2016 <- house_115_2016 %>%
  left_join(trumpvote[,c(2,7:10)], by = c('id' = 'bioguide')) # this two names are the same.
# change trumpvote's bioguide to id  

house_115_trump <- house_115_2016 %>%
  group_by(gender) %>%
  summarise(tv = mean(agree_pct, na.rm = TRUE)) 
View(house_115_trump)

ggplot(house_115_trump, aes(gender, tv, fill = gender)) + 
  geom_bar(stat = 'identity', position = position_dodge())


# t is very significant
t.test(house_115_2016$agree_pct[house_115_2016$gender == 'M'],
       house_115_2016$agree_pct[house_115_2016$gender == 'F'])

house_115_trump_p <- house_115_2016 %>%
  group_by(gender, party) %>%
  summarise(tv = mean(agree_pct, na.rm = TRUE))

trump_support_1 <- lm(agree_pct ~ gender, data =
                        house_115_2016)
summary(trump_support_1)

trump_support_2 <- lm(agree_pct ~ gender + party, data = house_115_2016) # (y ~ x1 + x2)
summary(trump_support_2) # we can know the intercept, p-value and R^2

# there are other term's of lawmaker. trumpvote has another data 
