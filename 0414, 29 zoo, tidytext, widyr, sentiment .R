library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr) # gather() and spread()
library(tidyverse)
library(zoo) # today

ym_1 <- as.yearmon('2015-04-07') # take only year and month
ym_1

as.yearmon(as.Date('2015-04-07')) # make it R date default and show only year and month

as.yearqtr('2015-04-07') # show year and quarter 

# quarter

as.yearqtr(as.Date('2015-04-07')) # make it R date default and show only year and quarter

# DATE -> MONTH
# DATE -> QUARTER
# MONTH -> THE first and last date of month
# QUARTER -> the first and last date of quarter 

date_convert <- data.frame(Date = c('2014-02-05', '2014-08-11', '2014-11-23'),
                           Arrivals = c(100, 150, 200))

date_convert$Date <- as.Date(date_convert$Date)

# date to month

Sys.setlocale(locale = "English") # Change to English 

date_convert$month <- as.yearmon(date_convert$Date) # 위에를 해줘야 Feb, Jan 이렇게 뜸
View(date_convert)

# date to quarter (same results)
date_convert$quarter <- as.yearqtr(date_convert$Date)

date_convert$quarter1 <- as.yearqtr(date_convert$month)

# same as above
# date_convert$quarter2 <- as.yearqtr(as.Date(date_convert$Date))


# month to first and last date of month

date_convert$date1 <- as.Date(date_convert$month, frac = 0) # first day of the month

date_convert$date2 <- as.Date(date_convert$month, frac = 1) # last day of the month 

date_convert$date5 <- as.Date(date_convert$month, frac = 2) # weird haha

# quarter to first and last date of quarter

date_convert$date3 <- as.Date(date_convert$quarter1, frac = 0) # I type wrong like quater so it didnt work

date_convert$date4 <- as.Date(date_convert$quarter1, frac = 1) 



# association

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
trumptweets <- read_xlsx('trumptweets.xlsx')


trumptweets$ym <- as.yearmon(as.Date(trumptweets$created_at))

trump_ym <- trumptweets %>%
  group_by(ym) %>%
  summarise(post = n())

unique(trump_ym)
ggplot(trump_ym, aes(x = ym, y = post, group = 1)) + geom_line() # wonder why JUNE~ JULY, NOV ~ DEC has that sacle 

# https://community.rstudio.com/t/ggplot-not-showing-all-dates-on-x-asis-even-when-forced/7635/9
# but error 
ggplot(trump_ym, aes(x = ym, y = post, group = 1)) + geom_line() + 
  scale_x_date(date_breaks="1 month", date_labels="%Y %m") + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


trumptweets$quarter <- as.yearqtr(as.Date(trumptweets$created_at))

trump_qn <- trumptweets %>%
  group_by(quarter) %>%
  summarise(post = n())

# unique(trump_ym)
ggplot(trump_qn, aes(x = quarter, y = post, group = 1)) + geom_line()



# install.packages("tm")
# install.packages("tidytext")
# install.packages("wordcloud2")
# install.packages("lexicon")
# install.packages("textclean")
# install.packages("widyr")

# Time series analysis -> 1 semester 
# 1. Changepoint analysis
# install.packages("changepoint")
# library(changepoint) # To identify which points' changes are significant

# 2. ARIMA
# install.packages("forecast")
# library(forecast) # x's changes affect y's changes?
# play or do not play affect how many ppl in Taiwan watch the news
# change of audiences' ram
# we utilize ARIMA when we analyze two variables relationship 





library(tidytext) # an efficient tool for text mining in R, merging with dplyr package


ici <- c("ICI is great",
         "NCCU is great",
         "Big data is so easy",
         "I love big data courses of ICI, NCCU",
         "ICI offer 10 great courses")
ici

ici_df <- data.frame(line = 1:5, text = ici, stringsAsFactors = FALSE)

# Let's tokenize ici_df
# help us to separate every word
ici_tokens <- ici_df %>%
  unnest_tokens(word, text) # new column called word and put text that is ici_df's column

ici_frq <- ici_tokens %>%
  group_by(word) %>%
  summarise(number = n()) %>%
  arrange(desc(number)) # 내림차순

# or
ici_frq_c <- ici_tokens %>% # result is the same as above
  count(word, sort = TRUE)


library(wordcloud2) # super cool library
wordcloud2(ici_frq_c)


# Before we start out exercise of text in reality, there is an important step:
# CLEAN DATA

# we want to remove words and symbols that do not have any impact on the meaning to it (Khaild 2020)
# Case by case: symbols, numbers, emoji, urls, hash tages, stop words...
# If it's not ENG, it would be more complicated

# gsub(pattern, replacement, string) => replace all matches
ici <- gsub("[[:digit:]]", '', ici) # it will remove the number in ici
ici

# There are spaces where the digits were, we need to remove it 
ici <- gsub("^\\s+", "", ici)
ici <- gsub("\\s+$", "", ici)
ici <- gsub("[ |\t]+", " ", ici)

ici_df <- data.frame(line = 1:5, text = ici, stringsAsFactors = FALSE)


emoji <- c('1:😀', '2:😍')
emoji_remove <- gsub("[^\x01-\x7F]", "", emoji)
emoji_remove

# converse emojis to words
library(lexicon)
emoji_dic <- hash_emojis # x is the code name, y is how it appears
emoji_dic

library(textclean)
emoji_rp <- replace_emoji(emoji, emoji_dt = emoji_dic)
emoji_rp

# Use stop words dictionary provided by tidytext

ici_tokens_s <- ici_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) # is, great, big, so 이런 거 사라짐

# Professor prefer this below
library(tm)
stopwords('ENGLISH') # 문맥상 필요없는 단어? 지우는 느낌


ici_tokens_tm <- ici_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords('ENGLISH')) # is 같은 동사 사라지고 목적어랑 형용사만?
# 원래는 filter 쓰면 () 안에 있는 것만 나오는데, !가 있어서 stopwords 가 없어지는 것


mystopword <- data.frame(stopword = c('ici', 'nccu'))

ici_tokens_my <- ici_df %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stopwords('ENGLISH'),
         !word %in% mystopword$stopword) # 추가로 내가 지정한 words 사라짐

# Trump stopword
# remove what we do not need and get some imt words via gsub and show us cloud

text <- trumptweets$text
text[1:10]

# Set the text to lowercase
text <- tolower(text)
text[1:10]


# Remove urls, emojis, etc.
text <- gsub("https?://.+", "", text)
text[1:10]


#text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", text)
text <- gsub("\\d+\\w*\\d*", "", text) 

# \d is a digit (a character in the range 0-9), and + means 1 or more times. So, \d+ is 1 or more digits. 
# ^[\w*]$ will match a string consisting of a single character, where that character is alphanumeric (letters, numbers) an underscore ( _ ) or an asterisk ( * ). 
# Details: The " \w " means "any word character" which usually means alphanumeric (letters, numbers, regardless of case) plus underscore (_)
#  \d matches any decimal digit. The signification of a "decimal digit" depends on the options of the regex: Without RegexOptions.
text <- gsub("[^\x01-\x7F]", "", text) # this is for emoji 
text[1:10]


# Remove references to other twitter users and hash tags
text <- gsub("@\\w+", "", text)
text[1:10]

# text <- gsub("#\\w+", "", text) 
# if I want to use analyze the hash tage, dont use this


# Remove number and punctuation
text <- gsub("[[:digit:]]", "", text)
text <- gsub("[[:punct:]]", " ", text)
text[1:10]


# Remove spaces and newlines
text <- gsub("amp;", "", text)
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)
text[1:10]

# Extra: Remove an alphabet
text <- gsub("\\W[a-zA-Z]\\W", "", text) 
text[1:10]

trumptweets$new_text <- text

trump_temp <- trumptweets %>%
  filter(is_retweet == FALSE) %>% # show only false one!
  select(status_id, new_text) # 모든 column 중에서 이것만 보여줘~

# 4. Tokenize trup_tweet with stop word
trump_tokens <- trump_temp %>%
  unnest_tokens(word, new_text) %>%
  filter(!word %in% stopwords('ENGLISH'))

# 5. Count word frequency
trump_frq <- trump_tokens %>%
  count(word, sort = TRUE)


# identify single letter such as s, t, u ...
# \w (word character) matches any single letter, number or underscore (same as [a-zA-Z0-9_] ). 
# The uppercase counterpart \W (non-word-character) matches any single character that doesn't match by \w (same as [^a-zA-Z0-9_] ). 
# In regex, the uppercase meta character is always the inverse of the lowercase counterpart.
# Go back to Step 1 and add:
# text <- gsub("\\W[a-zA-Z]\\W", "", text) 
# text[1:10]
# Run Step 1-6 again

# 6. Check the result of word frequency

wordcloud2(trump_frq[1:100, ], shape = 'circle') # we don't need to bring all the frq. just some like [1:10]

# practice 1- Create a Biden's Tweets' word cloud.

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
biden_tweet <- read.csv('JoeBidenTweets.csv')

text <- biden_tweet$tweet

text[1:10]

# Set the text to lowercase
text <- tolower(text)

text[1:10]

# Remove urls, emojis, etc.

text <- gsub("https?://.+", "", text)

text[1:10]

#text <- gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", text)
text <- gsub("\\d+\\w*\\d*", "", text)
text <- gsub("[^\x01-\x7F]", "", text)

text[1:10]

# Remove references to other twitter users and hash tags
text <- gsub("@\\w+", "", text)
#text <- gsub("#\\w+", "", text)

text[1:10]

# Remove number and punctuations

text <- gsub("[[:digit:]]", "", text)
text <- gsub("[[:punct:]]", " ", text)

text[1:10]

# Remove spaces and newlines
text <- gsub("amp;", "", text)
text <- gsub("\n", " ", text)
text <- gsub("^\\s+", "", text)
text <- gsub("\\s+$", "", text)
text <- gsub("[ |\t]+", " ", text)

text[1:10]

# Extra: Remove an alphabet
text <- gsub("\\W[a-zA-Z]\\W", "", text) 
text[1:10]

biden_tweet$new_text <- text

biden_temp <- biden_tweet %>%
  select(id, new_text)

biden_tokens <- biden_tweet %>%
  unnest_tokens(word, new_text) %>%
  filter(!word %in% stopwords("English"))

biden_frq <- biden_tokens %>%
  count(word, sort = TRUE) 


wordcloud2(trump_frq[1:100,], shape = "circle")


# 0428 widyr

library(tm)
library(lexicon)
library(textclean)
library(wordcloud2)
library(tidytext)
library(widyr)

ici_tokens_tm$line <- as.character(ici_tokens_tm$line)

# pairwise_cor
# Find correlations of pairs of items in a column, 
# based on a "feature" column that links them together
# https://rdrr.io/cran/widyr/man/pairwise_cor.html

# To calculate terms’ occurrence number in documents
# The result tells that when 'ici' is mentioned, 
# the text would very much care about whether it's 'courses' or 'love'
ici_cors <- ici_tokens_tm %>%
  group_by(word) %>%
  pairwise_cor(word, line, sort = TRUE)
 
# when you have tokenized tables, it's easy to calculate word association.
trump_cors <- trump_tokens %>%
  group_by(word) %>%
  pairwise_cor(word, status_id, sort = TRUE)

# Select a word you want to analyze
trump_china <- trump_cors %>%
  filter(item1 == 'china') # china is a criteria 



# Sentiment analysis ********IMP
# Load a sentiment dictionary
library(textdata)
afinn <- get_sentiments('afinn')

# Merge into a tokenized table
ici_senti <- ici_tokens_tm %>%
  inner_join(afinn, by = 'word') # tokens_tm 에 afinn 이라는 친구를 넣는데 중첩만 넣는다.



trump_senti <- trump_frq %>%
  inner_join(afinn, by = 'word')

# Create a plot to show Trump's tweet's changes in sentiment by month.
trump_sent_all <- trump_tokens %>%
  left_join(trumptweets[,c(1,2)], by = 'status_id')

library(zoo)

trump_sent_all <- trump_sent_all %>%
  mutate(date = as.POSIXct(created_at),
         month = as.yearmon(date))

trump_sent_all <- trump_sent_all %>%
  inner_join(afinn, by = 'word') # word 기준, afinn을 inner join 해라. sent_all 에 있는 단어들만 !

trump_sent_ym <- trump_sent_all %>%
  group_by(month) %>%
  summarise(sentiment = mean(value)) # mean <-> sum 둘다 가능

ggplot(trump_sent_ym, aes(month, sentiment)) + geom_line()
