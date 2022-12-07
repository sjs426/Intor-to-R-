# Regular expression
# which is an encoded text system for matching sets of strings
# In general, we use the skills of regular expressions 
# to find patterns of huge set of text dataset to deal with them.

library(dplyr)
library(ggplot2)
library(readxl)

re1 <- data.frame(id = 1:12,
                  birth = c("1978Y11M", "1968Y1M", "1828Y10M", "1967Y5M", "1717Y4M", 
                            "1948Y12M", "1952Y06M", "1828Y10M", "1927Y3M", "1854Y6M",
                            "287Y6M", "19Y10M"))

# regexpr: find starting position and length of first match
yp <- regexpr('Y', re1$birth) # when Y is appeared first on re1$birth? => from 5th and few are 4th and 3rd
yp 
# First info is the yp that indicates when it starts. 
# Second info is how many it has. It says "match.length"
 
nchar(re1$birth) # count the total character of re1$birth
# ex) 1978Y11M => total 8 characters
# 8 7 8 7 ... 

re1$year <- substr(re1$birth, 1, yp-1)
re1$month <- substr(re1$birth, yp + 1, nchar(re1$birth) - 1)
re1$month <- as.numeric(re1$month)

re2 <- data.frame(id = 1:13,
                  birth = c("1978Y11M", "1968Y1M", "1828Y10M", "1967Y5M", "1717Y4M", 
                            "1948Y12M", "1952Y06M", "1828Y10M", "1927Y3M", "1854Y6M",
                            "287Y6M", "19Y10M", "1921y12M"))

yp <- regexpr('Y', re2$birth) # last one is -1 since its y is lower cap
yp 

re2$year <- substr(re2$birth, 1, yp-1) # substr means cut it out from 1 to yp-1 and assign it with a new var
re2$month <- substr(re2$birth, yp + 1, nchar(re2$birth) -1) # nchar is the whole number of character number

# 2 opinions to fix this problems ( some is Y and some is y )
# 1. Tell R the pattern is Y or y
yp <- regexpr('Y|y', re2$birth) # | means or 
yp

# 2. Replace Y to y
# gsub(pattern, replace, x)
gsub('y', 'Y', re2$birth) # change low cap y to capital Y on re2$birth
yp <- regexpr('Y', re2$birth) # show me Y on re2$birth



re3 <- data.frame(nameid = c("Mary001", "Ben1029", "Billy6587", "John21000", "Jane410", 
                             "Max2946", "Catherine0358", "Eva9863", "Adam212", "Tracy1215"),
                  birth = c("1978Y11M", "1968Y1M", "1828Y10M", "1967Y5M", "1717Y4M", 
                            "1948Y12M", "1952Y06M", "1828Y10M", "1927Y3M", "1854Y6M"))

# Name and id are mixed up!
# You don't have a specific letter like (Y) to refer
# to match one of several characters in a specified set,
# we can enclose the characters of concern with square brackets []

# Quantifier => how many times I want to filter that?
# the preceding item will be...
# ? => optional and will be matched at most once
# * => Matched at least 0 or more times 
# + => Matched at least 1 or more times
# {n} => matched exactly n times
# {n,} => matched n or more times
# {n,m} => matched at least n times, but no more than m times

np_test <- regexpr("[a-z]+", re3$nameid) # find where alphabet [] starts
np_test # np_test stats from 2 since the first character is capital and from the second, it is low cap

np_test <- regexpr('[a-z]*', re3$nameid) # HERE IS A PROBLEM, WHY 'match.length' APPEARS AS 0, which is not same as using +
np_test #  some people have correct answers, so maybe my pc problem. Do not use then

np_test2 <- regexpr('[a-z0-9]+', re3$nameid) # find where alphabet and number [] starts
np_test2 # 6 6 8 8 6 ...

np_test3 <- regexpr('[0-9]+', re3$nameid) # when does it start from 0~9?
np_test3 # 
np_test3[1] # start from 5th, it starts number and it has the total 3 numbers

np <- regexpr('[A-Za-z]*', re3$nameid) # find where alphabet [] starts
np # In the result,  1 1 1 1 means where the alphabet starts
attr(np, 'match.length') # show me how many alphabet(A~Z + a~z together) exists in a word mixed with numbers

re3$name <- substr(re3$nameid, np, attr(np, 'match.length')) # np = 1 

re3$id <- substr(re3$nameid, attr(np, 'match.length') +1, nchar(re3$nameid))

#Practice 1

ex_re3 <- data.frame(nameid = c("001Mary", "1029Ben", "6587Billy", "21000John", "410Jane", 
                                "2946Max", "0358Catherine", "9863Eva", "212Adam", "1215Tracy"),
                     birth = c("1978Y11M", "1968Y1M", "1828Y10M", "1967Y5M", "1717Y4M", 
                               "1948Y12M", "1952Y06M", "1828Y10M", "1927Y3M", "1854Y6M"))

# Run ex_re3 df codes to separate name and id 

name_ex_re3 <- regexpr('[A-Za-z]+', ex_re3$nameid) # find where alphabet [] starts on ex_re3$nameid 
name_ex_re3 # In the result, 4 5 5 6... means where the alphabet starts
attr(name_ex_re3, 'match.length') # show me how many alphabet co-exists in a word mixed with numbers

ex_re3$name <- substr(ex_re3$nameid, name_ex_re3, nchar(re3$nameid)) # name_ex_re3 = where the arphabet starts  

ex_re3$id <- substr(ex_re3$nameid, 1, name_ex_re3 - 1)


re4 <- data.frame(nameid = c("MaRy001", "Ben1029", "billy6587", "JoHn21000", "Jane410", 
                             "max2946", "Catherine0358", "Eva9863", "aDAm212", "Tracy1215"),
                  birth = c("1978Y11M", "1968Y1M", "1828Y10M", "1967Y5M", "1717Y4M", 
                            "1948Y12M", "1952Y06M", "1828Y10M", "1927Y3M", "1854Y6M"))

# First letter: make it upper cap 
# other letters: make it lower cap
# USE
# 1. toupper() for the first letter
# 2. tolower() for the other letters
# 3. paste() them together

np <- regexpr('[A-Za-z]*', re4$nameid) # find where alphabet [] starts
np # In the result,  1 1 1 1 means where the alphabet starts

re4$name <- substr(re4$nameid, np, attr(np, 'match.length')) # np = 1 

re4$id <- substr(re4$nameid, attr(np, 'match.length') +1, nchar(re4$nameid))
# by here, we separate the name and id

# we are going to fix the name as the first letter for upper cap and from the second to the last for lower cap
re4$name_fix <- paste0(toupper(substr(re4$name, 1, 1)),
                       tolower(substr(re4$name, 2, nchar(re4$name))))
View(re4)

re5 <- data.frame(id = 1:12,
                  birth = c("1978[11]", "1968[1]", "1828[10]", "1967[5]", "1717[4]", 
                            "1948[12]", "1952[06]", "1828[10]", "1927[3]", "1854[6]",
                            "287[6]", "19[10]"))

# Specific marks
# Using [ to identify location
# we need to use escape string character
# http://math.oxford.emory.edu/site/math117/stringsInR/

yp <- regexpr('[', re5$birth) # error occurred 

yp <- regexpr('\\[', re5$birth) # use \\ (원화 표시) for escape string character 
yp

re5$year <- substr(re5$birth, 1, yp - 1)

re5$month <- substr(re5$birth, yp + 1, nchar(re5$birth) -1)

# by here, we separate the year and month

# we are going to fix the name as the first letter for upper cap and from the second to the last for lower cap

nm <- regexpr('[0]+', re5$month) # find where [] starts
nm # only 7th has 1 so change it to 6 

re5$month[7] <- 6


# BUT I AM NOT SURE HOW I CAN DO THIS WITH A NUMBER OF VARIABLES MAYBE IF FUNCTION?
# = > Because the data is stored in characters,"06" is also character.
# The most easy way to remove 0 in the 7th row is to charge the data type by using as.numeric(ex_re5$month).
# Then this variable might be seen as numbers, zero at the beginning will be removed automatically.
# Let's try below

#Practice 2

ex_re5 <- data.frame(id = 1:12,
                     birth = c("1978(11)", "1968(1)", "1828(10)", "1967(5)", "1717(4)", 
                               "1948(12)", "1952(06)", "1828(10)", "1927(3)", "1854(6)",
                               "287(6)", "19(10)"))


# PRACICE BACK HOME

yp <- regexpr('\\(', ex_re5$birth)
yp

ex_re5$year <- substr(ex_re5$birth, 1, yp - 1) # yp = 1 
ex_re5$month <- substr(ex_re5$birth, yp + 1, nchar(ex_re5$birth) - 1)

ex_re5$month <- as.numeric(ex_re5$month) # all the num in month were strings. I changed it to number

# 06 became 6 

# Ukraine Conflict Twitter
# due to the emoji, the file looks terrible

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
ua_example <- read_xlsx('UkraineTweets_example.xlsx')

# Apply regular expression to get all hashtages in text column
ua_example$text[3] 

ht <- regexpr('#[a-zA-Z0-9]+', ua_example$text[3])
ht # #UkraineWar starts from 188th and has 11 characters

substr(ua_example$text[3], ht, ht + attr(ht, 'match.length') - 1) # we need to take it out ',' so -1
# [1] "#UkraineWar" 
# if no -1 => [1] #UkraineWar,

# regexpr() only gives us the first pattern's location and length 
# gregexpr() can meet our requirement as it finds starting position and length of all matches
ht <- gregexpr('#[a-zA-Z0-9]+', ua_example$text[3])
ht # # is located 118th and 269th and it consists of 11 and 12 characters

#[[]] means whole result

ht[1] # get the whole data
ht[1][1] # get the whole data

ht[[1]] # get the whole data
ht[[1]][1] # get the first one which is 188
ht[[1]][2] # get the second one which is 269
ht[[1]][3] # Nothing. NA

attr(ht[[1]],"match.length")[1] # it should be exactly the same so use ". it will show 11

attr(ht[1],"match.length")[1] # It will show NULL

# Here, we would like to get the #HASHTAGE from the begin to the end
# ht[[1]][1] = 188, attr(ht[[1]],"match.length")[1] = 11
# If we do not type -1, we will have ","
# 188, 188 + 11 - 1
substr(ua_example$text[3], ht[[1]][1], ht[[1]][1] + attr(ht[[1]],"match.length")[1] - 1)
#UkraineWar

substr(ua_example$text[3], ht[[1]][2], ht[[1]][2] + attr(ht[[1]],"match.length")[2] - 1)
#JesusChrist

ht <- gregexpr("#[a-zA-Z0-9]+", ua_example$text)
ht

ua_example$tweetid <- as.character(ua_example$tweetid)
ht_df <- data.frame()
length(ht) # 10

for(i in 1:length(ht)) {
  ht_temp <- ht[[i]] # the whole data for order starting from #
  df_temp <- data.frame()
  
  for(x in 1:length(ht_temp)){ # I can't use i here. Double loop -> i , j 
    ht_content <- substr(ua_example$text[i], ht_temp[x], 
                         ht_temp[x] + attr(ht_temp, "match.length")[x] - 1)
    # This is like substr from begin to the end to get only hash tag
    temp <- data.frame(tweetid = ua_example$tweetid[i], hashtag = ht_content)
    df_temp <- rbind(df_temp, temp)
  }
  ht_df <- rbind(ht_df,df_temp)
}
ht_df$tweetid <- as.character(ht_df$tweetid)


attr(ht_temp, "match.length")[[x]] # 7
attr(ht_temp, "match.length")[x] # 7 we better use single bracket 

# Apply regular expression toe get all hyperlinks in text  column 


link <- gregexpr("https\\:[\\/a-zA-Z0-9.]+", ua_example$text)  # use \\ (원화 표시) for escape string character 
link
substr(ua_example$text[3], link[[1]][1], link[[1]][1] + attr(link[[1]],"match.length")[1] - 1)


