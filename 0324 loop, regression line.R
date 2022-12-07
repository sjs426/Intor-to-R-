#LOOP 

for (i in 1:10) {
  print(i)
}

for (go in 1:29) {
  print(go)
}

a <- 3
for (i in 1:10) {
  a <- a + i
  print(a)
}

ici <- c('nccu', 'taiwan', 'best', 'lovely')
# there is no 5th. so last one is NA
for (i in 1:5) {
  print(ici[i])
}

# use length(ici) to know the maximum length of ici, which is 4
for (i in 1:length(ici)) {
  print(ici[i])
}

# package
library(dplyr)
library(readxl)
library(ggplot2)


ntc_ref_2021 <- read_excel("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE/ntc_ref_2021.xlsx")
colnames(ntc_ref_2021)[3:8]

# change a name of column
colnames(ntc_ref_2021)[3] <- 'ref_ref_yea_3'
colnames(ntc_ref_2021)[3]


# easy way to add '_2021' at the end of the name on column 3~8 of ntc_ref_2018
ntc_ref_2021 <- read_excel("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE/ntc_ref_2021.xlsx")
colnames(ntc_ref_2021)[3:8]
paste(colnames(ntc_ref_2021)[3:8], '2021', sep = '_')


# difference btwn the normal paste way and loop
ntc_ref_2021 <- read_excel("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE/ntc_ref_2021.xlsx")
i <- 3
colnames(ntc_ref_2021)[i] <- 
  paste(colnames(ntc_ref_2021)[i], i, sep = '_')

# reload the file again after we assign i=3, i=4 examples 
# if boss asked me to put a number for each column. we can use LOOP
# sep means put '_' 
ntc_ref_2021 <- read_excel("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE/ntc_ref_2021.xlsx")
for(i in 3:8){
  colnames(ntc_ref_2021)[i] <- 
   paste(colnames(ntc_ref_2021)[i], i, sep = '_')
}


# Besides csv and xlsx, many websites provide JSON files
# What special JSON is? 
# JSON can stores complicated data: Nested data.
# For example: A JSON have many lists, and every list involves a table.

# We dealt with house_115 many weeks. 
# the original file is from ProPublica Congress API: congress-115.json
# House_115 list ?? results sublist ?? members sublist ?? member sublist ?? [[1]] sublist

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")
library(jsonlite)
house_115_json <- fromJSON('congress-115.json')

ntc_ref_json <- fromJSON('17_65000.json') 
# good to use with rbind
# there are 29 lists: every list has a data frame
# Every dataframe records every district??s Case 17 reults at li level.

ntc_ref_json[[1]] # why double? because it's inside and inside
ntc_ref_1 <- ntc_ref_json[[1]] # we can read the first list's content. 126 obs of 14 variables
ntc_ref_2 <- ntc_ref_json[[2]] # 119 obs of 14 variables
ntc_ref_2021 <- rbind(ntc_ref_1, ntc_ref_2) # 245 (126+119) obs of 14 variables

# Therefore, if we can create a loop, we can rbind() all dataframes in ntc_ref_json

# check the difference. only select when i = 2
ntc_ref_2021 <- data.frame() # erase ntc_ref_2021 or create a new empty dataframe -> data.frame()
i <- 2
temp_df <- ntc_ref_json[[i]] # temp_df has 119 obs of 14 var which is same as above ntc_ref_json[[2]]
ntc_ref_2021 <- rbind(ntc_ref_2021, temp_df) # empty df + temp_df = same as temp_df = ntc_ref_2021

# use LOOP
ntc_ref_2021 <- data.frame() # remove above ntc_ref_2021 and create a new empty dataframe -> data.frame()
for(i in 1:29) {
  temp_df <- ntc_ref_json[[i]] # 5 obs of 14 variables. 
  ntc_ref_2021 <- rbind(ntc_ref_2021, temp_df) 
} # After loop, ntc_ref_2021 has 1032 obs with 14 var

# i dont know why the temp_df shows 119 and 5 above. 
# => i will be 29 as it stops 29. so the value is from ntc_ref_json[[29]]

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

ntc_2018_list <- list.files('ntc_2018')
ntc_2018_list
ntc_2018_list[1]

ntc_2018_1 <- read.csv(ntc_2018_list[1]) # error occurred - Because your files are in ntc_2018 folder.
ntc_2018_1 <- read.csv('ntc_2018/ntc_2018_list[1]') # error occurred
# since R does not read any strings in '' as an object. there is no file named ntc_2018_list[1] in ntc_2018.
# so we need to use ,

# The difference between paste() and paste0() is that the argument sep by default is ?? ?? (paste) and ???? (paste0).
# In conclusion, paste0() is faster than paste() if our objective is concatenate strings without spaces because we don??t have to  specify the argument sep.
paste('ntc_2018/', ntc_2018_list[1], sep= '') # Value of sep: ""

paste0('ntc_2018/', ntc_2018_list[1]) # Default value of sep with paste function


ntc_2018_1 <- read.csv(paste0('ntc_2018/', ntc_2018_list[1]))

ntc_2018_2 <- read.csv(paste0('ntc_2018/', ntc_2018_list[2]))

ntc_2018 <- rbind(ntc_2018_1, ntc_2018_2)

# Practice 1 - Use loop function and codes of ntc_ref_2021 to rbind all districts?? 2018 election results into ntc_2018

ntc_2018_list <- list.files('ntc_2018')
ntc_2018 <- data.frame()
for(i in 1:length(ntc_2018_list)){
  ntc_2018_df <- read.csv(paste0('ntc_2018/', ntc_2018_list[i])) # 93 obs. of 21 var
  ntc_2018 <- rbind(ntc_2018, ntc_2018_df) # combine ntc_2018_df to ntc_2018
} # total 1032 obs of 21 var


# 0331 regression line ~~~~~~~~~~



ntc_2018 <- ntc_2018 %>%
  mutate(yea_rate = rf_16_yea / rf_16_turnout)

ggplot(ntc_2018,aes(closest_plant, yea_rate)) + geom_point() # check the difference with below 

# ntc_2018 is data, aes is x and y
ntc_plot <- ggplot(ntc_2018, aes(closest_plant, yea_rate)) + 
  geom_point() + # get plot point
  geom_smooth(method = lm) +  # lm is regression. so add regression line
  ggtitle('New Taipei City') # title 
ntc_plot

# we can save a png file in the same working directory

.ggsave(ntc_plot, file = 'ntc_plot.png', width = 14, height = 10, units = 'cm')

# product all districts' plots (x=closet_plant and y = yea_rate) to check individual district's results
# could you use loop function to do this task?

# how to create single district's table and allow loop to read it?

dlist <- unique(ntc_2018$district) # data list
dlist

ddf <- ntc_2018 %>%
  filter(district == dlist[1]) # we can get the first district's whole data
View(ddf)

# produce a district's plot of closest_plant (x) and yea_rate (y) with a regression line.

tamp_plot <- ggplot(ddf, aes(closest_plant, yea_rate)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle(dlist[1])
  # . Á¦???Ø¾? ?Ûµ?
  # .ggsave(tamp_plot, file = paste0(dlist[1],'.png'), width = 14, height = 10, units = 'cm')
tamp_plot

# Using a loop to produce every district's plot of closest_plant (x) and yea_rate (y) with a regression line.

for (i in 1:29) {
  ddf <- ntc_2018 %>%
    filter(district == dlist[i]) # we can get whole district's data
  
  tamp_plot <- ggplot(ddf, aes(closest_plant, yea_rate)) + 
    geom_point() + 
    geom_smooth(method = lm) + 
    ggtitle(dlist[i])
  
  # put it in ntc_plot/ folder
  # ggsave(tamp_plot, file = paste0('ntc_plot/', dlist[i],'.png'), width = 14, height = 10, units = 'cm')
}


# but only with this plot data, We don??t know the districts?? slope values and if the values?? are significant

yea_close <- lm(yea_rate ~ closest_plant, data = ddf)

tamp_plot <- ggplot(ddf, aes(closest_plant, yea_rate)) + 
  geom_point() + 
  geom_smooth(method = lm) + 
  ggtitle(paste(dlist[1], 
                "slope:", 
                signif(yea_close$coefficients[2], 3), 
                "pvalue:", 
                signif(summary(yea_close)$coef[2, 4], 3)
  ))
tamp_plot

# Bali??s slope of closest_plant is larger than the whole city, 
# but it??s insignificant (p-value is 0.712, which is over 0.05)
# we don't need to pay much attention to Bali

# p-value 
# if it is less than 0.05 ->
# the value is in critical region and the null hypothesis is rejected. 
# and the result is likely to be significant.
# if over 0.05 ->
# the value does not fall in critical region and the null hypothesis is not rejected 
# and the result is not likely to be significant.


# ADVANCED data plotting - 1
# get the few data from summary 
for (i in 1:29) {
  ddf <- ntc_2018 %>%
    filter(district == dlist[i]) # we can get whole district's data
  
  
  yea_close <- lm(yea_rate ~ closest_plant, data = ddf)
  
  tamp_plot <- ggplot(ddf, aes(closest_plant, yea_rate)) + 
    geom_point() + 
    geom_smooth(method = lm) + 
    ggtitle(paste(dlist[i], 
                  "slope:", 
                  signif(yea_close$coefficients[2], 3), 
                  "pvalue:", 
                  signif(summaryyea_close)$coef[2, 4], 3)
                  )
            ) 
  
  # put it in ntc_plot/ folder
  # ggsave(tamp_plot, file = paste0('ntc_plot/', dlist[i],'.png'), width = 14, height = 10, units = 'cm')
}
ddf
summary(yea_close)
tamp_plot

# ADVANCED data plotting - 2
# Too many plots? only produce plots that p-value is smaller than 0.1
for (i in 1:29) {
  ddf <- ntc_2018 %>%
    filter(district == dlist[i])
  yea_close <- lm(yea_rate ~ closest_plant, data = ddf)
  pv <- signif(summary(yea_close)$coef[2, 4], 3)
    if (pv <= 0.1) {
    temp_plot <- ggplot(ddf, aes(closest_plant, yea_rate)) +
      geom_point() +
      geom_smooth(method = lm) +
      ggtitle(paste(dlist[i], "slope:", signif(yea_close$coefficients[2], 3), "pvalue:", pv))
  
  # put it in ntc_plot/ folder
  # ggsave(temp_plot, file = paste0("ntc_plot_filter/", dlist[i], ".png"), width =14, height = 10, units = "cm") 
    }
}
pv
temp_plot