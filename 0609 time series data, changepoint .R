library(forecast) # for time series data
library(changepoint) # for time series data

library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)
library(readr)
library(reshape2)
library(magrittr)
library(imputeTS)
library(forecast)
library(changepoint)


# Attain 1~2 weeks before and after to compare
# Collect TW 10 ~ 11 cities mobility 
# use several algorithm to compare different cities' mobility


setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

#Sys.setlocale("LC_ALL","English")

#Read Apple Mobility Tread dataset

a_mobility <- read_csv("applemobilitytrends-2021-05-21.csv")


#Select Taipei's data

tpe <- a_mobility %>%
  filter(region == "Taipei City")

#Time series packages in R request a specific dataframe structure: a time column and one or more value columns.

ts_df <- data.frame(date = c("2020-3-22", "2020-3-23", "2020-3-24", "2020-3-25"),
                    walk = c(5839, 1940, 2930, 2203),
                    sleep = c(8, 7.4, 6, 8.2))


#Data manipulate: Move columns to row

tpe <- tpe %>%
  gather("date", "trend", 7:501)


#Data manipulate: Move row to columns


tpe <- tpe[, c(2, 3, 7, 8)]

tpe$date <- as.POSIXct(tpe$date) # if I put cursor on the column, it says Character so we need to convert it

tpe <- tpe %>% 
  spread(transportation_type, trend)

#Create a time series object


tpe_zoo <- zoo(tpe[, c(3:5)], order.by = tpe$date) # 3~5 is driving, transit, walking


plot(tpe_zoo)

#Cut a period you want: 1 week before and after the lockdown


tpe_zoo_1w <- window(tpe_zoo, start = "2021-5-8", end = "2021-5-22")

plot(tpe_zoo_1w)


#Cut a period you want: 2 weeks before and after the lockdown


tpe_zoo_2w <- window(tpe_zoo, start = "2021-5-1", end = "2021-5-29")

plot(tpe_zoo_1w)


#There are a lot of models for time series analysis. You can find an appropriate model for your purposes. 
#I show you two models related to this project. The first is autoregressive integrated moving average (ARIMA) models. 
#An ARIMA model is to examine intervention impacts on your time series data.


tpe_zoo_2w$ld <- 0


tpe_zoo_2w$ld[15:21] <- 1

a1 <- auto.arima(as.numeric(tpe_zoo_2w[, 1]))
summary(a1)

a2 <- Arima(as.numeric(tpe_zoo_2w[, 1]), order = c(0, 1, 0), xreg = as.numeric(tpe_zoo_2w[, 4]))
summary(a2)

library(lmtest)

lrtest(a1, a2) # to compare a1 and a2
# If p-value is smaller than 0.05 -> difference between a1 and a2 is significant, which means the impacts are significant
# the change of driving before intervention and after is significant. ( 크다? )
# The change of driving intervention in Taipei is significant.

#The second model is change point detection methods. 
#When you use the ARIMA model, you have to know the intervention's periods. 
#However, in some cases, we just want to know a time point that indicates an abrupt and significant change in your time series data. 
#As a result, we can use the change point detection method to find a time point that may have an intervention or identify an intervention's persistent period.

#ARIMA model: 
#     You know an intervention's affecting period.

#Change point detection method: 
#     1. You want to detect an intervention time point or 
#     2. An intervention's affecting period.

#cpt is check point detector 
mvalue <- cpt.mean(as.numeric(tpe_zoo_2w[, 1]), method= "PELT") #Pruned Exact Linear Time

cpts(mvalue)

plot(mvalue)


mvalue <- cpt.mean(as.numeric(tpe_zoo_2w[, 1]), method= "BinSeg", Q = 1) #Binary Segmentation
# Q1 means you just give me only one the day, which is 14

cpts(mvalue) # 14 means before and after 14 will has a line

plot(mvalue) # 15th starting day for level 3 


#Loop to show 3 kinds of mobility's results: plots and means 1 week before and after the lockdown


tpe_mean <- data.frame()

# driving, transit, walking. that's why 1~3
for (i in 1:3) {
  
  mvalue <- cpt.mean(as.numeric(tpe_zoo_1w[, i]), method= "BinSeg", Q = 1) #Binary Segmentation
  # 1 week b4 and after lockdown
  
  cp_order <- cpts(mvalue)
  
  plot(mvalue)
  
  city <- "taipei" 
  movemethod <- colnames(tpe_zoo_1w)[i]
  before <- mean(tpe_zoo_1w[1:cp_order, i]) # calculate the mean from the first row (0508) to 7th rows (0514)
  after <- mean(tpe_zoo_1w[(cp_order + 1):14, i]) # calculate the mean after the lockdown (0515)
  
  t_result <- t.test(tpe_zoo_1w[1:7, i], tpe_zoo_1w[8:14, i]) # instead of 7, we better put cp_order to make sure
  
  t_star <- case_when(t_result$p.value <= 0.05 ~ "*", 
                      TRUE ~ "0")
  # Less than 0.05 -> They are significant(차이가 크다) / More than 0.05 -> they are not different (차이가 없다)
  
  temp_mean <- data.frame(city = city, move_method = movemethod, before = before, after = after, sign = t_star)
  
  tpe_mean <- rbind(tpe_mean, temp_mean)
  # difference btwn them is significant, which means they have a big difference 
  
}


# The formal loop for the project: 
# to create all countries' lockdown plots and results that the journalists and designers were able to read them and pick up appropriate plots and information. 
# These products allowed us to discuss the content of news story.

# I create a citylist table to record the cities and lockdown dates
# Why I create citylist table? -> data analysis structure 
# Because journalists always change their mind, you have to predict the places they may want me to provide new information.
# When they want me to add a city or country, I can produce results in seconds.

citylist <- data.frame(apple = c("Los Angeles", "New York City", "London", "Paris", "Milan", "Singapore", "Tokyo", "Seoul", "Hong Kong" , "Taipei City", "New Taipei City"),
                       google = c("United States C", "United States N", "United Kingdom", "France", "Italy", "Singapore", "Japan", "South Korea", "Hong Kong", "Taiwan", "Taiwan"),
                       lddate = as.Date(c("2020-3-16", "2020-3-22", "2020-3-23", "2020-3-17", "2020-3-8", "2020-4-7", "2020-4-7", "2020-12-23", "2021-2-3", "2021-5-15", "2021-5-15")))

# before after a week
citylist$b1w <- citylist$lddate - 7
citylist$a1w <- citylist$lddate + 7

# before after 2 weeks
citylist$b2w <- citylist$lddate - 14
citylist$a2w <- citylist$lddate + 14

# before after 2 months
citylist$a2m <- citylist$lddate + 60


a_mobility <- read_csv("applemobilitytrends-2021-05-21.csv")

city_mean <- data.frame()


# plot, csv for designers -> If they don't like the plot visualization, they will use csv file for Illustrator to make it prettier like this below
# https://theinitium.com/article/20210524-taiwan-lockdown-data/
# we have to create two folders called "plot" and "csv" before executing the loop

for (i in 1:nrow(citylist)) {
  
  city <- a_mobility %>%
    filter(region == citylist$apple[i]) %>%
    gather("date", "trend", 7:501)
  
  city <- city[, c(2, 3, 7, 8)]
  
  city$date <- as.POSIXct(city$date)
  
  city <- city %>%
    spread(transportation_type, trend)
  
  mobility_class <- ncol(city) #some cities only have two measurements of mobility
  
  city_zoo <- zoo(city[, c(3:mobility_class)], order.by = city$date)
  
  city_zoo <- na.interpolation(city_zoo) #impute NA data
  
  jpeg(paste0("plot/", paste(citylist$apple[i], time(city_zoo)[1], time(city_zoo)[nrow(city_zoo)]), ".jpg"), width = 800, height = 600)
  
  #Create a plot from 2020-1-18 to 2021-5-21
  
  plot(city_zoo[, c(1:(mobility_class - 2))], ylim=c(0,150), main = paste(citylist$apple[i], time(city_zoo)[1], time(city_zoo)[nrow(city_zoo)]))
  
  dev.off()
  
  #before and after 1 week of the lockdowns
  
  city_zoo_1w <- window(city_zoo, start = as.character(citylist$b1w[i]), end = as.character(citylist$a1w[i]))
  
  jpeg(paste0("plot/", paste(citylist$apple[i], citylist$b1w[i], citylist$a1w[i]), ".jpg"), width = 300, height = 600)
  
  plot(city_zoo_1w[, c(1:(mobility_class - 2))], ylim=c(0,150), main = paste(citylist$apple[i], "\n", citylist$b1w[i], "\n", citylist$a1w[i])) 
  
  dev.off()
  
  #mean of before and after 1 week
  
  for(c in 1:ncol(city_zoo_1w)) {
    
    temp_mean <- data.frame(city = citylist$apple[i], transit = colnames(city_zoo_1w)[c], before_1week = mean(city_zoo_1w[1:7, c]), after_1week = mean(city_zoo_1w[8:nrow(city_zoo_1w), c]))
    
    city_mean <- rbind(city_mean, temp_mean)
    
  }
  
  #create a cp dataframe to save the change point detection method's results
  
  cp <- data.frame(date = time(city_zoo_1w))
  
  for (x in 1:ncol(city_zoo_1w)) {
    
    cp[[paste0(colnames(city_zoo_1w)[x], "_cp")]] <- 0
    
  }
  
  
  #Calculate every kind of mobility's change points: Draw plots and save results
  
  for (k in 1:(mobility_class - 2)) {
    
    mvalue = cpt.mean(as.numeric(city_zoo_1w[, k]), method= "BinSeg", Q = 1) 
    
    cpts(mvalue)
    
    jpeg(paste0("plot/", paste(citylist$apple[i], citylist$b1w[i], citylist$a1w[i], colnames(city_zoo_1w)[k]), ".jpg"), width = 300, height = 600)
    
    plot(mvalue, main = paste(citylist$apple[i], "\n", citylist$b1w[i], "\n", citylist$a1w[i], "\n", colnames(city_zoo_1w)[k]))
    
    dev.off()
    
    cp[cpts(mvalue), k + 1] <- 1
    
  }
  
  #Merge change point results into the zoo dataframe
  
  cp <- zoo(cp[, -1], order.by = cp$date)
  
  city_zoo_1w <- cbind(city_zoo_1w, cp)
  
  write.csv(as.data.frame(city_zoo_1w), paste0("csv/", paste(citylist$apple[i], citylist$b1w[i], citylist$a1w[i]), ".csv"))
  
  
  #before and after 2 weeks
  
  city_zoo_2w <- window(city_zoo, start = as.character(citylist$b2w[i]), end = as.character(citylist$a2w[i]))
  
  jpeg(paste0("plot/", paste(citylist$apple[i], citylist$b2w[i], citylist$a2w[i]), ".jpg"), width = 300, height = 600)
  
  plot(city_zoo_2w[, c(1:(mobility_class - 2))], ylim=c(0,150), main = paste(citylist$apple[i], "\n", citylist$b2w[i], "\n", citylist$a2w[i])) 
  
  dev.off()
  
  cp <- data.frame(date = time(city_zoo_2w))
  
  for (x in 1:ncol(city_zoo_2w)) {
    
    cp[[paste0(colnames(city_zoo_2w)[x], "_cp")]] <- 0
    
  }
  
  for (k in 1:(mobility_class - 2)) {
    
    mvalue = cpt.mean(as.numeric(city_zoo_2w[, k]), method= "BinSeg", Q = 1) 
    
    cpts(mvalue)
    
    jpeg(paste0("plot/", paste(citylist$apple[i], citylist$b2w[i], citylist$a2w[i], colnames(city_zoo_2w)[k]), ".jpg"), width = 300, height = 600)
    
    plot(mvalue, main = paste(citylist$apple[i], "\n", citylist$b2w[i], "\n", citylist$a2w[i], "\n", colnames(city_zoo_2w)[k]))
    
    dev.off()
    
    cp[cpts(mvalue), k + 1] <- 1
    
  }
  
  cp <- zoo(cp[, -1], order.by = cp$date)
  
  city_zoo_2w <- cbind(city_zoo_2w, cp)
  
  write.csv(as.data.frame(city_zoo_2w), paste0("csv/", paste(citylist$apple[i], citylist$b2w[i], citylist$a2w[i]), ".csv"))
  
  
  #before 2 weeks and after 2 months
  
  city_zoo_2m <- window(city_zoo, start = as.character(citylist$b2w[i]), end = as.character(citylist$a2m[i]))
  
  jpeg(paste0("plot/", paste(citylist$apple[i], citylist$b2w[i], citylist$a2m[i]), ".jpg"), width = 300, height = 600)
  
  plot(city_zoo_2m[, c(1:(mobility_class - 2))], ylim=c(0,150), main = paste(citylist$apple[i], "\n", citylist$b2w[i], "\n", citylist$a2m[i])) 
  
  dev.off()
  
  cp <- data.frame(date = time(city_zoo_2m))
  
  for (x in 1:ncol(city_zoo_2m)) {
    
    cp[[paste0(colnames(city_zoo_2m)[x], "_cp")]] <- 0
    
  }
  
  for (k in 1:(mobility_class - 2)) {
    
    mvalue = cpt.mean(as.numeric(city_zoo_2m[, k]), method= "BinSeg", Q = 2) 
    
    cpts(mvalue)
    
    jpeg(paste0("plot/", paste(citylist$apple[i], citylist$b2w[i], citylist$a2m[i], colnames(city_zoo_2w)[k]), ".jpg"), width = 300, height = 600)
    
    plot(mvalue, main = paste(citylist$apple[i],  "\n", citylist$b2w[i],  "\n", citylist$a2m[i],  "\n", colnames(city_zoo_2m)[k]))
    
    dev.off()
    
    cp[cpts(mvalue), k + 1] <- 1
    
  }
  
  cp <- zoo(cp[, -1], order.by = cp$date)
  
  city_zoo_2m <- cbind(city_zoo_2m, cp)
  
  write.csv(as.data.frame(city_zoo_2m), paste0("csv/", paste(citylist$apple[i], citylist$b2w[i], citylist$a2m[i]), ".csv"))
  
  
}

city_mean$descend <- city_mean$before_1week - city_mean$after_1week


# download csv
write.csv(city_mean, "city_mean_NYC.csv")
write.csv(as.data.frame(city_zoo), "Taiwan 20200103-20210521.csv")

