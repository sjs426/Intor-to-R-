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

#Tokyo announced COVID-19 state of emergency on 2020-04-07. Did this policy affect Japanese's mobility?

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

#Read Apple Mobility Tread dataset
a_mobility <- read_csv("applemobilitytrends-2021-05-21.csv")

#Select Tokyo's data
tokyo <- a_mobility %>%
  filter(region == "Tokyo")


#Data manipulate: Move columns to row
tokyo <- tokyo %>%
  gather("date", "trend", 7:501)

#Data manipulate: Move row to columns

tokyo <- tokyo[, c(2, 3, 7, 8)]

tokyo$date <- as.POSIXct(tokyo$date)

# spread into 3 transport type with each trend 
tokyo <- tokyo %>% 
  spread(transportation_type, trend)


#Create a time series object

tokyo_zoo <- zoo(tokyo[, c(3:5)], order.by = tokyo$date) # 3~5 is driving, transit, walking

plot(tokyo_zoo) # plot the data. 3 clear time series chart

# 2020-04-07 -> announced date
#Cut a period you want: Before and after the state of emergency 1 week

tokyo_zoo_1w <- window(tokyo_zoo, start = "2021-3-31", end = "2021-4-14")

plot(tokyo_zoo_1w)


#Cut a period you want: Before and after the state of emergency 2 week

tokyo_zoo_2w <- window(tokyo_zoo, start = "2021-3-24", end = "2021-4-21")

plot(tokyo_zoo_2w)


#Use an ARIMA model to examine the state of emergency's impact

View(tokyo_zoo_2w)

tokyo_zoo_2w$ld <- 0

tokyo_zoo_2w$ld[15:21] <- 1

a1 <- auto.arima(as.numeric(tokyo_zoo_2w[, 1]))
summary(a1)

a2 <- Arima(as.numeric(tokyo_zoo_2w[, 1]), order = c(0, 1, 0), xreg = as.numeric(tokyo_zoo_2w[, 4]))
summary(a2)

lrtest(a1, a2)
# The policy does not have any impact since p_value is over 0.05 (0.7622)


#Use a change point detection method to examine the state of emergency's impact


mvalue <- cpt.mean(as.numeric(tokyo_zoo_2w[, 1]), method= "PELT") #Pruned Exact Linear Time

cpts(mvalue)

plot(mvalue)


mvalue <- cpt.mean(as.numeric(tokyo_zoo_2w[, 1]), method= "BinSeg", Q = 1) #Binary Segmentation

cpts(mvalue)

plot(mvalue)

#Loop to show 3 kinds of mobility's results: plots and means 1 week before and after the lockdown

tokyo_mean <- data.frame()
# tokyo_mean <- list() # both okay

for (i in 1:3) {
  
  mvalue <- cpt.mean(as.numeric(tokyo_zoo_1w[, i]), method= "BinSeg", Q = 1) #Binary Segmentation
  
  cp_order <- cpts(mvalue)
  
  plot(mvalue)
  
  city <- "tokyo"
  movemethod <- colnames(tokyo_zoo_1w)[i]
  before <- mean(tokyo_zoo_1w[1:cp_order, i])
  after <- mean(tokyo_zoo_1w[(cp_order+1):14, i])
  
  t_result <- t.test(tokyo_zoo_1w[1:7, i], tokyo_zoo_1w[8:14, i])
  
  t_star <- case_when(t_result$p.value <= 0.05 ~ "*",
                      TRUE ~ "0")
  # Less than 0.05 -> They are significant(차이가 크다) / More than 0.05 -> they are not different (차이가 없다)
  
  temp_mean <- data.frame(city = city, move_method = movemethod, before = before, after = after, sign = t_star)
  
  tokyo_mean <- rbind(tokyo_mean, temp_mean)
  # difference btwn them is not significant, which means they do not have a big difference 
  
}
# t_result$p.value -> 0.1787 > 0.05
# it's not significant 