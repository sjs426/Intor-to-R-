#Class: Week 16
#Course: Big Data and Social Analysis
#Semester: Spring 2021
#Lesson: API and Leaflet
#Instructor: Chung-pei Pien
#Organization: ICI, NCCU

### Student Information --------

#Chinese Name: 辛鐘成
#English First Name: Jongsung Shin
#UID: 110ZU1038
#E-mail: sjongsung97@gmail.com

### Questions --------

#Since Ukraine-Russia war, global fact-check institutions have collaborated to collect misinformation of Ukraine-Russia war all over the world.
#ukraine_disinformation.xlsx is the confidential data I received from a member of global fact-check institutions.
#Although I delete few sensitive columns, Please do not send it to other people without my permission.

#We will use the World Bank data and misinformation in Ukraine-Russia war to plot a world leaflet map.

library(ggplot2)
library(readr)
library(stringr)
library(ggspatial)
library(dplyr)
library(tidyr)
library(spdep)
library(rgdal)
library(GISTools)
library(raster)
library(maptools)
library(leaflet)
library(htmlwidgets)
library(WDI)

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

#Q1: Please load the world shapefile "ne_110m_admin_0_countries.shp". (2 Points)
#If you have a program to load this file, you can try to update your R to 4.2, or you can find another world shapefiles on the Internet.

map <- shapefile("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
plot(map)


#Q2: Please use WDI to collect all countries' fixed broadband subscriptions in 2020. (5 Points)

#Bonus points: Because Taiwan is not a member of the World Bank, the data we collect doesn't involve Taiwan. 
#If you can find Taiwan's fixed broadband subscriptions in 2020 and add the data into the table, you can get 5 bonus points.

# I can find "fixed broadband subscriptions" indicator on the website or code below
ws_FBS <- WDIsearch("fixed broadband subscriptions")
ws_FBS # IT.NET.BBND is the indicator

# Find out data
FBS <- WDI(indicator = 'IT.NET.BBND', start = 2020, end = 2020)
colnames(FBS)

# add Taiwan FBS
library(readxl)
FBS_TW <- read_xlsx("FixedBroadbandSubscriptions_2000-2020.xlsx")
colnames(FBS_TW) # I found out that the value starts from number so I have to change the name
colnames(FBS_TW)[63] <- "IT.NET.BBND" 
unique(FBS_TW$Country) # Get Taiwan's name 

FBS_TW <- subset(FBS_TW, 
                 Country == 'Taiwan, Province of China', # I did not put two == so it did not work. I have to write full name
                 select=c(IT.NET.BBND)) 

# Taiwan's FBS = 5952411

# Create iso2c, Country, IT.NET.BBND, year vectors
iso2c <- c("TW")
country <- c("Taiwan")
IT.NET.BBND <- c(5952411)    
year <- c("2020")    

# Create Data Frame using above vectors    
FBS_TW <- data.frame(iso2c, country, IT.NET.BBND, year) 

# rbind since the colnames are the same. If one single alphabet is different, it occurs the error
FBS <- rbind(FBS, FBS_TW)


#Q3: Please read ukraine_disinformation.xlsx and calculate all countries' misinformation number. (5 Points)

# https://www.geeksforgeeks.org/count-number-of-rows-within-each-group-in-r-dataframe/
UDinfo <- read_xlsx("ukraine_disinformation.xlsx")

# group by iso2c
W_Dinfo <- UDinfo %>%
  group_by(iso2c) %>%
  summarise(no_minfo = n())

# Another way using tally()
# W_Dinfo <- UDinfo %>%
#   group_by(iso2c) %>%
#   tally()


#Q4: Please merge fixed broadband subscriptions and Ukraine-Russia misinformation data into the world shapefile. (5 Points)

# merge two data frames by ID and Country
FBS_URINFO <- merge(FBS, W_Dinfo, by=c("iso2c"))


#map@data$iso2c <- FBS_URINFO$iso2c

# change the name ISO_A2_EH to iso2c
map@data$ISO_A2_EH 
colnames(map@data) 
colnames(map@data)[46] <- "iso2c"


# We can't use dplyr's left_join in shapefile
map <- merge(map, FBS_URINFO, by = c("iso2c"))

# Change NA to 0
map$no_minfo[is.na(map$no_minfo)] <- 0
map$no_minfo


#Q5: We attempt to put markers on the countries which have Ukraine-Russia misinformation.
#Please calculate the center(mean) of the countries which have Ukraine-Russia misinformation. (5 Points)

# use fortify function from ggplot2, which change the map object into a data frame
map_fort <- fortify(map, region = "iso2c")

distcenters <- map_fort %>%
  group_by(id) %>%
  summarise(clat = mean(lat), clong = mean(long))
# we want to add a marker on this point, which shows the numbers of misinformation.

distcenters = distcenters[-1,] #Remove first line of a dataframe. since it shows -99

distcenters <- left_join(distcenters, map@data, by = c("id" = "iso2c"))

boxplot(map@data$no_minfo) 

bins <- c(20, 40, 60, 80, 100, Inf)
pal <- colorBin("YlOrRd", domain = map@data$no_minfo, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br>%d Misinformation",
  map@data$iso2c, round(map@data$no_minfo)
) %>% lapply(htmltools::HTML)

map@data$no_minfo


lf_m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -100.23, lat = 30.97, zoom = 6) %>%
  addPolygons(data = map,
              fillColor = ~pal(no_minfo),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))
lf_m
# it only shows the map without marks

# Too many marks so I narrowed down to 10
top10_m <- distcenters %>%
  top_n(10, no_minfo)

# I want to put marks too
misinfotop.lf <- lf_m %>%
  addTiles() %>% # Add default OpenStreetMap map tiles
  addMarkers(data = top10_m, ~clong, ~clat, popup = ~as.character(id)) %>%
  addLegend(data = map, pal = pal, values = ~no_minfo, opacity = 0.7, title = NULL, position = "bottomright")

misinfotop.lf



#Q6: Please create a leaflet map to show all countries' fixed broadband subscriptions. (5 Points)

bins <- c(200000, 400000, 600000, 8000000, 10000000, Inf)
pal <- colorBin("YlOrRd", domain = map$IT.NET.BBND, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br>%d all countries' fixed broadband subscriptions",
  map$iso2c, round(map$IT.NET.BBND)
) %>% lapply(htmltools::HTML)


# Change NA to 0 
map$IT.NET.BBND[is.na(map$IT.NET.BBND)] <- 0
map$IT.NET.BBND



lf_FBS <- leaflet() %>%
  addTiles() %>%
  setView(lng = -100.23, lat = 30.97, zoom = 6) %>%
  addPolygons(data = map,
              fillColor = ~pal(map$IT.NET.BBND),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))
lf_FBS

# marker => no need
# FBS.lf <- lf_FBS %>% 
#   addTiles() %>% # Add default OpenStreetMap map tiles
#   addMarkers(data = distcenters, ~clong, ~clat, popup = ~as.character(id)) %>%
#   addLegend(data = map, pal = pal, values = ~IT.NET.BBND, opacity = 0.7, title = NULL, position = "bottomright")
# 
# FBS.lf


#Q7: Please add markers on the countries which have Ukraine-Russia misinformation.
#The markers will show the number of Ukraine-Russia misinformation. (3 Points)
# you need to first filter out which country have misinformation.(not top ten).
# Then use this data to add a marker on the map.By using popup = ~as.character(no_minfo)
colnames(distcenters)

# remove 0 
M_FBS <- distcenters %>%
  filter(no_minfo > 0) 

M_FBS$no_minfo

M_FBS <- lf_FBS %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  addMarkers(data = M_FBS, ~clong, ~clat, popup = ~as.character(no_minfo)) %>%
  addLegend(data = map, pal = pal, values = ~IT.NET.BBND, opacity = 0.7, title = NULL, position = "bottomright")
M_FBS
