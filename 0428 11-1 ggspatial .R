library(plyr)
library(readr) # for parse_number
library(stringr) 
library(ggspatial)
library(dplyr)
library(tidyr)
library(ggplot2)
library(spdep)
library(GISTools)
library(raster)
library(leaflet)
library(htmlwidgets)
library(maptools)
library(rgdal)

# Leaflet 따로 공부 하기 
# https://rstudio.github.io/leaflet/

# You create a Leaflet map with these basic steps:
#   
#   Create a map widget by calling leaflet().
# Add layers (i.e., features) to the map by using layer functions (e.g. addTiles, addMarkers, addPolygons) to modify the map widget.
# Repeat step 2 as desired.
# Print the map widget to display it

# install.packages("leaflet")
# to install the development version from Github, run
# devtools::install_github("rstudio/leaflet")
library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

# 자세한 거는 저 웹사이트 참고

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

# GIS
# A geographic information system (GIS) is 
# a computer system for capturing, storing, checking, and displaying data 
# related to positions on Earth’s surface (National Geographic n. d.).

# We will use the data of US soybean production by county to show how to produce GIS plots.

# You can’t produce geospatial data by yourself. 
# However, many websites provide geospatial data. 
# In the US, you can try US Census Bureau website.

# In Taiwan, you can download diverse levels geospatial data through SEGIS website
# https://segis.moi.gov.tw/STAT/Web/Portal/STAT_PortalHome.aspx
# When you click a data you want to download, SEGIS shows a window with SHP icon.

# Leaflet package is a powerful tool to create an interactive GIS map. 
# Leaflet also uses shapefile to create maps. Go to Leaflet for R website to get more information: 
# https://rstudio.github.io/leaflet/

map <- shapefile("cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

# You can use plot() to see the shapefile plot.
plot(map)

us_data <- map@data

# We can merge our data into map's data.
soybean <- read.csv('soybean.csv')

# parse_number let it into real number
soybean$Value <- parse_number(soybean$Value)

#why not soybean$Value <- as.numeric(soybean$Value)?
soybean <- rename(soybean, value = Value)

# if number with 0, it is character. so we need to change it to number

soybean$State.ANSI <- as.character(soybean$State.ANSI)
soybean$County.ANSI <- as.character(soybean$County.ANSI)

# Add zero before the state and county codes
# some number is if 2 digits add one 0, if 3, add two 0
soybean$GEOID <- paste0(str_pad(soybean$State.ANSI, 2, pad = "0"), str_pad(soybean$County.ANSI, 3, pad = "0"))


# We can't use dplyr's left_join in shapefile
# group by GEOID and
# State, County, Value will be listed on the map$data  
map <- merge(map, soybean[, c(6, 10, 20, 22)], by = c("GEOID"))

# Use ggplot to plot
# Here is the basic codes
ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon()


# Cut Kentucky's shapefile
# we only see kentucky since 21 is kentucky's id
map <- map[map$STATEFP == '21',]

ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", size = 0.1)

View(map@data)

map$value # there are NA 

# we need to pus na as 0
map$value[is.na(map$value) == TRUE] <- 0 # I put capital V so it got error.

map$value # check if it becomes 0

# below is wrong code that we can't execute 
# ggplot(map, aes(x = long, y = lat, group = group, fill = value)) +
#  geom_polygon()

# we need to use another function like below
ggplot() +
  annotation_spatial(map) +
  layer_spatial(map, aes(fill = value))

# more Value, it should be more darker. but above, it's opposite
ggplot() +
  annotation_spatial(map) +
  layer_spatial(map, aes(fill = value)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")
