library(plyr)
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

map <- shapefile("cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

# You can use plot() to see the shapefile plot.
plot(map)

# We can merge our data into map's data.
wheat <- read.csv("wheat winter.csv")

# parse_number let it into numeric
# here Value is capital not like soybean (value)
wheat$Value <- parse_number(wheat$Value)

wheat <- rename(wheat, value = Value) # change to lower cap 

# if number with 0, it is character. so we need to change it to number
wheat$State.ANSI <- as.character(wheat$State.ANSI)
wheat$County.ANSI <- as.character(wheat$County.ANSI)

# Add zero before the state and county codes
# some number is if 2 digits add one 0, if 3, add two 0
wheat$GEOID <- paste0(str_pad(wheat$State.ANSI, 2, pad = "0"), str_pad(wheat$County.ANSI, 3, pad = "0"))

colnames(wheat)
# State, County, Value, GEOID
# We can't use dplyr's left_join in shapefile
map <- merge(map, wheat[,c(6, 10, 20, 22)], by = c("GEOID"))

# Cut Illinois's shapefile
# how can I get the STATEFP for Illionoise?
# search STATEFP
map <- map[map$STATEFP == '17',]



# 11-1 file 이랑 뭔가 에러가 뜨는듯. 그냥 다 전체 run 하면 됨.

map$value # NA 

# we need to change NA into 0
map$value[is.na(map$value) == TRUE] <- 0

map$value


ggplot() +
  annotation_spatial(map) +
  layer_spatial(map, aes(fill = value)) +
  scale_fill_gradient(low = "#ffffcc", high = "#ff4444", 
                      space = "Lab", na.value = "grey50",
                      guide = "colourbar")

# "Error: Discrete value supplied to continuous scale" on categorical y-axis
# https://stackoverflow.com/questions/29278153/plotting-with-ggplot2-error-discrete-value-supplied-to-continuous-scale-on-c