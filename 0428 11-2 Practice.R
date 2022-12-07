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

setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

# Illinois

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
# State, County, Value, GEOID
map <- merge(map, soybean[, c(6, 10, 20, 22)], by = c("GEOID"))

# Use ggplot to plot
# Here is the basic codes
ggplot(map, aes(x = long, y = lat, group = group)) +
  geom_polygon()


# Cut Kentucky's shapefile
# we only see kentucky since 21 is kentucky's id
map <- map[map$STATEFP == '21',]

#leaflet
bins <- c(0, 250000, 750000, 2000000, 4000000, 8000000, Inf)
pal <- colorBin("YlOrRd", domain = map$value, bins = bins)


labels <- sprintf(
  "<strong>%s</strong><br>%d Bushels",
  map$NAME, round(map$value)
) %>% lapply(htmltools::HTML)

wheat.lf <- leaflet() %>%
  addTiles() %>%
  setView(lng = -100.23, lat = 40.97, zoom = 6) %>%
  addPolygons(data = map,
              fillColor = ~pal(Value), # value or Value 
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

wheat.lf <- lf %>% addMarkers(data = distcenters, ~clong, ~clat, popup = ~as.character(id)) %>%
  addLegend(data = map, pal = pal, values = ~Value, opacity = 0.7, title = NULL, #value or Value
            position = "bottomright")

wheat.lf
# saveWidget(wheat.lf, file="wheat.html")
