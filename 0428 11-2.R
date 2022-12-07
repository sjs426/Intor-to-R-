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

map <- shapefile("cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

# You can use plot() to see the shapefile plot.
plot(map)

#leaflet

bins <- c(0, 250000, 750000, 2000000, 4000000, 8000000, Inf)
pal <- colorBin("YlOrRd", domain = map$value, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br>%d Bushels",
  map$NAME, round(map$value)
) %>% lapply(htmltools::HTML)

lf <- leaflet() %>%
  addTiles() %>%
  setView(lng = -100.23, lat = 40.97, zoom = 6) %>%
  addPolygons(data = map,
              fillColor = ~pal(value),
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

lf

#add markups


map_fort <- fortify(map, region = "GEOID")

distcenters <- map_fort %>%
  group_by(id) %>%
  summarise(clat = mean(lat), clong = mean(long))

distcenters <- left_join(distcenters, map@data, by = c("id" = "GEOID"))

bins <- c(0, 250000, 750000, 2000000, 4000000, 8000000, Inf) # change values # gradient scale
pal <- colorBin("YlOrRd", domain = map$value, bins = bins) # change values # color palette

labels <- sprintf(
  "<strong>%s</strong><br>%d Bushels",
  map$NAME, round(map$value)
) %>% lapply(htmltools::HTML)

lf <- leaflet() %>%
  addTiles() %>%
  setView(lng = -100.23, lat = 40.97, zoom = 4) %>%
  addPolygons(data = map,
              fillColor = ~pal(value),
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

soybean.lf <- lf %>% addMarkers(data = distcenters, ~clong, ~clat, popup = ~as.character(id)) %>%
  addLegend(data = map, pal = pal, values = ~value, opacity = 0.7, title = NULL,
            position = "bottomright")

soybean.lf

saveWidget(soybean.lf, file="soybean.html")
