library(stringr)
library(dplyr)
library(ggspatial)
library(spdep)
library(rgdal)
library(GISTools)
library(raster)
library(maptools)
library(leaflet)
library(htmlwidgets)
library(WDI)

# World Bank API
# search key words
# If I put, gdp also works => timesires would be great

ws_gini <- WDIsearch("gini")

# Find out real gini data
gini <- WDI(indicator = 'SI.POV.GINI', start = 1960, end = 2012)

# Specific countries' data
# Turkey and Thailand
gini_country <- WDI(indicator = 'SI.POV.GINI', country = c('TR', 'TH'), start = 1960, end = 2012)

# or

gini_country <- gini %>%
  filter(iso2c %in% c('TR', 'TH'))

# We could also make a whole list of indicators through the HTML 
# Merge US, UK, Japan, Germany's population, GDP, and GINI data from 2010-2020 in one table

UUJP_P <-WDI(indicator = 'SP.POP.TOTL', country = c('US', 'GB', 'JP', 'DZ'), start = 2010, end = 2020)

UUJP_GDP <- WDI(indicator = 'NY.GDP.MKTP.KD.ZG', country = c('US', 'GB', 'JP', 'DZ'), start = 2010, end = 2020)

UUJP_GINI <- WDI(indicator = 'SI.POV.GINI', country = c('US', 'GB', 'JP', 'DZ'), start = 2010, end = 2020)

# There must be a way that I can put it one time
innerJoinDf <- inner_join(UUJP_P, UUJP_GDP, by= c("iso2c", "country", "year"))

innerJoinDf <- inner_join(innerJoinDf, UUJP_GINI, by= c("iso2c", "country", "year"))

# join more than 2 datasets
# https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
# left_join(UUJP_P, UUJP_GDP, by=  c("iso2c", "country", "year")) %>%
#  left_join(, UUJP_GINI, by= c("iso2c", "country", "year")) 



##### library(plyr) we need plyr for join_all but we need to turn it off after that since it will crush with dplyr
a <- join_all(list(UUJP_P,UUJP_GDP,UUJP_GINI), by= c("iso2c", "country", "year"), type='left')




# professor way
four_country <- WDI(indicator = c('SP.POP.TOTL', 'NY.GDP.MKTP.KD.ZG','SI.POV.GINI'), country = c('US', 'GB', 'JP', 'DZ'), start = 2010, end = 2020)


# US census Bureau
library(censusapi) 
apis <- listCensusApis()

# Get a database's variable list
vars_list <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2020,
  type = "variables"
)

# search States code on internet

asc_df <- getCensus(name = "acs/acs5", 
                    vintage = 2020,
                    key = "72201e0516f96f80676bf5e8dc8cd5ab9b27398c",
                    vars = c("B01001_001E", "NAME", "B01002_001E", "B19013_001E", "GEO_ID"),
                    region = "county:*", regionin = "state:06")





# create a Texas income leaflet map
library(censusapi)
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
library(ggplot2)



setwd("C:/Users/sung/Desktop/Big data with R/BD EXCEL FILE")

# Cut Texas's shapefile
# we only see Texas since 48 is Texas's state code
#GIS

map <- shapefile("cb_2017_us_county_500k/cb_2017_us_county_500k.shp")

plot(map)

tx_map <- map[map$STATEFP == "48", ]

asc_df <- getCensus(name = "acs/acs5", vintage = 2020,
                    key = "fb56ed9550c10c063c8180ecf44d9dffefa6ef36",
                    vars = c("NAME", "B19013_001E", "GEO_ID"),
                    region = "county:*", regionin = "state:48")

#length(unique(asc_df$county))

tx_data <- tx_map@data %>%
  filter(STATEFP == "48") %>%
  left_join(asc_df[, c(2:4)], by = c("COUNTYFP" = "county")) # 합쳐야할 때, 바꿀 이름 = 바꿔야하는 이름 

tx_data$B19013_001E[tx_data$B19013_001E < 0] <- NA

# rename has got errors. I changed to rename_ and worked 
######=> bc I used dplyr and plyr both. I must use only one for here and group_by etc..
# how to change 
# https://stackoverflow.com/questions/33895570/is-there-a-dplyr-or-data-table-equivalent-to-plyrjoin-all-joining-by-a-list-o
tx_data <- rename(tx_data, "name" = "NAME.x")

tx_data <- rename(tx_data, "income_mid" = "B19013_001E")

tx_map@data <- tx_data

#leaflet

txmap_fort <- fortify(tx_map, region = "GEOID")

distcenters <- txmap_fort %>%
  group_by(id) %>%
  summarise(clat = mean(lat), clong = mean(long))

distcenters <- left_join(distcenters, tx_map@data, by = c("id" = "GEOID"))

boxplot(tx_map@data$income_mid)

bins <- c(20000, 40000, 60000, 800000, 100000, Inf)
pal <- colorBin("YlOrRd", domain = tx_map$income_mid, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br>%d income median",
  tx_map$name, round(tx_map$income_mid)
) %>% lapply(htmltools::HTML)

lf <- leaflet() %>%
  addTiles() %>%
  setView(lng = -100.23, lat = 30.97, zoom = 6) %>%
  addPolygons(data = tx_map,
              fillColor = ~pal(income_mid),
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

top10 <- distcenters %>%
  top_n(10, income_mid)
top10

income_top.lf <- lf %>% 
  addMarkers(data = top10, ~clong, ~clat, popup = ~as.character(name)) %>%
  addLegend(data = tx_map, pal = pal, values = ~income_mid, opacity = 0.7, title = NULL,
            position = "bottomright")

income_top.lf

# save the file 
# saveWidget(income_top.lf, file="tx_income.html")
