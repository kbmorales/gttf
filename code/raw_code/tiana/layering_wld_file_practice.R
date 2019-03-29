# overlaying image onto map

library(devtools)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(mapdata)
library(stringi)
library(sp)
library(ggspatial)
library(plotly)
library(rgdal)

maryland <- map_data("county",
                     "maryland")
# filtering for only baltimore area
baltimore = maryland[str_detect(maryland$subregion, "baltimore"), ]

leaflet() %>%
  addTiles() %>%
  addRasterImage()
  addPolygons(data = baltimore,
              lat = baltimore$lat,
              lng = baltimore$long)
