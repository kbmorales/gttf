library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom)
library(sp)
library(sf)
library(tidyverse)
library(ggmap)
library(maps)
library(tmap)
library(tmaptools)
library(htmlwidgets)

# way one -----------------------------------------------------------------
# PAUSING THIS

# step one
census_map <- read_csv("data/raw_data/census_pop.csv")
class(census_data)

# step two
census_map = st_as_sf(census_map, wkt = "the_geom")

# step three
census_map = as_Spatial(census_map, 
                        cast = TRUE)



# cb_2017_24_tract_500k.shp
# 2017 = 4 digit year
# 24 = state FIPS code
# tract = entity name
# 500k = resolution level ~ 1:500,000k

# black = census_map %>%
#   select(Blk_AfAm, the_geom)


# pal <- colorNumeric(palette = c("royalblue3", "royalblue2", "royalblue1", "royalblue","lightblue4","lightblue3", "lightblue2", "lightblue1", "lightblue", "lemonchiffon3", "lemonchiffon2", "lemonchiffon1", "lemonchiffon"),
#                     domain = census_map$Blk_AfAm)

c("firebrick4", "indianred4","firebrick3", "firebrick2", "firebrick1","firebrick", "indianred3", "indianred2", "indianred1", "indianred")


pal <- colorBin(palette = c("firebrick4", "indianred4","firebrick3", "firebrick2", "firebrick1","firebrick", "indianred3", "indianred2", "indianred1", "indianred"),
                domain = census_map$blAfAm_perc,
                bins = c(0,10,20,30,40,50,60,70,80,90,100))

labels <- sprintf("<strong>%s</strong><br/>%g percentage of Black/African American pop",
                  census_map$Name,
                  census_map$blkAfAm_perc) %>%
  lapply(htmltools::HTML)

# labels <- sprintf("<strong>%s</strong><br/>%e % of African Americans",
#                   census_map$Name,
#                   census_map$blkAfAm_perc) %>% lapply(htmltools::HTML)

# creating sp object work up ---------------------------
# joinig two different spatial data frames

# ref: http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html#spatial-spatial
# this method works the best.
# testing has NA reference system, we will set the reference system similar 
# bmore_hoods@proj4string
proj4string(testing) <- CRS("+proj=longlat +datum=WGS84 +no_defs
+ellps=WGS84 +towgs84=0,0,0")
# checks to see if the coords were properly saved and reprojected
range(coordinates(testing))

# maybe I have to run this line to reproject in order for the objects to match up?
# this line of code allows me to extract proj4string from bmore_hoods
# testing.projection <- CRS(proj4string(bmore_hoods))

# thid line matches them up so I can join later?
# testing.projected <- spTransform(testing, testing.projection)

# now i want to see what names of neighborhoods match the census tract
testing.hoods <- over(testing, bmore_hoods)
head(testing.hoods, 6)

testing <- spCbind(testing, testing.hoods)
census_map = testing



# attempting to set proj4string. unusable because testing@proj4string = NA
# testing.reprojected <- spTransform(testing, creatingProjection)

# census data map ---------------------------------------------------------
# NOTES FOR census_map DATA
# Housing = Occupied + Vacant
# Occupied = Occ_Owned + Occ_Rent
# Vacant = Vac_Rent + Vac_Sale + Vac_Other
# Population = White + Blk_AfAm + AmInd_AkNa + Asian + NatHaw_Pac + Other_Race + TwoOrMore
# Hisp_Lat is unaccounted for
# PopOver18 = Same as above but Ovr18
# Male + Female = Population
# AGE0:65&Over = Population



census_map@data = census_map@data %>%
  mutate(housing_perc = round((Vacant/Housing)*100))

names(census_map)[46] <- "blkAfAm_perc"

# reset map after alteration
black_pop = leaflet()

black_pop = leaflet(census_map) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *This is Census Data from 2010 Census Tracts. <a href="http://www.problemforward.com/index.html">Problem Forward &copy</a>') %>%  
  addPolygons(fillColor = ~pal(census_map$blkAfAm_perc),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.4,
              group = "black_perc",
              highlight = highlightOptions(weight = 5,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                       padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto"))%>%
  addCircles(data = bmoredemo_markers1,
             lat = ~lat,
             lng = ~lon) %>%
  addLegend("topleft",
            pal = pal, 
            values = census_map$blkAfAm_perc,
            group = "black_perc",
            position = "topright") %>%
  addLayersControl(overlayGroups = "black_perc")

# saves map
htmlwidgets::saveWidget(black_pop,
                        file = here::here("products/black_pop.html"),
                        selfcontained = TRUE)

webshot("products/black_pop.html",
        file = here::here("products/black_pop.png",
                          cliprect = "viewport"))



mapshot(black_pop, url = paste0(getwd(), "/black_pop.html"))



# using the image ken got with the data --------------------------------------------------



