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

options(stringAsFactors = FALSE)


# setting up the data for mapping only -----------------------------------------------------
# where I got the data
# https://catalog.data.gov/dataset/maryland-zip-codes-2010
# https://catalog.data.gov/dataset/maryland-counties

# download zip file an upload into work envorinment,
# the files unzip themselves within RStudio, but here is some code for you

# unzip(zipfile = "Maryland Zip Codes 2010.zip",
#       exdir = "md-zipcodes")

# unzip(zipfile = "Maryland Counties.zip",
#       exdir = "md_counties")

# load the data
zipcode_sections <- readOGR("data/raw_data/md_zipcodes/geo_export_md.shp")

county_sections <- readOGR("data/raw_data/md_counties/geo_export_mdc.shp")

# view attributes ---------- ZIPCODES
class(zipcode_sections)
extent(zipcode_sections)
crs(zipcode_sections)

plot(zipcode_sections)

# view attributes ---------- COUNTIES
class(county_sections)
extent(county_sections)
crs(county_sections)

plot(county_sections)

# lets filter down to only baltimore area zipcodes!
# we can filter for other zips after

# baltimore only
baltimore_county <- subset(county_sections, geodesc == "Baltimore County")
baltimore_city <- subset(county_sections, geodesc == "Baltimore City")


# lets do a quick plot to see if this shows!
plot(baltimore_zip)
plot(baltimore_city)



# cleaning data points for map MARKERS ONLY --------------------------------------------
# lets filter through some of the data to see if we can plot on our base map!

# the data got shifted a bit,
# another team member will go back to clean but for now lets just use this as our dataset!
mdcs_demo_dfc = bmore_demo %>%
  filter(defendant_state == "MD")

# we want to join datasets as we need to retain address information BUT also make sure we are only capturing the cases we need
# nrow(mdcs_demo_df) ~ in demo_filters.R bmore_demo = mdcs_demo_d (line 6)
# [1] 7748
# nrow(bmore_demo)
# [1] 7748
# > nrow(mdcs_demo_dfc)
# [1] 7687 <- num kept after filter
# bmore_demo %>% count(defendant_state)
# # A tibble: 13 x 2
# defendant_state        n
# <chr>              <int>
# 1 CA                   3
# 2 DC                   2
# 3 GA                   1
# 4 IA                   1
# 5 MD                7687
# 6 NC                   4
# 7 NY                  14
# 8 OH                   3
# 9 PA                  11
# 10 SC                  3
# 11 VA                  9
# 12 WV                  4
# 13 XX                  6

# now let's figure out what to do with these address columns. 
# I only want two cols and unite them again and back into dataset
bmore_address = mdcs_demo_dfc %>%
  select(case_num, defendant_address, defendant_city, defendant_state, defendant_zip)

# it appears that geocode_OSM will only take strings that look like below
# geocode_OSM(bmore_fulladdress[3])
# [1] "7007 N ALTER ST, BALTIMORE, MD, 21207 - 0000"
# so lets do some cleaning
# looking at first col needed $defendant_address
# there are addresses with many spaces and others with apts.
# lets deal with the spaces first!
bmore_address$defendant_address = trimws(bmore_address$defendant_address)
bmore_address$defendant_city = trimws(bmore_address$defendant_city)
bmore_address$defendant_state = trimws(bmore_address$defendant_state)
bmore_address$defendant_zip = trimws(bmore_address$defendant_zip)

# lets get rid of access whitespace within DS
bmore_address$defendant_address = str_squish(bmore_address$defendant_address)
bmore_address$defendant_city = str_squish(bmore_address$defendant_city)
bmore_address$defendant_state = str_squish(bmore_address$defendant_state)
bmore_address$defendant_zip = str_squish(bmore_address$defendant_zip)

# take away access numbers in zip codes
bmore_address$defendant_zip = str_trunc(bmore_address$defendant_zip,
                                        width = 5,
                                        side = "right",
                                        ellipsis = "")

# putting bacl together after applied filters
bmore_address$full_address = paste0(str_c(bmore_address$defendant_address, ", ", 
                                          bmore_address$defendant_city, ", ",
                                          bmore_address$defendant_state, ", ",
                                          bmore_address$defendant_zip, ", USA"))

bmore_address = bmore_address %>%
  select(case_num, full_address)

# lets use tmap package to see if we can get some coords
bmore_demo = left_join(bmore_demo, bmore_address, by = "case_num")

# have to do this in batches because the code can't take this
# crashes at 1000 rows
 
# the first 20 rows are not reading, only 5 rows are coming back as geocoded. 
# going to work with these 5 for now to plot 
geo_bmore <- geocode_OSM(bmore_demo$full_address[1:100])
geo_bmore2 <- geocode_OSM(bmore_fulladdress[100:200])
geo_bmore3 <- geocode_OSM(bmore_fulladdress[201:300])

bmoreaddy_unique = unique(bmore_demo$full_address)
# testing
testing_addy <- geocode_OSM(bmoreaddy_unique[2])

geo_bmore <- c()

# amount of times i want this loop to run
for(i in 1:round(length(bmore_demo$full_address)/100)) {
# this helps search through 1-6400
  test = geocode_OSM(bmore_demo$full_address[((i*100)-99):(i*100)])
  geo_bmore <- bind_rows(geo_bmore, test)
  }

# trying a repeat loop
# 
#   for (i in 1:(round(nrow(bmore_demo["full_address"])/ 100))) {
#     test = geocode_OSM(bmore_demo[((i*100)-99):(i*100),][["full_address"]])
#     geo_bmore <- bind_rows(geo_bmore, test)
#   }

# i = 1:77
# repeat{
#   geo_bmore = geocode_OSM(bmore_demo[((i*100)-99):(i*100),][["full_address"]])
#   if(i == 77){
#     break
#   }
#   print(geo_bmore)
# }


# trying while loop
# i = 1:77

# pick up 02.28.2019 ------------------------------------------------------
geo_bmore <- c()

i <- 1
# sometimes code will break because of 
while(i <= nrow(bmore_demo)) {
  geo_bmore_row <- geocode_OSM(bmore_demo[i,][["full_address"]])
  if(is.null(geo_bmore_row)){
    i <- i + 1
    next;
  }
  geo_bmore_row <- tibble(obs_num = i, 
                          address = geo_bmore_row[[1]],
                          lat = geo_bmore_row[[2]][1],
                          lon = geo_bmore_row[[2]][2]
                          )
  geo_bmore <- bind_rows(geo_bmore, geo_bmore_row)
  i <- i + 1
}

# Take a peek at problem address (ones that break the geocode code)
bmore_demo[i,]
# Write down the bad ones!
i <- i+1

save(bmore_demo, 
     file = here("data/tidy_data",
                 "bmore_demo.rda"))

# creating markers --------------------------------------------------------


# let's join this with other data to align
bmore_plot <- left_join(geo_bmore2, 
                        bmore_address,
                        by = "full_address")

bmoredemo_markers <- left_join(bmore_plot,
                           mdcs_demo_dfc,
                           by = "case_num")

# filtering out what I need only for demographic data
# bmore_markers %>% select(case_num, full_address, lat, lon, defendant_race, defendant_sex) %>% group_by(case_num)

# this still brings 45 rows!!
# breaking this down
# defendant_race has multiple variables from quick view, 
# lets look at those

bmoredemo_markers %>% select(defendant_race) %>% count
(defendant_race) %>% collect()
# defendant_race                n
# <chr>                     <int>
# 1 BLACK                      21
# 2 BLACK, AFRICAN AMERICAN    24

# for filtering and other purposes 
# we will stick with BLACK, yea
# this can easily be done with stringr
# this code only works on current data set
bmoredemo_markers$defendant_race = str_trunc(bmoredemo_markers$defendant_race,
                                        width = 5,
                                        side = "right",
                                        ellipsis = "")

# lets see if this helped
bmoredemo_markers %>% select(case_num, full_address, lat, lon, defendant_race, defendant_sex) %>% group_by(case_num)

# we still have distinctive rows --
# lets check out defendant_sex
bmoredemo_markers %>% select(defendant_sex) %>% count(defendant_sex) %>% collect()

# lets filter for sex to be M or F for MALE and FEMALE
bmoredemo_markers %>%
  mutate(sex_id = case_when(
    defendant_sex == ""
  ))
  

# it appears that geocode_OSM will only take strings that look like below
# geocode_OSM(bmore_fulladdress[3])
# [1] "7007 N ALTER ST, BALTIMORE, MD, 21207 - 0000"

# making the basemap ----------------------------------------------------------
# playing around with leaflet

############## USE THIS TO MARK CRIME INCIDENTS
# leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng =,
#              lat =, 
#              popup = )

# set value for min/max zoom settings
# leaflet(options = leafletOptions(minZoom = 0,
#                                 maxZoom = 18))

# creates map object
bmore_map = leaflet() %>%
  addProviderTiles(providers$MtbMap,
                   options = providerTileOptions(opacity = 0.65)) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)

# sets parameters for map around bmre city and county
# setting county lines
bmore_map = bmore_map %>%
  addPolygons(data = baltimore_county,
              group = "County",
              color = "#1d1030",
              fill = FALSE)

# setting city lines
bmore_map = bmore_map %>%
  addPolygons(data = baltimore_city,
              group = "City",
              color = "#1f004f",
              fill = FALSE) %>%
  addLayersControl(overlayGroups = c("County", "City"),
                   baseGroups = "Cases",
                   options = layersControlOptions(collapsed = FALSE))


bmore_map = bmore_map %>%
  addMarkers(data = bmore_markers,
             lng = ~lon,
             lat = ~lat,
             group = "Cases")

