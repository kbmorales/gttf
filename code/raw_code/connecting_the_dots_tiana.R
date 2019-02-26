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


# setting up the data -----------------------------------------------------
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
bmore_map = leaflet()

# sets parameters for map around bmre city and county
bmore_map = bmore_map %>%
  addPolygons(data = baltimore_county,
              group = "County",
              fill = FALSE) %>%
  addTiles()

bmore_map = bmore_map %>%
  addPolygons(data = baltimore_city,
              group = "City",
              fill = FALSE) %>%
  addLayersControl(overlayGroups = c("County", "City"),
                   options = layersControlOptions(collapsed = FALSE))





# cleaning data points for map --------------------------------------------
# lets filter through some of the data to see if we can plot on our base map!

# the data got shifted a bit,
# another team member will go back to clean but for now lets just use this as our dataset!
mdcs_demo_dfc = mdcs_demo_df %>%
  filter(defendant_state == "MD")

# we want to join datasets as we need to retain address information BUT also make sure we are only capturing the cases we need
# nrow(mdcs_cops_df)
# [1] 7821
# > nrow(mdcs_demo_dfc)
# [1] 7355 <- num kept after join

# lets change the col names for case num so they match accordingly!
mdcs_casenum = left_join(mdcs_demo_dfc,
                         mdcs_cops_df,
                         by = "case_num")

# now let's figure out what to do with these address columns. 
# I only want two cols and unite them again and back into dataset
bmore_address = mdcs_casenum %>%
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
                                        side = "left",
                                        ellipsis = "")

# putting bacl together after applied filters
bmore_address$full_address = paste0(str_c(bmore_address$defendant_address, ", ", 
                                          bmore_address$defendant_city, ", ",
                                          bmore_address$defendant_state, ", ",
                                          bmore_address$defendant_zip))

bmore_address = bmore_address %>%
  select(case_num, full_address)

# lets use tmap package to see if we can get some coords
bmore_fulladdress <- bmore_address$full_address

geo_bmore <- geocode_OSM(bmore_fulladdress)
# it appears that geocode_OSM will only take strings that look like below
# geocode_OSM(bmore_fulladdress[3])
# [1] "7007 N ALTER ST, BALTIMORE, MD, 21207 - 0000"
# so lets do some cleaning
# looking at first col needed $defendant_address
# there are addresses with many spaces and others with apts.


# lets try and add these markers