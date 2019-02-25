library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom)
library(sp)
library(sf)
options(stringAsFactors = FALSE)

# where I got the data
# https://catalog.data.gov/dataset/maryland-zip-codes-2010

# download zip file an upload into work envorinment,
# the files unzip themselves within RStudio, but here is some code for you

# unzip(zipfile = "Maryland Zip Codes 2010.zip",
#       exdir = "md-zipcodes")

# load the data
zipcode_sections <- readOGR("data/raw_data/geo_export_md.shp")

# view attributes
class(zipcode_sections)
extent(zipcode_sections)
crs(zipcode_sections)

plot(zipcode_sections)
