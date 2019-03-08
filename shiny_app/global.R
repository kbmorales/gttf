library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)

load("bmore_markers.rda")

county_sections <- readOGR("data/raw_data/md_counties/geo_export_mdc.shp")

baltimore_county <- subset(county_sections, geodesc == "Baltimore County")

baltimore_city <- subset(county_sections, geodesc == "Baltimore City")

city_county <- subset(county_sections, 
                      geodesc == "Baltimore County" | geodesc == "Baltimore City")

# edited version
addTiles = function (map, urlTemplate = "**http:**//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
                     attribution = NULL, layerId = NULL, group = NULL, options = tileOptions())
{
  options$attribution = attribution
  if (missing(urlTemplate) && is.null(options$attribution))
    options$attribution = paste("© <a href=\"http://openstreetmap.org\">OpenStreetMap",
                                "contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA")
  invokeMethod(map, getMapData(map), "addTiles", urlTemplate,
               layerId, group, options)
}