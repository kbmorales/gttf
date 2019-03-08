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

# setting up data ---------------------------------------------------------

# # load the data
county_sections <- readOGR("data/raw_data/md_counties/geo_export_mdc.shp")

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
city_county <- subset(county_sections, 
                      geodesc == "Baltimore County" | geodesc == "Baltimore City")
# cleaning data MARKERS ONLY-----------------------------------------------------------
# lets filter through some of the data to see if we can plot on our base map!
mdcs_demo_dfc = bmore_demo %>%
  filter(defendant_state == "MD")

# now let's figure out what to do with these address columns. 
bmore_address = mdcs_demo_dfc %>%
  select(case_num, defendant_address, defendant_city, defendant_state, defendant_zip)

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

# remove apt and comma from first address line
bmore_address$defendant_address = str_remove(bmore_address$defendant_address,
                                             "(,? APT.? \\w+.*)|(\\.? APT.?.*)|(\\/APT.?(\\d+|\\w+))|((-*)APT (\\d+|\\w+))|((,?|\\s)+\\#.*)|(\\s[A-Z]\\w+\\s#.*)|(,\\s.*)|(\\s[A-Z]\\w+#.*)")

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

# Create a index of case numbers to full address
bmore_address = bmore_address %>%
  select(case_num, full_address)


# geocode normal addresses ------------------------------------------------
# just remember to reference unique list to find the rows that are messed up!
# then look by case number from bmore_address, then join to full data set bmore_demo (which will eventually replace
# mdcs_cop_demo_df) reference 'raw_code/demo_filters.R' file
bmoreaddy_unique = unique(bmore_address$full_address)

geo_bmore <- c()
# i <- 1
# hit limit will run again at lunch
# i = 6328 ~ end result
while(i <= length(bmoreaddy_unique)) {
  geo_bmore_row <- geocode_OSM(bmoreaddy_unique[i])
  if(is.null(geo_bmore_row)){
    i <- i + 1
    next;
  }
  geo_bmore_row <- tibble(obs_num = i, 
                          full_address = geo_bmore_row[[1]],
                          # up and down
                          lon = geo_bmore_row[[2]][1],
                          # left to right
                          lat = geo_bmore_row[[2]][2]
  )
  # creating geo_bmore sp object
  geo_bmore <- bind_rows(geo_bmore, geo_bmore_row)
  i <- i + 1
}

# Take a peek at problem address (ones that break the geocode code)
bmoreaddy_unique[i]
# Write down the bad ones!
i <- i+1

save(geo_bmore, 
     file = here::here("data/tidy_data",
                       "geo_bmore_unique_addy.rda"))

# quality control for missed addresses ------------------------------------
# not creating new varibles, so im just going to do it like this
# three different ways to anti_join
# 1
no_geo_unique <- unique(bmore_address$full_address[!bmore_address$full_address%in%geo_bmore$full_address])

# 2
no_geo_case <- bmore_address[is.na(match(bmore_address$full_address, geo_bmore$full_address)), ]

# 3
anti_join(bmore_address, geo_bmore, by = "full_address")
# quality control workup --------------------------------------------------
no_geo = no_geo_case
# taking away so I can get unique values
no_geo = no_geo %>%
  select(-case_num)
# takes rowname col into actual rownames
no_geo = column_to_rownames(no_geo)
# only gives unique addresses and keeps rownames
no_geo = unique(no_geo)
# add rowname col back after only getting unique addys
# rownames basically = obs_num
# use this to join again
# no_geo = rownames_to_column(no_geo)

problem_rows = as_tibble(str_split_fixed(no_geo$full_address,
                                         ", ",
                                         n = 5))
problem_rows = paste0(str_c(problem_rows$V1,
                            ", ",
                            problem_rows$V3,
                            ", ",
                            problem_rows$V5
))
# converts from vector to df or tibble
problem_rows = enframe(problem_rows)

# renaming for future cleaning
colnames(problem_rows)[1] <- "rowname"
colnames(problem_rows)[2] <- "full_address"

# adds correct obs_num to tbl_df/tbl/data.frame
problem_rows = problem_rows %>%
  mutate(rowname = rownames(no_geo))
# sets appropriate "i" for while loop below. 
problem_rows = column_to_rownames(problem_rows)

# creating var
new_geo <- c()

i = 1
while(i <= length(problem_rows$full_address)) {
  no_geo_row <- geocode_OSM(problem_rows$full_address[i])
  if(is.null(no_geo_row)){
    i <- i + 1
    next;
  }
  new_geo_row <- tibble(obs_num = i, 
                        full_address = no_geo_row[[1]],
                        # up and down
                        lon = no_geo_row[[2]][1],
                        # left to right
                        lat = no_geo_row[[2]][2]
  )
  # creating geo_bmore sp object
  new_geo <- bind_rows(new_geo, new_geo_row)
  i <- i + 1
}

problem_rows$full_address[i]
i <- i+1

save(new_geo,
     file = here::here("data/tidy_data",
                       "new_geo.rda"))

# collecting all data together --------------------------------------------
# these both have correct coords and addresses
# lets unite them together 
head(new_geo)
head(geo_bmore)

# lets rename new_geo coded addresses so there isn't a mix up
colnames(new_geo)[2] <- "geocoded_addresses"


# but first we need to make sure that addresses in new_geo will match addresses
# bmore_demo YIKES YIKES YIKES!
# renaming col for binding purposes
colnames(problem_rows)[2] <- "geocoded_address"

# problem_rows = no_geo, basically
matching = cbind(problem_rows, no_geo)

# join new_geo and matching by geocoded_addresses
new_geo = left_join(new_geo, matching, by = "geocoded_address")

# take away incorrect current $obs_num and $geocoded_address
new_geo = new_geo %>%
  select(-obs_num)

new_geo = new_geo %>%
  select(-geocoded_address)

# lets rearrange col order for perfect alignment
new_geo = new_geo[, c(3,4,1,2)]

# change rowname to finally correct obs_num
colnames(new_geo)[1] <- "obs_num"

# let's rbind with geo_bmore and finally start mapping!!!!!!!!!!
geo_bmore = rbind(geo_bmore, new_geo)
geo_bmore = geo_bmore %>%
  arrange(as.double(obs_num))


# RUN TO MATCH geo_bmore DATASET - comment out after running
# bmore_demo$full_address = paste0(str_c(bmore_demo$full_address, ", USA"))

# DO NOT RUN UNTIL ALL GEOCODES HAVE BEEN ACCOUNTED FOR
# lets use tmap package to see if we can get some coords
bmore_demo = left_join(geo_bmore, bmore_demo, by = "full_address")
# nrow(bmore_demo)
# 6721

# FILTER FOR ONLY DISTRICT COURT CASES
bmore_demo <- bmore_demo %>%
  filter(case_type_2 != "Other")
# nrow(bmore_demo)
# 6078 ~ geocoded
# 7687 ~ filtered
# 7748 ~ original # cases

# Filter out Circuit Court cases
bmore_demo <- bmore_demo[str_detect(bmore_demo$court, "District"),]
# nrow(bmore_demo) - 80.46% retained
# geo_coded ~ 3645 
# filtered ~ 4530  

bmore_demo_geo = bmore_demo
# save this demo as bmore_demo_geo.rda
# please keep original data set!!!!
save(bmore_demo_geo,
     file = here::here("data/tidy_data",
                       "bmore_demo_geo.rda"))

# creating markers --------------------------------------------------------
# UPDATE AND RERUN EVERY TIME NEW GEO_BMORE SP DATA IS ADDED
# keeps only data that was geocoded
bmore_plot1 <- left_join(geo_bmore, 
                         bmore_address,
                         by = "full_address")
# keeps all rows and data
# bmore_plot2 <- left_join(bmore_address, 
#                          geo_bmore,
#                          by = "full_address")

# save under data set that has filtered out district court cases
bmoredemo_markers1 <- left_join(bmore_plot1,
                                mdcs_demo_dfc,
                                by = "case_num")
# keeps all rows and data
# bmoredemo_markers2 <- left_join(bmore_plot2,
#                                mdcs_demo_dfc,
#                                by = "case_num")

# have to do some sligt cleaning ~ run when data is new
# bmoredemo_markers1 = bmoredemo_markers1 %>% select(-full_address.y)
# colnames(bmoredemo_markers1)[colnames(bmoredemo_markers1) == "full_address.x"] <- "full_address"

# filtering out what I need only for demographic data
bmoredemo_markers1 = bmoredemo_markers1 %>% 
  dplyr::select(case_num,
                full_address,
                lat,
                lon,
                race_black,
                race_cat,
                age_yrs,
                sex_id,
                case_type,
                case_type_2) %>% 
  group_by(case_num) %>% 
  ungroup()

# there are two incomplete addresses that are throwing the map off. 
# bmoredemo_markers1 = bmoredemo_markers1 %>% filter(!case_num == "813141002"|!case_num == "1B02228612")
# bmoredemo_markers1 = bmoredemo_markers1[bmoredemo_markers1$case_num != "1B02228612", ]
# bmoredemo_markers1 = bmoredemo_markers1[bmoredemo_markers1$case_num != "813141002", ]

# saves into local environment
save(bmoredemo_markers1, 
     file = here::here("data/tidy_data",
                       "bmore_markers.rda"))



# making the base map vars ------------------------------------------------
# sex ~ M/F
sex_icons <- colorFactor(c("#0d2666", "#53a097", "#682140"),
                         domain = unique(bmoredemo_markers1$sex_id))
# mult races
racecat_icons <- colorFactor(c("#78840b", "#036852", "#a53208"),
                             domain = unique(bmoredemo_markers1$race_cat))
# black/nonblack
raceblack_icons <- colorFactor(c("#ffa500", "#b7b2ac"),
                               domain = unique(bmoredemo_markers1$race_black))
# age
pal <- colorNumeric(palette = diverge_hcl(5, "Berlin"),
                    domain = bmoredemo_markers1$age_yrs)
# map making --------------------------------------------------------------
# REMINDER: every time new variations are made must run empty leaflet line
# or the map will just be written over and not rerun

#          attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.



# creates map object
bmore_map = leaflet()

# adding data
# examples say to put addLayersControl() at end of map making
bmore_map = bmore_map %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
  attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.' ) %>%
  fitBounds(-76.89493, 39.19533, -76.32139, 39.72115) %>%
  # addProviderTiles(providers$Stamen.TonerLines,
  #                  options = providerTileOptions(opacity = 0.35)) %>%
  # addProviderTiles(providers$Stamen.TonerLabels) %>%
  addPolygons(data = baltimore_county,
              group = "County",
              color = "#b799ff",
              fill = FALSE) %>%
  addPolygons(data = baltimore_city,
              group = "City",
              color = "#c0a5ff",
              fill = FALSE) %>%
  addCircles(data = bmoredemo_markers1,
             lng = ~lon,
             lat = ~lat,
             group = "Points",
             popup = bmoredemo_markers1$case_num,
             weight = 3,
             radius = 40,
             stroke = TRUE,
             fillOpacity = 0.8,
             color = "#ff0000",
             label = ~as.character(bmoredemo_markers1$case_num)
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)
             ) %>%
  addCircles(data = bmoredemo_markers1,
             lng = ~lon,
             lat = ~lat,
             group = "Black/Non-Black",
             popup = bmoredemo_markers1$race_black,
             weight = 3,
             radius = 40,
             stroke = TRUE,
             fillOpacity = 0.8,
             color = ~raceblack_icons(race_black),
             label = ~as.character(bmoredemo_markers1$race_black)
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)
             ) %>%
  addLegend("bottomright",
            colors = c("#ffa500", "#b7b2ac"),
            labels = c("Black", "Non-Black"),
            title = "Cases by Race",
            group = "Black/Non-Black") %>%
  addCircles(data = bmoredemo_markers1,
             lng = ~lon,
             lat = ~lat,
             group = "Black/White/Other",
             popup = bmoredemo_markers1$race_cat,
             weight = 3,
             radius = 40,
             stroke = TRUE,
             fillOpacity = 0.8,
             color = ~racecat_icons(race_cat),
             label = ~as.character(bmoredemo_markers1$case_num)
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)
             ) %>%
  addLegend("bottomright",
            colors = c("#78840b", "#036852", "#a53208"),
            labels = c("Black", "Other/Unknown", "White"),
            title = "Cases by Race Mult",
            group = "Black/White/Other",
            labFormat = labelFormat(group = "Black/White/Other")) %>%
  addCircles(data = bmoredemo_markers1,
             lng = ~lon,
             lat = ~lat,
             group = "Female/Male",
             popup = bmoredemo_markers1$sex_id,
             weight = 3,
             radius = 40,
             stroke = TRUE,
             fillOpacity = 0.8,
             color = ~sex_icons(sex_id),
             label = ~as.character(bmoredemo_markers1$sex_id)
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)
             ) %>%
  addLegend("bottomright",
            colors = c( "#53a097", "#0d2666", "#682140"),
            labels = c("Male", "Female", "Other/Unknown"),
            title = "Cases by Sex",
            group = "Female/Male",
            labFormat = labelFormat()) %>%
  addCircles(data = bmoredemo_markers1,
             lng = ~lon,
             lat = ~lat,
             group = "Age",
             weight = 2,
             radius = 25,
             stroke = TRUE,
             fillOpacity = 0.5,
             color = ~pal(age_yrs),
             label = ~as.character(bmoredemo_markers1$age_yrs)
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)
             ) %>%
  addLegend("bottomright",
            pal = pal,
            values = bmoredemo_markers1$age_yrs,
            title = "Age",
            group = "Age") %>%
  addLayersControl(overlayGroups = c("City",
                                     "County"),
                   baseGroups = c("Points",
                                  "Black/Non-Black", 
                                  "Black/White/Other",
                                  "Female/Male",
                                  "Age"),
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE))
  

# data added on the map

# saves map
htmlwidgets::saveWidget(bmore_map,
           file = here::here("products/bmore_map_final.html"),
           selfcontained = FALSE)

webshot("products/bmore_map.html",
        file = here::here("products/bmore_map.png",
                          cliprect = "viewport"))



mapshot(bmore_map, url = paste0(getwd(), "/bmore_map.html"))


# possible save output for the map
# library(shiny)
# app <- shinyApp(
#   ui = fluidPage(leafletOutput('myMap')),
#   server = function(input, output) {
#     map = leaflet() %>% addTiles() %>% setView(-93.65, 42.0285, zoom = 17)
#     output$myMap = renderLeaflet(map)
#   }
# )
# if (interactive()) app

# actual function
function (map, urlTemplate = "//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", 
          attribution = NULL, layerId = NULL, group = NULL, options = tileOptions()) 
{
  options$attribution = attribution
  if (missing(urlTemplate) && is.null(options$attribution)) 
    options$attribution = paste("&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a>", 
                                "contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA</a>")
  invokeMethod(map, getMapData(map), "addTiles", urlTemplate, 
               layerId, group, options)
}



# edited function
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

