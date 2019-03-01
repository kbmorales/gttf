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

# test = mdcs_demo_dfc %>%
#   select(case_num, defendant_address, defendant_city, defendant_state, defendant_zip)

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

# have to do this in batches because the code can't take this
# crashes at 1000 rows
 
# the first 20 rows are not reading, only 5 rows are coming back as geocoded. 
# going to work with these 5 for now to plot 
# geo_bmore <- geocode_OSM(bmore_demo$full_address[1:100])
# geo_bmore2 <- geocode_OSM(bmore_fulladdress[100:200])
# geo_bmore3 <- geocode_OSM(bmore_fulladdress[201:300])

# # testing
# testing_addy <- geocode_OSM(bmoreaddy_unique[2])

# geo_bmore <- c()

# amount of times i want this loop to run
# for(i in 1:round(length(bmore_demo$full_address)/100)) {
# # this helps search through 1-6400
#   test = geocode_OSM(bmore_demo$full_address[((i*100)-99):(i*100)])
#   geo_bmore <- bind_rows(geo_bmore, test)
#   }

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

# pick up 03.01.2019 ------------------------------------------------------
# just remember to reference unique list to find the rows that are messed up!
# then look by case number from bmore_address, then join to full data set bmore_demo (which will eventually replace
# mdcs_cop_demo_df) reference demo_data.R file
bmoreaddy_unique = unique(bmore_address$full_address)

geo_bmore <- c()

# i <- 1
# hit limit will run again at lunch
# i = 5578
# sometimes code will break because of 
while(i <= length(bmoreaddy_unique)) {
  geo_bmore_row <- geocode_OSM(bmoreaddy_unique[i])
  if(is.null(geo_bmore_row)){
    i <- i + 1
    next;
  }
  # these are backwards. reverse lat/lon
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


# Left join bmore_address to geo_bmore by full_address
# WRITE ME

# Take a peek at problem address (ones that break the geocode code)
bmoreaddy_unique[i]

# Write down the bad ones!
# final edit ended on 3217 ~ prob hit query limit for day
i <- i+1

# DO NOT RUN UNTIL ALL HAVE BEEN ACCOUNTED FOR
# lets use tmap package to see if we can get some coords
bmore_demo = left_join(bmore_demo, bmore_address, by = "case_num")


# fixing geo_bmore row strucure
# problem_rows <- geo_bmore %>% filter(is.na(address))
# cool_rows <- geo_bmore %>% filter(!is.na(address))
# 
# problem_rows = problem_rows %>%
#   select(-address)
# 
# cool_rows = cool_rows %>%
#   select(-full_address)
# 
# colnames(cool_rows)[2:4] <- c("lat", "lon", "full_address")
# 
# geo_bmore = rbind(problem_rows, cool_rows)

# separating dataframe so that I can continue geocoding
# problem_rows <- geo_bmore[1:448, ]
# cool_rows <- geo_bmore[449:1070, ]

# first lets figure out why this 2k cases where not running
geo_bmore = problem_rows
# set i to last obs_num in ge_bmre
# i = 663

# this only returns successful geo_code addresses, there are still addresses that are not accuonted for - addressses that OSM cannot locate
save(geo_bmore, 
     file = here::here("data/tidy_data",
                       "geo_bmore_unique_addy.rda"))

save(bmore_demo, 
     file = here("data/tidy_data",
                 "bmore_demo.rda"))

# creating markers --------------------------------------------------------

# UPDATE AND RERUN EVERY TIME NEW GEO_BMORE SP DATA IS ADDED

# let's join this with other data to align
# inserts case number with addy's
# this will drop rows that were not able to find geo_codes
# want to keep full data set. will create 2 plot data

# keeps only data that was geocoded
bmore_plot1 <- left_join(geo_bmore, 
                        bmore_address,
                        by = "full_address")
# keeps all rows and data
# bmore_plot2 <- left_join(bmore_address, 
#                          geo_bmore,
#                          by = "full_address")

# keeps only data that was geocoded
# only working for this data fo now
bmoredemo_markers1 <- left_join(bmore_plot1,
                           mdcs_demo_dfc,
                           by = "case_num")

# keeps all rows and data
# bmoredemo_markers2 <- left_join(bmore_plot2,
#                                mdcs_demo_dfc,
#                                by = "case_num")

# colnames(bmoredemo_markers1)
# [1] "obs_num"           "full_address"      "lat"        
# [4] "lon"               "case_num"          "court_system"
# [7] "status_date"       "tracking_num"      "district_code"
# [10] "location_code"    "doc_type"        "case_disposition"
# [13] "complaint_num"    "district_case_num" "defendant_name" 
# [16] "defendant_race"   "defendant_sex"     "defendant_dob"  
# [19] "defendant_address" "defendant_city"   "defendant_state"
# [22] "defendant_zip"     "sex_id"            "race_black"   
# [25] "race_cat"          "age_yrs"           "name"       
# [28] "dob"               "party_type"        "court"    
# [31] "case_type"         "status"            "date"     
# [34] "caption"           "case_type_2"       "gttf_cop"    

# filtering out what I need only for demographic data
bmoredemo_markers1 = bmoredemo_markers1 %>% dplyr::select(case_num, full_address, lat, lon, race_black, race_cat, age_yrs, sex_id, case_type, case_type_2) %>% group_by(case_num) %>% ungroup()
# # A tibble: 3,095 x 10
# # Groups:   case_num [3,095]
# case_num full_address   lat   lon race_black race_cat age_yrs
# <chr>      <chr>      <dbl> <dbl> <chr>      <chr>    <chr>
# 1 3B01920… 1320 WILSON… -76.4  39.3 NONBLACK   UNKNOWN  37
# 2 5B01934… 7007 N ALTE… -76.7  39.4 BLACK      BLACK    29
# 3 1B01952… 909 N FULTO… -76.6  39.3 BLACK      BLACK    35
# 4 8100980… 433 MANSE C… -76.6  39.3 BLACK      BLACK    49
# 5 1B02053… 433 MANSE C… -76.6  39.3 BLACK      BLACK    49
# 6 8112900… 1951 EDMOND… -76.6  39.3 BLACK      BLACK    43
# 7 8112980… 901 N PAYSO… -76.6  39.3 BLACK      BLACK    30
# 8 4B02138… 901 N PAYSO… -76.6  39.3 BLACK      BLACK    30
# 9 8113070… 111 S AMITY… -76.6  39.3 BLACK      BLACK    40
# 10 2B02143… 111 S AMITY… -76.6  39.3 BLACK      BLACK    31
save(bmoredemo_markers1, 
     file = here("data/tidy_data",
                 "bmore_markers.rda"))
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

# adding zoom functionality
# setView(bmore_map,
#         )

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
                   baseGroups = "Black/NonBlack",
                   options = layersControlOptions(collapsed = FALSE))

# maybe i have to specify each thing?
# ex:
bmore_raceblack <- bmoredemo_markers1 %>%
  dplyr::select(lat, lon, race_black)

# using circles as advised, can use this code for other
# instances though
# making icons
# raceblack_icons = iconList(
#   Black = makeIcon("products/",
#                    24,
#                    # hieght ~ pixels
#                    24),
#   Non_Black = makeIcon("products/",
#                        18,
#                        18)
#   )

# apply these to respective observations
bmore_raceblack = bmore_raceblack %>%
  mutate(type = factor(ifelse(bmore_raceblack$race_black == "BLACK", "Black", "Non_Black")))

raceblack_icons <- colorFactor(c("050403", "#ffa500"),
                               domain = unique(bmore_raceblack$type))

bmore_map = bmore_map %>%
  addCircles(data = bmore_raceblack,
             lng = ~lon,
             lat = ~lat,
             group = "Black/NonBlack",
             popup = bmore_raceblack$type,
             weight = 3,
             radius = 40,
             stroke = TRUE,
             fillOpacity = 0.8,
             color = ~raceblack_icons(bmore_raceblack$type),
             label = ~as.character(full_address)) %>%
  addLegend("bottomright",
            colors = c("#050403", "#ffa500"),
            labels = c("Black", "Non-Black"),
            title = "Cases by Race"
            )

