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
# saves map
library(htmlwidgets)

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

# # load the data
# unusable data, bbox bounds are not written in lat or lon ~ finding other shp files
# zipcode_sections <- readOGR("data/raw_data/distinct_city_zips/ZIP_Codes.shp")

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
                      geodesc == "Baltimore County" | 
                      geodesc == "Baltimore City")
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
# i = 6328
# sometimes code will break because of 
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

# original data was pulled in section below
# LOOK AT THIS FILE TO SEE HOW DATA WAS AGGREGATED
# "cloud/project/code/raw_code/no_geo_workup.R"
geo_bmore <- bind_rows(geo_bmore, new_geo)
geo_bmore = geo_bmore %>% arrange(obs_num)
# nrow(geo_bmore)
# 5585

# Take a peek at problem address (ones that break the geocode code)
bmoreaddy_unique[i]

# Write down the bad ones!
i <- i+1


# pick up 03.05.2019 ------------------------------------------------------
#

# currently running into a problem:
# new rows have been added into geo_bmore
# but they != bmore_demo$fulladdress 
# because i str_c() them differently.
# unable to properly left_join
# .................. yea
# could possibly str_c(new_geo) into old problem_rows?
# unsure if this will work
# stringr::str_match??

# DID NOT RETURN DESIRED OUTPUT
# # ref: https://cran.r-project.org/web/packages/arsenal/vignettes/compare.html
# library(arsenal)
# summary(compare(bmore_demo, geo_bmore[1:3135]))

# let's try using intersect
# intersect(bmore_demo, geo_bmore)
# intersect(bmore_demo$full_address, geo_bmore$full_address)
# does not work!!!!!

#                      START HERE
# TRYING TO MATCH THE STRINGS WITH REGREX R
# use f ?gsub()

# using Ken's advice to re-create then join by row and obs col #
# recreating sample set to join
problem_rows = as_tibble(str_split_fixed(unique(no_geo$full_address),
                                         ", ",
                                         n = 5))
problem_rows = paste0(str_c(problem_rows$V1,
                            ", ",
                            problem_rows$V2,
                            ", ",
                            problem_rows$V3,
                            ", ",
                            problem_rows$V4,
                            ", ",
                            problem_rows$V5
))
# converts from vector to df or tibble
problem_rows = enframe(problem_rows)

# renaming for future cleaning
colnames(problem_rows)[1] <- "rowname"
colnames(problem_rows)[2] <- "full_address2"

# adds correct obs_num to tbl_df/tbl/data.frame
# only allowed if col is called rowname
problem_rows = problem_rows %>%
  mutate(rowname = rownames(no_geo))

# resetting column to join!
colnames(problem_rows)[1] <- "obs_num"
problem_rows = unique(problem_rows$full_address2)

# we are now set back similar to the join with other data set!
# head(problem_rows) ~ tibble
#   obs_num                 full_address
# 1 1           1208 WINDSAIL ROAD, ESSEX, MD, 21221, USA   
# 2 5           220 LOZERNE AVE, BALTIMORE, MD, 21224, USA  
# 3 6           1631 N BRADFORD, BALTIMORE, MD, 21213, USA  
# 4 7           708 DEACON HILL CT, BROOKLYN, MD, 21225, USA
# 5 10          6226 CAPORE WAY, BALTIMORE, MD, 21224, USA  
# 6 11          6226 CAPORE WAY, BALTIMORE, MD, 21224, USA  


# figuring out which rows are kept
# kept_addy <- problem_rows$obs_num[!problem_rows$obs_num%in%new_geo$obs_num]
# 
# if(problem_rows$obs_num %in% 1:length(new_geo$obs_num)) {
#   cbind(new_geo, problem_rows)
# }

# union(new_geo, no_geo, by = "full_address")

# FINALLY GOT WHAT I WANTED.
# THIS ALLOWS ME TO KEEP THE OBSERVATIONS I WANT
# WHILE ALSO BEING ABLE TO ADD full_address2 COL
# turned no_geo to no_geo_case because it incudes `$case_num`
# no_geo does not have `$case_num``
no_geo_case = no_geo
# taking away so I can get unique values
# have `no_geo_case`to ref for `$case_num`
no_geo = no_geo %>%
  select(-case_num)
# takes rowname col into actual rownames
no_geo = column_to_rownames(no_geo)
# only gives unique addresses and keeps rownames
no_geo = unique(no_geo)
# add rowname col back after only getting unique addys
# rownames basically = obs_num
no_geo = rownames_to_column(no_geo)



# MAY NEED FULL JOIN UNSURE
# geo_bmore = full_join(geo_bmore, problem_rows, by = "obs_num")


# so I need to run this for unique no_geo because the observation numbers are
# duplicates when joining in the data set :(


# LOAD ORIGINAL BMORE_DEMO -- ONLY RUN TO REJOIN DATASET
# nrow(bmore_demo)
# 7687
# load("data/tidy_data/bmore_demo.rda")


# RUN TO SEMI MATCH DATASET - do not run unless line 262 ran
# bmore_demo$full_address = paste0(str_c(bmore_demo$full_address, ", USA"))


# DO NOT RUN UNTIL ALL HAVE BEEN ACCOUNTED FOR
# lets use tmap package to see if we can get some coords
bmore_demo = left_join(geo_bmore, bmore_demo, by = "full_address")
# nrow(bmore_demo)
# 5731


# FILTER FOR ONLY DISTRICT COURT CASES
bmore_demo <- bmore_demo %>%
  filter(case_type_2 != "Other")
# nrow(bmore_demo)
# 5401

# Filter out Circuit Court cases
bmore_demo <- bmore_demo[str_detect(bmore_demo$court, "District"),]
# nrow(bmore_demo)
# 3135


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
# geo_bmore = problem_rows


# this only returns successful geo_code addresses, there are still addresses that are not accuonted for - addressses that OSM cannot locate
save(geo_bmore, 
     file = here::here("data/tidy_data",
                       "geo_bmore_unique_addy.rda"))

save(bmore_demo, 
     file = here::here("data/tidy_data",
                       # added _geo for this particular thing. 
                       # don't want to overwrite data
                       "bmore_demo_geo.rda"))


# address_not_inlcuded_qualityControl -------------------------------------

# figuring out the addresses that weren't geo_coded
# maybe reverse loop of above?
# or an anti-join??
# techinally becase bmoreaddy_unique is just
# unique(bmore_address$full_address) lets make
# bmoreaddy_unique into a tibble
# for (i in 1:length(unique(bmore_address$full_address))) {
#   bmoreaddy_unique = tibble(
#     full_address = unique(bmore_address$full_address),
#     obs_num = i)
#   if(!is.null(bmoreaddy_unique)){
#     i <- i + 1
#     next;
#   }
# }

# not creating new varibles, so im just going to do it like this
# three different ways to anti_join
# 1
no_geo_unique <- unique(bmore_address$full_address[!bmore_address$full_address%in%geo_bmore$full_address])

# 2
no_geo_case <- bmore_address[is.na(match(bmore_address$full_address, geo_bmore$full_address)), ]

# 3
anti_join(bmore_address, geo_bmore, by = "full_address")

#                           LOOKING FOR TRENDS
# places within outside city lines are not used in same address format
# Example:
#                       no_geo data.frame
# obs_num   case_num            full_address
# 45	     0B02145304	  2310 LORRETTA AVE, BALTIMORE, MD, 21223, USA
# 1	       0B01939987	  1208 WINDSAIL ROAD, ESSEX, MD, 21221, USA
# 5	       208059029	  220 LOZERNE AVE, BALTIMORE, MD, 21224, USA
# 10	     211307017	  6226 CAPORE WAY, BALTIMORE, MD, 21224, USA
# 17	     211356011	  1404 S WARD STREET, BALTIMORE, MD, 21230, USA
# 18 	     811280003	  7016 ARION RD, BALTIMORE, MD, 21234, USA ~ NA, PERIODT
# 19	     811290023	  1951 EDMONDSON AVE, BALTIMORE, MD, 21223, USA
# 20	     811291043	  11 W CLEMENTS STREET, BALTIMORE, MD, 21230, USA
# 29	     0B02131381	  20 CARROLLVIEW AVE, WESTMINSTER, MD, 21157, USA
# 31	     0B02136631	  8154 ELIZABETH ROAD, PASADENA, MD, 21122, USA



# what openstreetmaps.org needs for input to find this address:
#  Lorretta Alley, Linton at Ballenger, Ballenger Creek, Frederick County, Maryland, 21703, USA
#  1208, Windsail Road, Hartland Run, Port Cherry Gardens Farm, Baltimore County, Maryland, 21221, USA
# 220, North Luzerne Avenue, McElderry Park, Baltimore, Maryland, 21224, USA
# 6226, Copore Way, O'Donnell Heights, Baltimore, Maryland, 21224, USA
# 1404, Ward Street, Washington Village, Baltimore, Maryland, 21230, USA
# NOT FOUND ~ PERIODT
# 1951, Edmondson Avenue, Midtown-Edmonson, Baltimore, Maryland, 21223, USA
# 11, West Clement Street, Sharp-Leadenhall, Baltimore, Maryland, 21230, USA
# 20, Carroll View Avenue, Westminster Historic District, Westminster, Carroll County, Maryland, 21157, USA
# 8154, Elizabeth Road, Blossom Hills, Anne Arundel County, Maryland, 21122, USA


# WHEN ENTERING ADDRESSSES ON OPENSTREETMAPS.ORG, CERTAIN PLACES COME UP WHEN
# ONLY THE STREET ADDRESS, STATE, COUNTRY
# GOING TO FIX DATA ACCORDINGLY

# let's un-do, then re-do


# creating markers --------------------------------------------------------

# UPDATE AND RERUN EVERY TIME NEW GEO_BMORE SP DATA IS ADDED

# let's join this with other data to align
# inserts case number with addy's
# this will drop rows that were not able to find geo_codes
# want to keep full data set. will create 2 plot data

# unsure why but some addressses didnt save all the way. 
# fixed issue in console
# problem_rows = geo_bmore[!str_detect(geo_bmore$full_address, ", USA"), ]
# cool_rows = geo_bmore[str_detect(geo_bmore$full_address, ", USA"), ]
# problem_rows$full_address = paste0(str_c(problem_rows$full_address, ", USA"))
# geo_bmore = rbind(problem_rows, cool_rows)

# keeps only data that was geocoded
bmore_plot1 <- left_join(geo_bmore, 
                        bmore_address,
                        by = "full_address")
# keeps all rows and data
# bmore_plot2 <- left_join(bmore_address, 
#                          geo_bmore,
#                          by = "full_address")

# keeps only data that was geocoded
# only working for this data for now

# save under data set that has filtered out district court cases
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
     file = here::here("data/tidy_data",
                       "bmore_markers.rda"))
# making the basemap var----------------------------------------------------------
############## USE THIS TO MARK CRIME INCIDENTS
# leaflet() %>%
#   addTiles() %>%
#   addMarkers(lng =,
#              lat =, 
#              popup = )

# set value for min/max zoom settings
# leaflet(options = leafletOptions(minZoom = 0,
#                                 maxZoom = 18))

# set values used within map

# sex
sex_icons <- colorFactor(c("#0d2666", "#53a097", "#682140"),
                         domain = unique(bmoredemo_markers1$sex_id))

# mult races
racecat_icons <- colorFactor(c("#4c3711", "#c6beaf", "#b58a3d"),
                             domain = unique(bmoredemo_markers1$race_cat))

# black/nonblack
raceblack_icons <- colorFactor(c("#050403", "#ffa500"),
                               domain = unique(bmoredemo_markers1$race_black))


# map making --------------------------------------------------------------
# creates map object
# bmore_map = leaflet() 
# bmore_map = bmore_map %>%
#   addProviderTiles(providers$Stamen.TonerLines,
#                    options = providerTileOptions(opacity = 0.35)) %>%
#   addProviderTiles(providers$Stamen.TonerLabels) %>%
#   addPolygons(data = baltimore_county,
#               group = "County",
#               color = "#1d1030",
#               fill = FALSE) %>%
  # addPolygons(data = baltimore_city,
  #             group = "City",
  #             color = "#1f004f",
  #             fill = FALSE) %>%
# # data not in readable format ~ looking for different .shp files
# # addPolygons(data = zipcode_sections,
# #             group = "City Zip Code",
# #             color = "#660934",
# #             fill = FALSE) %>%
  # addLayersControl(baseGroups = c("County", 
  #                                 "City",
  #                                 "City Zip Code"),
  #                  overlayGroups = c("Black/Non-Black", 
  #                                    "Black/White/Other",
  #                                    "Female/Male"),
  #                  options = layersControlOptions(collapsed = FALSE)) %>%
  # addCircles(data = bmoredemo_markers1,
  #            lng = ~lon,
  #            lat = ~lat,
  #            group = "Black/NonBlack",
  #            popup = bmoredemo_markers1$race_black,
  #            weight = 3,
  #            radius = 40,
  #            stroke = TRUE,
  #            fillOpacity = 0.8,
  #            color = ~raceblack_icons(race_black),
  #            label = ~as.character(bmore_raceblack$full_address)) %>%
  # addLegend("bottomright",
  #           colors = c("#050403", "#ffa500"),
  #           labels = c("Black", "Non-Black"),
  #           title = "Cases by Race",
  #           group = "Black/NonBlack") %>%
  # addCircles(data = bmoredemo_markers1,
  #            lng = ~lon,
  #            lat = ~lat,
  #            group = "Black/White/Other",
  #            popup = bmoredemo_markers1$race_cat,
  #            weight = 3,
  #            radius = 40,
  #            stroke = TRUE,
  #            fillOpacity = 0.8,
  #            color = ~racecat_icons(race_cat),
  #            label = ~as.character(bmoredemo_markers1$full_address)) %>%
  # addLegend("bottomright",
  #           colors = c("#4c3711", "#c6beaf", "#b58a3d"),
  #           labels = c("Black", "White", "Other/Unknown"),
  #           title = "Cases by Race Mult",
  #           group = "Black/White/Other") %>%
  # addCircles(data = bmoredemo_markers1,
  #            lng = ~lon,
  #            lat = ~lat,
  #            group = "Female/Male",
  #            popup = bmoredemo_markers1$sex_id,
  #            weight = 3,
  #            radius = 40,
  #            stroke = TRUE,
  #            fillOpacity = 0.8,
  #            color = ~sex_icons(sex_id),
  #            label = ~as.character(bmoredemo_markers1$sex_id)) %>%
  # addLegend("bottomright",
  #           colors = c( "#53a097", "#0d2666", "#682140"),
  #           labels = c("Male", "Female", "Other/Unknown"),
  #           title = "Cases by Sex",
  #           group = "Female/Male"
  # )

# saves map
saveWidget(bmore_map,
           file = here::here("products/bmore_map.html"))

# map scratch code --------------------------------------------------------

# adding zoom functionality
# setView(bmore_map,
#         )

# sets parameters for map around bmre city and county
# setting county lines
# bmore_map = bmore_map %>%
#   addPolygons(data = baltimore_county,
#               group = "County",
#               color = "#1d1030",
#               fill = FALSE)

# setting city lines
# bmore_map = bmore_map %>%
#   addPolygons(data = baltimore_city,
#               group = "City",
#               color = "#1f004f",
#               fill = FALSE) %>%
#   addLayersControl(baseGroups = c("County", 
#                                   "City"),
#                    overlayGroups = c("Black/Non-Black", 
#                                      "Black/White/Other",
#                                      "Female/Male"),
#                    options = layersControlOptions(collapsed = FALSE))

# maybe i have to specify each thing?
# ex:
# bmore_raceblack <- bmoredemo_markers1 %>%
#   dplyr::select(lat, lon, race_black, full_address)

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
# bmore_raceblack = bmore_raceblack %>%
#   mutate(type = factor(ifelse(bmore_raceblack$race_black == "BLACK", "Black", "Non_Black")))
# 
# raceblack_icons <- colorFactor(c("#050403", "#ffa500"),
#                                domain = unique(bmore_raceblack$type))

# bmore_map = bmore_map %>%
  # addCircles(data = bmore_raceblack,
  #            lng = ~lon,
  #            lat = ~lat,
  #            group = "Black/NonBlack",
  #            popup = bmore_raceblack$type,
  #            weight = 3,
  #            radius = 40,
  #            stroke = TRUE,
  #            fillOpacity = 0.8,
  #            color = ~raceblack_icons(type),
  #            label = ~as.character(bmore_raceblack$full_address)) %>%
  # addLegend("bottomright",
  #           colors = c("#050403", "#ffa500"),
  #           labels = c("Black", "Non-Black"),
  #           title = "Cases by Race",
  #           group = "Black/NonBlack"
  #           )

####################### race_cat

# bmore_racecat <- bmoredemo_markers1 %>%
#   dplyr::select(lat, lon, race_cat, full_address)
# 
# racecat_icons <- colorFactor(c("#4c3711", "#c6beaf", "#b58a3d"),
#                                  domain = unique(bmore_racecat$race_cat)
#                              )

# bmore_map = bmore_map %>%
#   addCircles(data = bmore_racecat,
#              lng = ~lon,
#              lat = ~lat,
#              group = "Black/White/Other",
#              popup = bmore_racecat$race_cat,
#              weight = 3,
#              radius = 40,
#              stroke = TRUE,
#              fillOpacity = 0.8,
#              color = ~racecat_icons(race_cat),
#              label = ~as.character(bmore_racecat$full_address)) %>%
#   addLegend("bottomright",
#             colors = c("#4c3711", "#c6beaf", "#b58a3d"),
#             labels = c("Black", "White", "Other/Unknown"),
#             title = "Cases by Race",
#             group = "Black/White/Other"
#   )


##################### sex_id

# bmore_sex <- bmoredemo_markers1 %>%
#   dplyr::select(lat, lon, sex_id, full_address)
# 
# sex_icons <- colorFactor(c("#0d2666", "#53a097", "#682140"),
#                              domain = unique(bmore_sex$sex_id)
# )

# bmore_map = bmore_map %>%
#   addCircles(data = bmore_sex,
#              lng = ~lon,
#              lat = ~lat,
#              group = "Female/Male",
#              popup = bmore_sex$sex_id,
#              weight = 3,
#              radius = 40,
#              stroke = TRUE,
#              fillOpacity = 0.8,
#              color = ~sex_icons(sex_id),
#              label = ~as.character(bmore_sex$sex_id)) %>%
#   addLegend("bottomright",
#             colors = c( "#53a097", "#0d2666", "#682140"),
#             labels = c("Male", "Female", "Other/Unknown"),
#             title = "Cases by Sex",
#             group = "Female/Male" )




# census data for comparison ----------------------------------------------

# the map looks cool \(^.^)/
# map <- leaflet(bmore_census) %>% 
#   addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png', 
#            attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 


bmore_census <- read_csv("data/raw_data/census_pop2010.csv")

# apparently i have to go from .csv to .shp file before I can play around 
# visual representation of me on the inside -> ( T_T)
# tutorial ref: https://datacarpentry.org/r-raster-vector-geospatial/10-vector-csv-to-shapefile-in-r/

str(bmore_census)

# $the_geom looks like it contains sp data
head(bmore_census$the_geom)

# guessing this is considered CRS data. (._. )
# example had rand(LETTERS) and rand(NUMS) 
head(bmore_census$NAME)
