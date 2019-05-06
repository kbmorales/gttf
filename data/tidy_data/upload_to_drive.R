library(googledrive)
library(purrr)
library(tidyverse)

options(httr_oob_default = TRUE)

drive_auth()

# get team drive directory
pfdrive <- team_drive_get("Problem Forward")

# makes path for new specific folder for data
datapath <-drive_get("gttf/data/raw_data", team_drive = "Problem Forward")

# raw_data/arrests --------------------------------------------------------
# get all files we want to upload in GDrive
local_files <- list.files("/cloud/project/data/raw_data/arrests_data")

# creates the folder
folder <- drive_mkdir(name = "arrests_data",
                      parent = datapath)

map("/cloud/project/data/raw_data/arrests_data/BPD_Arrests.csv", drive_upload, path = folder, verbose = F)

# raw_data/baltimore_city_zipcode ------------------------------------------
local_files <- list.files("/cloud/project/data/raw_data/baltimore_city_zipcode/",
                          full.names = TRUE)

# makes path for new specific folder for data
datapath <-drive_get("gttf/data/raw_data", team_drive = "Problem Forward")

# creates the folder
folder <- drive_mkdir(name = "baltimore_city_zipcode",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)


# raw_data/baltimore_county_zipcode -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/baltimore_county_zipcode/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "baltimore_county_zipcode",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/bmore_hoods -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/bmore_hoods/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "bmore_hoods",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/census_data_shp -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/census_data_shp/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "census_data_shp",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/distinct_city_zips -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/distinct_city_zips/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "distinct_city_zips",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/gun_examination -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/gun_examination/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "gun_examination",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/gun_examination -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/gun_examination/gun_examination-master/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "gun_examination",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/md_counties -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/md_counties/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "md_counties",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/md_zipcodes -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/md_zipcodes/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "md_zipcodes",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/mdcs_casesearch_files -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/mdcs_casesearch_files/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "mdcs_casesearch_files",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)

# raw_data/police_district -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/police_district/",
                          full.names = TRUE)

# creates the folder
folder <- drive_mkdir(name = "police_district",
                      parent = datapath)

map(local_files, drive_upload, path = folder, verbose = F)


# tidy_data/ -------------------------------
local_files <- list.files("/cloud/project/data/raw_data/",
                          full.names = TRUE)

# makes path for new specific folder for data
datapath <-drive_get("gttf/data/raw_data", team_drive = "Problem Forward") 

map(local_files, drive_upload, path = datapath, verbose = F)
