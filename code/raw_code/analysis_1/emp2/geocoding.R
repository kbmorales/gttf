library(tidyverse)
library(tmaptools)
library(tmap)
library(raster)


# DISCLAIMER --------------------------------------------------------------

# PLEASE MAKE SURE YOU HAVE RUN "/code/final_code/demo_filters_clean.R" !!!!!!!!!!


# geocoding_addresses -----------------------------------------------------
# 1. just remember to reference unique list to find the rows that are messed up!
# 2. look by case number from bmore_demo
# 3. join to full data set bmore_demo (which will eventually replace mdcs_cop_demo_df) 
# reference "code/raw_code/demo_data.R" file

bmoreaddy_unique = unique(bmore_demo$full_address)

geo_bmore <- c()

i <- 1
# when it hits limit run again 
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
# Write down the bad ones! ~ REDACTED the missing ones are dealt with below
# Use this to skip the line. 
i <- i+1


save(geo_bmore, 
     file = here::here("data/tidy_data",
                       "geo_bmore_unique_addy.rda"))
# FOR NON GEO-CODED ADDRESS CASES WORK UP ------------------------------
# two ways for accounting non-geocoded addresses
no_geo_case <- bmore_demo[is.na(match(bmore_demo$full_address, geo_bmore$full_address)), ]
# anti_join(bmore_demo, geo_bmore, by = "full_address")
no_geo = no_geo_case

no_geo = no_geo %>%
  select(-case_num)

no_geo = column_to_rownames(no_geo)

no_geo = unique(no_geo)

problem_rows = as_tibble(str_split_fixed(no_geo$full_address,
                                         ", ",
                                         n = 5))
problem_rows = paste0(str_c(problem_rows$V1,
                            ", ",
                            problem_rows$V3,
                            ", ",
                            problem_rows$V5))
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



# BROKEN ADDRESSES BEING GEO-CODED -------------------------------------
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


# COMBINING GEO_CODED ADDRESSES --------------------------------------
geo_bmore <- bind_rows(geo_bmore, new_geo)
geo_bmore = geo_bmore %>% arrange(obs_num)

# small difference in full_address col
# for both geo_bmore and bmore_demo
bmore_demo$full_address[!bmore_demo$full_address%in%geo_bmore$full_address]

# keeps only data that was geocoded
bmore_plot1 <- left_join(geo_bmore, 
                         bmore_demo,
                         by = "full_address")
# keeps all rows and data
# bmore_plot2 <- left_join(bmore_demo, 
#                          geo_bmore,
#                          by = "full_address")

# save under data set that has filtered out district court cases
bmoredemo_markers1 <- left_join(bmore_plot1,
                                mdcs_df,
                                by = "case_num")

# keeps all rows and data
# bmoredemo_markers2 <- left_join(bmore_plot2,
#                                mdcs_demo_dfc,
#                                by = "case_num")

bmoredemo_markers1 = bmoredemo_markers1 %>%
  dplyr::select(case_num, full_address, lat, lon, race_black, race_cat, age_yrs, sex_id, case_type, case_type_2) %>%
  group_by(case_num) %>%
  ungroup()

save(bmoredemo_markers1,
     file = here::here("data/tidy_data",
                       "bmore_markers.rda"))

# after this script has run,
# please go to "code/final_code/censusData_maps.R" 
