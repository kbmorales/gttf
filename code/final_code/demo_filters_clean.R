library(dplyr)
library(stringr)
library(lubridate)

# are you up-to-date?
# if not check out Ken's file(s) to be up-to-date!
# must run his code to get accurate data
# pull then run if necessary!

# for bmore_demo
load("/cloud/project/data/tidy_data/mdcs_demo_data.rda")
load("/cloud/project/data/tidy_data/mdcs_cops_clean_df.rda")
# for age column
load("/cloud/project/data/tidy_data/mdcs_data.rda")

bmore_demo = mdcs_demo_df %>%
  filter(defendant_state == "MD")

# age column
mdcs_df = mdcs_df %>%
  filter(defendant_state == "MD")

i <- 1
# this is the correct format, below code worked too. 
for(i in 1:dim(bmore_demo)[i, ]) {
# for(i in 1:dim(bmore_demo)) {
bmore_demo$defendant_race = str_trim(bmore_demo$defendant_race,
                                     side = "both")
bmore_demo$defendant_race = str_squish(bmore_demo$defendant_race)

  bmore_demo = bmore_demo %>%
    mutate(race_black = case_when(defendant_race == "BLACK, AFRICAN AMERICAN" ~ "BLACK",
                                  defendant_race == "BLACK" ~ "BLACK",
                                  TRUE ~ "NONBLACK"))%>%
    mutate(race_cat = case_when(str_detect(bmore_demo$defendant_race, "BLACK") ~ "BLACK",
                                str_detect(bmore_demo$defendant_race, "WHITE") ~ "WHITE",
                                TRUE ~ "UNKNOWN")) %>%
    mutate(age_yrs = as.character(round(difftime(mdcs_df$date,
                                                 bmore_demo$defendant_dob,
                                                 units = "days") / 365)))
# let's deal with spacing 
    bmore_demo$defendant_address = trimws(bmore_demo$defendant_address)
    bmore_demo$defendant_city = trimws(bmore_demo$defendant_city)
    bmore_demo$defendant_state = trimws(bmore_demo$defendant_state)
    bmore_demo$defendant_zip = trimws(bmore_demo$defendant_zip)
# getting rid of access white space
    bmore_demo$defendant_address = str_squish(bmore_demo$defendant_address)
    bmore_demo$defendant_city = str_squish(bmore_demo$defendant_city)
    bmore_demo$defendant_state = str_squish(bmore_demo$defendant_state)
    bmore_demo$defendant_zip = str_squish(bmore_demo$defendant_zip)
# removing problem addresses
    # remove apt and comma from first address line
    bmore_demo$defendant_address = str_remove(bmore_demo$defendant_address,
                                                 "(,? APT.? \\w+.*)|(\\.? APT.?.*)|(\\/APT.?(\\d+|\\w+))|((-*)APT (\\d+|\\w+))|((,?|\\s)+\\#.*)|(\\s[A-Z]\\w+\\s#.*)|(,\\s.*)|(\\s[A-Z]\\w+#.*)")
# removing excess numbers in zip codes
    bmore_demo$defendant_zip = str_trunc(bmore_demo$defendant_zip,
                                            width = 5,
                                            side = "right",
                                            ellipsis = "")
# creating full_address column to geocode
    bmore_demo$full_address = paste0(str_c(bmore_demo$defendant_address, ", ", 
                                           bmore_demo$defendant_city, ", ",
                                           bmore_demo$defendant_state, ", ",
                                           bmore_demo$defendant_zip, ", USA"))    

}

# joining and saving.
  bmore_demo = left_join(bmore_demo, mdcs_cops_df, by = "case_num")

# FILTER FOR ONLY DISTRICT COURT CASES
  bmore_demo <- bmore_demo %>%
    filter(case_type_2 != "Other")
# nrow(bmore_demo)
# 5401
  
# Filter out Circuit Court cases
  bmore_demo <- bmore_demo[str_detect(bmore_demo$court, "District"),]
# nrow(bmore_demo)
# 3135
  
  save(bmore_demo, 
       file = here::here("data/tidy_data",
                         # added _geo for this particular thing. 
                         # don't want to overwrite data
                         "bmore_demo.rda"))

# go to file "/code/final_code/geocoding.R" to continue
  
  
  