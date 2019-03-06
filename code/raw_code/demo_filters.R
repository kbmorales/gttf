# this is Ken's updated revision of the dataset
library(dplyr)
library(stringr)
library(lubridate)

bmore_demo = mdcs_demo_df

# grouping data vars ------------------------------------------------------
colnames(bmore_demo)

# filtering for sex
bmore_demo %>%
  count(defendant_sex)

# # A tibble: 6 x 2
# defendant_sex       n
# <chr>           <int>
# 1 ""                5
# 2 F               501
# 3 FEMALE          218
# 4 M              4070
# 5 MALE           2953
# 6 U                 1

# creating new column
bmore_demo = bmore_demo %>%
  mutate(sex_id = case_when(
    defendant_sex == "FEMALE" ~ "F",
    defendant_sex == "MALE" ~ "M",
    defendant_sex == "" ~ "Unknown",
    defendant_sex == "U" ~ "Unknown",
    TRUE ~ as.character(defendant_sex)
  ))

# filtering for race
bmore_demo %>%
  count(defendant_race)

# A tibble: 10 x 2
# defendant_race                                     n
# <chr>                                          <int>
# 1 AMERICAN INDIAN, ALASKA NATIVE                     3
# 2 ASIAN, NATIVE HAWAIIAN, OTHER PACIFIC ISLANDER     8
# 3 BLACK                                           3042
# 4 BLACK, AFRICAN AMERICAN                         4088
# 5 OTHER                                              3
# 6 Unknown                                            9
# 7 UNKNOWN                                           18
# 8 UNKNOWN, OTHER                                    26
# 9 WHITE                                            108
# 10 WHITE, CAUCASIAN, ASIATIC INDIAN, ARAB          443

# per Ken we will create two columns with the dataset --
# race_black
# lets trimws() in case of things we can't miss
bmore_demo$defendant_race = str_trim(bmore_demo$defendant_race,
                                     side = "both")
bmore_demo$defendant_race = str_squish(bmore_demo$defendant_race)

bmore_demo = bmore_demo %>%
  mutate(race_black = case_when(
    defendant_race == "BLACK, AFRICAN AMERICAN" ~ "BLACK",
    defendant_race == "BLACK" ~ "BLACK",
    TRUE ~ "NONBLACK"
  ))


# race_cat ~ BLACK, WHITE, OTHER
bmore_demo = bmore_demo %>%
  mutate(race_cat = case_when(
    str_detect(bmore_demo$defendant_race, "BLACK") ~ "BLACK",
    str_detect(bmore_demo$defendant_race, "WHITE") ~ "WHITE",
    TRUE ~ "UNKNOWN"
  ))

# filtering for age
bmore_demo %>%
  count(is.na(defendant_dob))
# A tibble: 2 x 2
# `is.na(defendant_dob)`       n
# <lgl>                    <int>
# 1 FALSE                   7744
# 2 TRUE                       4

# only 4 NAs so still ok to do data set
bmore_demo = bmore_demo %>%
  mutate(age_yrs = as.character(round(difftime(mdcs_demo_dfc$date, bmore_demo$defendant_dob, units = "days") / 365)))                                         


# leftjoin with mdcs_cops_df
bmore_demo = left_join(bmore_demo, mdcs_cops_df, by = "case_num")

bmore_demo = bmore_demo %>%
  filter(defendant_state == "MD")

save(bmore_demo, 
     file = here::here("data/tidy_data",
                       "bmore_demo.rda"))
