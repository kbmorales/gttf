# Database drafts

library(tibble)
library(here)
library(readr)
library(stringr)

cops <- tibble(
  last_name = c("Allers", "Gondo", "Hendrix", "Jenkins", "Rayam", "Ward", "Hersl", "Taylor", "Clewell"),
  first_name = c("Thomas", "Momodu", "Evodio", "Wayne", "Jemell", "Maurice", "Daniel", "Marcus", "John")
)


save(cops, 
     file = here("data/tidy_data",
                 "cops_names.rda"))



# MDCS Data ---------------------------------------------------------------


# Jenkins

mdcs_jenkins <- c()

for(i in 2008:2017){
  mdcs_jenkins <- bind_rows(mdcs_jenkins,
                            read_csv(here("data/raw_data",
                                          str_c("Jenkins_", i, ".csv")),
                                    col_names = FALSE,
                                    col_types = "ccccccccc")
  )
}

save(mdcs_jenkins, 
     file = here("data/tidy_data",
                 "mdcs_jenkins.rda"))

# Hendrix

mdcs_hendrix <- c()

for(i in 2008:2017){
  mdcs_hendrix <- bind_rows(mdcs_hendrix,
                            read_csv(here("data/raw_data",
                                          str_c("Hendrix_", i, ".csv")),
                                     col_names = FALSE,
                                     col_types = "ccccccccc")
  )
}

save(mdcs_hendrix, 
     file = here("data/tidy_data",
                 "mdcs_hendrix.rda"))

# Gondo

mdcs_gondo <- c()

for(i in 2008:2017){
  mdcs_gondo <- bind_rows(mdcs_gondo,
                            read_csv(here("data/raw_data",
                                          str_c("Gondo_", i, ".csv")),
                                     col_names = FALSE,
                                     col_types = "ccccccccc")
  )
}

save(mdcs_gondo, 
     file = here("data/tidy_data",
                 "mdcs_gondo.rda"))

# Allers

mdcs_allers <- c()

for(i in 2008:2011){
  mdcs_allers <- bind_rows(mdcs_allers,
                           read_csv(here("data/raw_data",
                                         str_c("Allers_", i, ".csv")),
                                    col_names = FALSE,
                                    col_types = "ccccccccc")
  )
}

mdcs_allers <- bind_rows(mdcs_allers,
                         read_csv(here("data/raw_data",
                                       "Allers_2012_1.csv"),
                                  col_names = FALSE,
                                  col_types = "ccccccccc"))

mdcs_allers <- bind_rows(mdcs_allers,
                         read_csv(here("data/raw_data",
                                       "Allers_2012_2.csv"),
                                  col_names = FALSE,
                                  col_types = "ccccccccc"))

mdcs_allers <- bind_rows(mdcs_allers,
                         read_csv(here("data/raw_data",
                                       "Allers_2013_1.csv"),
                                  col_names = FALSE,
                                  col_types = "ccccccccc"))

mdcs_allers <- bind_rows(mdcs_allers,
                         read_csv(here("data/raw_data",
                                       "Allers_2013_2.csv"),
                                  col_names = FALSE,
                                  col_types = "ccccccccc"))

for(i in 2014:2017){
  mdcs_allers <- bind_rows(mdcs_allers,
                           read_csv(here("data/raw_data",
                                         str_c("Allers_", i, ".csv")),
                                    col_names = FALSE,
                                    col_types = "ccccccccc")
  )
}

save(mdcs_allers, 
     file = here("data/tidy_data",
                 "mdcs_allers.rda"))

# Rayam

mdcs_rayam <- c()

for(i in 2008:2017){
  mdcs_rayam <- bind_rows(mdcs_rayam,
    read_csv(here("data/raw_data",
                  str_c("Rayam_", i, ".csv")),
             col_names = FALSE))
}

save(mdcs_rayam, 
     file = here("data/tidy_data",
     "mdcs_rayam.rda"))

# Ward

mdcs_ward <- c()

for(i in 2008:2017){
  mdcs_ward <- bind_rows(mdcs_ward,
                         read_csv(here("data/raw_data",
                                        str_c("Ward_", i, ".csv")),
                                  col_names = FALSE))
}

save(mdcs_ward, 
     file = here("data/tidy_data",
                 "mdcs_ward.rda"))

# Hersl

mdcs_hersl <- c()

for(i in 2008:2017){
  mdcs_hersl <- bind_rows(mdcs_hersl,
                         read_csv(here("data/raw_data",
                                       str_c("Hersl_", i, ".csv")),
                                  col_names = FALSE))
}

save(mdcs_hersl, 
     file = here("data/tidy_data",
                 "mdcs_hersl.rda"))

# Taylor
# NOTE: searched for "Marcus R Taylor"
# NOTE: no entries for 2008-2009

mdcs_taylor <- c()

for(i in 2010:2017){
  mdcs_taylor <- bind_rows(mdcs_taylor,
                          read_csv(here("data/raw_data",
                                        str_c("Taylor_", i, ".csv")),
                                   col_names = FALSE))
}

save(mdcs_taylor, 
     file = here("data/tidy_data",
                 "mdcs_taylor.rda"))

# Clewell
# NOTE: no entries for 2008

mdcs_clewell <- c()

for(i in 2009:2017){
  mdcs_clewell <- bind_rows(mdcs_clewell,
                            read_csv(here("data/raw_data",
                                          str_c("Clewell_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_clewell, 
     file = here("data/tidy_data",
                 "mdcs_clewell.rda"))


# MDCS data workup --------------------------------------------------------


# Combine cop datasets together
bind_rows(mdcs_allers, 
          mdcs_clewell, 
          mdcs_gondo, 
          mdcs_hendrix, 
          mdcs_hersl, 
          mdcs_jenkins, 
          mdcs_rayam, 
          mdcs_taylor,
          mdcs_ward) %>%
  mutate(X8 = as.Date(X8, format = "%m/%d/%Y")) %>%
  rename(case_num = X1,
         name = X2,
         dob = X3,
         party_type = X4,
         court = X5,
         case_type = X6,
         status = X7,
         date = X8,
         caption = X9) %>%
  mutate(case_type_2 = case_when(
    case_type %in% c("CR", "Criminal") ~ "Criminal",
    case_type == "Appeal" ~ "Appeal",
    TRUE ~ "Other"
  )) %>%
  # Filter out bad Taylor names
  filter(name != "Taylor, Marcus Randolph" |
           name != "Taylor, Marcus Rezan" |
           name != "Taylor, Marcus Rezan Jr") %>%
  # filter(name != "")
  mutate(gttf_cop = case_when(
    str_detect(name, "Clewell") ~ "Clewell",
    str_detect(name, "Hersl") ~ "Hersl",
    str_detect(name, "Rayam") ~ "Rayam",
    str_detect(name, "Taylor") ~ "Taylor"
  ))



