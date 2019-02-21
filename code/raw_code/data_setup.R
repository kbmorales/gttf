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

