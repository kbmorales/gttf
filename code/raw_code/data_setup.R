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

