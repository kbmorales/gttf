# Database drafts
library(tibble)
library(here)
library(readr)

cops <- tibble(
  last_name = c("Allers", "Gondo", "Hendrix", "Jenkins", "Rayam", "Ward", "Hersl", "Taylor", "Clewell"),
  first_name = c("Thomas", "Momodu", "Evodio", "Wayne", "Jemell", "Maurice", "Daniel", "Marcus", "John")
)


save(cops, 
     file = "cops_names.rda")

# Start Date - 01/01/2008
# End Date - 03/01/2017
# reading in case numbers as .csv files
# if case search is over 500 separate .csv into years
# load into /raw_data
# MIDDLE INITIAL FOR GONDO - K
# MIDDLE INITIAL FOR HENDRIX - C
# MIDDLE INITIAL FOR JENKINS - E for Earl 
