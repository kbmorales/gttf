### Cops
### kbmorales
### kbmorales@protonmail.com


# Setup -------------------------------------------------------------------

library(tidyverse)

### From chat with Zach 2020-04-08

## Indicted or charged:
# Thomas Allers
# Keith Gladstone
# Momodu Gondo
# Robert Hankard
# Evodio Hendrix
# Daniel Hersl
# Wayne Jenkins
# Craig Jester
# Ivo Louvado
# Jemell Rayam
# Victor Rivera
# Eric Snell
# Marcus Taylor
# Carmine Vignola
# Maurice Ward

indicted <- tibble(last_name = c("Allers", 
                                 "Gladstone", 
                                 "Gondo", 
                                 "Hankard", 
                                 "Hendrix",
                                 "Hersl",
                                 "Jenkins",
                                 "Jester",
                                 "Louvado",
                                 "Rayam", 
                                 "Rivera",
                                 "Snell",
                                 "Taylor",
                                 "Vignola",
                                 "Ward"
                                 ),
                   first_name = c("Thomas", 
                                  "Keith", 
                                  "Momodu", 
                                  "Robert",
                                  "Evodio", 
                                  "Daniel",
                                  "Wayne", 
                                  "Craig",
                                  "Ivo",
                                  "Jemell", 
                                  "Victor",
                                  "Eric",
                                  "Marcus",
                                  "Carmine",
                                  "Maurice" 
                                  )
                   # middle_name = c("",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "T",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "")
                   ) %>%
  mutate(status = "indicted")
  
## Other names of interest:
# Sherrod Biggers
# John Clewell
# Ian Dombroski
# Tariq Edwards
# Jason Giordano
# Ryan Guinn
# Kenneth Ivery
# Tariq Toro Munford
# Dean Palmere
# Michael Sylvester
# Thomas Wilson III
# Michael Woodlon

interest <- tibble(last_name = c("Biggers", 
                                 "Clewell", 
                                 "Dombroski", 
                                 "Edwards", 
                                 "Giordano",
                                 "Guinn",
                                 "Ivery",
                                 "Munford",
                                 "Palmere",
                                 "Sylvester", 
                                 "Wilson", # III
                                 "Woodlon"
                                 ),
                   first_name = c("Sherrod", 
                                  "John", 
                                  "Ian", 
                                  "Tariq",
                                  "Jason", 
                                  "Ryan",
                                  "Kenneth", 
                                  "Tariq Toro",
                                  "Dean",
                                  "Michael", 
                                  "Thomas",
                                  "Michael" 
                                  )
                   # middle_name = c("",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 "",
                   #                 ""
                   #                 )
                   ) %>%
  mutate(status = "interest")

# Combine
cops <- bind_rows(indicted,
                  interest)

# to upper
cops <- cops %>% 
  mutate(last_name = toupper(last_name),
         first_name = toupper(first_name))

cops <- cops %>% 
  mutate(name = str_c(last_name,
                      ", ",
                      first_name)
  ) %>% 
  select(name, last_name, first_name, status)

## Want to add indicted date?

rm(indicted,
   interest)

save(cops, 
     file = here::here("data",
                       "cops_names.rda"))
