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


# MDCS data clean --------------------------------------------------------


# Combine cop datasets together
mdcs_cops_df <- bind_rows(mdcs_allers,
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
# different cases to not lose data
  mutate(case_type_2 = case_when(
    case_type %in% c("CR", "Criminal") ~ "Criminal",
    case_type == "Appeal" ~ "Appeal",
    TRUE ~ "Other"
  )) %>%
  # Filter out bad Allers names
  filter(name != "Allers, Thomas T") %>%
  # Filter out bad Jenkins names
  filter(name != "Jenkins, Wayne A",
           name != "Jenkins, Wayne Anthony",
           name != "Jenkins, Wayne Edward II",
           name != "Jenkins, Wayne Jarrell",
           name != "Jenkins, Wayne L",
           name != "Jenkins, Wayne Lee",
           name != "Jenkins, Wayne Lee Jr",
           name != "Jenkins, Wayne M II",
           name != "Jenkins, Wayne Maurice",
           name != "Jenkins, Wayne Maurice II",
           name != "Jenkins, Wayne T") %>%
  # Filter out bad Taylor names
  filter(name != "Taylor, Marcus Randolph",
           name != "Taylor, Marcus Rezan" |
           name != "Taylor, Marcus Rezan Jr") %>%
  # Filter out bad Ward names
  filter(name != "Ward, Maurice A",
           name != 	"Ward, Maurice A Jr",
           name != "Ward, Maurice A Jr.",
           name != "Ward, Maurice A Sr",	
           name != "Ward, Maurice A. Jr",
           name != "Ward, Maurice Alexander Jr",
           name != "Ward, Maurice D",
           name != "Ward, Maurice Devon",
           name != "Ward, Maurice Douglas",
           name != "Ward, Maurice Ethan",	
           name != "Ward, Maurice L",
           name != "Ward, Maurice Lanier",
           name != "Ward, Maurice R",
           name != "Ward, Maurice Reginald",
           name != "Ward, Maurice S",
           name != "Ward, Maurice Xavier") %>%
  mutate(gttf_cop = case_when(
    str_detect(name, "Allers") ~ "Allers",
    str_detect(name, "Clewell") ~ "Clewell",
    str_detect(name, "Gondo") ~ "Gondo",
    str_detect(name, "Hendrix") ~ "Hendrix",
    str_detect(name, "Hersl") ~ "Hersl",
    str_detect(name, "Jenkins") ~ "Jenkins",
    str_detect(name, "Rayam") ~ "Rayam",
    str_detect(name, "Taylor") ~ "Taylor",
    TRUE ~ "Ward"
  ))

save(mdcs_cops_df, 
     file = here("data/tidy_data",
                 "mdcs_cops_df.rda"))

### Clewell?

# Removing Clewell from dataset
mdcs_cops_df <- mdcs_cops_df %>% filter(gttf_cop != "Clewell")

nrow(mdcs_cops_df)


# MDCS Data Workup --------------------------------------------------------


# Identify cases with multiple cops associated with them
multicop_cases <- mdcs_cops_df %>% 
  group_by(case_num) %>%
  summarise(count = n_distinct(gttf_cop)) %>%
  filter(count > 1) %>% 
  pull(case_num)

# Filter out multi cop cases
single_cop_cases <- mdcs_cops_df %>% 
  anti_join(tibble(case_num = multicop_cases))

# Create db of multi-cop cases
multicop_cases <- mdcs_cops_df %>% 
  semi_join(tibble(case_num = multicop_cases))

# See how many cops are in each case by case number
multicop_cases %>%
  group_by(case_num) %>%
  summarise(n = n_distinct(gttf_cop))

# Change multcop cases to "Multiple", keep only one row from each
multicop_cases <- multicop_cases %>% 
  mutate(gttf_cop = "Multiple") %>%
  group_by(case_num) %>%
  filter(row_number()==1)

# Rejoin dataset together 
mdcs_cops_df <- bind_rows(single_cop_cases, multicop_cases)

# Examine multiple cases leftover
multicase_nums <- mdcs_cops_df %>% count(case_num) %>% filter(n > 1) %>% pull(case_num)

# Seems like its safe to get rid of them
mdcs_cops_df <- mdcs_cops_df %>% 
  group_by(case_num) %>% 
  filter(row_number()== 1)

# Order cops alphabetically with multiple last
mdcs_cops_df$gttf_cop <- factor(mdcs_cops_df$gttf_cop, levels = c("Allers",
                                                                  "Gondo",
                                                                  "Hendrix",
                                                                  "Hersl",
                                                                  "Jenkins",
                                                                  "Rayam",
                                                                  "Taylor",
                                                                  "Ward",
                                                                  "Multiple")
                                )

# Filter out "Other" case types
mdcs_cops_df <- mdcs_cops_df %>%
  filter(case_type_2 != "Other")


# Final stats:
nrow(mdcs_cops_df)
mdcs_cops_df %>% group_by(gttf_cop) %>% count()

