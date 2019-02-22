# Database drafts
library(tibble)
library(here)
library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

cops <- tibble(
  last_name = c("Allers", "Gondo", "Hendrix", "Jenkins", "Rayam", "Ward", "Hersl", "Taylor", "Clewell"),
  first_name = c("Thomas", "Momodu", "Evodio", "Wayne", "Jemell", "Maurice", "Daniel", "Marcus", "John")
)


save(cops, 
     file = "data/tidy_data/cops_names.rda")

save(mdcs_cops_df,
     file = "data/tidy_data/mdcs_cops_df.rda")

# Start Date - 01/01/2008
# End Date - 03/01/2017
# reading in case numbers as .csv files
# if case search is over 500 separate .csv into years
# load into /raw_data
# MIDDLE INITIAL FOR GONDO - K
# MIDDLE INITIAL FOR HENDRIX - C
# MIDDLE INITIAL FOR JENKINS - E for Earl 
# MIDDLE INITIAL FOR Ward - K; is a jr

######################## compiling .csv files into 1 with Ken's code

# Allers, Thomas ----------------------------------------------------------


mdcs_allers <- c()

for(i in 2008:2017){
  mdcs_allers <- bind_rows(mdcs_allers,
                          read_csv(here("data/raw_data",
                                        str_c("Allers_", i, ".csv")),
                                   col_names = FALSE))
}

save(mdcs_allers, 
     file = here("data/tidy_data",
                 "mdcs_allers.rda"))

# Gondo, Momodu -----------------------------------------------------------

mdcs_gondo <- c()

for(i in 2008:2017){
  mdcs_gondo <- bind_rows(mdcs_gondo,
                           read_csv(here("data/raw_data",
                                         str_c("Gondo_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_gondo, 
     file = here("data/tidy_data",
                 "mdcs_gondo.rda"))


# Hendrix, Evodio ---------------------------------------------------------

mdcs_hendrix <- c()

for(i in 2008:2017){
  mdcs_hendrix <- bind_rows(mdcs_hendrix,
                           read_csv(here("data/raw_data",
                                         str_c("Hendrix_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_hendrix, 
     file = here("data/tidy_data",
                 "mdcs_hendrix.rda"))


# Jenkins, Wayne ----------------------------------------------------------

mdcs_jenkins <- c()

for(i in 2008:2017){
  mdcs_jenkins <- bind_rows(mdcs_jenkins,
                           read_csv(here("data/raw_data",
                                         str_c("Jenkins_", i, ".csv")),
                                    col_names = FALSE))
}

save(mdcs_jenkins, 
     file = here("data/tidy_data",
                 "mdcs_jenkins.rda"))






# MDCS mark up ------------------------------------------------------------
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
  # Filter out bad Taylor names
  filter(name != "Taylor, Marcus Randolph" &
           name != "Taylor, Marcus Rezan" &
           name != "Taylor, Marcus Rezan Jr" &
           name != "Allers, Thomas T" &
           name != "Jenkins, Wayne A" &
           name != "Jenkins, Wayne Anthony" &
           name != "Jenkins, Wayne Edward II" &
           name != "Jenkins, Wayne Jarrell" &
           name != "Jenkins, Wayne L" &
           name != "Jenkins, Wayne Lee" &
           name != "Jenkins, Wayne Lee Jr" &
           name != "Jenkins, Wayne M II" &
           name != "Jenkins, Wayne Maurice" &
           name != "Jenkins, Wayne Maurice II" &
           name != "Jenkins, Wayne T" &
           name != "Ward, Maurice A" &
           name != "Ward, Maurice A Jr" &
           name != "Ward, Maurice A Jr." &
           name != "Ward, Maurice A Sr" &
           name != "Ward, Maurice A. Jr" &
           name != "Ward, Maurice Alexander Jr" &
           name != "Ward, Maurice D" &
           name != "Ward, Maurice Devon" &
           name != "Ward, Maurice Douglas" &
           name != "Ward, Maurice Ethan" &
           name != "Ward, Maurice L" &
           name != "Ward, Maurice Lanier" &
           name != "Ward, Maurice R" &
           name != "Ward, Maurice Reginald" &
           name != "Ward, Maurice S" &
           name != "Ward, Maurice Xavier" 
           ) %>%
  # filter(name != "")
  mutate(gttf_cop = case_when(
    str_detect(name, "Clewell") ~ "Clewell",
    str_detect(name, "Hersl") ~ "Hersl",
    str_detect(name, "Rayam") ~ "Rayam",
    str_detect(name, "Taylor") ~ "Taylor",
    str_detect(name, "Gondo") ~ "Gondo",
    str_detect(name, "Allers") ~ "Allers",
    str_detect(name, "Jenkins") ~ "Jenkins",
    str_detect(name, "Ward") ~ "Ward",
    str_detect(name, "Hendrix") ~ "Hendrix"
    ))



# Let's create plots ------------------------------------------------------

# distinctive cases
mdcs_cops_df %>%
  group_by(gttf_cop) %>%
  summarise(case_count = n()) 

# distinctive case numbers ~ n_distinct(case_num)
# gttf_cop     case_count
# <chr>            <int>
# 1 Allers         1147
# 2 Clewell        1461
# 3 Gondo           689
# 4 Hendrix         791
# 5 Hersl          2413
# 6 Jenkins         987
# 7 Rayam           770
# 8 Taylor          516
# 9 Ward           1660

# total case numbers ~ n()
# A tibble: 9 x 2
# gttf_cop     case_count
# <chr>            <int>
# 1 Allers         1415
# 2 Clewell        1695
# 3 Gondo           697
# 4 Hendrix         910
# 5 Hersl          2561
# 6 Jenkins        1078
# 7 Rayam           848
# 8 Taylor          545
# 9 Ward           1943


# trying to create a graph that lists the date[year] the case
# was filed, who filed, and status of case
mdcs_cops_df %>%
  filter(case_type_2 == "Criminal") %>%
ggplot(aes(x = status,
           y = date)) +
  geom_col(aes(fill = gttf_cop),
           position = "dodge") +
  theme(text = element_text(size=15)) +
  theme(legend.position="bottom") +
  ggtitle("Criminal Case Records Status") +
  labs(x = "Status",
       y = "Date",
       fill = "Cop Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  # + 
  # geom_point(shape=21,color="grey",size=2) + 
  # geom_smooth(aes(color = gttf_cop),
  #             size = 1.5,
  #             linetype = "longdash",
  #             alpha = 0.5,
  #             method = "lm",
  #             fullrange = TRUE,
  #             se = FALSE,
  #             show.legend = FALSE)
  # 

  
 