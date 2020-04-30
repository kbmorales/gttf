# BCPD Arrest Data
# Source: Open City Baltimore data
# https://data.baltimorecity.gov/Public-Safety/BPD-Arrests/3i3v-ibrt/data

# Does not include arrests of minors
# Arrests date back to 2014


# Setup -------------------------------------------------------------------


# Required libraries
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(scales)
library(viridis)

# Load BCPD arrest data
arrests <- read_csv(here::here("data/raw_data/arrests_data",
                    "BPD_Arrests.csv")) %>%
  mutate(ArrestDate = as.Date(ArrestDate,
                              format = "%m/%d/%Y"))


# Compare BPD to GTTF -----------------------------------------------------


# Calculate average yearly arest rates by race
arrests %>% 
  mutate(Year = year(ArrestDate)) %>%
  group_by(Year) %>% 
  count(Race) %>% 
  mutate(perc = n/sum(n) * 100) %>%
  group_by(Race) %>% 
  summarise(avg = mean(perc))

# Construct comparison datasets
arrests_comp <- arrests %>% 
  mutate(Year = year(ArrestDate)) %>%
  filter(Race != "U") %>%
  mutate(race_black = case_when(
    Race == "B" ~ "BLACK",
    TRUE ~ "NONBLACK"
  )) %>%
  group_by(Year) %>% 
  filter(Year <= 2016) %>%
  count(race_black) %>% 
  mutate(perc = n/sum(n) * 100) %>%
  mutate(type = "BPD")

# MDCS comparison dataset
mdcs_comp <- mdcs_df %>%
  mutate(Year = year(date)) %>%
  filter(Year >= 2014 & Year <= 2016) %>%
  group_by(Year) %>% 
  count(race_black) %>% 
  mutate(perc = n/sum(n) * 100) %>%
  mutate(type = "GTTF")

arrests_comp <- arrests_comp %>%
  bind_rows(mdcs_comp)

arrests_comp %>%
  ggplot(aes(x = Year, y = n, fill = race_black)) +
  geom_col(position = "fill") +
  facet_wrap(type ~ .) +
  theme_minimal() +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis(discrete = T) +
  labs(y = "Percentage of arrests",
       fill = "Race",
       title = "Arrests / Case Comparison by Race: BPD Overall vs. GTTF",
       subtitle = "For years in which data overlap",
       caption = "Dashed black line indicates overall proportion of nonblack race among BPD arrests\nDashed red line indicates nonblack proportion of Baltimore city residents"
       ) + 
  coord_flip() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0.178, linetype = "dashed") +
  geom_hline(yintercept = 1 - 0.6374, linetype = "dashed", color = "red")


# Chi2 --------------------------------------------------------------------


# Construct comparison datasets
arrests_comp <- arrests %>% 
  mutate(Year = year(ArrestDate)) %>%
  filter(Race != "U") %>%
  mutate(race_black = case_when(
    Race == "B" ~ "BLACK",
    TRUE ~ "NONBLACK"
  )) %>%
  filter(Year <= 2016 & Year >= 2014) %>%
  select(race_black) %>%
  mutate(type = "BPD")

# MDCS comparison dataset
mdcs_comp <- mdcs_df %>%
  mutate(Year = year(date)) %>%
  filter(Year >= 2014 & Year <= 2016) %>%
  select(race_black) %>%
  mutate(type = "GTTF")

arrests_comp <- arrests_comp %>%
  bind_rows(mdcs_comp)

chisq.test(table(arrests_comp$race_black, arrests_comp$type))
