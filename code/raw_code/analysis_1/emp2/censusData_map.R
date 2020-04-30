library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(mapview)
library(leaflet)
library(broom)
library(sp)
library(sf)
library(tidyverse)
library(ggmap)
library(maps)
library(tmap)
library(tmaptools)
library(htmlwidgets)
library(viridis)
# for compass on the plots
library(ggsn)
library(plotly)

# DISCLAIMER --------------------------------------------------------------

# PLEASE MAKE SURE TO RUN "code/final_code/geocoding.R" before continuing!


# MAP WORK UP ~ integration 1 --------------------------------------

# read in the data
census_map <- read_csv("data/raw_data/census_pop.csv")

# separate then join again
# have to turn the_geom colum separate lat/lon
census_map = st_as_sf(census_map, wkt = "the_geom")

# step three
census_map = as_Spatial(census_map, 
                        cast = TRUE)


# MAP WORK UP ~ integration 2 --------------------------------------

# add new column to have rownames of data
census_map@data$id <- rownames(census_map@data)

# create a df from spatial object
census_mapDF <- fortify(census_map, region = "id")

# merge "forified data' with data from sp object
census_map <- plyr::join(census_mapDF, census_map@data, by = "id")

census_map = census_map %>%
  mutate(vacant_house_perc = round((census_map$Vacant/census_map$Housing)*100)) %>%
  mutate(occcupied_house_perc = round((census_map$Occupied/census_map$Housing)*100)) %>%
  mutate(BlkAfAm_perc = round((census_map$Blk_AfAm/census_map$Population)*100))

# SETTING MAP BOUNDARIES --------------------------------------------------
# only want markers within baltimore regions
bmoredemo_markers1 = bmoredemo_markers1 %>%
  filter(lat >= "39.21") %>%
  filter(lat <= "39.73") %>%
  filter(lon >= "-76.34") %>%
  filter(lon <= "-76.90")


# creating plots -----------------------------------------------------------

# Percentage of AfAm
blkafam_plot <- ggplot() +
  geom_polygon(data = census_map,
               aes(x = long + 0.005,
                   y = lat - 0.002),
               color = "grey50",
               fill = "grey50",
               size = 0.2) +
  geom_polygon(data = census_map,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut_number(BlkAfAm_perc, 10)),
               color = "gray10",
               size = 0.2) +
  coord_equal() +
  viridis::scale_fill_viridis(option = "viridis",
                              discrete = TRUE) +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Percentage of African Americians in Baltimore City Neighborhoods",
       subtitle = "Percentage AfAm/Total Population for each district",
       caption = 'Problem Forward LLC') +
  theme_minimal() +
  theme(legend.position = c(0, 0.25),
        legend.text = element_text(size = 10),
        panel.border = element_rect(fill = NA,
                                    colour = "#cccccc"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times New Roman", 
                            size = 8))

# interactive plot
vachouse_plot <- ggplotly(blkafam_plot)

# Percentage of Vacant Housing 
blkafam_plot <- ggplot() +
  geom_polygon(data = census_map,
               aes(x = long + 0.005,
                   y = lat - 0.002),
               color = "grey50",
               fill = "grey50",
               size = 0.2) +
  geom_polygon(data = census_map,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut_number(vacant_house_perc, 10)),
               color = "gray10",
               size = 0.2) +
  coord_equal() +
  viridis::scale_fill_viridis(option = "viridis",
                              discrete = TRUE) +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Percentage of Vacant Houses in Baltimore City Neighborhoods",
       subtitle = "Percentage VacHouse/Total Housing for each district",
       caption = 'Problem Forward LLC') +
  theme_minimal() +
  theme(legend.position = c(0, 0.25),
        legend.text = element_text(size = 10),
        panel.border = element_rect(fill = NA,
                                    colour = "#cccccc"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times New Roman", 
                            size = 8))

# interactive plot
vachouse_plot <- ggplotly(vachouse_plot)


# Percentage of Occupied Housing 
occhouse_plot <- ggplot() +
  geom_polygon(data = census_map,
               aes(x = long + 0.005,
                   y = lat - 0.002),
               color = "grey50",
               fill = "grey50",
               size = 0.2) +
  geom_polygon(data = census_map,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut_number(occcupied_house_perc, 10)),
               color = "gray10",
               size = 0.2) +
  coord_equal() +
  viridis::scale_fill_viridis(option = "viridis",
                              discrete = TRUE) +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Percentage of Occupied Houses in Baltimore City Neighborhoods",
       subtitle = "Percentage OccHouse/Total Housing for each district",
       caption = 'Problem Forward LLC') +
  theme_minimal() +
  theme(legend.position = c(0, 0.25),
        legend.text = element_text(size = 10),
        panel.border = element_rect(fill = NA,
                                    colour = "#cccccc"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times New Roman", 
                            size = 8))


# interactive plot
vachouse_plot <- ggplotly(vachouse_plot)


# after this script has run,
# please go to "code/final_code/chargesData_map.R" 