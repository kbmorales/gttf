library(devtools)
library(ggplot2)
library(dplyr)
library(stringr)
library(maps)
library(mapdata)
library(stringi)
library(sp)
library(ggspatial)
library(plotly)
library(rgdal)
# DISCLAIMER ------------------------------------------

# please run "code/final_code/geocoding.R" before running this file

# MAP WORK UP ------------------------------------------------------------
# getting map outline
maryland <- map_data("county",
                     "maryland")
# filtering for only baltimore area
baltimore = maryland[str_detect(maryland$subregion, "baltimore"), ]

# TURNING SPATIAL OBJECT INTO DATAFRAME
# neighborhood data
bmore_hoods <- readOGR("data/raw_data/bmore_hoods/Neighborhoods.kmz")

# add new column to have rownames of data
bmore_hoods@data$id <- rownames(bmore_hoods@data)

# create a df from spatial object
neighborhoods <- fortify(bmore_hoods, region = "id")

# merge "forified data' with data from sp object
bmore_hoods <- plyr::join(neighborhoods, bmore_hoods@data, by = "id")

# only want markers within baltimore regions
bmoredemo_markers1 = bmoredemo_markers1 %>%
  filter(lat >= "39.21") %>%
  filter(lat <= "39.73") %>%
  filter(lon >= "-76.34") %>%
  filter(lon <= "-76.90")

# CREATING MAP  ---------------------------------------------------

# creating plot environment
bmore_plot = ggplot() +
  geom_polygon(data = baltimore,
               aes(x = long, 
                   y = lat, 
                   group = group),
               fill = NA,
               color = "black") +
  geom_polygon(data = bmore_hoods,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "gray50") +
  coord_equal()
    # coord_fixed(1.3)


# white, black, other
bmore_plot + 
  geom_point(data = bmoredemo_markers1,
                        aes(x = lon,
                            y = lat,
                            group = case_num,
                            colour = factor(race_cat))) +
  geom_jitter(width = 0.5, height = 0.5) +
  viridis::scale_color_viridis(option = "viridis",
                               discrete = TRUE) +
  theme(legend.position = "bottom") +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Races/Ethncities of Individuals GTTF",
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
wbo_plot <- ggplotly(tooltip = "colour")

# black vs. nonblack
bmore_plot + 
  geom_point(data = bmoredemo_markers1,
             aes(x = lon,
                 y = lat,
                 group = case_num,
                 colour = factor(race_black))) +
  geom_jitter(width = 0.5, height = 0.5) +
  viridis::scale_color_viridis(option = "viridis",
                              discrete = TRUE) +
  theme(legend.position = "bottom") +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Races/Ethncities of Individuals GTTF",
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
bvnb_plot <- ggplotly(tooltip = "colour")


# MALE/FEMALE
bmore_plot + 
  geom_point(data = bmoredemo_markers1,
             aes(x = lon,
                 y = lat,
                 group = case_num,
                 colour = sex_id)) +
  geom_jitter(width = 0.5, height = 0.5) +
  theme(legend.position = "bottom") +
  viridis::scale_fill_viridis(option = "magma") +
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
sex_plot <- ggplotly(tooltip = "colour")


# AGE
bmore_plot + 
  geom_point(data = bmoredemo_markers1,
             aes(x = lon,
                 y = lat,
                 group = case_num),
                 colour = bmoredemo_markers1$age_yrs) +
  geom_jitter(width = 0.5, height = 0.5) +
  theme(legend.position = "bottom") +
  viridis::scale_color_viridis(option = "viridis") +
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
age_plot <- ggplotly(tooltip = "colour")

