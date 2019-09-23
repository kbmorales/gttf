library(ggplot2)
library(RStoolbox)
library(plotly)
library(raster)
library(mapview)
library(raster)
library(sp)
library(sf)
library(raster)
library(mapview)

# DISCLAIMER --------------------------------------------------------------
 
# make sure you run the first 3 sections
# in the file "code/final_code/censusData_map.R

# creating map --------------------------------------------
# get mdcs_charges_df from "data/raw_data/mdcs_charges_data.rda"
demo_markers = left_join(mdcs_charges_df, bmoredemo_markers1)

demo_markers = demo_markers %>%
  filter(lat >= "39.20") %>%
  filter(lat <= "39.37") %>%
  filter(lon <= "-76.71") %>%
  filter(lon >= "-76.53")

demo_markers = demo_markers %>% 
  dplyr::select(lon, 
                lat, 
                charge_desc_2,
                sex_id,
                race_black,
                race_cat)

# MAPPING ---------------------------------------------
rdl_square3 <- brick("products/figures/redline_square2.tif")


p <- ggplot(demo_markers, aes(x = lon, y = lat))
p = p + 
  ggRGB(rdl_square3,
        r = 1,
        g = 2,
        b = 3,
        ggLayer = TRUE) +
  geom_point(aes(x = lon, y = lat, colour = race_cat),
             alpha = 0.55) +
  coord_equal() +
  viridis::scale_color_viridis(option = "viridis",
                               discrete = TRUE) +
  labs(fill = NULL,
       x = NULL,
       y = NULL,
       title = "Redlining Map in Baltimore",
       subtitle = "Comparing GTTF Activity to Redlining Map",
       caption = 'Problem Forward LLC') +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        panel.border = element_rect(fill = NA,
                                    colour = "#cccccc"),
        plot.margin = unit(c(0, 0.25, 0.0, 0.25), "in"),
        plot.title = element_text(size = 12, face = "bold"),
        text = element_text(family = "Times New Roman", 
                            size = 8))

redline_plot <- ggplotly(p,
                         tooltip = c("colour"))

export(redline_plot,
       file = here::here("products/figures",
                         "map.webp"),
       selenium = TRUE)

# mapview -------------------------------------------
test = demo_markers

xy <- test[,c(1,2)]

spdf <- SpatialPointsDataFrame(coords = xy, data = test,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


rdl_square3 <- brick("products/figures/redline_square2.tif")
map <- viewRGB(rdl_square3,
        maxpixels = 5e+05,
        r = 1,
        g = 2,
        b = 3) +
  mapview(spdf, zcol = c("race_cat"), legend = TRUE,
          alpha = 0.5,
          lwd = 1)

mapshot(map@map, 
        file = here::here("/products/figures",
                          "redlinemap.html"),
        remove_controls = c("homeButton", "scaleBar"),
        selfcontained = FALSE)
  



## this is where the above folder should be created
htmlwidgets::saveWidget(map@map, 
                        file = paste0(getwd(), "/map.html"), 
                        selfcontained = FALSE)
