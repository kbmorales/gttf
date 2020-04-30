library(tidyverse)
library(ggplot2)
library(raster)
# for plotting raster image
library(rasterVis)
library(colorRamps)
# library(magick)
library(RStoolbox)

# DISCLAIMER -----------------------------------

# make sure you run the first 3 sections
# in the file "code/final_code/censusData_map.R

# creating map --------------------------------------------
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

# MAP -------------------------------------------------

charges_plot <- ggplot() +
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

# experimenting  with raster -----------------------------------------
library(rasterVis)
library(ggplot2)

# reading in raster objects
redline <- raster("/cloud/project/products/figures/redlinemap.tif")

# second set
redline_squished <- raster("/cloud/project/products/figures/redline_squished.tif")
rdl_spdf <- as(redline_squished, "SpatialPixelsDataFrame")
rel <- as.data.frame(rdl_spdf)

# set 3
redline_modified <- raster("/cloud/project/products/figures/redline_modified.tif")
rdl_spdf <- as(redline_modified, "SpatialPixelsDataFrame")
rel <- as.data.frame(rdl_spdf)

# setting palettes
pal <- c("darkolivegreen", "paleturquoise4", "goldenrod2", "darksalmon", "grey78", "grey95")

# second way
# myTheme <- rasterTheme(region = rep(rev(rev(colorRamps::matlab.like(n=12))),c(1,1,1,1,1,1,1,1,1,1,1,1)))

# third way
# myTheme <- color.theme(symbol = brewer.pal(n = 6, ))

gplot(redline_modified, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) +
  geom_point(data = demo_markers,
             aes(x = lon,
                 y = lat,
                 group = NULL)) +
  facet_wrap(~ variable) +
  # trying to match colors of map
  scale_fill_gradient(low = "red", high = "white") +
  coord_equal(ylim(39.20, 39.37))


# read in data with raster::brick
rdl <- brick("products/figures/redlinemap.tif")


rdl_plot = plotRGB(rdl) 

rdl_plot +  
  ggplot() +
  geom_point(data = demo_markers,
             aes(x = lon,
                 y = lat))

# using regular ggplot
plot <- ggplot(data = demo_markers,
               aes(x = lon,
                   y = lat))  +
  geom_point()

# let's see this as a dataset
rdlDF <- ggRGB(rdl, ggObj = FALSE)

plot +
  ggRGB(rdl, ggLayer = TRUE) +
  coord_equal()

# checkpoint ---------------------------------------------------
# using RStoolbox here ~ turns into ggplot object ~ keeps colors
rdl <- brick("products/figures/redlinemap.tif")
rdl_spdf <- as(rdl, "SpatialPixelsDataFrame")
rel <- as.data.frame(rdl)

ggplot() +
  ggRGB(img = rdl,
        r = 1,
        g = 2,
        b = 3,
        ggLayer = TRUE,
        stretch = 'hist') +
  coord_equal()

line

# let's see if me changing lon/lat to x/y will help

demo_markers = demo_markers %>%
  # tidyverse is loaded, dplyr:select is muted 
  dplyr::select(lon, lat, case_type_2, case_num, race_cat, race_black, sex_id, age_yrs)

# renaming columns
names(demo_markers)[1] <- "x"
names(demo_markers)[2] <- "y"

p <- ggplot(demo_markers, aes(x, y))
p + ggRGB(rdl, ggLayer = TRUE) +
  geom_point(aes(x,y), 
             fill = "blue",
             alpha = 0.4) +
  coord_equal()

# experimenting with ggplot -----------------------------------------------
# using ggplot
ggplot() +
  geom_raster(data = rel, 
              aes_string(x = "x", y = "y", alpha = "redline_squished")) +
  scale_alpha(name = "", range = c(0.5, 1), guide = F) +
  # geom_point(data = demo_markers, 
  #            aes(x = lon, 
  #                y = lat), 
  #            colour = "black", 
  #            size = 3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_fill_identity()



# using rasterVis
gplot(redline_squished, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) +
  geom_point(data = demo_markers,
             aes(x = lon,
                 y = lat,
                 group = NULL)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = "red", high = "white")+
  coord_equal()




# charges_plot =
charges_plot +
  geom_rect(data = redline)
# geom_point(data = demo_markers,
#            aes(x = lon,
#                y = lat,
#                fill = as.factor(demo_markers$case_type_2),
#                group = NULL),
#            size = 2,
#            shape = 21,
#            colour = "black") 

# experimenting with magick --------------------------
library(ggplot2)
library(magick)
library(grid)

# doesnt work file too large
# plot(as.raster(redline))


# using grid
qplot(lon,lat,
      data = demo_markers) +
  rasterImage(redline)


# using tmap -------------------------------
library(tmap)
library(tmaptools)

tm_basemap(carto) +
  tm_shape(redline) +
  tm_raster()

# only for raster BRICK objects
# not raster layer

