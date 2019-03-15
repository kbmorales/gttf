# mapping charges data
load("data/tidy_data/mdcs_charges_data.rda")

# taking a look at the data
glimpse(mdcs_charges_df)
colnames(mdcs_charges_df)
View(mdcs_charges_df)

# mdcs_charges_df$charges_desc == "descripion of charges"

# can I join with existing datasets?
head(mdcs_charges_df$case_num, 6)
nrow(mdcs_charges_df)
# 12,764

head(bmoredemo_markers1$case_num, 6)
nrow(bmoredemo_markers1)
# 6906

leaflet() %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.' ) %>%
  fitBounds(-76.89493, 39.19533, -76.32139, 39.72115) %>%
  addCircles(data = demo_markers,
             lng = ~lon,
             lat = ~lat,
             # group = "Age",
             weight = 2,
             radius = 25,
             stroke = TRUE,
             fillOpacity = 0.5,
             # color = ~pal(age_yrs),
             label = as.character(demo_markers$charge_desc),
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE))




# putting multiple labels on a map
# this doesn't work because it still posts NAs,
# will use the one below
library( htmltools )
# map2 = leaflet( demo_markers ) %>%
#   addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
#            attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.' ) %>%
#   fitBounds(-76.89493, 39.19533, -76.32139, 39.72115) %>%
#   addCircles( lng = ~lon,
#               lat = ~lat,
#               fillColor = 'darkBlue',
#               radius = 25,
#               stroke = TRUE,
#               fillOpacity = 0.8,
#               label = HTML( paste0( '<p>',
#                                     demo_markers$charges_desc,
#                                     ', ',
#                                     demo_markers$charge_jailtime,
#                                     '</p><p>',
#                                     demo_markers$charge_credit_timeserved,
#                                     '</p>' )),
#               options = markerClusterOptions(removeOutsideVisibleBounds = TRUE))
# 

# creating data set
demo_markers = left_join(mdcs_charges_df, bmoredemo_markers1)

head(demo_markers$full_address, 10)


# at least puts them in order for to mess with
demo_markers = demo_markers %>%
  select(lon, lat, charge_desc, case_num, charge_desc_2) %>%
  group_by(case_num)

nrow(demo_markers)
# 13,348

# checks for all different charges
demo_markers = demo_markers %>% 
  ungroup() %>%
  # select(charge_desc_2) %>%
  filter(!is.na(lon)) %>%
  filter(!is.na(lat))
  # count(charge_desc_2)
nrow(demo_markers)
# 11,525

  
demo_markers %>% 
  ungroup() %>%
  select(charge_desc_2) %>%
  count(charge_desc_2)


# way 2
labs <- lapply(seq(nrow(demo_markers)), function(i) {
  paste0( '<p>', 'Charges: ', demo_markers[i, "charge_desc"], '<p></p>', 
          'Jailtime(yrs): ', demo_markers[i, "charge_jailtime"], '<p></p>', 
          'Time Served(yrs): ', demo_markers[i, "charge_credit_timeserved"],'</p>' ) 
  # if(is.na(demo_markers[i, ])) {
  #   next;
  #   } 
  })

map2 = leaflet( demo_markers ) %>%
  addTiles() %>%
  addCircles(lng = ~lon, 
             lat = ~lat, 
             fillColor = 'darkBlue', 
             radius = 25, 
             stroke = TRUE, 
             fillOpacity = 0.8,
             label = lapply(labs, HTML))

map2


# # A tibble: 11 x 2
#   charge_desc_2                       n
#   <chr>                             <int>
# 1 Assault                            226
# 2 CDS Distribituion / Manufacture   2957
# 3 CDS Paraphernalia Possession       571
# 4 Disorderly Conduct                  37
# 5 Firearms-related                  2410
# 6 Marijuana Possession              1353
# 7 Non-Marijuana Possession          3305
# 8 Other                              301
# 9 Resisting Arrest                    80
# 10 Theft / Burglary / Robbery        222
# 11 Trespassing                        63


# per ken's insight we are going to create new map
# future note: figure out way to toggle between maps!

# for whatever reason the map will correctly plot
# desired outcome but there isn't an available way
# have to create label vars

# practice map vars -------------------------------------------------------
assaults <- subset(demo_markers, charge_desc_2 == "Assault")
assault <- lapply(seq(nrow(assaults)), function(i) {
  paste0('<p>', assaults[i, "charge_desc_2"], '</p>',
         assaults[i, "case_num"], '</p>')
  })

cds_dists <- subset(demo_markers, charge_desc_2 == "CDS Distribituion / Manufacture")
cds_dist <- lapply(seq(nrow(cds_dists)), function(i) {
  paste0('<p>', cds_dists[i, "charge_desc_2"], '</p>',
         cds_dists[i, "case_num"], '</p>')
}) 

cds_paras <- subset(demo_markers, charge_desc_2 == "CDS Paraphernalia Possession")
cds_para <- lapply(seq(nrow(cds_paras)), function(i) {
  paste0('<p>', cds_paras[i, "charge_desc_2"], '</p>',
         cds_paras[i, "case_num"], '</p>')
})

conducts <- subset(demo_markers, charge_desc_2 == "Disorderly Conduct")
conduct <- lapply(seq(nrow(conducts)), function(i) {
  paste0('<p>', conducts[i, "charge_desc_2"], '</p>',
         conducts[i, "case_num"], '</p>')
})

firearms <- subset(demo_markers, charge_desc_2 == "Firearms-related")
firearm <- lapply(seq(nrow(firearms)), function(i) {
  paste0('<p>', firearms[i, "charge_desc_2"], '</p>',
         firearms[i, "case_num"], '</p>')
})

weeds <- subset(demo_markers, charge_desc_2 == "Marijuana Possession")
weed <- lapply(seq(nrow(weeds)), function(i) {
  paste0('<p>', weeds[i, "charge_desc_2"], '</p>',
         weeds[i, "case_num"], '</p>')
})

not_weeds <- subset(demo_markers, charge_desc_2 == "Non-Marijuana Possession")
not_weed <- lapply(seq(nrow(not_weeds)), function(i) {
  paste0('<p>', not_weeds[i, "charge_desc_2"], '</p>',
         not_weeds[i, "case_num"], '</p>')
})

others <- subset(demo_markers, charge_desc_2 == "Other")
other <- lapply(seq(nrow(others)), function(i) {
  paste0('<p>', others[i, "charge_desc_2"], '</p>',
         others[i, "case_num"], '</p>')
})


resists <- subset(demo_markers, charge_desc_2 == "Resisting Arrest")
resist <- lapply(seq(nrow(resists)), function(i) {
  paste0('<p>', resists[i, "charge_desc_2"], '</p>',
         resists[i, "case_num"], '</p>')
})

thefts <- subset(demo_markers, charge_desc_2 == "Theft / Burglary / Robbery")
theft <- lapply(seq(nrow(thefts)), function(i) {
  paste0('<p>', thefts[i, "charge_desc_2"], '</p>',
         thefts[i, "case_num"], '</p>')
})

trespasses <- subset(demo_markers, charge_desc_2 == "Trespassing")
trespass <- lapply(seq(nrow(trespasses)), function(i) {
  paste0('<p>', trespasses[i, "charge_desc_2"], '</p>',
         trespasses[i, "case_num"], '</p>')
})
# practice map -----------------------------------------------------------

leaflet(demo_markers,
        options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.',
           options = tileOptions(updateWhenIdle = TRUE)) %>%
  fitBounds(-76.89493, 39.19533, -76.32139, 39.72115) %>%
  addCircles(data = bmoredemo_markers1,
             lng = ~lon,
             lat = ~lat,
             group = "Points",
             # popup = bmoredemo_markers1$case_num,
             weight = 3,
             radius = 40,
             stroke = TRUE,
             fillOpacity = 0.8,
             color = "#ffffff",
             # label = ~ c(as.character(bmoredemo_markers1$case_num),
             #             as.character(bmoredemo_markers1$charge_desc)),
             options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)) %>%
  addCircles(lng = ~assaults$lon, 
             lat = ~assaults$lat, 
             color = '#008c96',
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Assault",
             # options = markerClusterOptions(spiderfyOnMaxZoom = TRUE),
             highlightOptions = highlightOptions(color='#008c96', bringToFront = TRUE),
             label = ~lapply(assault,HTML)) %>%
  addCircles(lng = ~cds_dists$lon, 
             lat = ~cds_dists$lat, 
             color = '#a35700', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "CDS Distribituion / Manufacture",
             highlightOptions = highlightOptions(color='#a35700', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(cds_dist,HTML)) %>%
  addCircles(lng = ~cds_paras$lon, 
             lat = ~cds_paras$lat, 
             color = '#af9cab', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "CDS Paraphernalia Possession",
             highlightOptions = highlightOptions(color='#af9cab', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(cds_para,HTML)) %>%
  addCircles(lng = ~conducts$lon, 
             lat = ~conducts$lat, 
             color = '#2b7a00', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Disorderly Conduct",
             highlightOptions = highlightOptions(color='#2b7a00', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(conduct,HTML)) %>%
  addCircles(lng = ~firearms$lon, 
             lat = ~firearms$lat, 
             color = '#8a9e03', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Firearms-related",
             highlightOptions = highlightOptions(color='#8a9e03', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(firearm,HTML)) %>%
  addCircles(lng = ~weeds$lon, 
             lat = ~weeds$lat, 
             color = '#874e65', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Marijuana Possession",
             highlightOptions = highlightOptions(color='#874e65', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(weed,HTML)) %>%
  addCircles(lng = ~not_weeds$lon, 
             lat = ~not_weeds$lat, 
             color = '#a39900', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Non-Marijuana Possession",
             highlightOptions = highlightOptions(color='#a39900', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(not_weed,HTML)) %>%
  addCircles(lng = ~others$lon, 
             lat = ~others$lat, 
             color = '#512235', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Other",
             highlightOptions = highlightOptions(color='#512235', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(other,HTML)) %>%
  addCircles(lng = ~resists$lon, 
             lat = ~resists$lat, 
             color = '#5b017a', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Resisting Arrest",
             highlightOptions = highlightOptions(color='#5b017a', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(resist,HTML)) %>%
  addCircles(lng = ~thefts$lon, 
             lat = ~thefts$lat, 
             color = '#416d1b', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Theft / Burglary / Robbery",
             highlightOptions = highlightOptions(color='#416d1b', bringToFront = TRUE, sendToBack = TRUE),
             label = ~lapply(resist,HTML)) %>%
  addCircles(lng = ~trespasses$lon, 
             lat = ~trespasses$lat, 
             color = '#0066a0', 
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.8,
             group = "Trespassing",
             highlightOptions = highlightOptions(color='#0066a0', 
                                                 bringToFront = TRUE, 
                                                 sendToBack = TRUE),
             label = ~lapply(trespass,HTML)) %>%
  addLayersControl(overlayGroups = c("Assault",
                                     "CDS Distribituion / Manufacture",
                                     "CDS Paraphernalia Possession",
                                     "Disorderly Conduct",
                                     "Firearms-related",
                                     "Marijuana Possession",
                                     "Non-Marijuana Possession",
                                     "Other",
                                     "Resisting Arrest",
                                     "Theft / Burglary / Robbery",
                                     "Trespassing"),
                   baseGroups = "Points",
                   options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Assault",
              "CDS Distribituion / Manufacture",
              "CDS Paraphernalia Possession",
              "Disorderly Conduct",
              "Firearms-related",
              "Marijuana Possession",
              "Non-Marijuana Possession",
              "Other",
              "Resisting Arrest",
              "Theft / Burglary / Robbery",
              "Trespassing"))

  
    # htmlwidgets::onRender("function(el, t) {
  # var defaultStyle = {
  # color: '#000000',
  # opacity:0.5,
  # weight: 1,
  # fillOpacity: 0.7,};
  # var highlightStyle = {
  # color: '#ff0000',
  # opacity:1,
  # weight: 3,
  # fillOpacity: 1,};
  # var myMap = this;
  # var layers = myMap._layers;
  # for(var i in layers) {
  # var layer = layers[i];
  # // need some way to identify our polygons
  # // as each polygon was assigned a Label we use that
  # // as our selection criteria
  # if(layer.label) {layer.on('mouseover',function(e) {
  #                   this.setStyle(highlightStyle);
  #                   this.bringToFront();
  #                   });
  #                   layer.on('mouseout',
  #                   function(e) {
  #                   this.setStyle(defaultStyle);
  #                   this.bringToBack();
  #                   });
  #                   }
  #                   }
  #                   }")


leaflet(demo_markers,
        options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
           attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.',
           options = tileOptions(updateWhenIdle = TRUE)) %>%
  fitBounds(-76.89493, 39.19533, -76.32139, 39.72115) %>%
  addCircles(lng = ~assaults$lon, 
             lat = ~assaults$lat, 
             color = '#8ae3f2',
             fill = TRUE,
             radius = 25, 
             # stroke = TRUE, 
             fillOpacity = 0.2,
             # group = "Assault",
             options = markerOptions(interactive = TRUE, 
                                     riseOnHover = TRUE,
                                     riseOffset = 250),
             highlightOptions = highlightOptions(color='#8ae3f2', bringToFront = TRUE),
             label = ~lapply(assault,HTML)) 
  # %>%
  # addCircles(lng = ~cds_dists$lon, 
  #            lat = ~cds_dists$lat, 
  #            color = '#fc9114', 
  #            fill = TRUE,
  #            radius = 25, 
  #            # stroke = TRUE, 
  #            fillOpacity = 0.2,
  #            # group = "CDS Distribituion / Manufacture",
  #            highlightOptions = highlightOptions(color='#fc9114',
  #                                                bringToFront = TRUE,
  #                                                sendToBack = TRUE),
  #            label = ~lapply(cds_dist,HTML))
