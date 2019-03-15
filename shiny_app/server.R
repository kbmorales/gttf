server <- function(input, output, session){

output$bmore_map <- renderLeaflet({

  # creates map object
  bmore_map = leaflet()
  
  # adding data
  # examples say to put addLayersControl() at end of map making
  bmore_map = bmore_map %>%
    addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
             attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>. *Displayed cases are 80% of total data set aggregated. Cases are over a period of ~10yrs.' ) %>%
    fitBounds(-76.89493, 39.19533, -76.32139, 39.72115) %>%
    # addProviderTiles(providers$Stamen.TonerLines,
    #                  options = providerTileOptions(opacity = 0.35)) %>%
    # addProviderTiles(providers$Stamen.TonerLabels) %>%
    addPolygons(data = baltimore_county,
                group = "County",
                color = "#b799ff",
                fill = FALSE) %>%
    addPolygons(data = baltimore_city,
                group = "City",
                color = "#c0a5ff",
                fill = FALSE) %>%
    addCircles(data = bmoredemo_markers1,
               lng = ~lon,
               lat = ~lat,
               group = "Points",
               popup = bmoredemo_markers1$case_num,
               weight = 3,
               radius = 40,
               stroke = TRUE,
               fillOpacity = 0.8,
               color = "#ff0000",
               label = ~as.character(bmoredemo_markers1$case_num),
               options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)) %>%
    addCircles(data = bmoredemo_markers1,
               lng = ~lon,
               lat = ~lat,
               group = "Black/Non-Black",
               popup = bmoredemo_markers1$race_black,
               weight = 3,
               radius = 40,
               stroke = TRUE,
               fillOpacity = 0.8,
               color = ~raceblack_icons(race_black),
               label = ~as.character(bmoredemo_markers1$race_black),
               options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)) %>%
    addLegend("bottomright",
              colors = c("#ffa500", "#b7b2ac"),
              labels = c("Black", "Non-Black"),
              title = "Cases by Race",
              group = "Black/Non-Black") %>%
    addCircles(data = bmoredemo_markers1,
               lng = ~lon,
               lat = ~lat,
               group = "Black/White/Other",
               popup = bmoredemo_markers1$race_cat,
               weight = 3,
               radius = 40,
               stroke = TRUE,
               fillOpacity = 0.8,
               color = ~racecat_icons(race_cat),
               label = ~as.character(bmoredemo_markers1$case_num),
               options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)) %>%
    addLegend("bottomright",
              colors = c("#78840b", "#036852", "#a53208"),
              labels = c("Black", "Other/Unknown", "White"),
              title = "Cases by Race Mult",
              group = "Black/White/Other",
              labFormat = labelFormat(group = "Black/White/Other")) %>%
    addCircles(data = bmoredemo_markers1,
               lng = ~lon,
               lat = ~lat,
               group = "Female/Male",
               popup = bmoredemo_markers1$sex_id,
               weight = 3,
               radius = 40,
               stroke = TRUE,
               fillOpacity = 0.8,
               color = ~sex_icons(sex_id),
               label = ~as.character(bmoredemo_markers1$sex_id),
               options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)) %>%
    addLegend("bottomleft",
              colors = c( "#53a097", "#0d2666", "#682140"),
              labels = c("Male", "Female", "Other/Unknown"),
              title = "Cases by Sex",
              group = "Female/Male",
              labFormat = labelFormat()) %>%
    addCircles(data = bmoredemo_markers1,
               lng = ~lon,
               lat = ~lat,
               group = "Age",
               weight = 2,
               radius = 25,
               stroke = TRUE,
               fillOpacity = 0.5,
               color = ~pal(age_yrs),
               label = ~as.character(bmoredemo_markers1$age_yrs),
               options = markerClusterOptions(removeOutsideVisibleBounds = TRUE)) %>%
    addLegend("bottomleft",
              pal = pal,
              values = bmoredemo_markers1$age_yrs,
              title = "Age",
              group = "Age") %>%
    addLayersControl(overlayGroups = c("City",
                                       "County"),
                     baseGroups = c("Points",
                                    "Black/Non-Black", 
                                    "Black/White/Other",
                                    "Female/Male",
                                    "Age"),
                     options = layersControlOptions(collapsed = TRUE, autoZIndex = TRUE))
  
  bmore_map
})

}