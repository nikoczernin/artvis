library(leaflet)
library(sf)


WORLD_DATA <<- read_sf("./data/worldmap/TM_WORLD_BORDERS_SIMPL-0.3.shp")


choropleth <<- function(plot_data, x, iso2_column="ISO2", colors="BuGn"){
  # plot data needs to have a coutnry  code iso2 column
  # you also have to rename the variable in the plot data 
  plot_data <- plot_data %>% rename(mapvar = x)
  
  # join the plot data with the world_sf map
  map_data <- WORLD_DATA %>%
    left_join(plot_data, by=c("ISO2"=iso2_column), keep=T)
  
  # create a palette
  mypalette <- colorNumeric(
    palette = colors, 
    domain = map_data$mapvar,
    na.color = "lightgray"
  )
  
  # Prepare the text for tooltips:
  tooltips <- paste(
                "Country:", map_data$NAME, "<br/>", 
                "Total:", map_data$mapvar
                ) %>% 
    lapply(htmltools::HTML)
  
  # cretea and return the plot
  map_data %>% 
    leaflet() %>%
    addTiles() %>%
    setView(0, 50, zoom = 4) %>%
    addPolygons(
      fillColor = ~ mypalette(mapvar), 
      stroke = FALSE,
      fillOpacity = 0.9,
      color = "white",
      weight = 3,
      label = tooltips,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "13px",
        # direction = "auto"
      )
    )
}



# # USE

# prepare the plot data (summarise)
# artvis %>%
#   group_by(e.country) %>%
#   summarise(n_exhibits = n_distinct(e.id)) %>%
#   # pass it to the choropleth
#   choropleth("n_exhibits", "e.country")


