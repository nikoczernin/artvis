library(jsonlite)
library(leaflet)
library(maps)

world <- map("world", fill = FALSE, interior = FALSE, plot=FALSE)

my_data <- data.frame(
  iso_a3 = c("USA", "CAN", "BRA", "DEU", "CHN"),
  value = c(50000, 45000, 15000, 42000, 20000) # Example values
) %>% print()

states <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")
class(states)
names(states)

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
m %>% addPolygons()
bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)

m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE))
