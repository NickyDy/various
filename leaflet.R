library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(sf)

sett <- st_read("data/sett.geojson")
df <- tibble(address = "Ямбол, Ормана") %>% geocode(address, method = "osm")
obsh_map <- st_read("data/obsh_map.gpkg")
#obsh_map <- st_read("data/municipalities.geojson")

map_places <- read_csv("data/map_places.csv")
st_karadjovo <- read_csv("data/st_karadjovo.csv")
dis_plots <- read_csv("data/dis_plots.csv")

drag <- read_csv("drag.csv", col_types = "ddf") %>% 
	mutate(Observed = fct_recode(Observed, "Ansent" = "0", "Present" = "1"))
glimpse(drag)

pal <- colorNumeric(palette = "Reds", domain = map$perc)

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = map, fill = ~perc, 
              label = ~obshtina_bg, color = ~pal)

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  #addPolygons(data = sett, fill = F, weight = 2) %>% 
  addCircles(data = za, weight = 5, color = "red") %>% 
  addLabelOnlyMarkers(data = za, label =  ~ name)

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = sett, fill = F, weight = 2) %>% 
  addCircles(data = water, weight = 5, color = "red")

leaflet() %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = zt, fill = F, weight = 2)

leaflet(map_places) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addCircles(data = map_places, weight = 5, color = "white") %>% 
  addLabelOnlyMarkers(label =  ~ name, 
                      labelOptions = labelOptions(noHide = T, 
                                                  style = list("color" = "white", 
                                                          "font-size" = "12px"),
                                                  direction = 'top',
                                                  textOnly = T))

leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolygons(data = obsh_map, fill = F, label = ~obshtina_bg) %>%
  setView(lng = 26, lat = 42.8, zoom = 7)

leaflet() %>%
  addTiles() %>%
  #addPolygons(data = obsh_map, stroke = FALSE, smoothFactor = 0.2, 
  #fillOpacity = 0.3, color = ~ pal(oblast)) %>%
  addCircles(data = map_sites, weight = 5, label = map_sites$sa2_label,
             radius = map_sites$total, color = ~pal(map_sites$population)) %>%
  setView(lng = 144, lat = -37, zoom = 7)

leaflet() %>%
  addProviderTiles(providers$OpenTopoMap) %>% 
  addPolygons(data = obsh_map, fill = F, weight = 2) %>%
  #addCircles(data = sections, color = "red", weight = 2) %>%
  setView(lng = 26, lat = 42.8, zoom = 7)

leaflet() %>%
	addTiles() %>%
	addCircles(data = drag, color = ~ pal(Observed), weight = 5) %>% 
	addLegend(labels = c("Present", "Absent"), colors = c("blue", "red"))

yamb <- religion %>% select(adress = obshtina) %>% distinct()

df <- yamb %>% 
	geocode(adress, method = "osm")
df <- df %>% filter(between(long, 20, 30) & between(lat, 40, 45))
