library(tidyverse)
library(mapview)
library(sf)
library(tidygeocoder)

mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"), fgb = F)
mapviewOptions(fgb = FALSE)

df <- tibble(address = c("Ямбол", "Бургас")) %>% geocode(address, method = "osm")
map <- st_read("data/obsh_map.gpkg")
zt <- st_read("data/zt.gpkg")
nh <- st_read("data/nh.gpkg")
nb <- st_read("data/nb.gpkg")
sett <- st_read("data/sett.geojson")
map_places <- read_csv("data/map_places.csv") %>% st_as_sf(coords = c("long", "lat"), crs = c(4326))
sof_sam <- st_read("sof_samokov.kmz")

sett %>% mapview(color = "blue", col.regions = "white", alpha.regions = 0)

sof_sam %>% mapview()

map %>% 
  #filter(str_detect(obshtina_bg, "^В")) %>% 
  mapview(color = "blue", zcol = "obshtina_bg", col.regions = c("red", "blue", "green", "lightblue", "orange"),
          legend = F)

zt %>% 
  mapview(color = "blue", zcol = "name", col.regions = c("red", "blue", "green", "lightblue", "orange"),
          legend = F)
nh %>% 
  mapview(color = "blue", zcol = "NAME_BG", col.regions = c("red", "blue", "green", "lightblue", "orange"),
               legend = F)
nb %>% 
  mapview(color = "blue", zcol = "NAME_BG", col.regions = c("red", "blue", "green", "lightblue", "orange"),
          legend = F)
zt %>% select(type, name, geom) %>% 
  mapview(legend = F)

map_places %>% 
  mapview(zcol = "name", legend = F, color = "red", col.regions = "red")

za %>% st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>% 
  mapview(legend = F, col.regions = "red", color = "red")

sett %>% select(-contains) %>% 
  mapview(legend = F, zcol = "nuts4", col.regions = "white", alpha.regions = 0, color = "blue")
glimpse(und_water)

und_water %>% select(oblast, site_name, lat, long) %>% drop_na() %>% 
  #filter(oblast == "Ямбол") %>% 
  st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>% 
  mapview(legend = F, zcol = "oblast", cex = 3, label = und_water$site_name)
