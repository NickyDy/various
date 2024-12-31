library(tidyverse)
library(mapview)
library(sf)
#library(tidygeocoder)

mapviewOptions(basemaps = c("OpenStreetMap"), fgb = F)
#"Esri.WorldImagery", "OpenTopoMap"

df <- tibble(address = c("Ямбол", "Бургас")) %>% geocode(address, method = "osm")
map <- st_read("data/obsh_map.gpkg")
zt <- st_read("data/zt.gpkg")
nh <- st_read("data/nh.gpkg")
nb <- st_read("data/nb.gpkg")
map_places <- read_csv("data/map_places.csv") %>% st_as_sf(coords = c("long", "lat"), crs = c(4326))
sof_sam <- st_read("sof_samokov.kmz")

oblasti <- st_read("data/oblasti.geojson")
obshtini <- st_read("data/obshtini.geojson")
sett <- st_read("data/settlements.geojson")
ekatte <- read_csv("data/ekatte.csv")

sett %>% 
  mapview(color = "blue", zcol = "sett_name",
                    label = df_sett$sett_name,
                    legend = F, col.regions = "white", 
                    alpha.regions = 0)

obshtini %>% 
  mapview(color = "blue", zcol = "obshtina_name",
                     label = obshtini$obshtina_name,
                     legend = F, col.regions = "white", 
                     alpha.regions = 0)
oblasti %>% 
  mapview(color = "blue", zcol = "oblast_name",
          label = oblasti$oblast_name,
          legend = F, col.regions = "white", 
          alpha.regions = 0)

map %>% 
  #filter(str_detect(obshtina_bg, "^В")) %>% 
  mapview(color = "blue", zcol = "perc", legend = T)

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
