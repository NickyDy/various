library(tidyverse)
library(mapview)
library(sf)
library(jsonlite)
mapviewOptions(basemaps = c("OpenStreetMap", "Esri.WorldImagery"), fgb = F)

#library(tidygeocoder)
#"Esri.WorldImagery", "OpenTopoMap"

df <- tibble(address = c("Ямбол", "Бургас")) %>% 
  geocode(address, method = "osm")
map <- st_read("data/obsh_map.gpkg")
zt <- st_read("data/zt.gpkg")
nh <- st_read("data/nh.gpkg")
nb <- st_read("data/nb.gpkg")
#----------------------------
all_places <- st_read("data/maps/all_places.geojson")

df1 <- st_read("data/maps/Ямбол - Асеново - Симеоново - Ямбол.kmz", layer = "ACTIVE LOG")
df2 <- st_read("data/maps/Ямбол - Стефан-Караджово - Елхово - Ямбол.kmz", layer = "Track")
df3 <- st_read("data/maps/Ямбол - Айваджика_ София - Витоша.kmz", layer = "ACTIVE LOG 002")
df4 <- st_read("data/maps/Ямбол - Генерал Инзово - Тенево - Ямбол.kmz", layer = "ACTIVE LOG 030")
df5 <- st_read("data/maps/Way points - Dissertation and Diploma.kmz", layer = "Waypoints")
df6 <- st_read("data/maps/София - Самоков - София.kmz", layer = "tracks")
df7 <- st_read("data/maps/Скалица - 3-ти август, 2014.kmz", layer = "tracks")

st_layers("data/maps/Скалица - 3-ти август, 2014.kmz")
#-----------------------------------------
oblasti <- st_read("data/oblasti.geojson")
obshtini <- st_read("data/obshtini.geojson")
sett <- st_read("data/settlements.geojson")
ekatte <- fromJSON("data/ek_atte.json") %>%
  select(1:18) %>% drop_na(ekatte)

elev <- ekatte %>% select(ekatte, text)
sett_elev <- sett %>% left_join(elev, by = "ekatte")

colors <- c('#fff7ec','#fee8c8','#fdd49e','#fdbb84','#fc8d59','#ef6548','#d7301f','#990000')

df5 %>% mapview(zcol = "Name", legend = F, color = "red", col.regions = "red")

all_places %>% 
  mapview(zcol = "name", legend = F, color = "red", col.regions = "red")

sett_elev %>% 
  mapview(color = "blue", zcol = "text",
                    label = sett_elev$text, lwd = 1,
                    legend = F, col.regions = colors, 
                    alpha.regions = 0.5)

obshtini %>% 
  mapview(color = "blue", zcol = "obshtina_name",
                     label = obshtini$obshtina_name, lwd = 2,
                     legend = F, col.regions = "white", 
                     alpha.regions = 0)
oblasti %>% 
  mapview(color = "blue", zcol = "oblast_name",
          label = oblasti$oblast_name, lwd = 2,
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

za %>% st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>% 
  mapview(legend = F, col.regions = "red", color = "red")

sett %>% select(-contains) %>% 
  mapview(legend = F, zcol = "nuts4", col.regions = "white", alpha.regions = 0, color = "blue")
glimpse(und_water)

und_water <- read_rds("shiny/und_water/underground_water.rds") %>% 
  select(oblast, site_name, lat, long) %>% drop_na()

und_water %>% 
  #filter(oblast == "Ямбол") %>% 
  st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>% 
  mapview(legend = F, zcol = "site_name", cex = 3, label = und_water$site_name,
          col.regions = "red", color = "red")

mapView(
  x,
  map = NULL,
  pane = "auto",
  canvas = useCanvas(x),
  viewer.suppress = mapviewGetOption("viewer.suppress"),
  zcol = NULL,
  burst = FALSE,
  color = mapviewGetOption("vector.palette"),
  col.regions = mapviewGetOption("vector.palette"),
  at = NULL,
  na.color = mapviewGetOption("na.color"),
  cex = 6,
  lwd = lineWidth(x),
  alpha = 0.9,
  alpha.regions = regionOpacity(x),
  na.alpha = regionOpacity(x),
  map.types = mapviewGetOption("basemaps"),
  verbose = mapviewGetOption("verbose"),
  popup = TRUE,
  layer.name = NULL,
  label = zcol,
  legend = mapviewGetOption("legend"),
  legend.opacity = 1,
  homebutton = mapviewGetOption("homebutton"),
  native.crs = FALSE,
  highlight = mapviewHighlightOptions(x, alpha.regions, alpha, lwd),
  maxpoints = getMaxFeatures(x),
  hide = FALSE,
  ...
)

