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

# ptp <- read_delim("various/mrv_database_done.csv") %>% 
#   rename(lat = y, long = x) %>% 
#   mutate(date = dmy(date),
#          month = month(date),
#          day = day(date),
#          lat = case_when(lat > 100 ~ lat / 100000, .default = lat),
#          long = case_when(long > 100 ~ long / 100000, .default = long)) %>% 
#   drop_na(lat, long)
# ptp <- st_read("various/PTP_Analysis_FINAL/PTP_Layer.shp") %>% 
#   rename(lat = y, long = x)
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

glimpse(ptp)
ptp %>% map_dfr(~ sum(is.na(.)))

sett_elev %>% 
  mutate(text = fct_relevel(text, "0 - 49 вкл.", "50 - 99 вкл.", "100 - 199 вкл.", "200 - 299 вкл.",
                                  "300 - 499 вкл.", "500 - 699 вкл.", "700 - 999 вкл.", "1000 и повече",
                                  "NA")) %>% 
  mapview(color = "blue", zcol = "text",
                    label = sett_elev$text, lwd = 1,
                    legend = T, col.regions = colors, 
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

sett %>% 
  mapview(legend = F, zcol = "sett_name", col.regions = "white", alpha.regions = 0, color = "blue")
glimpse(und_water)

und_water <- read_rds("shiny/und_water/underground_water.rds") %>% 
  select(oblast, site_name, lat, long) %>% drop_na()

und_water %>% 
  #filter(oblast == "Ямбол") %>% 
  st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>% 
  mapview(legend = F, zcol = "site_name", cex = 3, label = und_water$site_name,
          col.regions = "red", color = "red")

# ptp_map <- ptp %>%
#   filter(
#     date >= "2024-12-31",
#     #year %in% c(2025), 
#     #month %in% c(11), 
#     #day %in% c(31)
#     #type == "",
#     #died == "да",
#     #injured == "да"
#   )
# 
# ptp_map %>% 
#   st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>% 
#   mapview(label = ptp_map$type, zcol = "died", color = c("black", "red"),
#           legend = T, col.regions = c("black", "red"), cex = 3)