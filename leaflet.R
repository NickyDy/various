library(tidyverse)
library(leaflet)
library(sf)
#library(tidygeocoder)

sett <- st_read("data/sett.geojson")
df <- tibble(address = "Ямбол, Ормана") %>% geocode(address, method = "osm")
obsh_map <- st_read("data/obsh_map.gpkg")
minic_map <- st_read("data/municipalities.geojson")

map_places <- read_csv("data/map_places.csv")
st_karadjovo <- read_csv("data/st_karadjovo.csv")
dis_plots <- read_csv("data/dis_plots.csv")

drag <- read_csv("data/drag.csv", col_types = "ddf") %>% 
	mutate(Observed = fct_recode(Observed, "Ansent" = "0", "Present" = "1"))
glimpse(ptp)

ptp <- read_csv2("various/mrv_database_done.csv") %>% 
  rename(lat = y, long = x) %>% 
  mutate(date = dmy(date),
         month = month(date),
         day = day(date),
         lat = case_when(lat > 100 ~ lat / 1000, .default = lat),
         long = case_when(long > 100 ~ long / 1000, .default = long)) %>% 
  drop_na(lat, long)

ptp <- st_read("~/Downloads/PTP_Analysis_FINAL/PTP_Layer.shp")
#--------------------------------------------------
pal <- colorNumeric(palette = "Reds", domain = map$perc)
pal <- colorFactor(c("blue", "red"), domain = drag$Observed)
pal <- colorFactor(c("black", "red"), domain = ptp_map$died)

ptp_map %>% count(injured)

ptp_map <- ptp %>%
  filter(
    #date == "2025-03-31",
    year %in% c(2025), 
    month %in% c(3), 
    day %in% c(31),
    #type == "",
    #died == "да",
    #injured == "да"
  )

leaflet(ptp_map) %>% 
  addProviderTiles(providers$OpenStreetMap) %>% 
  addCircles(lng = ptp_map$long, lat = ptp_map$lat, 
             weight = 10, opacity = 1, color = "black") %>% 
  addLabelOnlyMarkers(label =  ~ type)

leaflet() %>%
  addTiles() %>%
  addCircles(data = ptp_map, opacity = 1,
             color = ~ pal(died), weight = 5) %>% 
  #addLabelOnlyMarkers(data = ptp_map, label =  ~ type) %>% 
  addLegend(title = "Загинали:", 
            labels = c("Да", "Не"), 
            opacity = 1, color = c("black", "red"))

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
  addProviderTiles(providers$OpenStreetMap) %>% 
  addPolygons(data = obsh_map, fill = F, weight = 2) %>%
  #addCircles(data = sections, color = "red", weight = 2) %>%
  setView(lng = 26, lat = 42.8, zoom = 7)

yamb <- religion %>% select(adress = obshtina) %>% distinct()

df <- yamb %>% 
	geocode(adress, method = "osm")
df <- df %>% filter(between(long, 20, 30) & between(lat, 40, 45))
