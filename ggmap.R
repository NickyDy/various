library(tidyverse)
library(ggmap)
library(lubridate)
library(sf)

obsh_map <- st_read("data/obsh_map.gpkg")
st_karadjovo <- read_csv("data/st_karadjovo.csv")
sections <- read_csv("data/sec.csv") %>% drop_na()
dis_plots <- read_csv("data/dis_plots.csv")

glimpse(world)

ggmap(yambol) +
	geom_path(data = st_karadjovo, aes(lon, lat, color = alt), size = 2) +
	scale_colour_viridis_c(option = "magma")

yambol <- get_stamenmap(
	bbox = c(left = 26.4702, right = 26.8609, top = 42.5065, bottom = 42.1242), zoom = 11, maptype = "terrain")
vitosha <- get_stamenmap(
	bbox = c(left = 23.1413, right = 23.3803, top = 42.6532, bottom = 42.4295), zoom = 12, maptype = "terrain")
topchiq <- get_stamenmap(
	bbox = c(left = 26.5423, right = 26.5855, top = 42.2298, bottom = 42.1756), zoom = 13, maptype = "terrain")

ggmap(vitosha) +
	geom_point(data = dis_dipl, aes(lon, lat, color = alt), size = 2) +
	scale_colour_viridis_c(option = "magma")
ggmap(topchiq) +
	geom_point(data = dis_dipl, aes(lon, lat, color = alt), size = .5) +
	geom_text(data = dis_dipl, aes(label = name), size = 3, check_overlap = T) +
	scale_colour_viridis_c(option = "magma")

bulgaria <- get_stamenmap(
	bbox = c(left = 22.335, right = 28.647, top = 44.229, bottom = 41.216), zoom = 8, maptype = "terrain")
ggmap(bulgaria) +
	geom_point(data = sections, aes(lon, lat), color = "red", size = .5)

obl_yambol <- get_stamenmap(
	bbox = c(left = 26.1626, right = 27.0538, top = 42.6471, bottom = 42.0126), zoom = 10, maptype = "terrain")
ggmap(obl_yambol) +
	geom_point(data = filter(sections, oblast == "ЯМБОЛ"), aes(lon, lat), color = "red", size = .5)
