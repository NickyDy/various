library(tidyverse)
library(sf)

obshtini <- st_read("data/obsh_map.gpkg")
oblasti <- st_read("data/obl_map.gpkg")
st_karadjovo <- read_csv("data/st_karadjovo.csv")
sections <- read_csv("data/sec.csv") %>% drop_na()
dis_plots <- read_csv("data/dis_plots.csv")

glimpse(ptp_oblasti)

ptp_oblasti <- jsonlite::fromJSON("https://data.egov.bg/resource/download/1ca5aa84-99dd-4eef-8530-8e73e7c72b05/json") %>% 
  as_tibble() %>% janitor::row_to_names(1) %>% mutate(across(2:13, as.numeric))


map <- oblasti %>% 
  mutate(oblast_bg = toupper(oblast_bg), oblast_bg = fct_recode(oblast_bg, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ")) %>% 
  inner_join(ptp_oblasti, by = c("oblast_bg" = "Области")) %>%
  mutate_if(is.numeric, round, 1)

map %>%
  ggplot() +
  geom_sf(aes(fill = `Разлика загинали бр.`), lwd = 0.5) +
  geom_sf_text(aes(label = oblast_bg), size = 3) +
  geom_sf_text(aes(label = `Разлика загинали бр.`), size = 4, vjust = 2) +
  theme_void() +
  theme(text = element_text(size = 16), legend.position = "none",
        axis.text = element_blank(), plot.title = element_text(hjust = 0.5, margin = margin(t = 50, b = 20)),
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Разлика в броя загинали при ПТП-та между 2023 г. и 2024 г.") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red")
#+ annotation_scale(location = "br", width_hint = 0.2, text_cex = 1.1) +
# annotation_north_arrow(location = "tr", which_north = "true",
# pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
# style = north_arrow_fancy_orienteering)


map <- oblasti %>% 
  mutate(oblast_bg = toupper(oblast_bg), 
         oblast_bg = fct_recode(oblast_bg, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ")) %>% 
  inner_join(ptp_obl_months_2024, by = c("oblast_bg" = "Области")) %>%
  mutate_if(is.numeric, round, 1)
map %>%
  pivot_longer(-c(oblast, oblast_bg, Общо, geom)) %>%
  mutate(name = fct_inorder(name)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), lwd = 0.5) +
  #geom_sf_text(aes(label = oblast_bg), size = 1) +
  geom_sf_text(aes(label = value), size = 3) +
  theme_void() +
  theme(text = element_text(size = 16), legend.position = "none",
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, 
                                  margin = margin(t = 50, b = 20)),
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Брой ПТП-та по месеци за 2024 г.") +
  scale_fill_gradient(low = "white", high = "darkorange") +
  facet_wrap(vars(name))

map <- oblasti %>% 
  mutate(oblast_bg = toupper(oblast_bg), 
         oblast_bg = fct_recode(oblast_bg, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ")) %>% 
  inner_join(injured_obl_months_2024, by = c("oblast_bg" = "Области")) %>%
  mutate_if(is.numeric, round, 1)
map %>%
  pivot_longer(-c(oblast, oblast_bg, geom)) %>%
  filter(str_detect(name, "Ранени - общо")) %>% 
  mutate(name = fct_inorder(name)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), lwd = 0.5) +
  geom_sf_text(aes(label = oblast_bg), size = 3, vjust = -1) +
  geom_sf_text(aes(label = value), size = 3) +
  theme_void() +
  theme(text = element_text(size = 16), legend.position = "none",
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, 
                                  margin = margin(t = 50, b = 20)),
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Общ брой ранени при ПТП-та за 2024 г.") +
  scale_fill_gradient(low = "white", high = "red")
map %>%
  pivot_longer(-c(oblast, oblast_bg, geom)) %>%
  filter(str_detect(name, "^Загинали - общо")) %>% 
  mutate(name = fct_inorder(name)) %>% 
  ggplot() +
  geom_sf(aes(fill = value), lwd = 0.5) +
  geom_sf_text(aes(label = oblast_bg), size = 3, vjust = -1, color = "red", fontface = "bold") +
  geom_sf_text(aes(label = value), size = 3, color = "red", fontface = "bold") +
  theme_void() +
  theme(text = element_text(size = 16), legend.position = "none",
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5, 
                                  margin = margin(t = 50, b = 20)),
        axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = NULL, 
       title = "Общ брой загинали при ПТП-та за 2024 г.") +
  scale_fill_gradient(low = "white", high = "black")
