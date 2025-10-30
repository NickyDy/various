library(tidyverse)
library(tidytext)
library(ggtext)

billa <- read_csv("https://kolkostruva.bg/upload/9639/import.csv", col_types = c("cccccdd")) %>% 
  mutate(market = "Billa", .before = everything())
vilton <- read_csv("https://kolkostruva.bg/upload/9593/import.csv", col_types = c("cccccdd")) %>% 
  mutate(market = "Vilton", .before = everything())
kaufland <- read_csv("https://kolkostruva.bg/upload/9641/import.csv", col_types = c("cccccdd")) %>% 
  mutate(market = "Kaufland", .before = everything())
lidl <- read_csv("https://kolkostruva.bg/upload/9607/import.csv", col_types = c("cccccdd")) %>% 
  mutate(market = "Lidl", .before = everything())
tmarket <- read_csv("https://kolkostruva.bg/upload/9694/KZP_27.10.2025.csv", col_types = c("cccccdd")) %>% 
  mutate(market = "T Market", .before = everything())

markets <- bind_rows(billa, vilton, kaufland, lidl, tmarket) %>% janitor::clean_names() %>% 
  mutate(cena_v_promocia = na_if(cena_v_promocia, 0))

glimpse(markets)

markets %>% 
  filter(str_detect(naimenovanie_na_produkta, regex("бургаско|burgasko", ignore_case = T)),
         str_detect(t_rgovski_obekt, regex("ямбол|М-", ignore_case = T)),
         !str_detect(naimenovanie_na_produkta, "^Krina;")) %>%
  reframe(cena_na_drebno = round(mean(cena_na_drebno, na.rm = T), 2),
          cena_v_promocia = round(mean(cena_v_promocia, na.rm = T), 2),
          .by = c(market, naseleno_masto, kod_na_produkta, 
                  kategoria, naimenovanie_na_produkta)) %>%
  mutate(cena_v_promocia = as.character(cena_v_promocia)) %>% 
  mutate(cena_v_promocia = str_replace(cena_v_promocia, "NaN", "")) %>%
  mutate(naimenovanie_na_produkta = reorder_within(naimenovanie_na_produkta, cena_na_drebno, market)) %>%
  ggplot(aes(cena_na_drebno, naimenovanie_na_produkta, fill = market)) +
  geom_col(show.legend = F) +
  geom_richtext(aes(label = glue::glue("{cena_na_drebno};  <span style='color:red'>{cena_v_promocia}</span>")), 
                position = position_dodge(width = 1), hjust = -0.01, size = 4.5, fill = NA, label.colour = NA) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.05, .7))) +
  labs(x = "Цена (лв); <span style='color:red'>Промоция (лв)</span>", y = NULL) +
  theme(text = element_text(size = 14), axis.title.x = element_markdown()) +
  facet_wrap(vars(market), scales = "free_y")

# read_csv_cc <- function(file) {
#   read_csv(file, col_types = "cccccdd")
# }
# 
# files <- dir_ls("/home/nick/Downloads/", glob = "*.csv")
# markets <- map(files, read_csv_cc) %>%
#   set_names(basename) %>%
#   list_rbind(names_to = "market") %>%
#   bind_rows() %>% distinct() %>% janitor::clean_names() %>% 
#   mutate(market = str_remove(market, ".csv$"))

