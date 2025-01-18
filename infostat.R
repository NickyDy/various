library(tidyverse)
library(arrow)
library(sf)
library(patchwork)
options(scipen = 100)

obl_map <-  st_read("data/obl_map.gpkg")
obsh_map <- st_read("data/obsh_map.gpkg")
# pop_residence <- read_csv("data/population.csv", col_types = "fffdd")
# loc_sex <- read_csv("data/loc_sex.csv")
# year_oblast_age <- read_csv("data/year_location_age.csv", col_types = "fffffd")
# town_age <- read_csv("data/settlement_age.csv", col_types = "fffffd")
# town_sex <- read_csv("data/settlement_sex.csv", col_types = "ffffd")
# oblast_age <- read_csv("data/location_age.csv", col_types = "fffd")
# regions_age <- read_csv("data/regions_age.csv", col_types = "fffffd")
# birth_location <- read_csv("data/birth_location.csv", col_types = "fffd")
# citizenship <- read_csv("data/citizenship.csv", col_types = "ffffd")
# labor_sex <- read_csv("data/labor_sex.csv", col_types = "fffffd")
# deaths <- read_delim("data/deaths.csv", col_types = "ffddfd")
# sentenced <- read_csv("data/sentenced.csv", col_types = "fdfd")
# hicp <- read_delim("data/hicp.csv", na = ".", col_names = F)
# ipc <- read_delim("data/ipc.csv", na = ".", col_names = F)
# ethno <- read_delim("data/ethno.csv", na = "-", col_names = F)
# religion <- read_delim("data/religion.csv", na = "-", col_names = F)
# water_sector_type <- read_delim("data/water_1.csv", na = ":")
# water_regions <- read_delim("data/water_regions.csv")
# water_type <- read_delim("data/water_type.csv")
# water_var <- read_delim("data/water_sector.csv")
# energy_bg <- read_delim("data/energy_bg.csv", na = "-", col_names = F)
# agro <- read_delim("data/agro.csv", na = c("-", "."))

colors <- c("Мъже" = "#F8766D", "Жени" = "#00BFC4", "Градове" = "#F8766D", "Села" = "#00BFC4")

glimpse(loc_sex_n)
loc_sex %>% map_dfr(~ sum(is.na(.)))
#-----------------------------------
agro <- read_delim("data/prices_agro.csv", na = "-")

agro %>% count(product) %>% view

agro %>% 
  pivot_longer(-c(1:2)) %>%
  filter(product == "11 Семена от слънчоглед") %>% 
  ggplot(aes(name, value)) +
  geom_col()
#-----------------------------------------------------------
hipc <- read_parquet("shiny/eurostat/prc_hicp_mmor.parquet")

# hipc_n <- hipc %>% 
#   slice(-c(1:1)) %>% 
#   rownames_to_column() %>%
#   pivot_longer(-rowname) %>%
#   pivot_wider(names_from = rowname, values_from = value) %>%
#   unite("united", 2:4, sep = "_") %>%
#   rownames_to_column() %>%
#   pivot_longer(-rowname) %>%
#   pivot_wider(names_from = rowname, values_from = value) %>%
#   select(!name) %>%
#   slice(-c(1:1)) %>%
#   mutate(`1` = str_replace(`1`, "__", "pokazatel")) %>%
#   janitor::row_to_names(row_number = 1) %>%
#   pivot_longer(2:1642, names_to = "name", values_to = "value") %>% 
#   separate(name, c("index", "year", "month"), sep = "_") %>%
#   mutate(value = parse_number(value)) %>% drop_na(value)

tot_year <- hipc %>%
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(coicop == "All-items HICP",
         geo == "Bulgaria",
         TIME_PERIOD >= "2019-01-01") %>% 
  summarise(s = round(sum(values), 1), .by = year)
tot <- hipc %>%
  filter(coicop == "All-items HICP",
         geo == "Bulgaria",
         TIME_PERIOD >= "2019-01-01") %>% 
  summarise(s = round(sum(values), 1))
seg <- tibble(
  a = c("2019-01-01", "2024-12-01"),
  b = c(16, 16))

hipc %>%
  filter(coicop == "All-items HICP",
         geo == "Bulgaria",
         TIME_PERIOD >= "2019-01-01") %>%
  mutate(col = values >= 0, csum = round(cumsum(values), 1)) %>%
  filter(TIME_PERIOD >= "2019-01-01") %>% view
  ggplot(aes(TIME_PERIOD, csum)) +
  geom_point(aes(color = col), show.legend = F, size = 3) +
  geom_line(linewidth = 0.2) +
  theme(text = element_text(size = 16)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 10) +
  geom_vline(xintercept = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", 
                                    "2022-01-01", "2023-01-01", "2024-01-01",
                                    "2025-01-01")), 
             color = "black", lty = 2) +
  scale_color_manual(values = c("red", "black")) +
  labs(x = NULL, y = "Инфлация (%)", 
       title = "Натрупана обща инфлация (Общ ХИПЦ) - по месеци, по години и общо за целия период - януари (2019)-ноември (2024)") +
  annotate("text", x = as.Date("2019-07-01"), y = 5, label = paste0(tot_year$s[1], "%"), size = 10) +
  annotate("text", x = as.Date("2020-07-01"), y = 5, label = paste0(tot_year$s[2], "%"), size = 10) +
  annotate("text", x = as.Date("2021-07-01"), y = 8, label = paste0(tot_year$s[3], "%"), size = 10) +
  annotate("text", x = as.Date("2022-07-01"), y = 22, label = paste0(tot_year$s[4], "%"), size = 10) +
  annotate("text", x = as.Date("2023-07-01"), y = 29, label = paste0(tot_year$s[5], "%"), size = 10) +
  annotate("text", x = as.Date("2024-07-01"), y = 31, label = paste0(tot_year$s[6], "%"), size = 10) +
  annotate("text", x = as.Date("2021-07-01"), y = 16, label = paste0(tot$s, "%"), size = 15) +
  geom_segment(aes(x = as.Date("2019-01-01"), y = 16, xend = as.Date("2021-03-01"), yend = 16)) +
  geom_segment(aes(x = as.Date("2021-11-01"), y = 16, xend = as.Date("2024-12-01"), yend = 16)) +
  geom_point(aes(as.Date(a), b), data = seg, size = 3)
#-----------------------------------------------------
cereals <- read_parquet("shiny/agri/cereals.parquet") %>% 
  filter(state == "Bulgaria", product == "Milling wheat", date >= "2021-12-31")
cereals <- cereals %>% 
  summarise(mean_price = mean(price_tonne_bgn), .by = c(date, state, stage_name, product))

an_cer <- tibble(perc = round((392 / 525) * 100, 0))
seg_cer <- tibble(
  a = c("2022-01-01", "2024-11-04"),
  b = c(550, 550))

a <- cereals %>%
  # filter(coicop == "All-items HICP",
  #        geo == "Bulgaria",
  #        between(TIME_PERIOD, as.Date("2019-01-01"), as.Date("2024-11-01"))) %>%
  # mutate(col = values >= 0, csum = round(cumsum(values), 1)) %>% 
  # filter(TIME_PERIOD >= "2019-01-01") %>% 
  ggplot(aes(date, mean_price)) +
  theme(text = element_text(size = 16)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 10) +
  geom_vline(xintercept = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", 
                                    "2022-01-01", "2023-01-01", "2024-01-01",
                                    "2025-01-01")), 
             color = "black", lty = 2) +
  scale_color_manual(values = c("red", "black")) +
  annotate("text", x = as.Date("2023-07-01"), y = 580, label = paste0("-", an_cer$perc, "%"), size = 12, color = "red") +
  geom_segment(aes(x = as.Date("2022-01-01"), y = 550, xend = as.Date("2024-11-01"), yend = 550), color = "red",
               linewidth = 0.3, linetype = "dashed") +
  geom_point(aes(as.Date(a), b), data = seg_cer, size = 3, color = "red") +
  geom_point(show.legend = F, size = 3) +
  geom_line(linewidth = 0.2) +
  labs(x = NULL, y = "Цена (€/тон)", title = "Промяна цената на хлебната пшеница от януари 2022 г. до ноември 2024 г.")

tot_year <- hipc %>%
  mutate(year = year(TIME_PERIOD)) %>% 
  filter(coicop == "Bread",
         geo == "Bulgaria",
         between(TIME_PERIOD, as.Date("2022-01-01"), as.Date("2024-11-01"))) %>% 
  summarise(s = round(sum(values), 1), .by = year)
tot <- hipc %>%
  filter(coicop == "Bread",
         geo == "Bulgaria",
         between(TIME_PERIOD, as.Date("2022-01-01"), as.Date("2024-11-01"))) %>% 
  summarise(s = round(sum(values), 1))
seg <- tibble(
  a = c("2022-01-01", "2024-11-01"),
  b = c(12.5, 12.5))

b <- hipc %>%
  filter(coicop == "Bread",
         geo == "Bulgaria",
         between(TIME_PERIOD, as.Date("2022-01-01"), as.Date("2025-01-01"))) %>%
  mutate(col = values >= 0, csum = round(cumsum(values), 1)) %>% 
  #filter(TIME_PERIOD >= "2022-01-01") %>% 
  ggplot(aes(TIME_PERIOD, csum)) +
  geom_vline(xintercept = as.Date(c("2022-06-01")), 
             color = "red", lty = 1, linewidth = 1) +
  geom_point(aes(), show.legend = F, size = 3) +
  geom_line(linewidth = 0.2) +
  theme(text = element_text(size = 16)) +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(n.breaks = 10) +
  geom_vline(xintercept = as.Date(c("2022-01-01", "2023-01-01", "2024-01-01","2025-01-01")), 
             color = "black", lty = 2) +
  scale_color_manual(values = c("red", "black")) +
  labs(x = NULL, y = "%", 
       title = "Промяна цената на хляба от януари 2022 г. до ноември 2024 г.") +
  # annotate("text", x = as.Date("2019-07-01"), y = 5, label = paste0(tot_year$s[1], "%"), size = 10) +
  # annotate("text", x = as.Date("2020-07-01"), y = 5, label = paste0(tot_year$s[2], "%"), size = 10) +
  # annotate("text", x = as.Date("2021-07-01"), y = 8, label = paste0(tot_year$s[3], "%"), size = 10) +
  # annotate("text", x = as.Date("2022-09-01"), y = 21, label = paste0(tot_year$s[1], "%"), size = 10) +
  # annotate("text", x = as.Date("2023-07-01"), y = 21, label = paste0(tot_year$s[2], "%"), size = 10) +
  # annotate("text", x = as.Date("2024-07-01"), y = 21, label = paste0(tot_year$s[3], "%"), size = 10) +
  annotate("text", x = as.Date("2023-07-01"), y = 14, label = paste0("+", tot$s, "%"), size = 12, color = "red") +
  annotate("text", x = as.Date("2022-07-01"), y = 8, label = "Тук се променя\nДДС-то от 20 на 0%", hjust = 0, 
           size = 6, color = "red") +
  geom_segment(aes(x = as.Date("2022-01-01"), y = 12.5, xend = as.Date("2024-11-01"), yend = 12.5), color = "red",
               linewidth = 0.3, linetype = "dashed") +
  #geom_segment(aes(x = as.Date("2022-01-01"), y = 16, xend = as.Date("2024-01-01"), yend = 16)) +
  geom_point(aes(as.Date(a), b), data = seg, size = 3, color = "red")

a / b
#---------------------------------------------------
loc_sex <- read_delim("loc_sex.csv", col_names = F, na = "-") %>% select(-X46)

loc_sex_n <- loc_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "location")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:45, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sex"), sep = "_") %>%
  mutate(oblast = case_when(str_detect(location, "^[:upper:]{3}\\s") ~ location),
         obshtina = case_when(str_detect(location, "^[:upper:]{3}\\d{2}") ~ location)) %>% 
  fill(oblast) %>% fill(obshtina) %>% fill(obshtina, .direction = "up") %>% 
  filter(!str_detect(location, "^[:upper:]{3}\\s"), 
         !str_detect(location, "^[:upper:]{3}\\d{2}")) %>% 
  mutate(pop = parse_number(pop)) %>% 
  select(oblast, obshtina, everything())
write_parquet(loc_sex_n, "shiny/demography/loc_sex.parquet")

loc_sex_n %>% 
  filter(year == 2023) %>% 
  summarise(obl_pop = sum(pop, na.rm = T), .by = c(oblast, sex)) %>% 
  ggplot(aes(obl_pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.05, 0.5))) +
  geom_text(aes(label = obl_pop), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_wrap(~ oblast)
loc_sex_n %>% drop_na() %>% 
  filter(oblast == "BLG Благоевград", 
         obshtina == "BLG03 Благоевград", 
         location == "68792 с. Логодаж") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
  geom_text(aes(label = pop), 
            position = position_dodge(width = 1), hjust = -0.1, size = 4.5, angle = 90) +
  theme(text = element_text(size = 16), legend.position = "right") +
  labs(x = NULL, y = "Брой жители", fill = "Пол:")
#-------------------------------------------------------------------
obsh_sex <- read_delim("data/obsh_sex.csv", col_names = F, na = "-")

obsh_sex_n <- obsh_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:94, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "settlement", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obsh_sex_n %>% count(obshtina) %>% view

obsh_sex_n %>% 
  filter(obshtina == "Плевен", year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = expansion(mult = 0.23)) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_wrap(~ settlement)
#------------------------------------------
obl_age_sex <- read_delim("obl_age_sex.csv", col_names = F, na = "-") %>% 
  select(-X8) %>% mutate(across(everything(), as.character))

obl_age_sex_n <- obl_age_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "NA_NA", "year"),
         `2` = str_replace(`2`, "_", "oblast"),
         `3` = str_replace(`3`, "_", "age")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(4:7, names_to = "name", values_to = "pop") %>% 
  separate(name, c("settlement", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% 
  mutate(age = str_remove_all(age, " "), age = fct_inorder(age)) %>% drop_na()
obl_age_sex_n %>% count(oblast) %>% view
write_parquet(obl_age_sex_n, "shiny/demography/obl_age_sex.parquet")

obl_age_sex_n %>% 
  mutate(age = str_remove_all(age, " "), age = fct_inorder(age)) %>% 
  filter(oblast == "Кърджали", year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_grid(age ~ settlement)
#------------------------------------------
obsh_age_sex <- read_delim("data/obsh_age_sex.csv", col_names = F, na = "-")

obsh_age_sex_n <- obsh_age_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "sett"),
         `2` = str_replace(`2`, "_", "settlement"),
         `3` = str_replace(`3`, "_", "sex")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(4:123, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "age"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obsh_age_sex_n %>% count(sett) %>% view

obsh_age_sex_n %>% 
  mutate(age = str_remove_all(age, " "), age = fct_inorder(age)) %>% 
  filter(sett == "Столична", year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_grid(age ~ settlement)
#------------------------------------------
obl_age <- read_delim("data/obl_age.csv", na = "-") %>% 
  mutate(across(everything(), as.character))

obl_age_n <- obl_age %>%
  pivot_longer(3:83, names_to = "age", values_to = "pop") %>% 
  select(oblast, year, age, pop) %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_age_n %>% count(oblast) %>% view

obl_age_n %>% 
  mutate(age = fct_inorder(age)) %>% 
  filter(oblast == "София (столица)", year == 2022) %>% 
  ggplot(aes(pop, age, fill = age)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:")
#------------------------------------------
labor_sett_sex <- read_delim("labor_sett_sex.csv", col_names = F, na = "-") %>% 
  mutate(X1 = case_when(row_number() %in% c(227:229) & X1 == "Бяла" ~ "Бяла (Русенско)",
                        row_number() %in% c(278:280) & X1 == "Бяла" ~ "Бяла (Варненско)", .default = X1))

labor_sett_sex_n <- labor_sett_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "obsh"),
         `2` = str_replace(`2`, "__", "labor")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:51, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sett", "sex"), sep = "_") %>%
  select(obsh, year, sett, labor, sex, pop) %>% 
  mutate(pop = parse_number(pop), 
         labor = fct_relevel(labor, "Под трудоспособна възраст",
                             "В трудоспособна възраст",
                             "Над трудоспособна възраст")) %>% 
  drop_na()
labor_sett_sex_n %>% count(obsh) %>% view
write_parquet(labor_sett_sex_n, "shiny/demography/labor_sett_sex.parquet")

labor_sett_sex_n %>% 
  filter(obsh == "Бургас", year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = colors) +
  scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_wrap(vars(labor))
#------------------------------------------
cit_age_sex <- read_delim("data/cit_age_sex.csv", col_names = F, na = "-")

cit_age_sex_n <- cit_age_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "sex"),
         `2` = str_replace(`2`, "_", "citizenship")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:91, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "age"), sep = "_") %>%
  select(citizenship, year, age, sex, pop) %>% 
  mutate(pop = parse_number(pop), age = str_remove_all(age, " ")) %>% drop_na()
cit_age_sex_n %>% count(obsh) %>% view

cit_age_sex_n %>% 
  mutate(citizenship = str_wrap(citizenship, width = 15)) %>% 
  filter(year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Мъже" = "#F8766D", "Жени" = "#00BFC4")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_grid(age ~ citizenship, scales = "free_x")
#------------------------------------------
cit_birth_sex <- read_delim("data/cit_birth_sex.csv", col_names = F, na = "-")

cit_birth_sex_n <- cit_birth_sex %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "birth_cit")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:24, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sex"), sep = "_") %>%
  select(birth_cit, year, sex, pop) %>% 
  mutate(pop = parse_number(pop)) %>% drop_na()
cit_birth_sex_n %>% count(obsh) %>% view

cit_birth_sex_n %>% 
  filter(year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Мъже" = "#F8766D", "Жени" = "#00BFC4")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:") +
  facet_wrap(vars(birth_cit), scales = "free_x")
#------------------------------------------
obl_birth <- read_delim("data/obl_birth.csv", col_names = F, na = "-")

obl_birth_n <- obl_birth %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "oblast")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:48, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "birth"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_birth_n %>% count(obsh) %>% view

obl_birth_n %>% 
  filter(year == 2022) %>% 
  ggplot(aes(pop, birth, fill = birth)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Живородено" = "#00BFC4", "Мъртвородено" = "#F8766D")) +
  scale_x_log10(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой раждания", title = "", fill = "Пол:") +
  facet_wrap(vars(oblast), scales = "free_x")
#------------------------------------------
obl_marr <- read_delim("data/obl_marr.csv", col_names = F, na = "-") %>% 
  mutate(across(everything(), as.character)) %>% select(-X8)

obl_marr_n <- obl_marr %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "NA_NA", "year"),
         `2` = str_replace(`2`, "_", "oblast"),
         `3` = str_replace(`3`, "_", "marriage")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(4:7, names_to = "name", values_to = "pop") %>% 
  separate(name, c("sett", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_birth_n %>% count(obsh) %>% view

obl_marr_n %>% 
  filter(oblast == "Варна", year == 2022) %>% 
  ggplot(aes(pop, sex, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Mомичета" = "#00BFC4", "Mомчета" = "#F8766D")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой раждания", fill = "Пол:") +
  facet_grid(marriage ~ sett, scales = "free_x")
#------------------------------------------
obsh_sex_birth <- read_delim("data/obsh_sex_birth.csv", col_names = F, na = "-") %>% 
  select(-X48)

obsh_sex_birth_n <- obsh_sex_birth %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:47, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_birth_n %>% count(obsh) %>% view

obsh_sex_birth_n %>% 
  filter(obshtina == "Елхово") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Mомичета" = "#00BFC4", "Mомчета" = "#F8766D")) +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "right") +
  labs(y = NULL, x = "Брой раждания", fill = "Пол:")
#------------------------------------------
obsh_sett_marr <- read_delim("data/obsh_sett_marr.csv", col_names = F, na = "-")

obsh_sett_marr_n <- obsh_sett_marr %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:93, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sett", "marriage"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_birth_n %>% count(obsh) %>% view

obsh_sett_marr_n %>% 
  filter(obshtina == "Бургас", year == 2022) %>% 
  ggplot(aes(pop, marriage, fill = marriage)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Брачно" = "#00BFC4", "Извънбрачно" = "#F8766D")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой раждания", fill = "Пол:") +
  facet_wrap(vars(sett))
#------------------------------------------
birth_rate <- read_delim("birth_rate.csv", col_names = F, na = "-") %>% 
  select(-X99)

birth_rate_n <- birth_rate %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "sett"),
         `2` = str_replace(`2`, "_", "oblast")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:98, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "coef"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_birth_n %>% count(obsh) %>% view
write_parquet(birth_rate_n, "shiny/demography//birth_rate.parquet")

birth_rate_n %>% 
  filter(coef == "Средна възраст на майката при раждане на дете (години)", year == 2023) %>% 
  ggplot(aes(pop, sett, fill = sett)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("В градовете" = "#00BFC4", "В селата" = "#F8766D")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Коефициент", fill = "Пол:") +
  facet_wrap(vars(oblast))
#------------------------------------------
mortality <- read_delim("mortality.csv", col_names = F, na = "-") %>% 
  mutate(across(everything(), as.character)) %>% select(-X11)

mortality_n <- mortality %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "NA_NA_NA", "year"),
         `2` = str_replace(`2`, "__", "oblast")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:10, names_to = "name", values_to = "pop") %>% 
  separate(name, c("coef", "sett", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
obl_birth_n %>% count(obsh) %>% view
write_parquet(mortality_n, "shiny/demography//mortality.parquet")

mortality_n %>% 
  filter(coef == "Коефициент на обща смъртност", year == 2022) %>% 
  ggplot(aes(pop, sett, fill = sex)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "right") +
  labs(y = NULL, x = "Коефициент (%)", fill = "Пол:") +
  facet_wrap(vars(oblast)) +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
brakove <- read_delim("brakove.csv", col_names = F, na = "-") %>% select(-X48) %>% 
  mutate(X1 = case_when(row_number() == 78 & X1 == "Бяла" ~ "Бяла (Русенско)",
                        row_number() == 95 & X1 == "Бяла" ~ "Бяла (Варненско)", .default = X1))

brakove_n <- brakove %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:47, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sett"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
brakove_n %>% count(obsh) %>% view
write_parquet(brakove_n, "shiny/demography//brakove.parquet")

brakove_n %>% 
  filter(obshtina == "Бяла") %>% 
  ggplot(aes(pop, sett, fill = sett)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("В градовете" = "#00BFC4", "В селата" = "#F8766D")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой бракове", fill = "Пол:") +
  facet_wrap(vars(year))
#------------------------------------------
razvodi <- read_delim("razvodi.csv", col_names = F, na = "-") %>% select(-X50) %>%
  mutate(X1 = case_when(row_number() == 78 & X1 == "Бяла" ~ "Бяла (Русенско)",
                        row_number() == 95 & X1 == "Бяла" ~ "Бяла (Варненско)", .default = X1))

razvodi_n <- razvodi %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:49, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sett"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
razvodi_n %>% count(obsh) %>% view
write_parquet(razvodi_n, "shiny/demography/razvodi.parquet")

razvodi_n %>% 
  filter(obshtina == "Благоевград") %>% 
  ggplot(aes(pop, sett, fill = sett)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("В градовете" = "#00BFC4", "В селата" = "#F8766D")) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(y = NULL, x = "Брой разводи", fill = "Пол:") +
  facet_wrap(vars(year))
#------------------------------------------
int_migration <- read_delim("int_migration.csv", col_names = F, na = "-") %>% select(-X147) %>% 
  mutate(X1 = case_when(row_number() %in% c(153:154) & X1 == "Бяла" ~ "Бяла (Русенско)",
                        row_number() %in% c(187:188) & X1 == "Бяла" ~ "Бяла (Варненско)", .default = X1))

int_migration_n <- int_migration %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "obshtina"),
         `2` = str_replace(`2`, "__", "sett")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:146, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "migrated", "sex"), sep = "_") %>%
  mutate(migrated = fct_relevel(migrated, "Заселени", "Изселени", "Механичен прираст"), 
         pop = parse_number(pop)) %>% drop_na()
int_migration_n %>% count(obsh) %>% view
write_parquet(int_migration_n, "shiny/demography//int_migration.parquet")

int_migration_n %>% 
  filter(obshtina == "Елхово", sett == "В селата") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "right") +
  labs(y = NULL, x = NULL, fill = "Пол:") +
  facet_wrap(vars(migrated), ncol = 1)
#------------------------------------------
ext_migration <- read_delim("data/ext_migration.csv", col_names = F, na = "-")

ext_migration_n <- ext_migration %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "sex"),
         `2` = str_replace(`2`, "_", "age")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:53, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "migrated"), sep = "_") %>%
  mutate(migrated = fct_relevel(migrated, "Заселени", "Изселени", "Механичен прираст"), 
         pop = parse_number(pop),
         age = str_remove_all(age, "\\s")) %>% drop_na()
ext_migration_n %>% count(obsh) %>% view
write_parquet(ext_migration_n, "shiny/demography//ext_migration.parquet")

ext_migration_n %>% 
  filter(age == "20-24") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(breaks = NULL, expand = expansion(mult = c(.01, .55))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 14), legend.position = "right") +
  labs(y = NULL, x = NULL, fill = "Пол:") +
  facet_wrap(vars(migrated), ncol = 1) +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
school <- read_delim("school.csv", col_names = F, na = "-") %>% select(-X50)

school_n <- school %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:49, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "education"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
school_n %>% count(location) %>% view
write_parquet(school_n, "shiny/demography//school.parquet")

school_n %>% 
  filter(obshtina == "SOF46 Столична") %>% 
  ggplot(aes(year, pop, fill = education)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Средно образование" = "#00BFC4", "Основно образование" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(y = "Брой ученици", x = NULL, fill = "Пол:") +
  facet_wrap(vars(education), ncol = 1) +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
university <- read_delim("university.csv", col_names = F, na = "-") %>% select(-X98)

university_n <- university %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "oblast")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:97, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "grade", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop),
         grade = fct_relevel(grade, "Магистър", "Бакалавър", "Професионален бакалавър")) %>% drop_na()
school_n %>% count(location) %>% view
write_parquet(university_n, "shiny/demography//university.parquet")

university_n %>% 
  filter(oblast == "Ямбол") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 16), legend.position = "right") +
  labs(y = "Брой студенти", x = NULL, fill = "Пол:") +
  facet_wrap(vars(grade), ncol = 1) +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
# health1 <- read_delim("data/health1.csv", col_names = F, na = "-") %>% select(-X842)
health1 <- read_csv2("data/health1.csv", col_names = F, na = "-")
health2 <- read_csv2("health2.csv", col_names = F, na = "-") %>% select(-X226)

health1_n <- health1 %>%
  slice(-c(1:1)) %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "zabolqvane")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:841, names_to = "name", values_to = "pop") %>%
  separate(name, c("year", "oblast", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()

health2_n <- health2 %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "zabolqvane")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:225, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "oblast", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()

health_n <- bind_rows(health1_n, health2_n)
health_n %>% count(zabolqvane) %>% view
write_parquet(health_n, "shiny/demography//health.parquet")

health_n %>% 
  filter(oblast == "София", zabolqvane == "Злокачествени новообразувания (C00-C97)") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 16), legend.position = "none") +
  labs(y = "Брой починали", x = NULL, fill = "Пол:") +
  facet_wrap(vars(sex), ncol = 1) +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
kinder_gardens <- read_delim("kinder_gardens.csv", col_names = F, na = "-") %>% select(-X50) %>% 
  mutate(X1 = case_when(row_number() == 78 & X1 == "Бяла" ~ "Бяла (Русенско)",
                        row_number() == 95 & X1 == "Бяла" ~ "Бяла (Варненско)", .default = X1))

kinder_gardens_n <- kinder_gardens %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:49, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop),
         sex = fct_recode(sex, "Момчета" = "Мъже", "Момичета" = "Жени")) %>% drop_na()
school_n %>% count(location) %>% view
write_parquet(kinder_gardens_n, "shiny/demography//kinder_gardens.parquet")

kinder_gardens_n %>% 
  filter(obshtina == "Бойница") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Момичета" = "#00BFC4", "Момчета" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = pop), 
            position = position_dodge(width = 1), hjust = -0.1, size = 4, angle = 90) +
  theme(text = element_text(size = 16), legend.position = "right", 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Брой деца", x = NULL, fill = "Пол:") +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
poverty <- read_delim("poverty.csv", col_names = F, na = "-") %>% select(-X19)

poverty_n <- poverty %>%
  #slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:3, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "_", "sex"),
         `2` = str_replace(`2`, "_", "age")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:18, names_to = "name", values_to = "pop") %>% 
  separate(name, c("perc", "year"), sep = "_") %>%
  mutate(pop = parse_number(pop)) %>% drop_na()
school_n %>% count(location) %>% view
write_parquet(poverty_n, "shiny/demography//poverty.parquet")

poverty_n %>% 
  #filter(obshtina == "Бойница") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = pop), 
            position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 16), legend.position = "right") +
  labs(y = "Процент от работещите", x = NULL, fill = "Пол:") +
  facet_wrap(vars(age), ncol = 1) +
  guides(fill = guide_legend(reverse = TRUE))
#------------------------------------------
potreblenie <- read_delim("potreblenie.csv", col_names = c("oblast", "product", "2008", "2009", "2010",
                                                            "2011", "2012", "2013", "2014", "2015", "2016",
                                                            "2017", "2018", "2019", "2020", "2021", "2022",
                                                            "2023"), na = "-") %>% select(-X19) %>% slice(-c(1:1)) %>% 
  mutate(across(everything(), as.character)) 

potreblenie_n <- potreblenie %>%
  #slice(-c(1:1)) %>% 
  # rownames_to_column() %>%
  # pivot_longer(-rowname) %>%
  # pivot_wider(names_from = rowname, values_from = value) %>%
  # unite("united", 2:3, sep = "_") %>%
  # rownames_to_column() %>%
  # pivot_longer(-rowname) %>%
  # pivot_wider(names_from = rowname, values_from = value) %>%
  # select(!name) %>%
  # slice(-c(1:1)) %>%
  # mutate(`1` = str_replace(`1`, "_", "sex"),
  #        `2` = str_replace(`2`, "_", "age")) %>%
  # janitor::row_to_names(row_number = 1) %>%
  pivot_longer(3:18, names_to = "year", values_to = "value") %>% 
  # separate(name, c("perc", "year"), sep = "_") %>%
  mutate(value = parse_number(value)) %>% drop_na()
school_n %>% count(location) %>% view
write_parquet(potreblenie_n, "shiny/demography/potreblenie.parquet")

potreblenie_n %>% 
  filter(product == "Хляб и тестени изделия - кг", oblast == "Ямбол") %>% 
  ggplot(aes(year, value)) +
  geom_col(position = position_dodge2(preserve = "single"), fill = "#00BFC4") +
  #scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 16), legend.position = "right") +
  labs(y = "Потребление", x = NULL)
#------------------------------------------
prestupnost <- read_delim("prestupnost.csv", col_names = F, na = "-") %>% select(-X322)

prestupnost_n <- prestupnost %>%
  slice(-c(1:1)) %>% 
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  unite("united", 2:4, sep = "_") %>%
  rownames_to_column() %>%
  pivot_longer(-rowname) %>%
  pivot_wider(names_from = rowname, values_from = value) %>%
  select(!name) %>%
  slice(-c(1:1)) %>%
  mutate(`1` = str_replace(`1`, "__", "oblast")) %>%
  janitor::row_to_names(row_number = 1) %>%
  pivot_longer(2:321, names_to = "name", values_to = "pop") %>% 
  separate(name, c("year", "age", "sex"), sep = "_") %>%
  mutate(pop = parse_number(pop), 
         age = str_remove_all(age, " "),
         age = str_replace(age, "60иповече", "60 и повече")) %>% drop_na()
school_n %>% count(location) %>% view
write_parquet(prestupnost_n, "shiny/demography/prestupnost.parquet")

prestupnost_n %>% 
  filter(oblast == "Ямбол") %>% 
  ggplot(aes(year, pop, fill = sex)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
  geom_text(aes(label = pop), 
            position = position_dodge(width = 1), vjust = -0.1, size = 4) +
  theme(text = element_text(size = 16), legend.position = "right",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(y = "Брой осъдени", x = NULL, fill = "Пол:") +
  facet_wrap(vars(age), ncol = 2, dir = "v") +
  guides(fill = guide_legend(reverse = TRUE))
#==========================================
# Maps-------------------------
loc_sex_n %>%
  mutate(oblast = str_remove(oblast, "^[:upper:]{3}\\s")) %>% 
  filter(year == "2022") %>%
  group_by(oblast) %>%
  summarise(s = sum(pop, na.rm = T)) %>%
  mutate(
    col = case_when(
      s <= 100000 ~ "<100000",
      s > 100000 & s <= 200000 ~ "100000-200000",
      s > 200000 & s <= 300000 ~ "200000-300000",
      s > 300000 & s <= 400000 ~ "300000-400000",
      s > 400000 & s <= 500000 ~ "400000-500000",
      s > 500000 & s <= 700000 ~ "500000-700000",
      s > 1000000 ~ ">1000000"
    ),
    col = fct_reorder(col, s),
    oblast = fct_recode(oblast, "София – област" = "София",
                        "София" = "София (столица)")) -> oblasti_df
obl_map %>%
  left_join(oblasti_df, by = c("oblast_bg" = "oblast")) %>%
  mutate_if(is.numeric, round, 1) -> oblasti
oblasti %>%
  ggplot() +
  geom_sf(aes(fill = col), alpha = .4) +
  geom_sf_text(aes(label = oblast_bg), check_overlap = TRUE, size = 4) +
  theme(
    text = element_text(size = 16), legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Население:") +
  scale_fill_brewer(palette = "PuRd")
#+
# facet_wrap(~vote_date, ncol = 2) #+
# annotation_scale(location = "br", width_hint = 0.2, text_cex = 1.1) +
# annotation_north_arrow(location = "tr", which_north = "true",
# pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
# style = north_arrow_fancy_orienteering)

location_sex %>%
  filter(year == "2021", !oblast == "България") %>%
  group_by(oblast) %>%
  summarise(s = sum(pop, na.rm = T)) %>%
  mutate(
    col = case_when(
      s > 50000 & s <= 100000 ~ "50000-100000",
      s > 100000 & s <= 200000 ~ "100000-200000",
      s > 200000 & s <= 300000 ~ "200000-300000",
      s > 300000 & s <= 400000 ~ "300000-400000",
      s > 400000 & s <= 500000 ~ "400000-500000",
      s > 500000 ~ ">500000"
    ),
    col = fct_reorder(col, s)
  ) -> oblasti_df
oblasti_map %>%
  left_join(oblasti_df, by = c("oblast_bg" = "oblast")) %>%
  mutate_if(is.numeric, round, 1) -> oblasti
oblasti %>%
  ggplot() +
  geom_sf(aes(fill = col), alpha = .4) +
  geom_sf_text(aes(label = oblast_bg), check_overlap = TRUE, size = 4) +
  theme(
    text = element_text(size = 16), legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()) +
  labs(x = NULL, y = NULL, fill = "Население:",
    title = "Население в булгаристанските области към 'лето Господне' 2021-во!") +
  scale_fill_brewer(palette = "Set1")
#------------------------------------
sentenced <- sentenced %>% 
	slice(-c(1:1)) %>% 
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>% 
	unite("united", 2:3, sep = "_") %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	select(!name) %>%
	slice(-c(1:1)) %>% 
	mutate(`1` = str_replace(`1`, "NA_NA", "oblast")) %>%
	janitor::row_to_names(row_number = 1) %>%
	pivot_longer(2:92, names_to = "name", values_to = "value") %>%
	separate(name, c("year", "sentence"), sep = "_")
write_csv(deaths, "data/deaths.csv")

ipc <- ipc %>%
	slice(-c(1:1)) %>% 
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	unite("united", 2:4, sep = "_") %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	select(!name) %>%
	slice(-c(1:1)) %>%
	mutate(`1` = str_replace(`1`, "__", "item")) %>%
	janitor::row_to_names(row_number = 1) %>%
	pivot_longer(2:337, names_to = "name", values_to = "inflation") %>%
	separate(name, c("index", "year", "month"), sep = "_") %>%
	mutate(month = fct_inorder(month), 
				 inflation = as.double(inflation), 
				 year = as.integer(year),
				 col = inflation > 0) %>% 
	drop_na(inflation)
glimpse(ipc)

ipc %>% count(item) %>% View()

ipc %>% 
	filter(year %in% c(2022), item %in% c("Хранителни", "Водоснабдяване", "Автомобили", "Услуги", 
																			"Здравеопазване", "Горива и смазочни материали за ЛТС")) %>% 
	#group_by(year) %>% 
	#summarise(tot_inf = sum(inflation, na.rm = T)) %>% 
	#mutate(col = tot_inf > 0) %>% 
	ggplot(aes(inflation, month, fill = col)) +
	geom_col(show.legend = F) +
	geom_text(aes(label = paste0(inflation, "%")), 
						position = position_dodge(width = 1), hjust = -0.2, size = 5) +
	scale_x_continuous(expand = expansion(mult = c(.05, .2))) +
	#geom_vline(aes(xintercept = mean(inflation)), linewidth = 0.5, lty = 2, color = "black") +
	#annotate("text", label = "Средна инфлация = 15.3%", x = 17, y = 1.5, size = 5) +
	labs(x = NULL, y = NULL) + facet_wrap(~ item) +
	theme(text = element_text(size = 16), axis.text.x = element_blank(), axis.ticks.x = element_blank())

icp %>% 
	filter(item == "Общ ИПЦ", !year %in% c("1995", "1996", "1997")) %>% 
	ggplot(aes(inflation, month, fill = col)) +
	geom_col(show.legend = F) +
	labs(y = NULL, x = "Инфлация (%)") +
	facet_wrap(~year)
#------------------
ethno <- ethno %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	unite("united", 2:3, sep = "_") %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	select(!name) %>%
	slice(-c(1:1)) %>%
	mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
	janitor::row_to_names(row_number = 1) %>%
	pivot_longer(2:8, names_to = "name", values_to = "value") %>%
	separate(name, c("year", "ethno"), sep = "_") %>%
	mutate(obshtina = str_sub(obshtina, 7L, 50L), obshtina = str_squish(obshtina)) %>% 
	drop_na(value) %>% 
	mutate(across(1:3, as.factor)) %>% 
	mutate(value = as.double(value))
ethno %>% 
	filter(obshtina %in% c("Ямбол", "Елхово", "Тунджа", "Болярово")) %>% 
	group_by(obshtina) %>% 
	mutate(sum = sum(value), prop = value / sum) %>% 
	mutate(ethno = reorder(ethno, prop)) %>% 
	ggplot(aes(prop, ethno, fill = ethno)) +
	geom_col(show.legend = F) +
	geom_text(aes(label = scales::percent(round(prop, digits = 3))), 
						position = position_dodge(width = 1), hjust = -0.1, size = 5) +
	theme(text = element_text(size = 18), axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) +
	scale_x_continuous(expand = expansion(mult = c(.05, .12))) +
	labs(x = NULL, y = "Етническа принадлежност", 
			 title = "Етническата принадлежност на населението на област Ямбол - лето Господне 2021-во!") +
	facet_wrap(~ obshtina)
ethno %>% group_by(ethno) %>% 
	summarise(s = sum(value)) %>% 
	mutate(g = sum(s), prop = s / g) %>% 
	mutate(ethno = reorder(ethno, prop)) %>% 
	ggplot(aes(prop, ethno, fill = ethno)) +
	geom_col(show.legend = F) +
	geom_text(aes(label = scales::percent(round(prop, digits = 4))), 
						position = position_dodge(width = 1), hjust = -0.1, size = 5) +
	theme(text = element_text(size = 18), axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) +
	scale_x_continuous(expand = expansion(mult = c(.05, .12))) +
	labs(x = NULL, y = "Етническа принадлежност", 
			 title = "Етническата принадлежност на населението на Булгаристан - лето Господне 2021-во!")
#------------------
religion <- religion %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	unite("united", 2:3, sep = "_") %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	select(!name) %>%
	slice(-c(1:1)) %>%
	mutate(`1` = str_replace(`1`, "_", "obshtina")) %>%
	janitor::row_to_names(row_number = 1) %>%
	pivot_longer(2:9, names_to = c("year", "religion"), 
							 values_to = "value", names_sep = "_",
							 values_drop_na = T) %>%
	mutate(obshtina = str_sub(obshtina, 7L, 50L), 
				 obshtina = str_squish(obshtina)) %>% 
	mutate(across(1:3, as.factor)) %>% 
	mutate(value = as.double(value))
religion %>% 
	filter(obshtina %in% c("Столична", "Пловдив", "Варна", "Бургас", "Русе", "Плевен"), value > 0) %>% 
	group_by(obshtina) %>% 
	mutate(sum = sum(value), prop = value / sum) %>% 
	mutate(religion = reorder(religion, prop)) %>% 
	ggplot(aes(prop, religion, fill = religion)) +
	geom_col(show.legend = F) +
	geom_text(aes(label = scales::percent(round(prop, digits = 4))), 
						position = position_dodge(width = 1), hjust = -0.1, size = 5) +
	theme(text = element_text(size = 18), axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) +
	scale_x_continuous(expand = expansion(mult = c(.05, .3))) +
	labs(x = NULL, y = "Вероизповедание", title = NULL) +
	facet_wrap(~ obshtina, ncol = 3)
religion %>% group_by(religion) %>% 
	summarise(s = sum(value)) %>% 
	mutate(g = sum(s), prop = s / g) %>% 
	mutate(religion = reorder(religion, prop)) %>% 
	ggplot(aes(prop, religion, fill = religion)) +
	geom_col(show.legend = F) +
	geom_text(aes(label = scales::percent(round(prop, digits = 4))), 
						position = position_dodge(width = 1), hjust = -0.1, size = 5) +
	theme(text = element_text(size = 18), axis.text.x = element_blank(),
				axis.ticks.x = element_blank()) +
	scale_x_continuous(expand = expansion(mult = c(.05, .12))) +
	labs(x = NULL, y = "Вероизповедание", 
			 title = "Религиозна принадлежност на населението на Булгаристан - лето Господне 2021-во!")
#------------------
energy_bg <- energy_bg %>%
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	unite("united", 2:3, sep = "_") %>% 
	rownames_to_column() %>%
	pivot_longer(-rowname) %>%
	pivot_wider(names_from = rowname, values_from = value) %>%
	select(!name) %>% 
	slice(-c(1:1)) %>% 
	mutate(`1` = str_replace(`1`, "_", "type")) %>%
	janitor::row_to_names(row_number = 1) %>% 
	pivot_longer(2:243, names_to = c("year", "fuel"), 
							 values_to = "th_tons_oil_ekv", names_sep = "_",
							 values_drop_na = T) %>%
	mutate(across(1:3, as.factor)) %>% 
	mutate(th_tons_oil_ekv = as.double(th_tons_oil_ekv))


