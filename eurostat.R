library(tidyverse)
library(eurostat)
library(scales)
library(arrow)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(sf)
# library(patchwork)

toc <- get_eurostat_toc() %>% 
	janitor::clean_names() %>% 
	select(-values, -last_table_structure_change) %>% 
	filter(type %in% c("table", "dataset")) %>% 
	distinct()

eur <- ne_download(scale = 50, type = "sovereignty", returnclass = "sf") %>% 
  janitor::clean_names() %>% 
  select(name, geometry) %>% 
  mutate(name = fct_recode(name, "Czechia" = "Czech Rep.", "North Macedonia" = "Macedonia",
                           "Bosnia and Herzegovina" = "Bosnia and Herz."))
#-------------------------------------------------------------------------
write_parquet(prc_hicp_mmor, "shiny/eurostat/prc_hicp_mmor.parquet")
prc_hicp_mmor %>% map_dfr(~ sum(is.na(.)))

nama_10_pc <- get_eurostat("nama_10_pc", type = "label", time_format = "date") %>%
   mutate_if(is_character, as_factor)

tps00155 %>% 
  filter(TIME_PERIOD == "2024-01-01") %>%
  mutate(geo = fct_reorder(geo, values),
         col = if_else(geo == "Bulgaria", "1", "0")) %>% 
  ggplot(aes(values, geo, fill = col)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
  geom_text(aes(label = space_s(values)),
            position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
  scale_fill_manual(values = c("gray50", "red")) +
  theme(text = element_text(size = 14), legend.position = "none") +
  labs(x = "Минимална заплата (\u20AC) към 01.01.2024 г.", y = NULL, 
       caption = "Източник на данните: Eurostat")

hlth_cd_apr %>% 
  filter(icd10 == "Total",
         mortalit == "Total",
         sex == "Total",
         unit == "Rate",
         !str_detect(geo, "^Euro")) %>% 
  ggplot(aes(TIME_PERIOD, values)) +
  geom_line() +
  facet_wrap(vars(geo))

apri_ap_crpouta %>% filter(geo == "Bulgaria") %>% 
  count(prod_veg) %>% view

tps00155 %>%
  filter(currency == "National currency", 
         geo == "Bulgaria", 
         #prod_ani == "Fattening lambs - prices per 100 kg live weight",
         !geo %in% c("Germany including former GDR", "France (metropolitan)"),
         !str_detect(geo, "^Euro")) %>% 
  mutate(values_1 = values / 100) %>% 
  ggplot(aes(TIME_PERIOD, values_1)) +
  geom_smooth(se = F) +
  geom_point() +
  scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  # geom_text(aes(label = round(values_1, 2)),
  #           position = position_dodge(width = 1),
  #           vjust = -0.5, size = 4, color = "black") +
  labs(x = "Year", y = "Soft wheat - prices per 1 t") +
  theme(text = element_text(size = 14)) +
  facet_wrap(vars(prod_ani), scales = "free_y")

teina205 %>% 
  filter(TIME_PERIOD == "2023-04-01", 
         unit == "Percentage of gross domestic product (GDP), (SCA)",
         !geo %in% c("European Union - 27 countries (from 2020)",
                  "Euro area - 19 countries  (2015-2022)",
                  "Euro area – 20 countries (from 2023)")) %>% 
  mutate(col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1")) %>% 
  mutate(geo = fct_reorder(geo, values)) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(x = "", y = NULL) +
  theme(text = element_text(size = 18))

demo_find %>% 
  filter(indic_de == "Total fertility rate", geo == "Bulgaria") %>% 
  ggplot(aes(time, values)) +
  geom_point() +
  geom_path() +
  scale_x_date(breaks = "2 years", date_labels = "%Y") +
  geom_text(aes(label = values), check_overlap = T,
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 4, color = "black")

hlth_ehis_bm1e %>% 
  filter(time == "2019-01-01", bmi == "Normal", age == "Total",
         isced11 == "Tertiary education (levels 5-8)",
         !geo %in% c("European Union - 27 countries (from 2020)", 
                  "Euro area - 19 countries  (2015-2022)",
                  "Euro area – 20 countries (from 2023)")) %>% 
  mutate(col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
         sex = fct_recode(sex, "Жени" = "Females", "Мъже" = "Males", "Общо" = "Total")) %>% 
  mutate(geo = fct_reorder(geo, values)) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(x = "", y = NULL) +
  theme(text = element_text(size = 18)) +
  facet_wrap(vars(sex))

tps00205 %>% 
  filter(time == "2022-01-01", geo != "European Union - 27 countries (from 2020)") %>% 
  mutate(col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
         sex = fct_recode(sex, "Жени" = "Females", "Мъже" = "Males", "Общо" = "Total")) %>% 
  mutate(geo = fct_reorder(geo, values)) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(x = "Очаквана продължителност на живота (към 01.01.2022 г.)", y = NULL) +
  theme(text = element_text(size = 18)) +
  facet_wrap(vars(sex))

sdg_03_11 %>% 
  filter(time == "2021-01-01", geo != "European Union - 27 countries (from 2020)") %>% 
  mutate(col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
         sex = fct_recode(sex, "Жени" = "Females", "Мъже" = "Males", "Общо" = "Total")) %>% 
  mutate(geo = fct_reorder(geo, values)) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(x = "Очаквани години здравословен живот (към 01.01.2021 г.)", y = NULL) +
  theme(text = element_text(size = 18)) +
  facet_wrap(vars(sex))

tps00155 %>% 
  filter(time == "2023-07-01") %>% 
  mutate(col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1")) %>% 
  mutate(geo = fct_reorder(geo, values)) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = paste0(values, " \u20AC")), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(x = "Минимална заплата към 1.07.2023 г.", y = NULL) +
  theme(text = element_text(size = 18))

prices <- prc_hicp_mmor %>%
	mutate(geo = fct_recode(geo, "Turkey" = "Türkiye", 
													"Germany" = "Germany (until 1990 former territory of the FRG)",
													"United Kingdom" = "United Kingdom",
													"Kosovo" = "Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
	filter(coicop == "All-items HICP", time > "2022-11-30") %>% 
	group_by(coicop, geo) %>% 
	summarise(sm = sum(values, na.rm = T)) %>% 
	mutate_if(is.numeric, round, 1)
df <- eur %>% 
	inner_join(prices, by = c("name" = "geo"))
df %>% 
	ggplot() +
	geom_sf(aes(fill = sm), alpha = 0.4) +
	coord_sf(xlim = c(-25, 40), ylim = c(34, 72)) + 
	geom_sf_text(aes(label = paste0(sm, "%")), check_overlap = TRUE, size = 3.5) + 
	scale_fill_gradient(low = "white", high = "red") +
	labs(x = NULL, y = NULL, fill = "%") +
	theme(text = element_text(size = 16), legend.position = "none",
				axis.text = element_blank(),
				axis.ticks = element_blank()) +
	facet_wrap(~ coicop, ncol = 3)

for_sup_cp <- get_eurostat("for_sup_cp", type = "code", time_format = "num") %>%
  mutate_if(is_character, as_factor)

for_sup_cp %>% 
	filter(str_detect(for_acc, "P1"), 
				 time > 2010 & time < 2022, 
				 geo == "BG",
				 unit == "MIO_EUR",
				 !geo %in% c("EU27_2020", "European Union - 27 countries (from 2020)"),
				 !for_acc == "P1") %>% 
	mutate(values = round(values, digits = 0),
				 for_acc = fct_recode(for_acc, 
				 										 "Goods characteristic of forestry" = "P1_GD",
				 										 "Trees, tree plants and forest tree seeds" = "P1_TR_PL",
				 										 "Forest trees" = "P1_TR",
				 										 "Roundwood" = "P1_RW",
				 										 "Industrial roundwood" = "P1_RWIN",
				 										 "Fuel wood" = "P1_RWFW",
				 										 "Services output characteristic of forestry" = "P1_SERV",
				 										 "Products from connected secondary activities in the local KAU" = "P1_SEC",
				 										 "Output for own final use" = "P12",
				 										 "Live forest tree plants and tree seeds" = "P1_PL",
				 										 "Other products" = "P1_O",
				 										 "Non-wood products" = "P1_NW")) %>% 
	ggplot(aes(values, fct_reorder(for_acc, values), fill = for_acc)) +
	geom_col(position = "dodge") +
	scale_x_continuous(expand = expansion(mult = c(0, 0.21))) +
	scale_y_discrete(labels = function(x) str_wrap(x, 40)) +
	geom_text(aes(label = values), position = position_dodge(width = 1), 
						hjust = -0.2, size = 3.5, color = "black") +
	labs(x = "Милиони евро", y = NULL, fill = "Легенда:") +
	facet_wrap(~ time, ncol = 5) +
	theme(legend.position = "none", text = element_text(size = 12))
#---------------------
# Energy prices %
teicp250 <- get_eurostat("teicp250", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

prices <- teicp250 %>% 
	mutate(geo = fct_recode(geo, "Turkey" = "Türkiye", 
									 "Germany" = "Germany (until 1990 former territory of the FRG)",
									 "Kosovo" = "Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
	filter(unit == "Percentage change m/m-12",
				 time == "2023-04-01") %>% 
	#group_by(geo) %>% 
	#summarise(sm = sum(values)) %>% 
	mutate_if(is.numeric, round, 1) %>% 
	select(geo, values)
df <- eurasia %>% 
	inner_join(prices, by = c("name" = "geo"))
df %>% 
	ggplot() +
	geom_sf(aes(fill = values), alpha = 0.4) +
	coord_sf(xlim = c(-25, 40), ylim = c(34, 72), expand = FALSE) + 
	geom_sf_text(aes(label = values), check_overlap = TRUE, size = 3) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
	labs(x = NULL, y = NULL, fill = "%") +
	theme(text = element_text(size = 16), legend.position = "right",
				axis.text = element_blank(),
				axis.ticks = element_blank())
# Gini
ilc_di12 <- get_eurostat("ilc_di12", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

eu <- ilc_di12 %>%
  mutate(geo = fct_recode(geo, "Turkey" = "Türkiye", 
                          "Germany" = "Germany (until 1990 former territory of the FRG)",
                          "Kosovo" = "Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
  filter(time == "2022-01-01") %>%
  group_by(geo) %>%
  summarise(sm = sum(values)) %>%
  mutate_if(is.numeric, round, 1)
df <- eur %>%
  inner_join(eu, by = c("name" = "geo"))
df %>%
  ggplot() +
  geom_sf(aes(fill = sm), alpha = 0.4) +
  coord_sf(xlim = c(-15, 40), ylim = c(34, 73), expand = FALSE) +
  geom_sf_text(aes(label = sm), size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = NULL, y = NULL, fill = "Години:", 
       title = "Gini, 2022 (0 = максимално равенство, 100 = максимално неравенство)") +
  theme(text = element_text(size = 16), legend.position = "none")

ilc_di12 %>% 
  mutate(geo = fct_recode(geo, "Turkey" = "Türkiye", 
                          "Germany" = "Germany (until 1990 former territory of the FRG)",
                          "Kosovo" = "Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
	filter(!geo %in% c("Euro area - 19 countries  (2015-2022)", "Kosovo",
			"Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
			"European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
			"Euro area - 18 countries (2014)", 
			"Euro area - 19 countries  (from 2015)",
			"European Union - 27 countries (from 2020)", 
			"European Union - 28 countries (2013-2020)",
			"European Union - 27 countries (2007-2013)",
			"European Union - 28 countries (2013-2020)",
			"New Member States - 10 countries (2004-2006)",
			"European Union - 15 countries (1995-2004)",
			"European Union - 25 countries (2004-2006)",
			"Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
	ggplot(aes(time, values)) +
	geom_path() +
	labs(x = "Години", y = "Gini", 
	     title = "Коефициент Gini (0 = абсолютно равенство; 100 = абсолютно неравенство)") +
	theme(text = element_text(size = 16), legend.position = "none") +
	facet_wrap(~ geo)

# Criminal offenses 
crim_off_cat <- get_eurostat("crim_off_cat", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

crim_off_cat %>%
  filter(geo == "Bulgaria", unit == "Per hundred thousand inhabitants") %>%
  ggplot(aes(time, values)) +
  geom_point() +
  geom_line() +
  facet_wrap(~iccs)

teiis080 <- get_eurostat("teiis080", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

teiis080 %>%
  filter(
    time > "2023-01-01" & time < "2023-03-01",
    unit == "Percentage change m/m-12 (CA)",
    !is.na(values),
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)")) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = values >= 0,
    col = ifelse(geo == "Bulgaria", "0", col),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.14)) +
  scale_fill_manual(values = c("#228B22", "#F8766D", "#00BFC4")) +
  theme(text = element_text(size = 16)) +
  labs(x = "Процентна промяна в индустриалното производство в сравнение със същия месец на миналата година", y = NULL) +
  facet_wrap(~time, ncol = 6)

sdg_16_50 <- get_eurostat("sdg_16_50", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

sdg_16_50 %>%
  filter(
    time == "2022-01-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Корупционен индекс (0 = максимална корупция; 100 = липса на корупция) (към 01.01.2022 г.)", y = NULL)

tps00122 <- get_eurostat("tps00122", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

tps00122 %>%
  filter(
    sex == "Total", 
    age == "Total",
    time == "2020-01-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой самоубийства на 100 000 души за 2019 година", y = NULL)

tps00146 <- get_eurostat("tps00146", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

tps00146 %>%
  filter(
    sex == "Total", 
    age == "Total",
    time == "2020-01-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой убийства на 100 000 души за 2019 година", y = NULL)

demo_fabortind <- get_eurostat("demo_fabortind", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

demo_fabortind %>%
  filter(
    indic_de == "Abortion rate",
    time == "2021-01-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой аборти на 1000 жени в репродуктивна възраст за 2020 година", y = NULL)

tps00216 <- get_eurostat("tps00216", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

tps00216 %>%
  filter(
    indic_de == "Crude divorce rate",
    time == "2021-01-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой разводи на 1000 души за 2020 година", y = NULL)

tps00206 <- get_eurostat("tps00206", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

tps00206 %>%
  filter(
    indic_de == "Crude marriage rate",
    time == "2021-01-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"),
    geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = 0.09)) +
  theme(text = element_text(size = 16)) +
  labs(x = "Брой бракове на 1000 души за 2020 година", y = NULL)

pop_area <- read_csv("data/world_population.csv") %>% janitor::clean_names()

pop <- pop_area %>%
  filter(continent == "Europe") %>%
  mutate(country = fct_reorder(country, x2022_population),
         col = case_when(country == "Bulgaria" ~ "0", TRUE ~ "1")) %>%
  ggplot(aes(country, x2022_population, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = space_s(x2022_population)), hjust = -0.1, size = 4, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.13))) +
  labs(y = "Популация (към 2022 година)", x = NULL) +
  theme(text = element_text(size = 14)) +
  coord_flip()
area <- pop_area %>%
  filter(continent == "Europe") %>%
  mutate(country = fct_reorder(country, area_km2),
         col = case_when(country == "Bulgaria" ~ "0", TRUE ~ "1")) %>%
  ggplot(aes(country, area_km2, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = space_s(area_km2)), hjust = -0.1, size = 4, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.13))) +
  labs(y = expression(paste("Територия, km"^"2")), x = NULL) +
  theme(text = element_text(size = 14)) +
  coord_flip()

pop + area
# Energy------------------------------------------------------------------------
ei_isen_m <- get_eurostat("ei_isen_m", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

p1 <- ei_isen_m %>%
  filter(
    indic == "Imports of natural gas, TJ (GCV)",
    time == "2023-04-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    values > 0) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    gwh = values * 0.27777777777778,
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"), geo = fct_reorder(geo, gwh)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(gwh, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = gwh), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.13))) +
  theme(text = element_text(size = 16)) +
  labs(x = "Внос на природен газ (GWh) в ЕС към 01.04.2023", y = NULL)

p2 <- ei_isen_m %>%
  filter(
    indic == "Consumption of electricity - GWh",
    time == "2023-04-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)")) %>%
  mutate(
    geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)"),
    col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1"), geo = fct_reorder(geo, values)) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.1, size = 4, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.13))) +
  theme(text = element_text(size = 16)) +
  labs(x = "Потребление на електричество (GWh) в ЕС към 01.04.2023", y = NULL)

p1 + p2
#-----------------------------------
earn_mw_cur <- get_eurostat("earn_mw_cur", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

earn_mw_cur %>%
  filter(
    currency == "Euro",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    time == "2023-01-01") %>%
  mutate(geo = fct_reorder(geo, values), col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1")) %>%
  mutate(geo = fct_recode(geo, "Germany" = "Germany (until 1990 former territory of the FRG)")) %>% 
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = values), hjust = -0.2, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.13))) +
  theme(text = element_text(size = 16)) +
  labs(x = "Минимална месечна заплата в евро към 01.01.2023 г.", y = NULL)

nrg_pc_204 <- get_eurostat("nrg_pc_204", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

joined <- elec %>% left_join(min_wage, by = "geo") %>% 
  mutate(ration = abs(wage / electricity))

nrg_pc_204 %>%
  filter(
    product %in% c("Electrical energy"), nrg_cons %in% c("Consumption less than 1 000 kWh - band DA"),
    unit %in% c("Kilowatt-hour"), tax %in% c("All taxes and levies included"), currency %in% c("Euro"),
    !geo %in% c(
      "European Union - 27 countries (from 2020)",
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015, EA20-2023)"),
    time == "2023-01-01") %>%
  mutate(geo = fct_reorder(geo, values), col = case_when(geo == "Bulgaria" ~ "0", TRUE ~ "1")) %>%
  select(geo, col, values) %>% 
  ggplot(aes(values, geo, fill = col)) +
  geom_col(show.legend = F) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  geom_text(aes(label = paste0(values, " \u20AC/KWh")), hjust = -0.02, size = 5, color = "black") +
  theme(text = element_text(size = 16)) +
  labs(x = "Цена на тока в ЕС към 01.01.2023 г.", y = NULL)

ei_cphi_m <- get_eurostat("ei_cphi_m", type = "label", time_format = "date") %>%
  mutate_if(is_character, as_factor)

teicp000 %>%
  filter(
    unit == "Percentage change m/m-12",
    time > "2021-12-01",
    !geo %in% c(
      "Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)",
      "European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)",
      "Euro area - 18 countries (2014)", "Euro area - 19 countries  (from 2015)",
      "European Union - 27 countries (from 2020)", "European Union - 28 countries (2013-2020)"),
    as.integer(geo) %in% c(1:20)) %>%
  mutate(pos = values >= 0) %>%
  mutate_if(is.numeric, round, 1) %>%
  ggplot(aes(time, values, fill = pos)) +
  geom_col() +
  geom_text(aes(label = values), hjust = -0.2, size = 4, color = "black") +
  labs(y = "Промяна на цената на 'ранъта' спрямо предходния месец (%)", x = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(text = element_text(size = 14), legend.position = "none") +
  coord_flip() +
  facet_wrap(~geo, scales = "free_x")

prc_hicp_ctrb <- get_eurostat("prc_hicp_ctrb", type = "code", time_format = "date") %>%
  mutate_if(is_character, as_factor)

prc_hicp_ctrb %>%
  filter(
    coicop %in% c("CP01", "CP02", "CP03", "CP04", "CP05", "CP06", "CP07", "CP08", "CP09", "CP10", "CP11", "CP12"),
    time > "2021-08-01") %>%
  mutate(coicop = fct_recode(coicop,
    "Храна и неалкохолни напитки" = "CP01",
    "Алкохолни напитки и тютюн" = "CP02",
    "Облекла и обувки" = "CP03",
    "Наеми, вода, електричество, газ и други горива" = "CP04",
    "Мебели, домакински уреди и поддръжка на дома" = "CP05",
    "Здравеопазване" = "CP06",
    "Транспорт" = "CP07",
    "Комуникации" = "CP08",
    "Рекреация и култура" = "CP09",
    "Образование" = "CP10",
    "Ресторантьорство и хотелиерство" = "CP11",
    "Лични грижи" = "CP12")) %>%
  mutate(pos = values >= 0) %>%
  mutate_if(is.numeric, round, 2) %>%
  View()
ggplot(aes(time, values, fill = pos)) +
  geom_col() +
  geom_text(aes(label = values), hjust = -0.2, size = 4, color = "black") +
  labs(y = "Инфлационен принос (%)", x = NULL) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(text = element_text(size = 16), legend.position = "none") +
  coord_flip() +
  facet_wrap(~coicop)

prc_hicp_mmor <- get_eurostat("prc_hicp_mmor", type = "code", time_format = "date") %>%
  mutate_if(is_character, as_factor)

prc_hicp_mmor %>%
  filter(coicop %in% c("CP01", "CP02", "CP03", "CP04", "CP05", "CP06", 
                  "CP07", "CP08", "CP09", "CP10", "CP11", "CP12"),
    time > "2022-04-01", geo == "BG") %>%
  mutate(coicop = fct_recode(coicop,
    "Храна и неалкохолни напитки" = "CP01",
    "Алкохолни напитки и тютюн" = "CP02",
    "Облекла и обувки" = "CP03",
    "Наеми, вода, електричество, газ и други горива" = "CP04",
    "Мебели, домакински уреди и поддръжка на дома" = "CP05",
    "Здравеопазване" = "CP06",
    "Транспорт" = "CP07",
    "Комуникации" = "CP08",
    "Рекреация и култура" = "CP09",
    "Образование" = "CP10",
    "Ресторантьорство и хотелиерство" = "CP11",
    "Лични грижи" = "CP12")) %>%
  mutate(pos = values > 0) %>%
  mutate_if(is.numeric, round, 2) %>%
  ggplot(aes(time, values, fill = pos)) +
  geom_col() +
  geom_text(aes(label = values), vjust = -0.1, size = 4, color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(text = element_text(size = 16), legend.position = "none", 
        axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Месечна промяна на цените в БГ (%)", x = NULL) +
  coord_cartesian(xlim = as.Date(c("2022-05-01", "2023-04-01"))) +
  facet_wrap(~coicop)

prc_hicp_mmor %>%
  filter(
    coicop %in% c("CP01", "CP02", "CP03", "CP04", "CP05", "CP06", 
                  "CP07", "CP08", "CP09", "CP10", "CP11", "CP12"),
    time > "2022-04-01", geo == "BG") %>%
  mutate(coicop = fct_recode(coicop,
    "Храна и неалкохолни напитки" = "CP01",
    "Алкохолни напитки и тютюн" = "CP02",
    "Облекла и обувки" = "CP03",
    "Наеми, вода, електричество, газ и други горива" = "CP04",
    "Мебели, домакински уреди и поддръжка на дома" = "CP05",
    "Здравеопазване" = "CP06",
    "Транспорт" = "CP07",
    "Комуникации" = "CP08",
    "Рекреация и култура" = "CP09",
    "Образование" = "CP10",
    "Ресторантьорство и хотелиерство" = "CP11",
    "Лични грижи" = "CP12")) %>%
  group_by(coicop) %>%
  summarise(sum_val = sum(values)) %>%
  mutate(coicop = fct_reorder(coicop, sum_val), pos = sum_val > 0) %>%
  ggplot(aes(sum_val, coicop, fill = pos)) +
  geom_col() +
  geom_text(aes(label = sum_val), hjust = -0.2, size = 5, color = "black") +
  labs(x = "Годишна инфлация в БГ (%)", y = NULL) +
  theme(text = element_text(size = 18), legend.position = "none")
