library(tidyverse)
library(scales)
options(scipen = 999)

energy <- read_csv("https://nyc3.digitaloceanspaces.com/owid-public/data/energy/owid-energy-data.csv")
covid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
gini <- read_csv("gini.csv") %>% mutate(across(`1960`:`2021`, as.double)) %>% 
	pivot_longer(`1960`:`2021`, names_to = "year", values_to = "gini") %>% drop_na(gini) %>% 
	group_by(country) %>% 
	summarise(gini = mean(gini))
pop <- read_csv("data/world_population_2023.csv") %>% janitor::clean_names()

colors <- c("Биогориво" = "lightblue", "Въглища" = "black",
            "Природен газ" = "#088F8F", "Хидро" = "blue",
            "Ядрена" = "red", "Нефт" = "brown",
            "Слънце" = "green", "Вятър" = "lightgreen")

glimpse(covid)

energy %>% count(year) %>% pull(year)

energy %>%
  filter(country %in% c("World"), year == 2022) %>%
  select(country, year, contains("share_elec"), -low_carbon_share_elec, -fossil_share_elec, -renewables_share_elec,
         -other_renewables_share_elec, -other_renewables_share_elec_exc_biofuel) %>%
  pivot_longer(-c(country, year)) %>%
  mutate(across(year:name, as.factor)) %>% 
  mutate(name = fct_recode(name, "Биогориво" = "biofuel_share_elec", "Въглища" = "coal_share_elec",
                           "Природен газ" = "gas_share_elec", "Хидро" = "hydro_share_elec",
                           "Ядрена" = "nuclear_share_elec", "Нефт" = "oil_share_elec",
                           "Слънце" = "solar_share_elec", "Вятър" = "wind_share_elec"), 
         name = fct_reorder(name, value)) %>%
  ggplot(aes(value, name, fill = name)) +
  geom_col(show.legend = F) +
  scale_x_continuous(expand = expansion(mult = c(.01, .25))) +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = paste0(round(value, 1), "%")), hjust = -0.1, size = 5) +
  labs(x = "Проценти", y = NULL, title = "Енергиен микс") +
  theme(text = element_text(size = 14), 
        axis.title.x = element_text(vjust = 1, hjust = 1), 
        axis.title.y = element_text(vjust = 1, hjust = 1)) +
  facet_wrap(vars(country))

pop %>% 
	slice_max(order_by = population2023, n = 50) %>% 
	mutate(country = reorder(country, population2023)) %>% 
	ggplot(aes(population2023, country, fill = country)) +
	geom_col(show.legend = F) +
  geom_text(aes(label = space_s(round(population2023, 0))), hjust = -0.1, size = 5) +
  scale_x_continuous(labels = scales::label_number(suffix = " M", scale = 1e-6),
                     breaks = c(0, 5e7, 1e8, 25e7, 5e8, 1e9, 1.4e9),
                     expand = expansion(mult = c(.01, .25))) +
	labs(x = "Популация (2023 година)", y = NULL) +
	theme(text = element_text(size = 16))

gini %>% 
	mutate(country = reorder(country, gini)) %>% 
	slice_max(order_by = gini, n = 50) %>% 
	ggplot(aes(gini, country, fill = country)) +
	geom_col(show.legend = F)

glimpse(covid)

covid %>% 
  filter(date == "2023-08-01") %>% 
  slice_max(order_by = total_deaths_per_million, n = 100) %>% 
  select(location, tpm = total_deaths_per_million) %>% 
  print(n = Inf)
  
covid %>% 
  filter(location == "World") %>% 
  select(date, location, total_vaccinations_per_hundred, total_deaths_per_million, total_cases_per_million,
         icu_patients_per_million, hosp_patients_per_million) %>% 
  pivot_longer(-c(date, location)) %>% drop_na(value) %>% 
	ggplot(aes(date, value, colour = name)) +
	geom_line(linewidth = 1.5) +
  #scale_color_manual(values = c("black", "cyan")) +
  labs(x = "Години", y = "",
       caption = "Източник на данните: OWID") +
  theme(text = element_text(size = 16), legend.position = "top") +
  facet_wrap(~ name, scales = "free_y", ncol = 1)

covid %>% 
  filter(continent == "Europe", new_deaths > 0, location %in% c("Italy", "Spain", "Germany", "Ukraine",
                                                                "United Kingdom", "Romania", "Poland",
                                                                "France", "Bulgaria")) %>% 
  ggplot(aes(date, new_deaths)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(n.breaks = 5) +
  labs(x = "Години", y = "Нови случаи на COVID", caption = "Източник на данните: OWID") +
  theme(text = element_text(size = 16)) +
  facet_wrap(~ location, scales = "free_y")
	
energy %>% 
	filter(country == "Bulgaria") %>% 
	ggplot(aes(year, population)) +
	geom_point() +
	scale_x_continuous(n.breaks = 25) +
	scale_y_continuous(label = scales::comma) +
	geom_vline(aes(xintercept = 1985), size = 0.5, lty = 2, color = "black") +
	labs(x = "Year", y = "Population") +
	theme(text = element_text(size = 16))
energy %>% 
	filter(country == "Bulgaria") %>% 
	ggplot(aes(year, coal_cons_change_twh)) +
	geom_point() +
  geom_smooth() +
	scale_x_continuous(n.breaks = 30) +
	#scale_y_continuous(label = scales::comma) +
	#geom_vline(aes(xintercept = 1985), size = 0.5, lty = 2, color = "black") +
	labs(x = "Year", y = "Electricity demand (TWh)") +
  coord_cartesian(xlim = c(1966, 2021)) +
	theme(text = element_text(size = 16))
energy %>% 
	filter(country == "Bulgaria") %>% 
	select(country, year, starts_with("coal")) %>% 
	pivot_longer(3:14, names_to = "name", values_to = "value") %>% 
	ggplot(aes(year, value)) +
	geom_point() +
	geom_smooth() +
	xlim(1900, NA) +
	#geom_vline(aes(xintercept = 1985), size = 0.5, lty = 2, color = "black") +
	labs(x = "Year", y = "%") +
	theme(text = element_text(size = 16)) +
	facet_wrap(~name, scales = "free", ncol = 3)
