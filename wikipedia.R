library(tidyverse)
library(rvest)

military <- read_html("https://en.wikipedia.org/wiki/List_of_countries_with_highest_military_expenditures") %>% 
  html_table()
military_2023 <- military[[2]] %>% 
  janitor::clean_names() %>% 
  mutate(country = str_remove_all(country, "[:punct:][:alpha:][:punct:]"))
military_2025 <- military[[3]] %>% 
  janitor::clean_names() %>% 
  mutate(country = str_remove_all(country, "[:punct:][:alpha:][:punct:]"))
