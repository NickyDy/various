library(tidyverse)
library(tabulapdf)

july_24_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2024%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"
july_23_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2023%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"
july_22_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2022%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"
july_21_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2021%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"
july_18_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2018%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"
july_16_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2016%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"

july_17_2025 <- "https://www.dksbt.bg/doc/%D0%A1%D0%BF%D1%80%D0%B0%D0%B2%D0%BA%D0%B0%20%D1%86%D0%B5%D0%BD%D0%B8%2017%20%D1%8E%D0%BB%D0%B8%202025_DKSBT.pdf"

links1 <- c(july_24_2025, july_23_2025, july_22_2025, july_21_2025, july_18_2025, july_16_2025)
links2 <- c(july_17_2025, july_23_2025)

extr_tables_page_1 <- function(links) {
  
  extract_tables(links, col_names = F, method = "stream", pages = 1)
  
}

df_p1 <- map(links1, extr_tables_page_1) %>% 
  map_dfr(., bind_rows) %>% 
  filter(str_detect(X1, "^\\d{2}\\.")) %>% select(-X16) %>% 
  select(date = X1, Домати = X2, Краставици = X3, Картофи = X4, Зеле = X5, Моркови = X6,
         `Лук зрял` = X7, `Червен пипер` = X8, `Зелен пипер` = X9, Тиквички = X10,
         Лимони = X11, Праскови = X12, Кайсии = X13, Дини = X14, Ябълки = X15) %>% 
  mutate(date = dmy(paste0(date, "2025"))) %>% 
  mutate(across(everything(), ~str_replace_all(., ",", "."))) %>% 
  distinct()


tab_1 <- tab[[1]] %>% 
  filter(str_detect(X1, "^\\d{2}\\.")) %>% select(-X16) %>% 
  select(date = X1, Домати = X2, Краставици = X3, Картофи = X4, Зеле = X5, Моркови = X6,
         `Лук зрял` = X7, `Червен пипер` = X8, `Зелен пипер` = X9, Тиквички = X10,
         Лимони = X11, Праскови = X12, Кайсии = X13, Дини = X14, Ябълки = X15) %>% 
  mutate(date = dmy(paste0(date, "2025"))) %>% 
  mutate(across(everything(), ~str_replace_all(., ",", ".")))

tab <- extract_tables(july_21_2025, col_names = F, method = "stream", pages = 3)

tab_2 <- tab[[1]] %>% 
  slice(6:15) %>% 
  select(Захар = X2, Фасул = X3, Ориз = X4, Брашно = X5, Олио = X6,
         Яйца = X7, Кашкавал = X8, Сирене = X9, `МАСЛО/125г` = X10,
         `Кисело мляко над 3%` = X11, `Прясно мляко 3%` = X12, `Свинско месо` = X13, Пиле = X14) %>% 
  mutate(across(everything(), ~str_replace_all(., ",", ".")))

july_22_2025 <- bind_cols(tab_1, tab_2)
july_21_2025 <- bind_cols(tab_1, tab_2)

july_2025 <- bind_rows(tab_1, tab_2) %>% distinct()
#----------------------------------------------------------------
june_2025 <- "https://www.dksbt.bg/doc/%20%D1%86%D0%B5%D0%BD%D0%B8%20%D1%8E%D0%BD%D0%B8%202025_DKSBT.pdf"
pages <- seq(from = 1, to = 101, by = 5)
table <- extract_tables(june_2025, col_names = F, method = "stream", pages = pages) %>% bind_rows()

tab <- table %>% 
  mutate(across(everything(), ~str_replace_all(., ",", "."))) %>% 
  filter(str_detect(X1, "^\\d{2}\\.")) %>% select(-X16) %>% 
  separate_wider_delim(c(X1), names = c("X1", "X1_1"), delim = " ", too_few = "align_start") %>%
  separate_wider_delim(c(X2), names = c("X2", "X2_1"), delim = " ", too_few = "align_start")

write_csv(tab, "tab.csv")
tab <- read_csv("tab.csv") %>% distinct()

tab %>% 
  select(date = X1, Домати = X2, Краставици = X3, Картофи = X4, Зеле = X5, Моркови = X6,
         `Лук зрял` = X7, `Червен пипер` = X8, `Зелен пипер` = X9, Тиквички = X10,
         Лимони = X11, Праскови = X12, Кайсии = X13, Дини = X14, Ябълки = X15) %>% 
  mutate(date = dmy(paste0(date, "2025")))

bak <- table[[3]] %>% 
  slice(6:15) %>% 
  select(Захар = X2, Фасул = X3, Ориз = X4, Брашно = X5, Олио = X6,
         Яйца = X7, Кашкавал = X8, Сирене = X9, `МАСЛО/125г` = X10,
         `Кисело мляко над 3%` = X11, `Прясно мляко 3%` = X12, `Свинско месо` = X13, Пиле = X14) %>% 
  mutate(across(everything(), ~str_replace_all(., ",", ".")))