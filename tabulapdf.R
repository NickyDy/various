library(tidyverse)
library(tabulapdf)

dams <- "https://www.moew.government.bg/static/media/ups/tiny/Daily%20Bulletin/27102025_Bulletin_Daily.pdf"

dams_table <- extract_tables(dams, col_names = F, method = "stream", pages = c(3:5)) %>% 
  pluck(1) %>% as_tibble()

extr_tables_page_1 <- function(links) {
  
  extract_tables(dams, col_names = F, method = "stream", pages = c(3:5))
  
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

