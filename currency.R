library(tidyverse)
library(rvest)
library(janitor)
library(patchwork)
library(tidytext)
library(httr2)

bnb<-read_html("https://www.bnb.bg/Statistics/StExternalSector/StExchangeRates/StERForeignCurrencies/index.htm") %>% 
  html_table()
bnb<-bnb[[2]] %>% select(1, 4) %>% slice(29) %>% rename("cur" = "Наименование", "selling" = "Лева (BGN)") %>% 
  mutate(cur = fct_recode(cur, "USD" = "Щатски долар"), bank = "BNB", date = today(tzone = ""),
         selling = as.numeric(selling)) %>% 
  add_row(cur = "EUR", selling = 1.95583, bank = "BNB", date = today(tzone = ""))

ckb<-read_html("https://www.ccbank.bg/bg/valutni-kursove") %>% 
  html_table()
ckb<-ckb[[1]] %>% select(1,5,6) %>% slice(1, 9) %>% 
  rename("cur" = "Валута", "buying" = "Курс купува", "selling" = "Курс продава") %>% 
  mutate(bank = "CKB", date = today(tzone = ""), selling = as.numeric(selling), buying = as.numeric(buying))

mun<-read_html("https://www.municipalbank.bg/?page=currency") %>%
  html_table()
mun<-mun[[2]] %>% select(2, 4, 5) %>% slice(1, 2) %>%
  rename("cur" = "Валута", "selling" = "Курс продава*", "buying" = "Курс купува*") %>%
  mutate(selling = str_replace(selling, ",", "."), buying = str_replace(buying, ",", "."),
         bank = "MUNICIPALBANK", date = today(tzone = "")) %>%
  mutate(selling = as.numeric(selling), buying = as.numeric(buying))

post<-read_html("https://www.postbank.bg/Valutni-Kursove") %>% 
  html_table()
post<-post[[2]] %>% row_to_names(row_number = 1) %>% select(2,6,7) %>% slice(1,2) %>% 
  rename("cur" = "Код", "buying" = "Купува", "selling" = "Продава") %>% 
  mutate(bank = "POST", date = today(tzone = ""), selling = as.numeric(selling), buying = as.numeric(buying))

dbank<-read_html("https://www.dbank.bg/bg/valutni-kursove") %>% 
  html_table()
dbank<-dbank[[1]] %>% select(1,4,5) %>% slice(1,2) %>% 
  rename("cur" = "Валута:", "buying" = "Курс купува на каса:", "selling" = "Курс продава на каса:") %>% 
  mutate(bank = "DBANK", date = today(tzone = ""), selling = as.numeric(selling), buying = as.numeric(buying))

tokuda<-read_html("https://www.tokudabank.bg/bg/valutni-kursove/") %>% 
  html_table()
tokuda<-tokuda[[1]] %>% select(1,3,4) %>% slice(2,1) %>% 
  rename("cur" = "Валута", "buying" = "Банката купува", "selling" = "Банката продава") %>% 
  mutate(selling = str_replace(selling, ",", "."), buying = str_replace(buying, ",", "."),
         bank = "TOKUDA", date = today(tzone = "")) %>% 
  mutate(selling = as.numeric(selling), buying = as.numeric(buying))

tbi<-read_html("https://tbibank.bg/valutni-kursove/") %>% 
  html_table()
tbi<-tbi[[1]] %>% select(2:3) %>% slice(11,5) %>% mutate(cur = c("USD", "EUR")) %>% 
  select("cur", "buying" = "Купува", "selling" = "Продава") %>% 
  mutate(bank = "TBI", date = today(tzone = ""))

df <- bind_rows(bnb, 
                ckb, 
                mun,
                #post,
                #dbank, 
                tokuda, 
                tbi) %>% 
  mutate(amount = 2556.46) %>% 
  select(date, bank, cur, buying, selling, amount) %>% 
  mutate(Купува = buying * amount, Продава = selling * amount) %>%
  group_by(cur) %>% 
  mutate(Разлика_купува = Купува - min(Купува, na.rm = T),
         Разлика_продава = Продава - min(Продава, na.rm = T)) %>% 
  ungroup() %>% 
  mutate_if(is.numeric, round, 3) %>% 
  pivot_longer(7:10, names_to = "action", values_to = "value")

p1 <- df %>% 
  filter(cur == "USD", action %in% c("Купува", "Продава")) %>% 
  mutate(bank = reorder_within(bank, value, action)) %>% 
  ggplot(aes(value, bank, fill = value)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  geom_text(aes(label = value), hjust = -0.2, size = 4) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.05, .15))) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = NULL, x = "Сума (лв)", title = paste0(df$amount[1], " USD")) +
  theme(text = element_text(size = 16)) +
  facet_wrap(~action, scales = "free_y")
p2 <- df %>% 
  filter(cur == "EUR", action %in% c("Купува", "Продава")) %>% 
  mutate(bank = reorder_within(bank, value, action)) %>% 
  ggplot(aes(value, bank, fill = value)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_y_reordered() +
  geom_text(aes(label = value), hjust = -0.2, size = 4) +
  scale_x_continuous(expand = expansion(mult = c(.05, .15))) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = NULL, x = "Сума (лв)", title = paste0(df$amount[1], " EUR")) +
  theme(text = element_text(size = 16)) +
  facet_wrap(~action, scales = "free_y")
p1/p2

p3 <- df %>% 
  filter(cur == "USD", action %in% c("Разлика_купува", "Разлика_продава")) %>% 
  mutate(bank = reorder_within(bank, value, action)) %>% 
  ggplot(aes(value, bank, fill = value)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = value), hjust = -0.2, size = 4) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.05, .15))) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = NULL, x = "Разлика (лв)", title = paste0(df$amount[1], " USD")) +
  theme(text = element_text(size = 16)) +
  facet_wrap(~action, scales = "free_y")
p4 <- df %>% 
  filter(cur == "EUR", action %in% c("Разлика_купува", "Разлика_продава")) %>% 
  mutate(bank = reorder_within(bank, value, action)) %>% 
  ggplot(aes(value, bank, fill = value)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = value), hjust = -0.2, size = 4) +
  scale_y_reordered() +
  scale_x_continuous(expand = expansion(mult = c(.05, .15))) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(y = NULL, x = "Разлика (лв)", title = paste0(df$amount[1], " EUR")) +
  theme(text = element_text(size = 16)) +
  facet_wrap(~action, scales = "free_y")
p3/p4
