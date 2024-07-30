library(tidyverse)
library(rvest)

read_html("https://www.uwo.ca/chem/people/graduate_students.html") %>% 
  html_elements("a") %>% 
  html_text2() %>% 
  str_subset("@") %>% 
  str_flatten(";")

# Speech scraping---------------
scrape_speech <- function(url) {
  speech_page <- read_html(url)
  title <- speech_page %>%
    html_node(".article-header__title") %>%
    html_text()
  date <- speech_page %>%
    html_node(".content-data__list:nth-child(1) strong") %>%
    html_text() %>%
    dmy()
  location <- speech_page %>%
    html_node(".content-data__list+ .content-data__list strong") %>%
    html_text()
  abstract <- speech_page %>%
    html_node(".leader--first-para p") %>%
    html_text()
  text <- speech_page %>%
    html_nodes("#preamble p") %>%
    html_text() %>%
    list()
  tibble(
    title = title, date = date, location = location,
    abstract = abstract, text = text, url = url
  )
}

all_speeches_page <- read_html("https://www.gov.scot/collections/first-ministers-speeches/")

covid_speech_urls <- all_speeches_page %>%
  html_nodes(".collections-list a") %>%
  html_attr("href") %>%
  str_subset("covid-19") %>%
  str_c("https://www.gov.scot", .)

covid_speeches <- map_dfr(covid_speech_urls, scrape_speech)






