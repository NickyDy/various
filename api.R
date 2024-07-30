library(tidyverse)
library(httr2)

beef_carc_new <- request("https://www.ec.europa.eu/agrifood/api") %>% 
  req_url_path_append("beef/prices") %>% 
  req_url_query(years = 2024) %>% 
  req_perform() %>% 
  resp_body_json() %>% 
  bind_rows(.id = "name") %>%
  select(begin_date = beginDate, end_date = endDate, state = memberStateName,
         category, product = productCode, price) %>% 
  mutate(price = parse_number(price)) %>%
  mutate(across(begin_date:end_date, dmy))

beef_live_new <- request("https://www.ec.europa.eu/agrifood/api") %>% 
  req_url_path_append("liveAnimal/prices") %>% 
  req_url_query(years = 2024) %>% 
  req_perform() %>% 
  resp_body_json() %>% 
  bind_rows(.id = "name") %>%
  select(begin_date = beginDate, end_date = endDate, state = memberStateName,
         category, unit, price) %>% 
  mutate(price = parse_number(price)) %>%
  mutate(across(begin_date:end_date, dmy))

                