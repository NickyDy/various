library(tidyverse)
library(jsonlite)

json <- fromJSON(glue::glue("https://archive-api.open-meteo.com/v1/archive?latitude={coord$lat}&longitude={coord$long}&start_date=1940-01-01&end_date={Sys.Date()}&daily=temperature_2m_mean,temperature_2m_max,temperature_2m_min,precipitation_sum,rain_sum,snowfall_sum,wind_speed_10m_max,wind_direction_10m_dominant&timezone=auto"))

tr <- fromJSON("https://data.egov.bg/resource/download/bd061198-bd6b-4bdb-9ac0-9ea2991391eb/json", flatten = T)

tr1 <- tr %>% 
  map_if(is.data.frame, list) %>% 
  as_tibble() %>% 
  unnest() %>% 
  unnest() %>% 
  unnest() %>% 
  unnest() %>% 
  unnest(IncomingPackageInfo, names_sep = "_") %>% 
  unnest(IncomingPackageInfo_Documents, names_sep = "_") %>% 
  unnest(SubDeed, names_sep = "_") %>% 
  unnest() %>% 
  glimpse()

glimpse(tr1)

tr2 <- tr1 %>% select(-Partner) %>% 
  unnest(names_sep = "_") %>% 
  glimpse()
