library(tidyverse)
library(jsonlite)
library(nanoparquet)

json <- fromJSON(glue::glue("https://archive-api.open-meteo.com/v1/archive?latitude={coord$lat}&longitude={coord$long}&start_date=1940-01-01&end_date={Sys.Date()}&daily=temperature_2m_mean,temperature_2m_max,temperature_2m_min,precipitation_sum,rain_sum,snowfall_sum,wind_speed_10m_max,wind_direction_10m_dominant&timezone=auto"))


