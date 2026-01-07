library(tidyverse)
library(jsonlite)

tr_02_05_2025 <- fromJSON("https://data.egov.bg/resource/download/e404608f-670b-4cd9-9082-5546f62071c8/json")
tr_03_05_2025 <- fromJSON("https://data.egov.bg/resource/download/6fdb35f6-4733-468f-939f-a99b89efa07a/json")
tr_01_05_2025 <- fromJSON("https://data.egov.bg/resource/download/7e4f2e6c-53e5-4c9d-aace-b67bc26cc798/json")

df_may_02 <- tr_02_05_2025[["Message"]][["Body"]][[1]][["Deeds"]][[1]][["Deed"]][[1]][["$"]] %>% as_tibble()
df_may_03 <- tr_03_05_2025[["Message"]][["Body"]][[1]][["Deeds"]][[1]][["Deed"]][[1]][["$"]] %>% as_tibble()
df_may_01 <- tr_01_05_2025[["Message"]][["Body"]][[1]][["Deeds"]][[1]][["Deed"]][[1]][["$"]] %>% as_tibble()

df <- bind_rows(df_may_02, df_may_03, df_may_01) %>% distinct()

  