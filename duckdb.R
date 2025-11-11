library(duckdb)
library(tidyverse)
library(nanoparquet)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

dbWriteTable(con, "finance", finance, overwrite = TRUE)

dbListTables(con)

dbRemoveTable(con, "finance")

finance <- tbl(con, "finance") %>%
  summarise(vaartus = sum(vaartus, na.rm = T), 
            .by = c(report_id, tabel, elemendi_label, elemendi_nimetus)) %>% 
  collect()

duckdb_read_csv(con, "rgfi", "work/rgfi.csv")

dbDisconnect(con)

rgfi <- read_parquet("work/rgfi.parquet")
votes_new <- read_rds("shiny/elections/votes_new.rds")

entities <- read_parquet("work/entities.parquet")

glimpse(tbl(con, "finance"))
