library(duckdb)
library(tidyverse)
library(nanoparquet)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

dbWriteTable(con, "rgfi", rgfi)

dbRemoveTable(con, "rgfi")

dbListTables(con)

oct_2024_new <- tbl(con, "oct_2024_new") %>% collect()

duckdb_read_csv(con, "rgfi", "work/rgfi.csv")

dbDisconnect(con)

rgfi <- read_parquet("work/rgfi.parquet")
