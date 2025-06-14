library(duckdb)
library(tidyverse)
library(nanoparquet)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

dbWriteTable(con, "votes_new", votes_new)

dbRemoveTable(con, "votes_new")

dbListTables(con)

oct_2024_new <- tbl(con, "oct_2024_new") %>% collect()

duckdb_read_csv(con, "rgfi", "work/rgfi.csv")

dbDisconnect(con)

rgfi <- read_parquet("work/rgfi.parquet")
votes_new <- read_rds("shiny/elections/votes_new.rds")