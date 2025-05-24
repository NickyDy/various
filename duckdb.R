library(duckdb)
library(tidyverse)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")

dbWriteTable(con, "rgfi", rgfi)

dbRemoveTable(con, "rgfi")

dbListTables(con)

oct_2024_new <- tbl(con, "oct_2024_new") %>% collect()

duckdb_read_csv(con, "rgfi_2024", "~/Desktop/R/work/RGFI_javna_objava_2023_new.csv")

dbDisconnect(con)

