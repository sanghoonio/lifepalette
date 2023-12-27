library(tidyverse)
library(data.table)
library(DBI)

user_list <- data.frame(matrix(nrow = 1, ncol = 0)) %>% mutate(
  user = 'default',
  dob = '2020-01-01'
)

user_data <- data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
  week = 1:(52*91),
  user = rep('default', 52*91),
  color = rep('black', 52*91),
  comment = rep('', 52*91)
)

con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')

dbExecute(con, '
  CREATE TABLE "user_list" (
    "index" INTEGER PRIMARY KEY AUTOINCREMENT,
    "user" TEXT,
    "dob" TEXT
  )
')

dbExecute(con, '
  CREATE TABLE "user_data" (
    "index" INTEGER PRIMARY KEY AUTOINCREMENT,
    "week" INTEGER,
    "user" TEXT,
    "color" TEXT,
    "comment" TEXT
  )
')

dbWriteTable(con, 'user_list', user_list, row.names = FALSE, append = TRUE)
dbWriteTable(con, 'user_data', user_data, row.names = FALSE, append = TRUE)
dbDisconnect(con)
