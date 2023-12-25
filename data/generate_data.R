library(tidyverse)
library(data.table)
library(DBI)

user_colors <- data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
  index = 1:(52*91),
  default = rep('black', 52*91),
  # `samp1800@live.com` = rep('black', 52*91)
)

user_comments <- data.frame(matrix(nrow = 52*91, ncol = 0)) %>% mutate(
  index = 1:(52*91),
  default = rep('', 52*91),
  # `samp1800@live.com` = rep('', 52*91)
)

user_data <- data.frame(matrix(nrow = 2, ncol = 0)) %>% mutate(
  index = 1:2,
  default = c('default', '2020-01-01'),
  # `samp1800@live.com` = c('samp1800@live.com', '2000-01-08'),
)

# user_list <- data.frame(matrix(nrow = 0, ncol = 1)) %>% setnames('users')

con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
dbWriteTable(con, 'user_colors', user_colors, overwrite = TRUE)
dbWriteTable(con, 'user_comments', user_comments, overwrite = TRUE)
dbWriteTable(con, 'user_data', user_data, overwrite = TRUE)
# dbWriteTable(con, 'user_list', user_list, overwrite = TRUE)
dbDisconnect(con)

# con <- dbConnect(RSQLite::SQLite(), 'data/db.sqlite')
# get_test <- dbGetQuery(con, 'SELECT "samp1800@live.com" FROM user_comments')
# dbDisconnect(con)
