library(dplyr)
library(here)
library(tidyverse)

season2023 <- read.csv(here::here("R/Hockey/data/2023/season.csv"))
total_games <- nrow(season2023)

season2023_OT <- season2023 |>
  filter(season2023$EX == "SO" | season2023$EX == "OT")
total_games_ot <- nrow(season2023_OT)

ot_freq <- (total_games_ot / total_games) * 100

cat("[Total Games]: ", total_games, "\n")
cat("[Overtime Games]: ", total_games_ot, "\n")
cat("[Overtime Freq]: ", ot_freq, "\n")