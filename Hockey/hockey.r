library(hockeyR)
library(here)
library(dplyr)
library(tidyverse)

#############################
# Calculate Indvidual Stats
#############################
pbp <- load_pbp(2024)

stats <- calculate_individual(pbp, type = c("R", "P"), game_strength = "all")