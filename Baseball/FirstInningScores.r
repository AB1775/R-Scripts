library(dplyr)
library(readr)
library(here)
library(tidyverse)
library(ggplot2)

retro2023 <-
  read_csv(here::here("Baseball/data/retrosheet/download.folder/unzipped/all2023.csv"))
team_ids <-
  read_csv(here::here("Baseball/data/team_ids.csv"))

retro2023 <- tibble(retro2023)

first_inning_data <- retro2023 |>
  filter(inn_ct == 1) |>
  mutate(
    away_team_id = away_team_id,
    home_team_id = substr(game_id, 1, 3)
  ) |>
  select(game_id, away_team_id, home_team_id,
         bat_home_id, inn_ct, home_score_ct, away_score_ct)

first_inning_scores <- first_inning_data |>
  mutate(
    away_team_score = away_score_ct,
    home_team_score = home_score_ct
  ) |>
  select(game_id, away_team_id, home_team_id, away_team_score, home_team_score)

away_team_scores <- first_inning_scores |>
  group_by(away_team_id) |>
  summarize(
    total_first_inning_scores = sum(away_team_score, na.rm = TRUE),
    games_played = n(),
    scoring_percentage = (total_first_inning_scores / games_played) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(scoring_percentage))

home_team_scores <- first_inning_scores |>
  group_by(home_team_id) |>
  summarize(
    total_first_inning_scores = sum(home_team_score, na.rm = TRUE),
    games_played = n(),
    scoring_percentage = (total_first_inning_scores / games_played) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(scoring_percentage))

#######################################################################################################
#                 DIFFERENCE BETWEEN @HOME 1 Inn. SCORES VS AWAY 1 Inn. Scores By Team
#######################################################################################################
########################################################
# Merge Home & Away Scores to Analyize the Scoring Gap
########################################################
colnames(away_team_scores)[colnames(away_team_scores) == "away_team_id"] <-
  "home_team_id"

merged_scores <- merge(home_team_scores, away_team_scores, by = "home_team_id", suffixes = c("_home", "_away"))

merged_scores$gap <-
  merged_scores$scoring_percentage_home - merged_scores$scoring_percentage_away

merged_scores$total_first_inning_scores <-
  merged_scores$total_first_inning_scores_home + merged_scores$total_first_inning_scores_away

merged_scores$total_games_played <-
  merged_scores$games_played_home + merged_scores$games_played_away

merged_scores$total_perc <-
  (merged_scores$total_first_inning_scores / merged_scores$total_games_played) * 100

sorted_scores <- merged_scores[order(-merged_scores$gap), ]

######################
# Visualize Findings
######################
# score_gap_bar <- ggplot(sorted_scores, aes(x = reorder(home_team_id, gap), y = gap, fill = home_team_id)) +
#  geom_bar(stat = "identity") +
#  coord_flip() +
#  labs(title = "Gap Between Home and Away First Inning Scores",
#    x = "Team", # nolint: indentation_linter.
#    y = "Gap (1st Inning Home Scoring Percentage - Away Scoring Percentage)") +
#  theme_classic()

######################################################################################################
#                                  INNING SCORES DATA 
######################################################################################################
##############################
# Percentage of Scores Over 1
##############################
scores_greater <- first_inning_scores |>
  filter(away_team_score > 1 | home_team_score > 1)

scores_greater_percentage <-
  (nrow(scores_greater) / nrow(first_inning_scores)) * 100

###########################
# Percentage of 0-0 Scores
###########################
zero_zero <- first_inning_scores |>
  filter(home_team_score == 0 & away_team_score == 0)

zero_zero_percentage <-
  (nrow(zero_zero) / nrow(first_inning_scores)) * 100

#######################################
# Percentage of Tied Firsts (not 0-0)
#######################################
tied_firsts <- first_inning_scores |>
  filter(away_team_score > 0 & home_team_score > 0) |>
  filter(away_team_score == home_team_score)

tied_first_percentage <-
  (nrow(tied_firsts) / nrow(first_inning_scores)) * 100

##########################################
# Percentage of 1-0 / 0-1 First Innings
##########################################
one_score_firsts <- first_inning_scores |>
  filter(
    away_team_score == 1 & home_team_score == 0 |
      away_team_score == 0 & home_team_score == 1
  )

one_score_first_percentage <-
  (nrow(one_score_firsts) / nrow(first_inning_scores)) * 100

######################################################################################################

#########################################
# File Operations for Later Reference
#########################################
#merged_path <- "data/first_inning/score_gaps.csv"
#if (!file.exists(merged_path)) {
#  write.csv(sorted_scores, here::here("data/first_inning/score_gaps.csv"))
#} else {
#  cat("[+] score_gaps.csv found. Skipping creation...\n")
#}