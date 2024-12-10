library(baseballr)
library(here)
library(tidyverse)

#########################################################
# Calculating Runs Scored in the Remainder of the Inning
#########################################################
retro2023 <-
  read_csv(here::here("R/Baseball/data/retrosheet/download.folder/unzipped/all2023.csv"))
retro2023 <- tibble(retro2023)

retro2023 <- retro2023 |>
  mutate(
    runs_before = as.numeric(away_score_ct) + as.numeric(home_score_ct),
    half_inning = paste(game_id, inn_ct, bat_home_id),
    runs_scored = (bat_dest_id > 3) + (run1_dest_id > 3) +
      (run2_dest_id > 3) + (run3_dest_id > 3)
  )

half_innings <- retro2023 |>
  group_by(half_inning) |>
  summarize(
    outs_inning = sum(event_outs_ct),
    runs_inning = sum(runs_scored),
    runs_start = first(runs_before),
    max_runs = runs_inning + runs_start
  )
# runs_roi = runs scored in the remainder of the inning
retro2023 <- retro2023 |>
  inner_join(half_innings, by = "half_inning") |>
  mutate(runs_roi = max_runs - runs_before)
# Creating the Run Expectancy Matrix
retro2023 <- retro2023 |>
  mutate(
    bases = paste0(
      if_else(is.na(base1_run_id) | base1_run_id == "", 0, 1),
      if_else(is.na(base2_run_id) | base2_run_id == "", 0, 1),
      if_else(is.na(base3_run_id) | base3_run_id == "", 0, 1)
    ),
    # game_state adds the number of outs to the bases variable
    game_state = paste(bases, outs_ct)
  )
# Only consider plays where there is a change in the runners
# on base, number of outs, or runs scored
retro2023 <- retro2023 |>
  mutate(
    is_runner1 = as.numeric(run1_dest_id == 1 | bat_dest_id == 1),
    is_runner2 = as.numeric(run1_dest_id == 2 | run2_dest_id == 2 |
                              run3_dest_id == 3 | bat_dest_id == 3),
    is_runner3 = as.numeric(run1_dest_id == 3 | run2_dest_id == 3 |
                              run3_dest_id == 3 | bat_dest_id == 3),

    new_outs = outs_ct + event_outs_ct,
    new_bases = paste0(is_runner1, is_runner2, is_runner3),
    new_game_state = paste(new_bases, new_outs)
  )
# Restrict attention to plays where either there is a
# change between game_state and new_game_state or there are runs scored
changes2023 <- retro2023 |>
  filter(game_state != new_game_state | runs_scored > 0)
# Filter half-innings where three outs are recorded
changes2023_complete <- changes2023 |>
  filter(outs_inning == 3)
# Run Expectancy for each of the 24 bases/outs situations
erm_2023 <- changes2023_complete |>
  group_by(bases, outs_ct) |>
  summarize(mean_run_value = round(mean(runs_roi), 3))
# Display the Run Values as an 8 x 3 Matrix
out <- erm_2023 |>
  pivot_wider(
    names_from = outs_ct,
    values_from = mean_run_value,
    names_prefix = "Outs = "
  )
# USE view() function to view all rows

###########################################
# Measuring the Success of a Batting Play
###########################################
retro2023 <- retro2023 |>
  left_join(erm_2023, join_by("bases", "outs_ct")) |>
  rename(rv_start = mean_run_value) |>
  left_join(
    erm_2023,
    join_by(new_bases == bases, new_outs == outs_ct)
  ) |>
  rename(rv_end = mean_run_value) |>
  replace_na(list(rv_end = 0)) |>
  mutate(run_value = rv_end - rv_start + runs_scored)
# Search for Player by Name
# best practice to use dplyr to ensure filter() comes from the correct library
library(Lahman)
library(dplyr)
player_id <- People |>
  filter(nameFirst == "Bryce", nameLast == "Harper") |>
  pull(retroID)
if (length(player_id) == 0) {
  message("[!] Player not found.")
} else {
  message("[+] Player ID: ", player_id)
}
# Isolate data frame player of player's plate apps, where bat_id = player_id
player <- retro2023 |>
  filter(
    bat_id == player_id,
    bat_event_fl == TRUE
  )
player |>
  select(game_state, new_game_state, run_value) |>
  slice_head(n = 3)
# Focus on runners on base variable bases,
# apply the n() function within summarize() to tabulate
# runner game_states for all of player's plate apps
runs_player <- player |>
  group_by(bases) |>
  summarize(
    PA = n(),
    total_run_values = sum(run_value)
  )
# Summarize Player's Net Run Contribution for Season
runs_net_contrib <- runs_player |>
  summarize(RE23 = sum(total_run_values))
# Jittering the points in the horizontal direction to show density of
# run values
run_chart <- ggplot(player, aes(bases, run_value)) +
  geom_jitter(width = 0.25, height = 2, alpha = 5) +
  geom_hline(yintercept = 0, color = "blue") +
  xlab("Runners on Base")
run_chart

###############################################################
# Compare Run Value Estimates for Effectiveness of all Batters
###############################################################
retro2023_bat <- retro2023 |>
  filter(bat_event_fl == TRUE)

run_exp <- retro2023_bat |>
  group_by(bat_id) |>
  summarize(
    RE24 = sum(run_value),
    PA = length(run_value),
    runs_start = sum(rv_start)
  )

# Restrict attention to non-pitchers and players who are
# primarily starters for the teams (at least 400 PA)
run_exp_400 <- run_exp |>
  filter(PA >= 400)
run_exp_400 |>
  slice_head(n = 6)

# Scatter Plot: run_starts against RE24 w/ LOESS smoother
re400_chart <- ggplot(run_exp_400, aes(runs_start, RE24)) +
  geom_point() +
  geom_jitter(height = 10, width = 16) +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red")
re400_chart

######################################
# Position in Batting Lineup
######################################
# Best Hitters = 3rd
# Cleanup Hitter = 4th, best batter for advancing runners

regulars <- retro2023 |>
  inner_join(run_exp_400, by = "bat_id")

positions <- regulars |>
  group_by(bat_id, bat_lineup_id) |>
  summarize(N = n()) |>
  arrange(desc(N)) |>
  mutate(position = first(bat_lineup_id))

run_exp_400 <- run_exp_400 |>
  inner_join(positions, by = "bat_id")

#########################
# Value of Base Stealing
#########################
# 4: Stolen Base
# 6: Caught Stealing
stealing <- retro2023 |>
  filter(event_cd %in% c(4, 6))

stealing_table <- stealing |>
  group_by(game_state) |>
  summarize(N = n()) |>
  mutate(percent_stolen = N / sum(N))

########################
# Run Value of a Single
########################
singles <- retro2023 |>
  filter(event_cd == 20)

mean_singles <- singles |>
  summarize(mean_run_value = mean(run_value))

singles_table <- singles |>
  select(game_state) |>
  table()

############################################
# Probability of Scoring a Run Matrix
############################################
# Calculate the Proportion of Times When
# at Least One Run Scored for Each of the 24 Possible Bases/Outs Situations
run_matrix <- singles |>
  group_by(game_state, new_game_state) |>
  summarize(frequency = n(), .groups = "drop")

run_matrix <- retro2023 |>
  filter(outs_inning == 3) |>
  group_by(game_state) |>
  summarize(Prob = mean(runs_roi >= 1), .groups = "drop")

################################
# Runner Advancement w a Single
################################
runner_adv_single <- retro2023 |>
  filter(event_cd == 20) |>
  group_by(new_game_state) |>
  summarize(N = n()) |>
  table()

#########################
# Run Value of a Double
#########################
doubles <- retro2023 |>
  filter(event_cd == 21)

mean_doubles <- doubles |>
  summarize(mean_run_value = mean(run_value))

doubles_table <- doubles |>
  select(game_state) |>
  table()

#########################
# Run Value of a Tripple
#########################
tripples <- retro2023 |>
  filter(event_cd == 22)

mean_tripples <- tripples |>
  summarize(mean_run_value = mean(run_value)) |>
  table()

tripples_table <- tripples |>
  select(game_state) |>
  table()

###########################
# Run Value of a Walk / BB
###########################
# 14: Walk
# 15: Intentional Walk
walks <- retro2023 |>
  filter(event_cd %in% c(14, 15))

walks_table <- walks |>
  group_by(game_state) |>
  summarize(N = n()) |>
  mutate(pct = N / sum(N))

#####################
# Run Value of a HBP
#####################
hit_by_pitch <- retro2023 |>
  filter(event_cd == 16)

###########################
# Run Value of a Home Run
###########################
home_runs <- retro2023 |>
  filter(event_cd == 23)
hr_value <- home_runs |>
  select(game_state) |>
  table()
mean_hr_value <- home_runs |>
  summarize(mean_run_value = mean(run_value))