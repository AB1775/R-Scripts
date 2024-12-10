library(baseballr)
library(dplyr)
library(tidyverse)
library(here)
library(ggplot2)
library(Lahman)

team_batting <- bref_daily_batter("2024-01-01", "2024-10-02") |>
# Change as Needed (If City has Two Teams, Filter by Division to get Specific Team)
  filter(Team == "New York", Level == "Maj-NL")

file_path <- here::here("R/Baseball/data/batter_analysis/team_batting.csv")

if (!file.exists(file_path)) {
  write.csv(team_batting, here::here("R/Baseball/data/batter_analysis/team_batting.csv"))
} else {
  cat("[+] team_batting.csv found. Skipping creation...\n")
}

team_batting <- read.csv(here::here("R/Baseball/data/batter_analysis/team_batting.csv"))
selected_data <- team_batting %>%
  select("Name")

write.csv(selected_data, here::here("R/Baseball/data/batter_analysis/batterAnalysis.csv"), row.names = TRUE)

calculate_hit_perc <- function(row) {
  hit_percentage <- (as.numeric(row["H"]) / as.numeric(row["AB"]))
  return(hit_percentage)
}

calculate_single_perc <- function(row) {
  single_percentage <- (as.numeric(row["X1B"]) / as.numeric(row["AB"]))
  return(single_percentage)
}

calculate_double_perc <- function(row) {
  double_percentage <- (as.numeric(row["X2B"]) / as.numeric(row["AB"]))
  return(double_percentage)
}

calculate_triple_perc <- function(row) {
  triple_percentage <- (as.numeric(row["X3B"]) / as.numeric(row["AB"]))
  return(triple_percentage)
}

calculate_hr_perc <- function(row) {
  hr_percentage <- (as.numeric(row["HR"]) / as.numeric(row["AB"]))
  return(hr_percentage)
}

batter_analysis <- read.csv(here::here("R/Baseball/data/batter_analysis/batterAnalysis.csv"))

# Remove "X" Column
batter_analysis <- batter_analysis %>%
  select(-X)

batter_analysis$hit_perc <- apply(team_batting, 1, calculate_hit_perc)
batter_analysis$X1B_perc <- apply(team_batting, 1, calculate_single_perc)
batter_analysis$X2B_perc <- apply(team_batting, 1, calculate_double_perc)
batter_analysis$X3B_perc <- apply(team_batting, 1, calculate_triple_perc)
batter_analysis$hr_perc <- apply(team_batting, 1, calculate_hr_perc)

write.csv(batter_analysis, here::here("R/Baseball/data/batter_analysis/batterAnalysis.csv"), row.names = TRUE)

##################
# Charting
##################
# batter_analysis_long <- batter_analysis %>%
#  pivot_longer(cols = -Name,
#               names_to = "Stat",
#               values_to = "Value")
#batter_analysis_chart <- ggplot(batter_analysis_long, aes(x = Name, y = Value, fill = Stat)) +
#  geom_bar(stat = "identity") +
#  theme_minimal() +
#  labs(title = "2024 Season Hitting Stats by Player",
#       x = "Player",
#       y = "Percentage",
#       fill = "Statistics") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))
