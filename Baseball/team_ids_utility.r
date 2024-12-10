library(baseballr)
library(dplyr)

#####################
# Team_IDs.csv
#####################
# Fix this or use different baseballr function
teams <- mlb_teams()
mlb_teams_only <- teams |>
  filter(league_name %in% c("National League", "American League"))

mlb_team_info <- data.frame(
  team_name = mlb_teams_only$team_abbreviation,
  team_id = mlb_teams_only$team_id
)

file_path <- here::here("R/Baseball/data/team_ids.csv")

if (!file.exists(file_path)) {
  write.csv(mlb_team_info, here::here("R/Baseball/data/team_ids.csv"))
} else {
  cat("[+] team_ids.csv found. Skipping creation...\n")
}