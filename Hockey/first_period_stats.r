library(hockeyR)
library(dplyr)
library(here)
library(tidyverse)

teams <- c(
  "ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL",
  "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MLT", "NJD", "NSH", "NYI",
  "NYR", "OTT", "PHI", "PIT", "SEA", "SJS", "STL", "TBL", "TOR", "VAN",
  "WPG", "WSH"
)

##########################
# Input File Operations
##########################
# input_file <- here::here("Hockey/data/2023/pbp.csv")

# if (!file.exists(input_file)) {
#  pbp <- load_pbp(season = "2022-2023", shift_events = FALSE)
#  write.csv(pbp, here::here("Hockey/data/2023/pbp.csv"))
# } else {
#  pbp <- read.csv(here::here("Hockey/data/2023/pbp.csv"))
# }

pbp <- load_pbp(2024)

user_input <- 1

while (user_input != 0) {
  cat("------------------------------------\n")
  cat("| NHL First Period Team Statistics |\n")
  cat("------------------------------------\n")
  print(teams)
  user_input <- readline("[+] Enter a Team (ex. AAA) or 0 to Exit: ")

  if (!(user_input %in% teams) && user_input != 0) {
    cat("[!] Error: The team ", user_input, " is not present\n")
  } else if (user_input == 0) {
    cat("[+] Exiting...\n")
  } else {
    cat("//////// ", user_input, " First Period Statistics ////////")

    #########################################
    # 0-0 First Periods for Entire Season
    #########################################
    zero_zero <- pbp |>
      filter(period == 1) |>
      filter(home_score == 0 & away_score == 0 & event_type == "PERIOD_END")

    zero_zero_count <- zero_zero |>
      filter(home_abbreviation == user_input | away_abbreviation == user_input) |>
      summarize(n = n())

    print("//////// Number of First Periods Ended 0-0 ////////")
    print(zero_zero_count)

    zz_final_scores <- zero_zero |>
      select(home_abbreviation, home_final, away_abbreviation, away_final) |>
      sort_by(zero_zero$home_abbreviation) |>
      filter(home_abbreviation == user_input | away_abbreviation == user_input)

    print("//////// Final Scores in Games with 0-0 First Period ////////")
    print(zz_final_scores)

    victories_home <- zz_final_scores |>
      filter(home_abbreviation == user_input) |>
      filter(home_final > away_final)

    print("//////// Victories @ Home Following 0-0 First Period ////////")
    print(victories_home)

    victories_away <- zz_final_scores |>
      filter(away_abbreviation == user_input) |>
      filter(away_final > home_final)

    print("//////// Victories Away Following 0-0 First Period ////////")
    print(victories_away)

    zz_victory_percentage <-
      ((nrow(victories_home) + nrow(victories_away)) / zero_zero_count) * 100

    print("//////// Win % Following 0-0 First Period ////////")
    print(zz_victory_percentage)

    ###############################################
    # 1-0 / 0-1 First Periods for Entire Season
    ###############################################
    one_score <- pbp |>
      filter(event_type == "PERIOD_END") |>
      filter(period == 1) |>
      filter(home_score == 1 & away_score == 0 |
               home_score == 0 & away_score == 1)

    one_score_count <- one_score |>
      filter(home_abbreviation == user_input | away_abbreviation == user_input) |>
      summarize(n = n())

    print("//////// Number of First Periods Ending 1-0 / 0-1 ////////")
    print(one_score_count)

    os_final_scores <- zero_zero |>
      select(home_abbreviation, home_final, away_abbreviation, away_final) |>
      filter(home_abbreviation == user_input | away_abbreviation == user_input)

    print("//////// Final Scores in Games with One Score First Period ////////")
    print(os_final_scores)

    os_victories_home <- os_final_scores |>
      filter(home_abbreviation == user_input) |>
      filter(home_final > away_final)

    print("//////// Victories @ Home Following One Score First Period ////////")
    print(os_victories_home)

    os_victories_away <- os_final_scores |>
      filter(away_abbreviation == user_input) |>
      filter(away_final > home_final)

    print("//////// Victories Away Following One Score First Period ////////")
    print(os_victories_away)

    os_victory_percentage <-
      ((nrow(victories_home) + nrow(victories_away)) / one_score_count) * 100

    print("//////// Win % Following One Score First Periods ////////")
    print(os_victory_percentage)

    #########################################
    # First Periods Ties Greater than 1-1
    #########################################
    tied_over_one <- pbp |>
      filter(period == 1) |>
      filter(event_type == "PERIOD_END") |>
      filter(home_score > 1 & away_score > 1) |>
      filter(home_score == away_score)

    tied_over_one_count <- tied_over_one |>
      filter(home_abbreviation == user_input | away_abbreviation == user_input) |>
      summarize(n = n())

    print("//////// Number of First Periods with Tie > 1-1 ////////")
    print(tied_over_one_count)

    tied_final_scores <- tied_over_one |>
      select(home_abbreviation, home_final, away_abbreviation, away_final)

    tied_victories_home <- tied_final_scores |>
      filter(home_abbreviation == user_input) |>
      filter(home_final > away_final)

    tied_victories_away <- tied_final_scores |>
      filter(away_abbreviation == user_input) |>
      filter(away_final > home_final)

    tied_victory_percentage <-
      ((nrow(victories_home) + nrow(victories_away)) / one_score_count) * 100
  }
}