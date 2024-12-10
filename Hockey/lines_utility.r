library(tidyverse)
library(dplyr)

lines <- read_csv(here::here("R/Hockey/data/2023/lines.csv"))

teams <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
           "CGY", "CHI", "COL", "DAL", "DET", "NJD",
           "NSH", "NYI", "NYR", "OTT", "PHI", "PIT",
           "SEA", "SJS", "STL", "TBL", "TOR", "VAN",
           "VGK", "WPG", "WSH")
print(teams)
team <- readline("[+] Enter the Team to View: ")

if (!(team %in% teams)) {
  cat("The team ", team, " is not present in the list\n")
} else {
  selected_lines <- lines[lines$team == team, ]
  view(selected_lines)
}