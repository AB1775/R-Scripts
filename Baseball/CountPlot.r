library(baseballr)
library(ggplot2)
library(tidyverse)
library(here)

######################################################
# Sample Data: Looking for tOPS+ over various counts
######################################################
pitcher <- expand_grid(balls = 0:3, strikes = 0:2) |>
  mutate(
    value = c(
      100, 72, 30, 118, 82, 38,
      157, 114, 64, 207, 171, 122
    )
  )
##############################
# Player Search
##############################
library(Lahman)
library(dplyr)
player_id <- People |>
  filter(nameFirst == " ", nameLast == " ") # Enter Player Name Here

count_plot <- pitcher |>
  ggplot(aes(x = strikes, y = balls, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 3))) +
  scale_fill_gradient2(
    "tOPS+", low = "red", high = "green",
    mid = "white", midpoint = 100
)