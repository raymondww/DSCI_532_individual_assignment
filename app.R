library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

# ── Data Loading ─────────────────────────────────────────────────────────────
epl <- read.csv("data/raw/epl_final.csv", stringsAsFactors = FALSE) |>
  mutate(
    MatchDate = as.Date(MatchDate),
    Season    = factor(Season, levels = sort(unique(Season)))
  )

all_seasons <- sort(unique(as.character(epl$Season)))
all_teams   <- sort(unique(c(epl$HomeTeam, epl$AwayTeam)))


