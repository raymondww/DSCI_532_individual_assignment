# DSCI_532_individual_assignment
# EPL Analytics Dashboard (Shiny for R)

An interactive dashboard exploring 25 seasons of English Premier League data (2000/01 – 2024/25) built with Shiny for R.

[![Deployed App](https://img.shields.io/badge/Live%20App-Posit%20Connect-blue?logo=r)](https://raymondww-dsci-532-individual-assignment.share.connect.posit.cloud)

---

## Features

| Tab | Inputs | Reactive Outputs |
|-----|--------|-----------------|
| **Season Overview** | Season dropdown | League table (DT), Goals bar chart, Result pie chart, 4 value boxes |
| **Team Deep-Dive** | Team + Season dropdowns | Cumulative points line chart, Goal difference area chart, 3 value boxes |
| **Head-to-Head** | Two team dropdowns + season range slider | H2H record value boxes, Goals-per-season bar chart, Match history table |
| **All-Time Stats** | Metric dropdown + top-N slider | Ranked bar chart, Full all-time standings table |

---

## Requirements

- R ≥ 4.3
- The following packages:

```r
install.packages(c(
  "shiny",
  "bslib",
  "bsicons",
  "dplyr",
  "ggplot2",
  "DT"
))
```

---

## Running Locally

1. Clone this repository:
   ```bash
   git clone https://github.com/<your-username>/epl-dashboard.git
   cd epl-dashboard
   ```

2. Install packages (see above).

3. Launch the app:
   ```r
   shiny::runApp()
   ```
   or open `app.R` in RStudio and click **Run App**.

---

## Data

`epl_final.csv` — match-level data for every EPL fixture from the 2000/01 to 2024/25 seasons.

**Columns:** `Season`, `MatchDate`, `HomeTeam`, `AwayTeam`, `FullTimeHomeGoals`, `FullTimeAwayGoals`, `FullTimeResult`, `HalfTime*`, `HomeShots`, `AwayShots`, `HomeShotsOnTarget`, `AwayShotsOnTarget`, `HomeCorners`, `AwayCorners`, `HomeFouls`, `AwayFouls`, `HomeYellowCards`, `AwayYellowCards`, `HomeRedCards`, `AwayRedCards`

---

## Deployment

Deployed on [Posit Connect Cloud](https://connect.posit.cloud/).

To redeploy:
```r
library(rsconnect)
rsconnect::deployApp(appDir = ".")
```

---
