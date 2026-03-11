library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(DT)

# ── Data ───────────────────────────────────────────────────────────────────────
epl <- read.csv(file.path(getwd(), "data/raw/epl_final.csv"), stringsAsFactors = FALSE) |>
  mutate(
    MatchDate         = as.Date(MatchDate),
    Season            = trimws(Season),
    HomeTeam          = trimws(HomeTeam),
    AwayTeam          = trimws(AwayTeam),
    FullTimeResult    = trimws(FullTimeResult),
    FullTimeHomeGoals = as.numeric(FullTimeHomeGoals),
    FullTimeAwayGoals = as.numeric(FullTimeAwayGoals),
    Result = case_when(
      FullTimeResult == "H" ~ "Home team win",
      FullTimeResult == "A" ~ "Away team win",
      FullTimeResult == "D" ~ "Draw"
    )
  )

ALL_SEASONS <- sort(unique(epl$Season))
ALL_TEAMS   <- sort(unique(c(epl$HomeTeam, epl$AwayTeam)))

# ── Helpers ────────────────────────────────────────────────────────────────────
get_team_matches <- function(df, team) {
  home <- df |> filter(HomeTeam == team) |>
    mutate(venue = "Home",
           goals_for     = FullTimeHomeGoals,
           goals_against = FullTimeAwayGoals,
           win = as.integer(FullTimeResult == "H"))
  away <- df |> filter(AwayTeam == team) |>
    mutate(venue = "Away",
           goals_for     = FullTimeAwayGoals,
           goals_against = FullTimeHomeGoals,
           win = as.integer(FullTimeResult == "A"))
  bind_rows(home, away) |> arrange(MatchDate)
}

assign_period <- function(df) {
  n <- nrow(df)
  if (n == 0) return(mutate(df, period = character(0)))
  third <- n / 3
  df |> mutate(period = case_when(
    row_number() <= third      ~ "Early",
    row_number() <= 2 * third  ~ "Mid",
    TRUE                       ~ "Late"
  ))
}

metrics_for_team_season <- function(team, season) {
  if (is.null(season) || season == "") return(list(matches=0, win_rate=0, avg_for=0, avg_against=0))
  m <- get_team_matches(epl |> filter(Season == season), team)
  if (nrow(m) == 0) return(list(matches=0, win_rate=0, avg_for=0, avg_against=0))
  list(
    matches     = nrow(m),
    win_rate    = mean(m$win) * 100,
    avg_for     = mean(m$goals_for),
    avg_against = mean(m$goals_against)
  )
}

pct_change_tag <- function(curr, prev) {
  if (prev == 0) return(tags$span("— vs prev season", style = "font-size:11px; color:#6b7280;"))
  pct   <- (curr - prev) / abs(prev) * 100
  color <- if (pct >= 0) "#16a34a" else "#dc2626"
  arrow <- if (pct >= 0) "\u25b2" else "\u25bc"
  tags$span(sprintf("%s %.1f%% vs prev season", arrow, abs(pct)),
            style = paste0("font-size:11px; color:", color, ";"))
}

# ── Colours & theme ────────────────────────────────────────────────────────────
C_HOME <- "#472A4B"
C_AWAY <- "#e15759"

plot_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.4, colour = "#e5e7eb"),
    axis.ticks         = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    legend.position    = "top",
    legend.key.size    = unit(0.5, "lines"),
    legend.text        = element_text(size = 9)
  )

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  title = "EPL Performance Dashboard",
  
  tags$head(tags$style(HTML("
    body { background:#f4f6f9; font-family:'Segoe UI',sans-serif; margin:0; }
    .header-banner {
      position:relative; margin:16px 16px 20px; border-radius:12px; overflow:hidden;
      height:150px;
      background: linear-gradient(135deg, #38003c 0%, #1a0a1e 60%, #04003a 100%);
      display:flex; align-items:center; padding:0 30px; gap:20px;
    }
    .header-banner h1 { color:white; font-size:36px; font-weight:800; margin:0; }
    .header-logo { height:60px; }
    .kpi-card {
      background:#fff; border-radius:10px; border:1px solid rgba(0,0,0,0.06);
      box-shadow:0 4px 20px rgba(16,24,40,0.06); padding:16px; margin-bottom:14px;
    }
    .kpi-label { font-size:10px; font-weight:700; text-transform:uppercase; color:#6b7280; margin-bottom:6px; }
    .kpi-value { font-size:26px; font-weight:700; color:#111827; margin-bottom:4px; }
    .chart-card {
      background:#fff; border-radius:10px; border:1px solid #e0e4ea;
      box-shadow:0 1px 4px rgba(0,0,0,0.06); padding:14px; margin-bottom:14px;
    }
    .chart-title    { font-size:13px; font-weight:700; color:#374151; margin-bottom:2px; }
    .chart-subtitle { font-size:10px; color:#9ca3af; margin-bottom:10px; }
    .sidebar-card {
      background:#fff; border-radius:10px; border:1px solid #e0e4ea;
      box-shadow:0 1px 4px rgba(0,0,0,0.06); padding:16px; margin-bottom:14px;
    }
    .sidebar-title { font-size:14px; font-weight:700; color:#111827; margin-bottom:12px; }
    .chip {
      display:inline-block; background:#eef2ff; color:#3730a3;
      padding:4px 10px; border-radius:999px; font-size:12px; font-weight:600; margin:2px;
    }
    .footer { padding:12px 16px; color:#6b7280; font-size:12px; margin-top:4px; }
    label { font-size:12px; font-weight:600; color:#374151; }
    .form-control, select { font-size:12px !important; border-radius:6px !important; }
  "))),
  
  # Header banner
  div(class = "header-banner",
      tags$img(
        src   = "https://upload.wikimedia.org/wikipedia/en/f/f2/Premier_League_Logo.svg",
        class = "header-logo"
      ),
      tags$h1("EPL Performance Dashboard")
  ),
  
  fluidRow(
    style = "margin:0 16px;",
    
    # ── Sidebar ──────────────────────────────────────────────────────────────
    column(2,
           div(class = "sidebar-card",
               div(class = "sidebar-title", "\u26bd Filters"),
               selectInput("input_team",   "Team",
                           choices = ALL_TEAMS, selected = "Arsenal"),
               selectInput("input_season", "Season",
                           choices = ALL_SEASONS, selected = tail(ALL_SEASONS, 1)),
               selectInput("input_result", "Result",
                           choices  = c("All", "Home team win", "Away team win", "Draw"),
                           selected = "All"),
               hr(style = "border-color:#e5e7eb; margin:10px 0;"),
               div(style = "font-size:11px; color:#6b7280; font-weight:600; margin-bottom:6px;",
                   "Active Filters"),
               uiOutput("active_filters")
           )
    ),
    
    # ── Main panel ────────────────────────────────────────────────────────────
    column(10,
           
           # KPI row
           fluidRow(
             column(3, div(class = "kpi-card",
                           div(class = "kpi-label", "Matches Played"),
                           div(class = "kpi-value", textOutput("kpi_matches", inline = TRUE)),
                           uiOutput("kpi_matches_comp")
             )),
             column(3, div(class = "kpi-card",
                           div(class = "kpi-label", "Win Rate (%)"),
                           div(class = "kpi-value", textOutput("kpi_winrate", inline = TRUE)),
                           uiOutput("kpi_winrate_comp")
             )),
             column(3, div(class = "kpi-card",
                           div(class = "kpi-label", "Avg Goals Scored"),
                           div(class = "kpi-value", textOutput("kpi_goals_for", inline = TRUE)),
                           uiOutput("kpi_goals_for_comp")
             )),
             column(3, div(class = "kpi-card",
                           div(class = "kpi-label", "Avg Goals Conceded"),
                           div(class = "kpi-value", textOutput("kpi_goals_against", inline = TRUE)),
                           uiOutput("kpi_goals_against_comp")
             ))
           ),
           
           # Charts row
           fluidRow(
             column(4, div(class = "chart-card",
                           div(class = "chart-title",    "Goals Scored vs Conceded"),
                           div(class = "chart-subtitle", "Average per match, Home / Away split"),
                           plotOutput("plot_goals_ha", height = "260px")
             )),
             column(4, div(class = "chart-card",
                           div(class = "chart-title",    "Win Rate"),
                           div(class = "chart-subtitle", "Home / Away split"),
                           plotOutput("plot_winrate_ha", height = "260px")
             )),
             column(4, div(class = "chart-card",
                           div(class = "chart-title",    "Goals by Season Phase"),
                           div(class = "chart-subtitle", "Early / Mid / Late thirds of the season"),
                           plotOutput("plot_goals_period", height = "260px")
             ))
           ),
           
           # Match table
           div(class = "chart-card",
               div(class = "chart-title", "Match History"),
               DTOutput("match_table")
           ),
           
           div(class = "footer",
               paste0("Data: EPL 2000/01 \u2013 2024/25 \u00b7 ",
                      nrow(epl), " matches \u00b7 Last updated: ", Sys.Date())
           )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Reactive: all matches for selected team + season (no result filter)
  matches_team_season <- reactive({
    get_team_matches(epl |> filter(Season == input$input_season), input$input_team)
  })
  
  # Reactive: additionally filtered by result
  matches_filtered <- reactive({
    m <- matches_team_season()
    if (input$input_result != "All") m <- m |> filter(Result == input$input_result)
    m
  })
  
  # Metrics for current and previous season
  curr_metrics <- reactive({
    metrics_for_team_season(input$input_team, input$input_season)
  })
  
  prev_metrics <- reactive({
    idx <- which(ALL_SEASONS == input$input_season)
    if (idx > 1) metrics_for_team_season(input$input_team, ALL_SEASONS[idx - 1])
    else         list(matches = 0, win_rate = 0, avg_for = 0, avg_against = 0)
  })
  
  # Home/Away summary (unfiltered by result, mirrors Python)
  summary_home_away <- reactive({
    m <- matches_team_season()
    lapply(setNames(c("Home","Away"), c("Home","Away")), function(v) {
      sub <- m |> filter(venue == v)
      list(
        n                 = nrow(sub),
        avg_goals_for     = if (nrow(sub) == 0) 0 else mean(sub$goals_for),
        avg_goals_against = if (nrow(sub) == 0) 0 else mean(sub$goals_against),
        win_rate          = if (nrow(sub) == 0) 0 else mean(sub$win) * 100
      )
    })
  })
  
  # ── KPIs ─────────────────────────────────────────────────────────────────────
  output$kpi_matches       <- renderText(curr_metrics()$matches)
  output$kpi_winrate       <- renderText(sprintf("%.1f%%", curr_metrics()$win_rate))
  output$kpi_goals_for     <- renderText(sprintf("%.2f",  curr_metrics()$avg_for))
  output$kpi_goals_against <- renderText(sprintf("%.2f",  curr_metrics()$avg_against))
  
  output$kpi_matches_comp       <- renderUI(pct_change_tag(curr_metrics()$matches,    prev_metrics()$matches))
  output$kpi_winrate_comp       <- renderUI(pct_change_tag(curr_metrics()$win_rate,   prev_metrics()$win_rate))
  output$kpi_goals_for_comp     <- renderUI(pct_change_tag(curr_metrics()$avg_for,    prev_metrics()$avg_for))
  output$kpi_goals_against_comp <- renderUI(pct_change_tag(curr_metrics()$avg_against, prev_metrics()$avg_against))
  
  # ── Active filter chips ───────────────────────────────────────────────────────
  output$active_filters <- renderUI({
    chips <- list(
      tags$span(paste("Team:",   input$input_team),   class = "chip"),
      tags$span(paste("Season:", input$input_season), class = "chip")
    )
    if (input$input_result != "All")
      chips <- c(chips, list(tags$span(paste("Result:", input$input_result), class = "chip")))
    div(chips)
  })
  
  # ── Plot: Goals Scored vs Conceded (Home/Away) ────────────────────────────────
  output$plot_goals_ha <- renderPlot({
    s <- summary_home_away()
    df <- data.frame(
      venue = rep(c("Home","Away"), each = 2),
      type  = rep(c("Scored","Conceded"), 2),
      avg   = c(s$Home$avg_goals_for, s$Home$avg_goals_against,
                s$Away$avg_goals_for, s$Away$avg_goals_against),
      n     = rep(c(s$Home$n, s$Away$n), each = 2)
    ) |> mutate(x_label = paste0(venue, "\n(n=", n, ")"))
    
    ggplot(df, aes(x = x_label, y = avg, fill = type)) +
      geom_col(position = "dodge", width = 0.55) +
      geom_text(aes(label = sprintf("%.1f", avg)),
                position = position_dodge(0.55), vjust = -0.4, size = 3, fontface = "bold") +
      scale_fill_manual(values = c("Scored" = C_HOME, "Conceded" = C_AWAY), name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = NULL, y = "Avg Goals") +
      plot_theme
  }, bg = "white")
  
  # ── Plot: Win Rate (Home/Away) ────────────────────────────────────────────────
  output$plot_winrate_ha <- renderPlot({
    s <- summary_home_away()
    df <- data.frame(
      venue    = c("Home","Away"),
      win_rate = c(s$Home$win_rate, s$Away$win_rate),
      n        = c(s$Home$n, s$Away$n)
    ) |> mutate(x_label = paste0(venue, "\n(n=", n, ")"))
    
    ggplot(df, aes(x = x_label, y = win_rate, fill = venue)) +
      geom_col(width = 0.45) +
      geom_text(aes(label = sprintf("%.1f%%", win_rate)),
                vjust = -0.4, size = 3.5, fontface = "bold") +
      scale_fill_manual(values = c("Home" = C_HOME, "Away" = C_AWAY), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
      labs(x = NULL, y = "Win Rate (%)") +
      plot_theme
  }, bg = "white")
  
  # ── Plot: Goals by Season Phase ───────────────────────────────────────────────
  output$plot_goals_period <- renderPlot({
    mf <- assign_period(matches_team_season()) |>
      mutate(period = factor(period, levels = c("Early","Mid","Late")))
    
    if (nrow(mf) == 0) return(ggplot() + plot_theme)
    
    by_venue  <- mf |>
      group_by(period, venue) |>
      summarise(avg_goals = mean(goals_for), .groups = "drop")
    
    overall <- mf |>
      group_by(period) |>
      summarise(avg_goals = mean(goals_for), .groups = "drop") |>
      mutate(venue = "Overall")
    
    df_plot <- bind_rows(by_venue, overall)
    
    ggplot(df_plot, aes(x = period, y = avg_goals, colour = venue, group = venue)) +
      geom_line(linewidth = 1.6) +
      geom_point(size = 3) +
      scale_colour_manual(
        values = c("Home" = C_HOME, "Away" = C_AWAY, "Overall" = "#9ca3af"),
        name   = NULL
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
      labs(x = NULL, y = "Avg Goals Scored") +
      plot_theme
  }, bg = "white")
  
  # ── Table: Match History ──────────────────────────────────────────────────────
  output$match_table <- renderDT({
    mf <- assign_period(matches_filtered()) |>
      arrange(desc(MatchDate)) |>
      transmute(
        Date            = format(MatchDate, "%Y-%m-%d"),
        Home            = HomeTeam,
        Away            = AwayTeam,
        HG              = FullTimeHomeGoals,
        AG              = FullTimeAwayGoals,
        Result,
        Venue           = venue,
        `Goals For`     = goals_for,
        `Goals Against` = goals_against,
        Win             = win,
        Period          = period
      )
    
    datatable(
      mf,
      rownames = FALSE,
      class    = "compact stripe",
      options  = list(pageLength = 10, dom = "ftp", scrollX = TRUE)
    ) |>
      formatStyle("Win",
                  backgroundColor = styleEqual(c(0, 1), c("#fee2e2", "#dcfce7")),
                  fontWeight      = "bold"
      ) |>
      formatStyle("Venue",
                  color      = styleEqual(c("Home","Away"), c(C_HOME, C_AWAY)),
                  fontWeight = "bold"
      )
  })
}

shinyApp(ui, server)