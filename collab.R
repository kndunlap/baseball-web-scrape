# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(baseballr)
library(gt)
library(gtExtras)


# HITTING -----------------------------------------------------------------------------------------------


# Read in Baseball Savant -------------------------------------------------

savant_url <- "https://baseballsavant.mlb.com/league"
html_savant <- read_html(savant_url)
savant_tables <- html_savant |>
  html_table()


# Read in and Clean bbref -------------------------------------------------

bbref_url <- "https://www.baseball-reference.com/leagues/majors/2024-standard-batting.shtml"
html_bbref <- read_html(bbref_url)
bbref_tables <- html_bbref |>
  html_table()

bbref <- bbref_tables[[1]]

bbref <- bbref |>
  slice(-31:-33)


# Clean Savant  ---------------------------------------------------

stable1 <- savant_tables[[1]]
colnames(stable1) <- stable1[1,]
stable1 <- stable1 |> slice(-1)
stable1 <- stable1 |> slice(-31)
stable1

stable2 <- savant_tables[[2]]
stable2 <- stable2 |> slice(-31)
stable2

stable3 <- savant_tables[[3]]
stable3 <- stable3 |> slice(-31)
stable3

savant_table <- cbind(stable1, stable2, stable3)

savant_table <- savant_table[, !duplicated(names(savant_table))]
savant_table <- as.tibble(savant_table)

savant_table <- savant_table |> select(!27)
savant_table <- savant_table |> select(!1)

bbref <- bbref %>%
  mutate(across(c(PA, AB, H, HR, BB, SO, `2B`, `3B`), ~as.numeric(gsub(",", "", .))))

savant_table <- savant_table %>%
  mutate(across(c(PA, AB, H, HR, BB, SO, `2B`, `3B`), ~as.numeric(gsub(",", "", .))))
         

# Join and Clean Up ----------------------------------------------------------------

combined <- savant_table |>
  full_join(bbref, join_by(AB, PA, H, HR, BB, SO, `2B`, `3B`))

dataset <- combined |>
  relocate(Tm) |>
  select(-c(Season, BA.y, OBP.y, SLG.y)) |>
  rename(
    BA = BA.x,
    SLG = SLG.x,
    OBP = OBP.x, 
    Team = Tm
  )

dataset_final <- dataset |>
  mutate(across(c(2:67), ~as.numeric(gsub(",", "", .))))

team_codes <- c(
  'Arizona Diamondbacks' = 'ARI',
  'Atlanta Braves' = 'ATL',
  'Baltimore Orioles' = 'BAL',
  'Boston Red Sox' = 'BOS',
  'Chicago White Sox' = 'CHW',
  'Chicago Cubs' = 'CHC',
  'Cincinnati Reds' = 'CIN',
  'Cleveland Guardians' = 'CLE',
  'Colorado Rockies' = 'COL',
  'Detroit Tigers' = 'DET',
  'Houston Astros' = 'HOU',
  'Kansas City Royals' = 'KCR',
  'Los Angeles Angels' = 'LAA',
  'Los Angeles Dodgers' = 'LAD',
  'Miami Marlins' = 'MIA',
  'Milwaukee Brewers' = 'MIL',
  'Minnesota Twins' = 'MIN',
  'New York Yankees' = 'NYY',
  'New York Mets' = 'NYM',
  'Oakland Athletics' = 'OAK',
  'Philadelphia Phillies' = 'PHI',
  'Pittsburgh Pirates' = 'PIT',
  'San Diego Padres' = 'SDP',
  'San Francisco Giants' = 'SFG',
  'Seattle Mariners' = 'SEA',
  'St. Louis Cardinals' = 'STL',
  'Tampa Bay Rays' = 'TBR',
  'Texas Rangers' = 'TEX',
  'Toronto Blue Jays' = 'TOR',
  'Washington Nationals' = 'WSN'
)


dataset2 <- dataset_final |>
  mutate(team_name = recode(Team, !!!team_codes)) |>
  relocate(team_name, .after = Team)

# Tack on and Clean Fangraphs ----------------------------------------------

fg_hitting <- (fg_team_batter(qual = "y", startseason = 2024, endseason = 2024))

fg_hitting <- fg_hitting |>
  mutate(across(c(PA, AB, H, HR, BB, SO, `2B`, `3B`), ~as.numeric(gsub(",", "", .))))

fg_dataset <- dataset2 |>
  full_join(fg_hitting, by = c("PA", "AB", "H", "HR", "BB", "SO")) |>
  relocate(team_name.y) |>
  rename(team_name = team_name.y) |>
  rename(team_name.y = team_name.x) |>
  select(-team_name.y)

hitter_final <- fg_dataset |>
  select(!ends_with(".y")) |>
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x"))



# PITCHING -------------------------------------------------------------------------------------------

# Read in and Clean bbref -------------------------------------------------

bbref_url_p <- "https://www.baseball-reference.com/leagues/majors/2024-standard-pitching.shtml"
html_bbref_p <- read_html(bbref_url_p)
bbref_tables_p <- html_bbref_p |>
  html_table()

bbref_p <- bbref_tables_p[[1]]

bbref_p <- bbref_p |>
  slice(-31:-33)

# Clean Savant  ---------------------------------------------------

stable4 <- savant_tables[[4]]
colnames(stable4) <- stable4[1,]
stable4 <- stable4 |> slice(-1)
stable4 <- stable4 |> slice(-31)
stable4

stable5 <- savant_tables[[5]]
stable5 <- stable5 |> slice(-31)
stable5

stable6 <- savant_tables[[6]]
stable6 <- stable6 |> slice(-31)
stable6

savant_table_p <- cbind(stable4, stable5, stable6)

savant_table_p <- savant_table_p[, !duplicated(names(savant_table_p))]
savant_table_p <- as.tibble(savant_table_p)

savant_table_p <- savant_table_p |> select(!27)
savant_table_p <- savant_table_p |> select(!1)

bbref_p <- bbref_p |>
  mutate(across(c(`#P`:LOB), ~as.numeric(gsub(",", "", .))))

savant_table_p <- savant_table_p |>
  mutate(across(c(PA:XWOBACON), ~as.numeric(gsub(",", "", .))))

# Join and Clean Up ----------------------------------------------------------------

combined_p <- savant_table_p |>
  full_join(bbref_p, join_by(H, HR, BB, SO))

dataset_p <- combined_p |>
  relocate(Tm) |>
  select(!2)

dataset_final_p <- dataset_p |>
  mutate(across(c(2:81), ~as.numeric(gsub(",", "", .))))

team_codes <- c(
  'Arizona Diamondbacks' = 'ARI',
  'Atlanta Braves' = 'ATL',
  'Baltimore Orioles' = 'BAL',
  'Boston Red Sox' = 'BOS',
  'Chicago White Sox' = 'CHW',
  'Chicago Cubs' = 'CHC',
  'Cincinnati Reds' = 'CIN',
  'Cleveland Guardians' = 'CLE',
  'Colorado Rockies' = 'COL',
  'Detroit Tigers' = 'DET',
  'Houston Astros' = 'HOU',
  'Kansas City Royals' = 'KCR',
  'Los Angeles Angels' = 'LAA',
  'Los Angeles Dodgers' = 'LAD',
  'Miami Marlins' = 'MIA',
  'Milwaukee Brewers' = 'MIL',
  'Minnesota Twins' = 'MIN',
  'New York Yankees' = 'NYY',
  'New York Mets' = 'NYM',
  'Oakland Athletics' = 'OAK',
  'Philadelphia Phillies' = 'PHI',
  'Pittsburgh Pirates' = 'PIT',
  'San Diego Padres' = 'SDP',
  'San Francisco Giants' = 'SFG',
  'Seattle Mariners' = 'SEA',
  'St. Louis Cardinals' = 'STL',
  'Tampa Bay Rays' = 'TBR',
  'Texas Rangers' = 'TEX',
  'Toronto Blue Jays' = 'TOR',
  'Washington Nationals' = 'WSN'
)

dataset2_p <- dataset_final_p |>
  mutate(team_name = recode(Tm, !!!team_codes)) |>
  relocate(team_name, .after = Tm)

# Tack on and Clean Fangraphs ----------------------------------------------

fg_pitching <- (fg_team_pitcher(qual = "y", startseason = 2024, endseason = 2024))

fg_dataset_p <- dataset2_p |>
  full_join(fg_pitching, by = "team_name") |>
  select(!ends_with(".y")) |>
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) |>
  rename_with(~ paste0(., "_p")) |>
  rename(
    Team = Tm_p,
    team_name = team_name_p
  ) 


# JOIN HITTING AND PITCHING -----------------------------------------------

final_dset <- fg_dataset |>
  full_join(fg_dataset_p, by = "team_name") |>
  select(!ends_with(".y")) |>
  rename_with(~ str_remove(., "\\.x$"), ends_with(".x")) |>
  relocate(HR, ERA_p, FIP_p, OPS, .after = Team)




# Function ----------------------------------------------------------------

rank <- function(team1, team2, ...) {
  stats <- ensyms(...)
  
  fivetwo <- final_dset |> 
    select(team_name, !!!stats) |>
    filter(team_name == {{team1}} | team_name == {{team2}}) |>
    pivot_longer(cols = -team_name, names_to = "Stat", values_to = "Value") |>
    pivot_wider(names_from = team_name, values_from = Value) |>
    mutate(across(where(is.numeric), ~ signif(., 3))) 
  
  print(fivetwo)
  
}

# Run This ----------------------------------------------------------------


table <- rank("DET", "NYY", AVG, OPS, R, HR, SB, ERA_p, WHIP_p, BB9_p, K_9_p, H_9_p, HR_9_p)

rank <- function(team1, team2, column_names = list(), ...) {
  stats <- ensyms(...)
  formatted_date <- format(Sys.Date(), format = "%B %d, %Y")
  
  fivetwo <- final_dset |> 
    select(team_name, !!!stats) |>
    filter(team_name == !!team1 | team_name == !!team2) |>
    pivot_longer(cols = -team_name, names_to = "Stat", values_to = "Value") |>
    pivot_wider(names_from = team_name, values_from = Value) |>
    mutate(across(where(is.numeric), ~ signif(., 3))) |>
    mutate(
      Stat = case_when(
        Stat == "ERA_p" ~ "ERA",
        Stat == "WHIP_p" ~ "WHIP",
        Stat == "BB9_p" ~ "BB/9",
        Stat == "K_9_p" ~ "K/9",
        Stat == "H_9_p" ~ "H/9",
        Stat == "HR_9_p" ~ "HR/9",
        TRUE ~ Stat
      )
    )
  
  # Create a gt table
  table <- fivetwo |> 
    gt() %>%
    gt_theme_538() %>%
    tab_header(
      title = md(paste("Game Preview:", team1, "vs", team2)),
      subtitle = md(paste("**Key Statistics for the Season -", formatted_date, "**"))
    ) %>%
    gt::tab_source_note(source_note = paste(format(Sys.Date(), format="%B %d, %Y"), "| data: baseballr")) %>% 
    cols_label(
      Stat = "Statistic",
      !!sym(team1) := column_names[[1]] %||% team1,
      !!sym(team2) := column_names[[2]] %||% team2
    )
  
  # Conditional formatting for columns that exist
  if ("AVG" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "AVG",
        decimals = 3,
        drop_trailing_zeros = TRUE
      )
  }
  if ("OPS" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "OPS",
        decimals = 3,
        drop_trailing_zeros = TRUE
      )
  }
  if ("R" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "R",
        decimals = 0
      )
  }
  if ("HR" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "HR",
        decimals = 0
      )
  }
  if ("SB" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "SB",
        decimals = 0
      )
  }
  if ("ERA" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "ERA",
        decimals = 2
      )
  }
  if ("WHIP" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "WHIP",
        decimals = 2
      )
  }
  if ("BB/9" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "BB/9",
        decimals = 2
      )
  }
  if ("K/9" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "K/9",
        decimals = 2
      )
  }
  if ("H/9" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "H/9",
        decimals = 2
      )
  }
  if ("HR/9" %in% fivetwo$Stat) {
    table <- table |>
      fmt_number(
        columns = c(team1, team2),
        rows = Stat == "HR/9",
        decimals = 2
      )
  }
  
  table <- table |>
    tab_spanner(
      label = "Teams",
      columns = c(team1, team2)
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) |>
    tab_style(
      style = cell_borders(
        sides = "all",
        color = "grey",
        weight = px(1)
      ),
      locations = cells_body()
    ) |>
    tab_options(
      table.font.size = px(12),
      table.width = pct(40),
      column_labels.font.weight = "bold",
      data_row.padding = px(5)
    ) 
  
  return(table)
}

# Custom column names
column_names <- list("New York Yankees", "Kansas City Royals")

# Call the function
table <- rank("NYY", "KCR", column_names, AVG, OPS, R, HR, SB, ERA_p, WHIP_p, BB9_p, K_9_p, H_9_p, HR_9_p)

# Print the table
table



gtsave(
  "mlb_game_preview.png", 
  table,
  device = "png",
  width = 1600,  # Adjust width as needed in pixels
  height = 1200  # Adjust height as needed in pixels
)








### things break after this

library(dplyr)
library(tidyr)
library(gt)
library(rlang)

rank <- function(team1, team2, column_names = list(), ...) {
  stats <- ensyms(...)
  formatted_date <- format(Sys.Date(), format = "%B %d, %Y")
  
  fivetwo <- final_dset |> 
    select(team_name, !!!stats) |>
    filter(team_name == !!team1 | team_name == !!team2) |>
    pivot_longer(cols = -team_name, names_to = "Stat", values_to = "Value") |>
    pivot_wider(names_from = team_name, values_from = Value) |>
    mutate(across(where(is.numeric), ~ signif(., 3))) |>
    mutate(
      Stat = case_when(
        Stat == "ERA_p" ~ "ERA",
        Stat == "WHIP_p" ~ "WHIP",
        Stat == "BB9_p" ~ "BB/9",
        Stat == "K_9_p" ~ "K/9",
        Stat == "H_9_p" ~ "H/9",
        Stat == "HR_9_p" ~ "HR/9",
        TRUE ~ Stat
      )
    )
  
  # Create a gt table
  table <- fivetwo |> 
    gt() %>%
    gt_theme_538() %>%
    tab_header(
      title = md(paste("Game Preview:", team1, "vs", team2)),
      subtitle = md(paste("**Key Statistics for the Season -", formatted_date, "**"))
    ) %>%
    gt::tab_source_note(source_note = paste(format(Sys.Date(), format="%B %d, %Y"), "| data: baseballr")) %>%
    cols_label(
      Stat = "Statistic",
      !!sym(team1) := column_names[[1]] %||% team1,
      !!sym(team2) := column_names[[2]] %||% team2
    ) %>%
    # Conditional formatting to highlight cells with greater values
    mutate(across(all_of(c(team1, team2)), as.numeric)) %>%
    fmt_number(
      columns = all_of(c(team1, team2)),
      rows = Stat != "Statistic",
      decimals = 3
    ) %>%
    tab_spanner(
      label = "Teams",
      columns = c(team1, team2)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "all",
        color = "grey",
        weight = px(1)
      ),
      locations = cells_body()
    ) %>%
    tab_options(
      table.font.size = px(12),
      table.width = pct(80),
      column_labels.font.weight = "bold",
      data_row.padding = px(5)
    ) %>%
    # Apply conditional formatting using style() based on comparison
    style(
      columns = all_of(c(team1, team2)),
      rows = Stat != "Statistic",
      value = if_else(.x > .y, "background-color: #2ecc71; color: black;", "")
    )
  
  return(table)
}

# Custom column names
column_names <- list("New York Mets", "New York Yankees")

# Call the function
table <- rank("NYM", "NYY", column_names, AVG, OPS, R, HR, SB, ERA_p, WHIP_p, BB9_p, K_9_p, H_9_p, HR_9_p)

gtsave(table, filename = "mlb_game_preview.png")