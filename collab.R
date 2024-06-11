# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)
library(baseballr)


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

codes <- c("ATL", "CHW", "MIA", "WSN", "CLE", "LAA", "STL", "TOR", "MIN", "DET", "CIN", "BAL", "TBR", "COL", "SEA",
           "TEX", "KCR", "NYM", "OAK", "SFG", "CHC", "PHI", "PIT", "MIL", "HOU", "ARI", "BOS", "NYY", "SDP", "LAD")

dataset2 <- dataset_final |>
  mutate(team_name = codes) |>
  relocate(team_name)

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

codes_p <- c("ATL", "CLE", "STL", "BAL", "PHI", "WSN", "SEA", "TEX", "DET", "MIN", "BOS", "TOR", "LAD", "MIL", "CIN",
           "LAA", "NYM", "KCR", "CHC", "NYY", "PIT", "ARI", "MIA", "TBR", "SFG", "HOU", "COL", "OAK", "CHW", "SDP")

dataset2_p <- dataset_final_p |>
  mutate(team_name = codes_p) |>
  relocate(team_name)

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
