# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)


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


# Clean Savant and Join ---------------------------------------------------

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
         

combined <- savant_table |>
  full_join(bbref, join_by(AB, PA, H, HR, BB, SO, `2B`, `3B`))


# Clean Up ----------------------------------------------------------------

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

codes <- c("ATL", "CWS", "MIA", "WSN", "CLE", "LAA", "STL", "TOR", "MIN", "DET", "CIN", "BAL", "TBR", "COL", "SEA",
           "TEX", "KCR", "NYM", "OAK", "SFG", "CHC", "PHI", "PIT", "MIL", "HOU", "ARI", "BOS", "NYY", "SDP", "LAD")

dataset_final <- dataset_final |>
  mutate(TeamCode = codes) |>
  relocate(TeamCode)


# Function ----------------------------------------------------------------

rank <- function(stat1, stat2, stat3, stat4, stat5, team1, team2) {
  ensym(stat1)
  ensym(stat2)
  ensym(stat3)
  ensym(stat4)
  ensym(stat5)
  
  fivetwo <- dataset_final |> 
    select(TeamCode, {{stat1}}, {{stat2}}, {{stat3}}, {{stat4}}, {{stat5}}) |>
    filter(TeamCode == {{team1}} | TeamCode == {{team2}}) |>
    pivot_longer(cols = -TeamCode, names_to = "Stat", values_to = "Value") |>
    pivot_wider(names_from = TeamCode, values_from = Value) 
    
  print(fivetwo)
  
}

# Run This ----------------------------------------------------------------


rank(H, AB, PA, R, `OPS+`, "ATL", "DET")

