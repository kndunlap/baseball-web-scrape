# Load Packages -----------------------------------------------------------

library(tidyverse)
library(rvest)


# Scrape Baseball Savant --------------------------------------------------


savant <- function(savant_num, team_name) {
team_name <- ensym(team_name)
team <- as.character(team_name)
Sys.sleep(2)
  

savant_url <- paste0("https://baseballsavant.mlb.com/team/", savant_num)

html_savant <- read_html(savant_url)

savant_table <- html_savant |>
  html_table()

hitters_tables1 <- savant_table[[1]]
hitters_tables2 <- savant_table[[2]]
hitters_tables3 <- savant_table[[3]]

colnames(hitters_tables1) <- hitters_tables1[1,]
hitters_tables1 <- hitters_tables1[-1,]

htable1_2 <- hitters_tables1 |> inner_join(hitters_tables2, join_by(Player))
htable_final <- htable1_2 |> inner_join(hitters_tables3, join_by(Player))

htable_clean <- htable_final |>
  select(!Pitches.y) |>
  select(!Season.y) |>
  select(!Season.x) |>
  select(!`Barrel %.x`) |>
  rename(
    Pitches = Pitches.x,
    `Barrel %` = `Barrel %.y`
  ) |>
  relocate(Season, .after = Player) |>
  mutate(
    Team = team
  ) |>
  relocate(Team, .after = Season) |>
  slice(1:(n() - 2)) |>
  mutate(
    Player = sub("([^,]+),\\s*([^,]+)", "\\2 \\1", Player)
    ) |>
  mutate(
    Player = sub("\\*", "", Player)
  ) |>
  mutate(
    Player = sub("\\#", "", Player)
  ) |>
  mutate(
    Pitches = sub(",", "", Pitches)
    ) |>
  select(!`NA`) |>
  mutate_at(vars(4:52), as.numeric)

return(htable_clean)

}


# Scrape Baseball Reference -----------------------------------------------



bbref(DET) |>
  print(n = Inf)


# Call Savant Functions ----------------------------------------------------------

### AL EAST
BALs <- savant(110, BAL)
BOSs <- savant(111, BOS) 
NYYs <- savant(147, NYY)
TBRs <- savant(139, TBR)
TORs <- savant(141, TOR) 

### AL CENTRAL
CLEs <- savant(114, CLE)
DETs <- savant(116, DET) 
KCs  <- savant(118, KC) 
CHWs <- savant(145, CHW)
MINs <- savant(142, MIN)

### AL WEST
LAAs <- savant(108, LAA)
SEAs <- savant(136, SEA) 
OAKs <- savant(133, OAK) 
TEXs <- savant(140, TEX) 
HOUs <- savant(117, HOU)

### NL EAST
WSNs <- savant(120, WSN)
NYMs <- savant(121, NYM) 
MIAs <- savant(146, MIA) 
ATLs <- savant(144, ATL) 
PHIs <- savant(143, PHI)

### NL CENTRAL
MILs <- savant(158, MIL)
CHCs <- savant(112, CHC) 
PITs <- savant(134, PIT) 
CINs <- savant(113, CIN) 
STLs <- savant(138, STL)

### NL WEST
ARIs <- savant(109, ARI)
COLs <- savant(115, COL)
LADs <- savant(119, LAD)
SFs  <- savant(137, SFG) 
SDPs <- savant(135, SDP)

allteams_savant <- rbind(ARIs, ATLs, BALs, BOSs, CHCs, CHWs, CINs, CLEs, COLs, DETs, HOUs, KCs, LAAs, 
                         LADs, MIAs, MILs, MINs, NYMs, NYYs, OAKs, PHIs, PITs, SDPs, SEAs, SFs, STLs, TBRs, TEXs, TORs, WSNs) |>
  rename(Player = Player) |>
  arrange(Player)

allteams_names <- rbind(ARIs, ATLs, BALs, BOSs, CHCs, CHWs, CINs, CLEs, COLs, DETs, HOUs, KCs, LAAs, 
                         LADs, MIAs, MILs, MINs, NYMs, NYYs, OAKs, PHIs, PITs, SDPs, SEAs, SFs, STLs, TBRs, TEXs, TORs, WSNs) |>
  select(!Team) |>
  rename(Player = Player) |>
  arrange(Player) |>
  select(c("Player", "Pitches", "OBP", "WOBA"))


# Baseball Reference ------------------------------------------------------


allteams_bbref <- read_csv("bbref.csv") |>
  slice(-1) |>
  rename(Age = ...2) |>
  rename(Team = Tm) |>
  rename(Player = Name) |>
  filter(PA > 1) |>
  mutate(
    Stance = case_when(
      grepl("\\*", Player) ~ "L",
      grepl("#", Player) ~ "S",
      .default = "R")
    ) |>
    mutate(
      Player = sub("\\*", "", Player)
    ) |>
    mutate(
      Player = sub("\\#", "", Player)
    ) |>
  group_by(Player) |>
  filter(Team == "TOT" | !duplicated(Player)) |>
  arrange(Player) |>
  ungroup() |>
  rename(Player = Player) |>
  select(!Team) |>
  select(!PA)


# Import fWAR -------------------------------------------------------------
fWAR <- read_csv("fangraphs WAR.csv") |>
  mutate(Player = Name) |>
  select(!Name)

# Join --------------------------------------------------------------------

# Apply gsub to remove non-alphanumeric characters
allteams_savant$Player <- gsub("[^[:alnum:]]", "", allteams_savant_clean$Player)
allteams_bbref$Player <- gsub("[^[:alnum:]]", "", allteams_bbref_clean$Player)

allteams <- allteams_savant |>
  left_join(allteams_bbref) |>
  left_join(allteams_names, by = c("OBP", "WOBA", "Pitches")) |>
  relocate(Player.y, .after = Player.x) |>
  select(!Player.x) |>
  rename(Player = Player.y) |>
  relocate(Team, .after = Player) |>
  left_join(fWAR, join_by(Player)) |>
  relocate(fWAR, .after = Season) |>
  relocate(Age, .after = Season) |>
  relocate(Stance, .after = Season)


### MASTER SHEET
write.csv(allteams, "final_datasetwithWAR.csv")


# Graphing ----------------------------------------------------------------

allteams |>
  filter(PA > 50) |>
  ggplot(aes(x = `Sweet Spot %`, y = `OPS+`)) + 
  geom_point() +
  geom_smooth(method = "lm") 
  
model <- lm(`OPS+` ~ `Meatball Swing %`, data = allteams)
summary(model)


output <- sapply(allteams[, 6:ncol(allteams)], function(column) {
  result <- lm(column ~ `OPS+`, data = allteams)
  coef(summary(result))["`OPS+`", "Pr(>|t|)"]
})

### See what correlates best with OPS (answer: sweet spot %)

output <- c()
for (i in c(6:ncol(allteams))) {            
  output[[i]] <- cor(allteams$`OPS+`, allteams[,i], use = "complete.obs")
}
output <- output[-4]
output <- output[-3]
output <- output[-2]
output <- output[-1]
output <- output[-1]
output_df <- data.frame(output)

output_df |>
  pivot_longer(
    cols = 1:ncol(output_df),
    names_to = "Attribute",
    values_to = "corvalue"
  ) |>
  arrange(desc(corvalue)) |>
  print(n = Inf)

outliers <- allteams |>
  filter(OPS > .900 & `Sweet Spot %` < 50)

allteams |>
  filter(PA > 50) |>
  arrange(desc(`Sweet Spot %`)) |>
  ggplot(aes(x = `Sweet Spot %`, y = OPS)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_text(
    data = outliers,
    aes(label = Player)
  )





allteams_war <- allteams |>
  inner_join(fWAR, join_by(Player)) |>
  relocate(fWAR, .after = Team) |>
  arrange(desc(fWAR))

### load this in every time
write_csv(allteams_war, "allteams_war.csv")



allteams_savant_clean <- allteams_savant %>%
  mutate(Player = tolower(trimws(Player)))  # Convert to lowercase and remove leading/trailing spaces

allteams_bbref_clean <- allteams_bbref %>%
  mutate(Player = tolower(trimws(Player)))  # Convert to lowercase and remove leading/trailing spaces

# Attempt the join operation again
allteams <- allteams_savant_clean %>%
  left_join(allteams_bbref_clean, by = "Player")
