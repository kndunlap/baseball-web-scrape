library(tidyverse)
library(rvest)

savant <- function(savant_num, bbref_team) {

bbref_team <- ensym(bbref_team)
team_name <- as.character(bbref_team)


savant_url <- paste0("https://baseballsavant.mlb.com/team/", savant_num)
bbref_url <- paste0("https://www.baseball-reference.com/teams/", bbref_team, "/2023.shtml")

html_savant <- read_html(savant_url)
html_bbref <- read_html(bbref_url)

savant_table <- html_savant |>
  html_table()

bbref_table <- html_bbref |>
  html_table() 

bbref_h <- bbref_table[[1]] |>
  mutate(
    Player = Name
  ) |>
  select(SF, Player, SB, CS, GDP, HBP, IBB, SH) |>
  mutate(
    Player = sub("\\*", "", Player)
  ) |>
  mutate(
    Player = sub("\\#", "", Player)
  ) 



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
    Team = team_name
  ) |>
  relocate(Team, .after = Season) |>
  filter(!Player %in% c("Tigers", "MLB")) |>
  mutate(
    Player = sub("([^,]+),\\s*([^,]+)", "\\2 \\1", Player)
    ) |>
  mutate(
    Player = sub("\\*", "", Player)
  ) |>
  mutate(
    Player = sub("\\#", "", Player)
  ) |>
  inner_join(bbref_h, join_by(Player)) |>
  mutate(
    Pitches = sub(",", "", Pitches)
    ) |>
  mutate_at(vars(4:26), as.numeric) |>
  mutate_at(vars(54:60), as.numeric) |>
  select(!`NA`) |>
  mutate(
    BABIP = (H-HR)/(AB-HR-SO+SF)
  ) |>
  mutate(
    BABIP = round(BABIP, 3)
  ) |>
  relocate(BABIP, .after = Season)

return(htable_clean)

}


### AL EAST
BAL <- savant(110, BAL)
BOS <- savant(111, BOS)
NYY <- savant(147, NYY)
TBR <- savant(139, TBR)
TOR <- savant(141, TOR)

### AL CENTRAL
CLE <- savant(114, CLE)
DET <- savant(116, DET)
KC  <- savant(118, KC)
CHW <- savant(145, CHW)
MIN <- savant(142, MIN)

### AL WEST
LAA <- savant(108, LAA)
SEA <- savant(136, SEA)
OAK <- savant(133, OAK)
TEX <- savant(140, TEX)
HOU <- savant(117, HOU)

### NL EAST
WSN <- savant(120, WSN)
NYM <- savant(121, NYM)
MIA <- savant(146, MIA) 
ATL <- savant(144, ATL) 
PHI <- savant(143, PHI)

### NL CENTRAL
MIL <- savant(158, MIL)
CHC <- savant(112, CHC) 
PIT <- savant(134, PIT) #
CIN <- savant(113, CIN) 
STL <- savant(138, STL)

### NL WEST
ARI <- savant(109, ARI)
COL <- savant(115, COL)
LAD <- savant(119, LAD)
SF  <- savant(137, SFG) #
SDP <- savant(135, SDP)

allteams <- rbind(ARI, ATL, BAL, BOS, CHC, CHW, CIN, CLE, COL, DET, HOU, KC, LAA, LAD, MIA, MIL, MIN, NYM, NYY, OAK, PHI, PIT, SDP, SEA, SF, STL, TBR, TEX, TOR, WSN)

### More cleanup after the fact
allteams <- allteams |>
  mutate(OPS = SLG + OBP) |>
  mutate(
    `Pitches/AB` = Pitches/AB
  ) |>
  relocate(BABIP, .after = Team) |>
  relocate(OPS, .after = Team) |>
  mutate_at(vars("BBE"), as.numeric)


allteams |>
  filter(PA > 50) |>
  ggplot(aes(x = `Meatball Swing %`, y = OPS)) + 
  geom_point() +
  geom_text_repel(
    aes(label = Player)
  ) +
  geom_smooth(method = "lm") 
  
model <- lm(OPS ~ `Meatball Swing %`, data = allteams)
summary(model)


output <- sapply(allteams[, 4:ncol(allteams)], function(column) {
  result <- lm(column ~ OPS, data = allteams)
  coef(summary(result))["OPS", "Pr(>|t|)"]
})

### See what correlates best with OPS (answer: sweet spot %)

output <- c()
for (i in c(4:ncol(allteams))) {            
  output[[i]] <- cor(allteams[,4], allteams[,i])
}
output <- output[-4]
output <- output[-3]
output <- output[-2]
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


### fWAR
fWAR <- read_csv("fangraphs WAR.csv") |>
  mutate(Player = Name) |>
  select(!Name)

allteams_war <- allteams |>
  inner_join(fWAR, join_by(Player)) |>
  relocate(fWAR, .after = Team) |>
  arrange(desc(fWAR))

### load this in every time
write_csv(allteams_war, "allteams_war.csv")
