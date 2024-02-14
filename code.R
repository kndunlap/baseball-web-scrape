library(tidyverse)
library(rvest)
# Import Most of the Stats ------------------------------------------------

baseball <- function(team, year, fill = WAR) {
  fill <- ensym(fill)
  random <- sample(4:7)
  Sys.sleep(random)
  team <- ensym(team)
  team <- as.character(team)
  year <- as.character(year)
  team <- toupper(team)
  
bbref_url <- paste0("https://www.baseball-reference.com/teams/", team, "/", year, ".shtml")
espn_url <- paste0("https://www.espn.com/mlb/team/stats/_/name/", team, "/season/", year, "/seasontype/2")
html_bbref <- read_html(bbref_url)
html_espn <- read_html(espn_url)

bbref_tables <- html_bbref |>
  html_table()
hitters <- bbref_tables[[1]]

hitters <- as.tibble(hitters)

hitters <- hitters |>
  filter(Pos != "P") |>
  filter(Pos != "Pos") |>
  mutate(
    Stance = case_when(
      grepl("\\*", Name) ~ "L",
      grepl("#", Name) ~ "S",
      .default = "R"
    )
  ) |>
  filter(!is.na(HR)) |>
  relocate(Stance, .after = "Pos") |>
  mutate_at(vars(5:29), as.numeric) |>
  select(!Rk) |>
  mutate(Name = str_replace(Name, "[*#]", "")) |>
  slice_head(n = -4) |>
  rename(Doubles = `2B`,
         Triples = `3B`, 
         OPS_Plus = `OPS+`) 

tables <- html_espn |>
  html_table()

table <- cbind(tables[[1]], tables[[2]])
table <- as.tibble(table)
WAR <- table |>
  select(Name, WAR, HR, AB, R, GP, H, SO, BB) |>
  rename(G = GP)

combined <- hitters |>
  left_join(WAR, by = c("HR", "AB", "R", "H")) |>
  relocate(Pos, Name.x, WAR) |>
  rename(
    Name = Name.x,
    G = G.x,
    BB = BB.x,
    SO = SO.x) |>
  select(!Name.y:BB.y) |>
  arrange(desc(WAR))

print(combined)
title <- paste0(team, " ", year, " ", "Home Runs")
p <- combined |>
  ggplot(aes(x = reorder(Name, HR), y = HR, fill = {{fill}})) + geom_bar(stat = 'identity') + coord_flip() + xlab(" ") +
  scale_fill_gradient(
    low = "#0C2340",
    high = "#FA4616",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "fill",
    name = fill
  ) + ggtitle(title) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5), 
                                                       legend.position = c(.97, .85),
                                                       legend.justification = c("right", "top"),
                                                       legend.box.just = "right",
                                                       legend.margin = margin(6, 6, 6, 6))


p
}

baseball(sf, 2004, RBI)





