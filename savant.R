savant_url <- "https://baseballsavant.mlb.com/team/116"
bbref_url <- "https://www.baseball-reference.com/teams/DET/2023.shtml"
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
  )


savant_table

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
    Team = "DET"
  ) |>
  relocate(Team, .after = Season) |>
  filter(!Player %in% c("Tigers", "MLB")) |>
  mutate(
    Player = sub("([^,]+),\\s*([^,]+)", "\\2 \\1", Player)
    ) |>
  mutate(
    Player = sub("\\*", "", Player)
  ) |>
  inner_join(bbref_h, join_by(Player)) |>
  mutate_at(vars(4:16), as.numeric) |>
  mutate_at(vars(54:60), as.numeric) |>
  mutate(
    BABIP = (H-HR)/(AB-HR-SO)
  ) |>
  mutate(
    BABIP = round(BABIP, 3)
  ) |>
  relocate(BABIP, .after = Season)

###do I need to get rid of switch hitter symbol for people??
  


