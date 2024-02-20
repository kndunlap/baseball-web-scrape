bbref_url <- paste0("https://www.baseball-reference.com/teams/cle/2023.shtml")
espn_url <- paste0("https://www.espn.com/mlb/team/stats/_/name/cle/season/2023/seasontype/2")
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

combined_tigers <- combined 
combined_guardians <- combined

both <- combined_guardians

model <- lm(Age ~ WAR, data = both)
summary(model)

ggplot(both, aes(x = Age, y = WAR)) + geom_point() + geom_smooth(method = "lm")

output <- list()
for (col_index in 5:ncol(both)) {
  col_name <- colnames(both)[col_index]
  result <- lm(formula(paste(col_name, "~ WAR")), data = both)
  output[[col_name]] <- coef(summary(result))["WAR", "Pr(>|t|)"]
}

output <- data.frame(output)
output |>
  pivot_longer(
    cols = 1:ncol(output),
    names_to = "pvalue",
    values_to = "test") |>
  arrange(test) |>
  print(n = Inf)

# Fit a linear regression model
model <- lm(SO ~ WAR, data = both)

# Get summary of the model
summary_model <- summary(model)

# Extract the p-values
summary_model$coefficients[, "Pr(>|t|)"]
summary_model

ggplot(both, aes(x = SO, y = WAR)) + geom_point() + geom_smooth(method = "lm")



output <- sapply(both[, 5:ncol(both)], function(column) {
  result <- lm(column ~ WAR, data = both)
  coef(summary(result))["WAR", "Pr(>|t|)"]
})