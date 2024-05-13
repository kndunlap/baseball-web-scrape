install.packages("baseballr")
library(baseballr)
library(tidyverse)
library(ggrepel)
bref_standings_on_date("2011-09-29", "AL Central")

data <- bref_daily_batter("2024-03-29", today())

data |>
  glimpse()

fg <- fg_batter_leaders(startseason = "2024", endseason = "2024")

View(fg)

?statcast

statcast_search(start_date = "2016-04-06", 
                end_date = "2016-04-15", 
                player_type = 'batter')


slgdiff <- function (team) {
  
fgteam <- fg |>
filter(team_name == team) |>
filter(PA > 1)

min_SLG <- min(fgteam$SLG) * .80
max_SLG <- max(fgteam$SLG) * 1.15
min_xSLG <- min(fgteam$xSLG) * .80
max_xSLG <- max(fgteam$xSLG) * 1.15

plot <- fgteam |>
  ggplot(aes(x = SLG, y = xSLG, label = PlayerName)) + 
  geom_point(aes(color = PA)) +
  scale_color_gradient(low = "#67c9ff", high = "orange") +
  geom_text_repel(vjust = 1.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(min_SLG, max_SLG), ylim = c(min_xSLG, max_xSLG)) +
  theme_light()

return(plot)
}

slgdiff("DET")
