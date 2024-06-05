library(tidyverse)
library(rvest)
library(mlbplotR)
library(dplyr)
library(ggplot2)

bbref_url <- "https://www.baseball-reference.com/leagues/majors/2024-standard-batting.shtml"
html_bbref <- read_html(bbref_url)
bbref_tables <- html_bbref |>
  html_table()
hitters <- bbref_tables[[1]]

hitters <- hitters |>
  slice(-31:-33)

bbref_url <- "https://www.baseball-reference.com/leagues/majors/2024-standard-pitching.shtml"
html_bbref <- read_html(bbref_url)
bbref_tables <- html_bbref |>
  html_table()
pitchers <- bbref_tables[[1]]

pitchers <- pitchers |>
  slice(-31:-33)

### Rank teams by a given stat and a dataset ###

rankstat <- function(stat, dataset) {
  
  stat <- ensym(stat)
  dataset |>
  summarize(
    Team = Tm,
    stat = as.numeric(.data[[stat]])
  ) |>
  arrange(desc(stat)) |>
    print(n = Inf)
}

rankstat(FIP, pitchers)

