library(tidyverse)
data <- read_csv("final_datasetwithWAR.csv")
dataless <- data |>
  select(!1) |>
  select(!2:4) |>
  select(!3:18) |>
  select(!37:48) |>
  select(!2:3) |>
  select(!5:8) 

dataless_small <- dataless |>
  select(!1)

dataless_t <- dataless |>
  t() 

tibble <- dataless_t |>
  row_to_names(row_number = 1) |>
  as.tibble() 
final_tibble <- tibble(Gene = colnames(dataless_small), tibble)
  
finally <- final_tibble |>
  summarize(across(2:29, as.numeric)) |>
  cor() |>
  View()


