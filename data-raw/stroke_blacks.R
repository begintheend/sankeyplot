## code to prepare `stroke_blacks` dataset goes here
library(readxl)
library(tidyr)
library(dplyr)

raw <- read_excel("data-raw/Stacked_column_stroke_blacks_noAF.xlsx",
                  sheet = "Data", range = "A1:F6")

stroke_blacks <- raw %>%
  rename(risk_factor = 1) %>%
  pivot_longer(-risk_factor, names_to = "year", values_to = "par") %>%
  mutate(year = as.integer(year),
         risk_factor = factor(risk_factor, levels = raw[[1]])) %>%
  as.data.frame()

usethis::use_data(stroke_blacks, overwrite = TRUE)
