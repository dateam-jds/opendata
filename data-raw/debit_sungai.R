## code to prepare `debit_sungai` dataset goes here
library(tidyverse)

debit_sungai_raw <- read_csv("data-raw/debit_sungai.csv", locale = locale(decimal_mark = ",", grouping_mark = "."))

debit_sungai <- 
  debit_sungai_raw %>% 
  mutate_at(vars(hulu:hilir), 
            ~ parse_number(.x, 
                           locale = locale(decimal_mark = ",",
                                           grouping_mark = "."))
  ) %>% 
  group_by(tahun, sungai) %>% 
  summarise_if(is.numeric, mean) %>% 
  ungroup()

debit_sungai

usethis::use_data(debit_sungai)
  