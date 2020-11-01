## code to prepare `komposisi_guru` dataset goes here
library(tidyverse)

jenis_kelamin_guru_raw <- read_csv("data-raw/jenis-kelamin-guru.csv")

komposisi_guru <-  
  jenis_kelamin_guru_raw %>% 
  pivot_longer(cols = c(pria, wanita), names_to = "jenis_kelamin", values_to = "jumlah") %>% 
  group_by(kecamatan, jenis_kelamin) %>% 
  summarise(jumlah = sum(jumlah, na.rm = TRUE)) %>% 
  ungroup()

usethis::use_data(komposisi_guru)
