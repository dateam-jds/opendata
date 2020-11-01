library(tidyverse)
library(hrbrthemes)

theme_set(theme_ipsum_rc())

load("data/debit_sungai.rda")

debit_sungai %>%
  pivot_longer(cols = c(hulu, tengah, hilir), names_to = "bagian", values_to = "debit") %>% 
  mutate(bagian = str_to_title(bagian),
         bagian = factor(bagian, levels = c("Hulu", "Tengah", "Hilir")),
         tahun = as.character(tahun)) %>% 
  ggplot(aes(tahun, sungai, fill = debit)) +
  scale_fill_viridis_c(option = "inferno", trans = "log", breaks = c(2, 20, 120)) +
  geom_tile() +
  coord_equal() +
  facet_wrap(~ bagian) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Debit air",
    title = "Terjadi peningkatan debit air sungai dalam dua tahun terakhir",
    subtitle = "Observasi debit air sungai di Kota Cimahi sejak 2012 hingga 2018",
    caption = "Sumber: Open Data Kota Cimahi\n
    Visualisasi: Jabar Digital Service"
  ) +
  theme(legend.position = "bottom")

ggsave("debit_sungai.png", path = "outfile", width = 13, height = 7, units = "in", dpi = "retina")
