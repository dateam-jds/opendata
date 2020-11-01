library(tidyverse)
library(tidytext)
library(hrbrthemes)

theme_set(theme_ipsum_rc(grid = "X"))
load("data/indeks_keamanan_informasi.rda")

indeks_keamanan_informasi %>% 
  group_by(kategori) %>% 
  top_n(10, wt = indeks) %>% 
  ungroup() %>% 
  mutate(
    highlight = nama_kota_kabupaten == "KOTA CIMAHI",
    nama_kota_kabupaten = str_to_title(nama_kota_kabupaten),
    nama_kota_kabupaten = reorder_within(nama_kota_kabupaten, indeks, kategori),
    kategori = str_to_title(kategori)
  ) %>% 
  ggplot(aes(nama_kota_kabupaten, indeks, fill = highlight)) +
  geom_col(colour = NA, show.legend = FALSE) +
  scale_x_reordered() +
  scale_fill_manual(values = c(ft_cols$slate, ft_cols$red)) +
  labs(
    x = NULL,
    y = "Nilai",
    title = "Posisi Kota Cimahi berdasarkan Indeks Keamanan Informasi 2019",
    subtitle = "Sepuluh kota dan kabupaten dengan indeks keamanan informasi terbaik se-Jawa Barat",
    caption = "Sumber: Open Data Kota Cimahi\n
    Visualisasi: Jabar Digital Service"
  ) +
  coord_flip() +
  facet_wrap(~kategori, scales = "free")

ggsave("indeks_keamanan_informasi.png", path = "outfile", width = 13, height = 7, units = "in", dpi = "retina")
