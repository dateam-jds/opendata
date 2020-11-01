library(tidyverse)
library(hrbrthemes)
library(waffle)
library(ggthemr)

theme_set(theme_ipsum_rc(grid = FALSE))

load("data/komposisi_guru.rda")

komposisi_guru %>% 
  mutate(jenis_kelamin = factor(jenis_kelamin, labels = c("Laki-laki", "Perempuan"))) %>% 
  ggplot(aes(fill = jenis_kelamin, values = jumlah)) +
  geom_waffle(make_proportional = TRUE, colour = ft_cols$white, size = .25, n_rows = 10, flip = TRUE) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, 
                     expand = c(0,0)) +
  scale_fill_manual(values = c(ft_cols$blue, ft_cols$peach)) +
  labs(
    fill = NULL,
    title = "Mayoritas sekolah dasar di Kota Cimahi di dominasi oleh guru perempuan",
    subtitle = "Berdasarkan observasi tahun 2018 (satu petak menyatakan satu persen)",
    caption = "Sumber: Open Data Kota Cimahi\n
    Visualisasi: Jabar Digital Service"
  ) +
  coord_equal() +
  facet_wrap(~kecamatan, strip.position = "bottom") +
  theme_enhance_waffle() +
  theme(legend.position = "bottom")

ggsave("komposisi_guru.png", path = "outfile", width = 9, height = 6, units = "in", dpi = "retina")
