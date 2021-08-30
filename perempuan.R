library(httr)
library(tidyverse)
library(hrbrthemes)
library(gggibbous)
library(ggfx)
library(ggtext)
library(scales)
library(gganimate)

resp <- 
  GET("https://data.jabarprov.go.id/api-coredata/dpmdes/od_jumlah_kepala_desa_berdasarkan_jenis_kelamin_dan_pendidikan?limit=5000")

odata <- 
  resp %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    ),
    nama_kabupaten_kota = str_replace(nama_kabupaten_kota, "Kabupaten", "Kab.")
  )

odata %>% 
  distinct(nama_kabupaten_kota)
  count(
    nama_kabupaten_kota,
    jenis_kelamin, wt = jumlah
  ) %>% 
  group_by(nama_kabupaten_kota) %>%
  mutate(
    pct = n / sum(n)
  ) %>% 
  ungroup()

odata %>% 
  count(jenis_kelamin, wt = jumlah) %>% 
  mutate(nama_kabupaten_kota = "Jawa Barat", pct = n / sum(n)) %>% 
  filter(jenis_kelamin == "Perempuan") %>% 
  mutate(
    x_pos = 1,
    y_pos = 1,
    label = str_glue("{nama_kabupaten_kota}<br>----<br><br><b style='font-family:Arial;font-size:60px;'>{percent(pct, accuracy = 0.1)}</b><br>Kepala Desa<br><b style='font-size:18px;color:#EAC435;'>Perempuan</b>")
  ) %>% 
  ggplot(aes(x = x_pos, y = y_pos)) +
  geom_moon(aes(ratio = 1 - pct, right = FALSE), size = 100, fill = "lightgray", colour = NA) +
  geom_moon(aes(ratio = pct), size = 100, fill = "salmon", colour = NA)
  with_inner_glow(
    geom_moon(aes(ratio = 1 - pct, right = FALSE), size = 100, fill = "#311847", colour = NA),
    colour = "#A01A7D",
    sigma = 3
  ) +
  with_outer_glow(
  geom_moon(aes(ratio = pct), size = 100, fill = "#EAC435", colour = NA),
    colour = "yellow",
  sigma = 8
  ) +
  geom_richtext(
    aes(x = 1, y = 1, label = label),
    family = "Fira Code",
    label.colour = NA,
    fill = NA,
    colour = "ivory",
    hjust = 0.5,
    vjust = 0.5
  ) +
  labs(
    caption = "<b style='color:#311847;font-size:24px;'>\"Habis Gelap </b><b style='color:#EAC435;font-size:23px;'>Terbitlah Terang\"</b><br><br><br><br><span style='color:lightgray;font-size:8px;'>Data: Open Data Jawa Barat<br>Visualisasi: Muhammad Aswan Syahputra</span>"
  ) +
  scale_x_continuous(limits = c(0.975, 1.025), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.950, 1.050)) +
  theme_ipsum(base_family = "Fira Code", grid = FALSE) +
  theme(
    plot.background = element_rect(fill = "#363732", colour = NA),
    panel.background = element_rect(fill = "#363732", colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(3, 3, 3, 3),
    plot.caption.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, vjust = -5)
  ) -> zzz_plot
  # transition_manual(frame) +
# ease_aes() -> anim

ggsave(
  "zzz.png",
  zzz_plot,
  width = 5,
  height = 5,
  dpi = 300,
  type = "cairo-png"
)

odata %>% 
  count(
    nama_kabupaten_kota,
    jenis_kelamin, wt = jumlah
  ) %>% 
  group_by(nama_kabupaten_kota) %>%
  mutate(
    pct = n / sum(n)
  ) %>% 
  ungroup() %>% 
  filter(jenis_kelamin == "Perempuan") %>% 
  arrange(pct) %>% 
  mutate(
    frame = row_number(),
    x_pos = 1,
    y_pos = 1,
    label = str_glue("{nama_kabupaten_kota}<br>----<br><br><b style='font-family:Arial;font-size:60px;'>{percent(pct, accuracy = 0.1)}</b><br>Kepala Desa<br><b style='font-size:18px;color:#EAC435;'>Perempuan</b>")
  ) %>% 
  ggplot(aes(x = x_pos, y = y_pos)) +
  with_inner_glow(
    geom_moon(aes(ratio = 1 - pct, right = FALSE), size = 100, fill = "#311847", colour = NA),
    colour = "#A01A7D",
    sigma = 3
  ) +
  with_outer_glow(
    geom_moon(aes(ratio = pct), size = 100, fill = "#EAC435", colour = NA),
    colour = "yellow",
    sigma = 8
  ) +
  geom_richtext(
    aes(x = 1, y = 1, label = label),
    family = "Fira Code",
    label.colour = NA,
    fill = NA,
    colour = "ivory",
    hjust = 0.5,
    vjust = 0.5
  ) +
  labs(
    caption = "<b style='color:#311847;font-size:24px;'>\"Habis Gelap </b><b style='color:#EAC435;font-size:23px;'>Terbitlah Terang\"</b><br><br><br><br><span style='color:lightgray;font-size:8px;'>Data: Open Data Jawa Barat<br>Visualisasi: Muhammad Aswan Syahputra</span>"
  ) +
  scale_x_continuous(limits = c(0.975, 1.025), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.950, 1.050)) +
  theme_ipsum(base_family = "Fira Code", grid = FALSE) +
  theme(
    plot.background = element_rect(fill = "#363732", colour = NA),
    panel.background = element_rect(fill = "#363732", colour = NA),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.margin = margin(3, 3, 3, 3),
    plot.caption.position = "plot",
    plot.caption = element_markdown(hjust = 0.5, vjust = -5)
  ) +
  transition_manual(frame) +
  ease_aes('sine-in-out') +
  enter_appear() +
  exit_disappear() -> anim

anim_save(
  "zzz.gif",
  animation = anim,
  duration = 15,
  fps = 30,
  width = 5,
  height = 5,
  units = "in",
  device = "png",
  type = "cairo-png",
  res = 150,
  renderer = gifski_renderer(loop = TRUE)
)
