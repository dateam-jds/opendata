library(httr)
library(tidyverse)
library(hrbrthemes)
library(gggibbous)
library(ggfx)
library(ggtext)
library(scales)
library(gganimate)

igp_prov <- 
  GET("https://data.jabarprov.go.id/api-coredata/dp3akb/od_akumulasi_indeks_pemberdayaan_gender?limit=5000") %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    )
  )


igp_kabkot <- 
  GET("https://data.jabarprov.go.id/api-coredata/dp3akb/od_indeks_pemberdayaan_gender?limit=5000") %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    ),
    indeks_pemberdayaan_gender = parse_number(indeks_pemberdayaan_gender, locale = locale(decimal_mark = ".", grouping_mark = ",")),
    nama_kabupaten_kota = str_replace(nama_kabupaten_kota, "Kabupaten", "Kab.")
  )

bind_rows(
  igp_prov %>% 
    filter(tahun %in% c(2014, 2018)) %>% 
    transmute(
      area = provinsi,
      tahun = as.character(tahun),
      indeks_pemberdayaan_gender
    ),
  igp_kabkot %>% 
    filter(tahun %in% c(2014, 2018)) %>% 
    transmute(
      area = nama_kabupaten_kota,
      tahun = as.character(tahun),
      indeks_pemberdayaan_gender
    )
) %>% 
  ggplot(aes(tahun, indeks_pemberdayaan_gender, group = area, colour = area == "Jawa Barat")) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  labs(x = NULL,
       y = NULL) +
  scale_x_discrete(expand = c(0.005, 0.005)) +
  scale_y_continuous(sec.axis = dup_axis()) +
  scale_colour_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "gray90")
  ) +
  theme_ipsum_tw(grid = "X", ticks = TRUE)

igp_kabkot %>% 
  filter(tahun %in% c(2014, 2018)) %>% 
  ggplot(aes(as.character(tahun), indeks_pemberdayaan_gender, group = nama_kabupaten_kota)) +
  geom_line(colour = "lightgray") +
  geom_line(
    data = igp_prov %>% 
      filter(tahun %in% c(2014, 2018)),
    aes(
      as.character(tahun), 
      indeks_pemberdayaan_gender, group = 1),
    inherit.aes = FALSE,
    colour = "seagreen"
  ) +
  geom_point(size = 1.5) +
  geom_point(
    data = igp_prov %>% 
      filter(tahun %in% c(2014, 2018)),
    aes(
      as.character(tahun), 
      indeks_pemberdayaan_gender, group = 1),
    inherit.aes = FALSE,
    colour = "seagreen"
  ) +
  labs(x = NULL,
       y = NULL) +
  scale_x_discrete(expand = c(0.005, 0.005)) +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme_ipsum_tw(grid = "X", ticks = TRUE)
