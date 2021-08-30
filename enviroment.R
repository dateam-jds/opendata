library(httr)
library(tidyverse)
library(ragg)
library(hrbrthemes)
library(ggtext)
library(scales)


#' TODO:
#' 1. Cari jumlah desa/kelurahan per kabupaten/kota
#' 2. % relatif dampak pencemaran terhadap jumlah desa/kelurahan dalam kabupaten/kota
#' 3. Overlap pencemaran desa

village <- 
  read_csv("~/Projects/jds/covscore/data-raw/jabar_area.csv")

village %>% 
  count(district_name_kemendagri) %>% 
  summarise(sum(n))

village %>% 
  count(nama_kabupaten) %>% 
  summarise(sum(n))

enviro <- 
  GET("https://data.jabarprov.go.id/api-coredata/dpmdes/od_jml_desa_mengalami_pencemaran_lingkungan__dampak?limit=5000") %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    ),
    nama_kabupaten_kota = str_replace(nama_kabupaten_kota, "Kabupaten", "Kab.")
  )

enviro_air <- 
  GET("https://data.jabarprov.go.id/api-coredata/dpmdes/od_jumlah_desa_yang_mengalami_pencemaran_udara?limit=5000") %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    ),
    nama_kabupaten_kota = str_replace(nama_kabupaten_kota, "Kabupaten", "Kab.")
  )

enviro_water <- 
  GET("https://data.jabarprov.go.id/api-coredata/dpmdes/od_jumlah_desa_yang_mengalami_pencemaran_air?limit=5000") %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    ),
    nama_kabupaten_kota = str_replace(nama_kabupaten_kota, "Kabupaten", "Kab.")
  )

enviro_soil <- 
  GET("https://data.jabarprov.go.id/api-coredata/dpmdes/od_jumlah_desa_yang_mengalami_pencemaran_tanah?limit=5000") %>% 
  content(as = "parsed", simplifyVector = TRUE) %>% 
  pluck("data") %>% 
  as_tibble() %>% 
  mutate(
    across(
      where(is.character), ~ str_to_title(.x)
    ),
    nama_kabupaten_kota = str_replace(nama_kabupaten_kota, "Kabupaten", "Kab.")
  )


enviro %>% 
  count(dampak, wt = jumlah) 

enviro_air %>% 
  count(
    experience = mengalami_pencemaran_udara, 
    wt = jumlah
  )

enviro_water %>% 
  count(
    experience = mengalami_pencemaran_air, 
    wt = jumlah
  )

enviro_soil %>% 
  count(
    experience = mengalami_pencemaran_tanah, 
    wt = jumlah
   )

enviro %>% 
  select(nama_kabupaten_kota, dampak, jumlah) %>% 
  mutate(jumlah_)

enviro %>% 
  filter(dampak == "Menimbulkan Kematian")


agg_png(width = 8, height = 5, units = "in", res = 300)

enviro %>% 
  group_by(nama_kabupaten_kota) %>% 
  mutate(pct = jumlah) %>% 
  # mutate(pct = jumlah / sum(jumlah)) %>%
  ungroup() %>% 
  mutate(
    dampak = factor(
      dampak,
      levels = c(
        "Tidak Mempengaruhi Kesehatan",
        "Menyebabkan Gangguan Kesehatan Ringan",
        "Menimbulkan Penyakit/ Infeksi",
        "Menimbulkan Kematian"
      )
    ),
    dampak = fct_rev(dampak)
  ) %>% 
  ggplot(aes(pct, nama_kabupaten_kota, fill = dampak)) +
  geom_col(
    data = ~.x %>% 
      filter(dampak %in% c("Menimbulkan Penyakit/ Infeksi", "Menimbulkan Kematian"))
  ) +
  geom_col(
    data = ~.x %>% 
      filter(!dampak %in% c("Menimbulkan Penyakit/ Infeksi", "Menimbulkan Kematian")) %>% 
      mutate(dampak = fct_rev(dampak)),
    aes(x = -1 * pct)
  ) +
  theme_minimal()

dev.off()
