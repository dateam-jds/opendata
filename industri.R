library(httr)
library(tidyverse)

resp <- 
  GET("https://data.jabarprov.go.id/api-coredata/disindag/od_jumlah_investasi_pada_unit_industri_kecil_menengah_dan_besar?limit=5000")

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
  count(year = tahun, wt = jumlah) %>% 
  # filter(year %in% 2018:2019) %>% 
  # mutate(n = n * 1e6) %>% 
  mutate(n = n / 1e6) %>% 
  rename(value_in_trillion = n)
  
zzz <- 
  read_csv("~/Downloads/disindag-od_jumlah_investasi_pada_unit_industri_kecil_menengah_dan_besar_data.csv")

zzz %>% 
  count(year = tahun, wt = jumlah)

zzz2 <- 
  read_csv("~/Downloads/disindag-od_jumlah_investasi_pada_unit_industri_kecil_menengah_dan_besar_data.xlsx")


all_kabkot_2015 = 290451666
kota_bekasi_2015 = 8489368055
all_kabkot_2015 > kota_bekasi_2015
