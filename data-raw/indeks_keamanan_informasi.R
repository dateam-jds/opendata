## code to prepare `indeks_keamanan_informasi` dataset goes here
library(jabr)

jabr_list_dataset(update = TRUE)

indeks_keamanan_informasi <- jabr_fetch(id = "ef3af93b")

usethis::use_data(indeks_keamanan_informasi)
