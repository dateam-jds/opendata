library(jabr)
library(tidyverse)

dir.create("outfile")

opendata <- jabr_list_dataset(update = TRUE)

opendata <- 
  opendata %>% 
  mutate(provider = replace_na(provider, "Lainnya"))

opendata

download_to_csv <- function(opd) {
  opendata %>% 
    filter(provider == opd) %>% 
    pull(url) %>% 
    walk(~{
      dir.create(opd, showWarnings = FALSE)
      download.file(.x, file.path(opd, basename(.x)))
    })
}

opendata %>% 
  distinct(provider) %>% 
  pull() %>% 
  walk(download_to_csv)