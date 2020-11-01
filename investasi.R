library(jabr)
library(hrbrthemes)

theme_set(theme_ft_rc())

x <- jabr_list_dataset(update = TRUE)

investasi_asing <- jabr_fetch("0ddebd9f", keep_title = FALSE)
investasi_domestik <- jabr_fetch("6ac6e696", keep_title = FALSE)

investasi_asing %>% 
  select(-satuan,
         asing = jumlah_investasi) %>% 
  left_join(
    investasi_domestik %>% 
      select(-satuan,
             domestik = jumlah_investasi)  
  ) %>% 
  filter(!is.na(asing),
         !is.na(domestik)) %>% 
  mutate_at(vars(asing, domestik), ~ .x / 1e9) %>%
  ggplot(aes(domestik, asing, colour = sektor_usaha, label = sektor_usaha)) +
  geom_abline(intercept = 0, slope = 1, colour = ft_cols$slate, linetype = "dashed") +
  geom_point(size = 2.5, show.legend = FALSE) +
  ggrepel::geom_label_repel(show.legend = FALSE) +
  scale_x_log10(expand = c(0,0), labels = scales::dollar_format(prefix = "Rp.", suffix = "M", big.mark = ".", decimal.mark = ",")) +
  scale_y_log10(expand = c(0,0), labels = scales::dollar_format(prefix = "Rp.", suffix = "M", big.mark = ".", decimal.mark = ",")) +
  labs(
    x = "Jumlah Investasi Dalam Negeri",
    y = "Jumlah Investasi Luar Negeri",
    title = "Sektor Usaha di Jawa Barat Didominasi Oleh Investor Luar Negeri",
    subtitle = "Industri Makanan dan Konstruksi Menjadi Sektor Andalan Investor Dalam Negeri",
    caption = "Data Analyst @ Jabar Digital Service"
  ) +
  coord_cartesian(clip = "off")

ggsave("rasio_investasi.png", width = 13, height = 8)
