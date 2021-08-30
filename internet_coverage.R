library(googlesheets4)
library(pins)
library(ooklaOpenDataR)
library(sf)


internet2021q1 <- 
  get_performance_tiles(
    service = "mobile",
    year = 2021,
    quarter = 1
  )


jabar_bbox <- 
  jabar_province %>% 
  st_union() %>% 
  st_bbox()


jabar_internet <- 
  internet2021q1 %>% 
  st_as_sf(wkt = "tile", crs = 4326) %>% 
  filter_by_quadkey(bbox = jabar_bbox)

jabar_internet %>% 
  write_rds("data-raw/jabar_internet.rds")

write_rds(internet2021q1, "data-raw/internet2021q1.rds", compress = "bz2")

vroom::vroom_write(internet2021q1, "data-raw/internet2021q1.csv.gz")
?object.size


board_register_github(
  repo = "dateam-jds/board",
  branch = "main", 
  token = Sys.getenv("GITHUB_PAT")
)

jabar_province <- 
  pin_get(
    "jabar-province", 
    board = "github"
  ) %>% 
  st_as_sf()
  

jabar_code <- 
  pin_get(
    "jabar-code", 
    board = "github"
  )

coverage_raw <- 
  read_sheet(
    ss = "1yesCfzTSpw0ridMayk26enyzfLZ1Bqr0pbQM_5Lnr0o",
    sheet = "coverage_all_kominfo"
  )

hmmm <-
coverage_raw %>% 
  janitor::clean_names() %>% 
  select(kode_desa, kab_kota, kecamatan, kel_desa, starts_with("avg")) %>% 
  left_join(
    jabar_code,
    by = c("kode_desa" = "village_code_bps")
  ) %>% 
  select(-c(district_name_bps:village_code_kemendagri)) %>% 
  left_join(
    jabar_code,
    by = c("kab_kota" = "district_name_bps","kecamatan" = "subdistrict_name_bps", "kel_desa" = "village_name_bps")
  ) %>% 
  mutate(
    village_code_bps = coalesce(kode_desa, village_code_bps)
  ) %>% 
  select(-kode_desa) %>% 
  rename(
    district_name_bps = kab_kota,
    subdistrict_name_bps = kecamatan,
    village_name_bps = kel_desa
  )

jabar_code %>% 
  colnames() -> kolom
hasil <- hmmm %>% 
  select(all_of(kolom))
hasil %>% 
  anyNA()

waldo::compare(
  jabar_code,
  hasil
)

jabar_code %>% 
  filter(str_detect(village_name_bps, "SAMBENG")) %>% 
  glimpse()

zzz %>% 
  select(village_code_bps, kab_kota, kecamatan, kel_desa) %>%
  inner_join(
    jabar_code %>% 
      select(village_code_bps, district_name_bps,subdistrict_name_bps, village_name_bps)
  ) %>% 
  view()
