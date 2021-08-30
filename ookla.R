library(tidyverse)
library(patchwork)
library(janitor)
library(ggrepel)
library(usethis)
library(lubridate)
library(colorspace)
library(scales)
library(kableExtra)
library(knitr)
library(sf)

# colors for plots
purple <- "#A244DA"
light_purple <- colorspace::lighten("#A244DA", 0.5)
green <- colorspace::desaturate("#2DE5D1", 0.2)
blue_gray <- "#464a62"
mid_gray <- "#ccd0dd"
light_gray <- "#f9f9fd"

# set some global theme defaults
theme_set(theme_minimal())
theme_update(text = element_text(family = "sans", color = "#464a62"))
theme_update(plot.title = element_text(hjust = 0.5, face = "bold"))
theme_update(plot.subtitle = element_text(hjust = 0.5))

use_zip("https://gisco-services.ec.europa.eu/distribution/v2/nuts/download/ref-nuts-2021-01m.shp.zip", destdir = "data")

uk_nuts_3 <- read_sf("data/ref-nuts-2021-01m.shp/NUTS_RG_01M_2021_3857_LEVL_3.shp/NUTS_RG_01M_2021_3857_LEVL_3.shp") %>%
  filter(CNTR_CODE == "UK") %>%
  st_transform(4326) %>%
  clean_names() %>%
  mutate(urbn_desc = case_when( # add more descriptive labels for urban variable
    urbn_type == 1 ~ "Urban",
    urbn_type == 2 ~ "Intermediate",
    urbn_type == 3 ~ "Rural"
  ),
  urbn_desc = factor(urbn_desc, levels = c("Urban", "Intermediate", "Rural")))

# contextual city data
uk_cities <- read_sf("https://opendata.arcgis.com/datasets/6996f03a1b364dbab4008d99380370ed_0.geojson") %>%
  clean_names() %>%
  filter(fips_cntry == "UK", pop_rank <= 5)

ggplot(uk_nuts_3) +
  geom_sf(color = mid_gray, fill = light_gray, lwd = 0.08) +
  geom_text_repel(data = uk_cities, 
                  aes(label = city_name, geometry = geometry), 
                  family = "sans", 
                  color = blue_gray, 
                  size = 2.2, 
                  stat = "sf_coordinates",
                  min.segment.length = 2) +
  labs(title = "United Kingdom",
       subtitle = "NUTS 3 Areas") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

uk_bbox <- uk_nuts_3 %>%
  st_union() %>% # otherwise would be calculating the bounding box of each individual area
  st_bbox()

# download the data with the following code:

use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=1/2020-01-01_performance_mobile_tiles.zip", destdir = "data")
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=2/2020-04-01_performance_mobile_tiles.zip", destdir = "data")
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=3/2020-07-01_performance_mobile_tiles.zip", destdir = "data")
use_zip("https://ookla-open-data.s3.amazonaws.com/shapefiles/performance/type=mobile/year=2020/quarter=4/2020-10-01_performance_mobile_tiles.zip", destdir = "data")

# and then read in those downloaded files
mobile_tiles_q1 <- read_sf("data/2020-01-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(uk_bbox)
mobile_tiles_q2 <- read_sf("data/2020-04-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(uk_bbox)
mobile_tiles_q3 <- read_sf("data/2020-07-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(uk_bbox)
mobile_tiles_q4 <- read_sf("data/2020-10-01_performance_mobile_tiles/gps_mobile_tiles.shp") %>%
  st_crop(uk_bbox)