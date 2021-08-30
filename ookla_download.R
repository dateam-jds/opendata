# Package installation, skip it if you already have the packages i --------

options(Ncpus = 2)
install.packages("remotes")
install.packages(c("vroom", "arrow"))
remotes::install_github("teamookla/ooklaOpenDataR")

# Activate the packages ---------------------------------------------------

library(ooklaOpenDataR)
library(vroom)

# Download data -----------------------------------------------------------

internet_speed_mobile <- 
  get_performance_tiles(
    service = "mobile",
    year = 2021,
    quarter = 1
  )

internet_speed_broadband <- 
  get_performance_tiles(
    service = "fixed",
    year = 2021,
    quarter = 1
  )

# Export data -------------------------------------------------------------

vroom_write(
  internet_speed_mobile, 
  "internet_speed_mobile.csv.gz"
)

vroom_write(
  internet_speed_broadband, 
  "internet_speed_broadband.csv.gz"
)