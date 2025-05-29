# This code contains the basic setup tasks for the Cancer Treatment project

# Load libraries ---------------------------------------------------------------
# Check how to use renv for packages
# Temporarily reading in packages 

# The pacman package detects if a package is available and automatically downloads it if not
# install.packages("pacman")
library(pacman)

p_load(
  dplyr,   
  readxl,
  data.table,
  lubridate,
  openxlsx,
  readr,
  NDRSAfunctions,
  here,
  DBI,
  
  update = F
)

# Set parameters ---------------------------------------------------------------
#CAS_username <- Sys.getenv("analysisnataliapetersen")
CAS_username <- ("analysisnataliapetersen")
QA_analyst <- ("analysischarlotteeversfield")

snapshot <- "cas2504l"

end_year <- 2022

IMD_19_years <- "('2014', '2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022')"

IMD_table <-"imd.LSOA21_IMD_15_19_OHID"
  
charlson_table <- "AV2022.CHARLSON_2006TO2022"

lsoa_year <- "21"

# This populates: icb_2024_name, icb_2024_code, canalliance_2024_name, canalliance_2024_code
geography_year <- "24"

SOP_number <- "4p10"
