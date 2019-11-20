library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)
library(raster)
library(dplyr)
library(sf)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# ACS: Proportion of households with broadband -------------------------------------------------------------------------------------------------------------
#

# B28002 - PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD, Universe: Households
# 001: Number of households
# 007: Broadband such as cable, fiber optic or DSL

# State FIPS
state_fips <- unique(fips_codes$state)[1:51]

# ACS variables
acsvars <- c("B28002_001", "B28002_007")

# Get tract-level variables from ACS 2013-2017 (5-year)
acs <- get_acs(geography = "tract", state = state_fips[1], variables = acsvars, year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                   keep_geo_vars = TRUE)
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "tract", state = state_fips[i], variables = acsvars, year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = TRUE,
                 keep_geo_vars = TRUE)
  acs <- rbind(acs, tmp)
}

# Calculate variables
acs_ <- acs %>% transmute(
  GEOID = GEOID,
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  AFFGEOID = AFFGEOID,
  ALAND = ALAND,
  AWATER = AWATER,
  LSAD = LSAD,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  bband = B28002_007E / B28002_001E,
  hholds = B28002_001E,
  geometry = geometry
)


#
# ACS: FCC: Number of subscriptions per 1,000 households -------------------------------------------------------------------------------------------------------------
#

# pcat_all: Residential Fixed High-Speed Connections over 200 kbps in at least one direction per per 1,000 Households
# pcat_10x1: Residential Fixed High-Speed Connections at least 10 Mbps downstream and at least 1 Mbps upstream per 1,000 Households 

# Code  Connections per 1,000 HHs
# 0     Zero
# 1     Zero < x <= 200
# 2     200 < x <=400
# 3     400 < x <=600
# 4     600 < x <=800
# 5     800 < x

# Read in
fcc <- read_csv("./data/original/FCC/tract_map_dec_2015/tract_map_dec_2015.csv", col_names = TRUE, cols(tractcode = "c"))

# Recode
fcc <- fcc %>% mutate(conn10min = case_when(pcat_10x1 == 0 ~ 0,
                                            pcat_10x1 == 1 ~ 0,
                                            pcat_10x1 == 2 ~ 200/1000,
                                            pcat_10x1 == 3 ~ 400/1000,
                                            pcat_10x1 == 4 ~ 600/1000,
                                            pcat_10x1 == 5 ~ 800/1000),
                      conn10max = case_when(pcat_10x1 == 0 ~ 0,
                                            pcat_10x1 == 1 ~ 200/1000,
                                            pcat_10x1 == 2 ~ 400/1000,
                                            pcat_10x1 == 3 ~ 600/1000,
                                            pcat_10x1 == 4 ~ 800/1000,
                                            pcat_10x1 == 5 ~ 1))