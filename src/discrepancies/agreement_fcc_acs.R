library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)
library(raster)
library(dplyr)
library(sf)
library(readr)
library(readxl)
library(sf)
library(ggplot2)
library(ggthemes)
library(scales)

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
acs <- acs %>% transmute(
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


#
# Join FCC and ACS -------------------------------------------------------------------------------------------------------------
#

colnames(fcc)[colnames(fcc) == "tractcode"] <- "GEOID"
data <- left_join(acs, fcc, by = "GEOID")

head(data)


#
# Add RUCA codes -------------------------------------------------------------------------------------------------------------
#

# Documentation: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/documentation/
# 1	Metropolitan area core: primary flow within an urbanized area (UA)
# 2	Metropolitan area high commuting: primary flow 30% or more to a UA
# 3	Metropolitan area low commuting: primary flow 10% to 30% to a UA
# 4	Micropolitan area core: primary flow within an urban cluster of 10,000 to 49,999 (large UC)
# 5	Micropolitan high commuting: primary flow 30% or more to a large UC
# 6	Micropolitan low commuting: primary flow 10% to 30% to a large UC
# 7	Small town core: primary flow within an urban cluster of 2,500 to 9,999 (small UC)
# 8	Small town high commuting: primary flow 30% or more to a small UC
# 9	Small town low commuting: primary flow 10% to 30% to a small UC
# 10	Rural areas: primary flow to a tract outside a UA or UC
# 99	Not coded: Census tract has zero population and no rural-urban identifier information

# Read in, skip row #1 because it is a note
ruca <- read_excel("./data/original/ruca2010revised.xlsx", col_names = TRUE, progress = readxl_progress(), skip = 1)

names(ruca)[1] <- "StateCounty"
names(ruca)[2] <- "State"
names(ruca)[3] <- "County"
names(ruca)[4] <- "Tract"
names(ruca)[5] <- "primRUCA"
names(ruca)[6] <- "secRUCA"
names(ruca)[7] <- "TractPop10"
names(ruca)[8] <- "LandSqmile10"
names(ruca)[9] <- "PopDens10"

# Join
data <- left_join(data, ruca, by = c("GEOID" = "Tract"))


#
# Create indicators -------------------------------------------------------------------------------------------------------------
#

data <- data %>% mutate(iswithin0 = ifelse((bband >= conn10min & bband <= conn10max), 1, 0),
                        iswithin5 = ifelse((bband >= (conn10min + (5*conn10min)/100) & bband <= (conn10max + (5*conn10max)/100)) & iswithin0 == 0, 1, 0), 
                        iswithin15 = ifelse((bband >= (conn10min + (15*conn10min)/100) & bband <= (conn10max + (15*conn10max)/100)) & iswithin0 == 0 & iswithin5 == 0, 1, 0),
                        iswithin20 = ifelse((bband >= (conn10min + (20*conn10min)/100) & bband <= (conn10max + (20*conn10max)/100)) & iswithin0 == 0 & iswithin5 == 0 & iswithin15 == 0, 1, 0),
                        cats = case_when(iswithin0 == 1 & iswithin5 == 0 & iswithin15 == 0 & iswithin20 == 0 ~ "ACS within 0% FCC range",
                                        iswithin0 == 0 & iswithin5 == 1 & iswithin15 == 0 & iswithin20 == 0 ~ "ACS within 5% FCC range",
                                        iswithin0 == 0 & iswithin5 == 0 & iswithin15 == 1 & iswithin20 == 0 ~ "ACS within 15% FCC range",
                                        iswithin0 == 0 & iswithin5 == 0 & iswithin15 == 0 & iswithin20 == 1 ~ "ACS within 20% FCC range",
                                        iswithin0 == 0 & iswithin5 == 0 & iswithin15 == 0 & iswithin20 == 0 ~ "ACS outside FCC range"))
data$cats <- factor(data$cats, levels = c("ACS within 0% FCC range", "ACS within 5% FCC range", "ACS within 15% FCC range", "ACS within 20% FCC range", "ACS outside FCC range"))

head(data)

table(data$cats, useNA = "always")
table(data$cats, data$iswithin0)
table(data$cats, data$iswithin5)
table(data$cats, data$iswithin15)
table(data$cats, data$iswithin20)


#
# Plot -------------------------------------------------------------------------------------------------------------
#

# Get viridis colors
show_col(viridis_pal()(20))

# Contiguous states only for now
# 2 = Alaska, 15 = Hawaii, American Samoa = 60, Guam = 66, Mariana Islands = 69, Puerto Rico 72, Virgin Islands = 78
# https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696

contig <- data %>% filter(STATEFP != "02" & STATEFP != "15" & STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78")

# Plot contiguous states
ggplot(data = contig) +
  geom_sf(aes(fill = cats), size = 0.001) +
  labs(title = "ACS and FCC broadband subscription estimate congruence by tract", 
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Match range", values = c("#FDE725", "#56C667", "#238A8D", "#3F4788", "#f0f0f0"))
