library(readr)
library(dplyr)
library(sf)
library(randomForest)
library(caret)
library(naniar)


#
# Read in data & prepare -------------------------------------------------------------------------------
#

# FCC 2017 county
fcc_orig <- read_csv("./data/working/fcc_processed_county_25_2017.csv")
fcc <- fcc_orig %>% select(-X1, -Description)
fcc$availability_adv <- fcc$availability_adv*100
fcc$county <- ifelse(nchar(fcc$county) == 4, paste0("0", fcc$county), fcc$county)
colnames(fcc)[colnames(fcc) == "county"] <- "GEOID"
remove(fcc_orig)

# ACS 2013-17 county (see discr_getacs_county for coding)
acs_orig <- st_read("./data/working/acs_2013-17/calc/acs_2013-17_calc_count.shp")
acs <- acs_orig %>% select(-AFFGEOI, -ALAND, -AWATER)
acs$GEOID <- as.character(acs$GEOID)
acs$GEOID <- ifelse(nchar(acs$GEOID) == 4, paste0("0", acs$GEOID), acs$GEOID)
remove(acs_orig)

# Microsoft 2018 county
ms_orig <- read_csv("./data/original/microsoft/microsoft.csv")
ms <- ms_orig
ms$`COUNTY ID` <- as.character(ms$`COUNTY ID`)
ms$`COUNTY ID` <- ifelse(nchar(ms$`COUNTY ID`) == 4, paste0("0", ms$`COUNTY ID`), ms$`COUNTY ID`)
colnames(ms)[colnames(ms) == "COUNTY ID"] <- "GEOID"
colnames(ms)[colnames(ms) == "COUNTY NAME"] <- "countyname"
colnames(ms)[colnames(ms) == "BROADBAND USAGE"] <- "usage"
any(ms$usage == "-")
sum(ms$usage == "-")
ms$usage <- ifelse(ms$usage == "-", NA, ms$usage)
ms$usage <- as.numeric(ms$usage)
ms$usage <- ms$usage*100
ms <- ms %>% select(-"BROADBAND AVAILABILITY PER FCC")
remove(ms_orig)


#
# Merge ----------------------------------------------------------------------------------------------
#

# Notes on merge: see county_discrepancy_aland.R
# FCC has more rows because it includes American Samoa, Guam, Northern Mariana Islands, Puerto Rico, Virgin Islands
# 02270 is an old FIPS code
# 46113 is an old FIPS code
# 51515 city is no longer a city, is now part of a county

sum(fcc$GEOID %in% acs$GEOID)
discr <- left_join(fcc, acs, by = "GEOID")

sum(ms$GEOID %in% discr$GEOID)
discr <- left_join(discr, ms, by = "GEOID")


#
# Create measures, address missingness --------------------------------------------------------------------------------------
#

# RUCC dichotomization
discr$ru_binary <- ifelse(discr$RUCC_2013 > 3, "nonmetro", "metro")

# FCC-MS discrepancy
discr$dis_rel_fcc_ms <- discr$availability_adv - discr$usage
table(discr$dis_rel_fcc_ms == 0)

# Missingness
table(is.na(discr$dis_rel_fcc_ms))
discr <- discr %>% filter(!is.na(dis_rel_fcc_ms))

# Plot
hist(discr$dis_rel_fcc_ms)


#
# Random forest regression: FCC-MS ---------------------------------------------------------------------------
#

# Select data
data <- discr %>% select(dis_rel_fcc_ms, RUCC_2013, hs_r_ls, poverty, ag_65_l, hispanc, black, density, family, foreign)

# Set seed
set.seed(2410)

# Create features and target
x <- data %>% select(RUCC_2013, hs_r_ls, poverty, ag_65_l, hispanc, black, density, family, foreign)
y <- data$dis_rel_fcc_ms

# Split
index <- createDataPartition(data$dis_rel_fcc_ms, p = 0.75, list = FALSE)
x_train <- x[ index, ]
x_test <- x[-index, ]
y_train <- y[index]
y_test <- y[-index]

# Scale
x_train <- scale(x_train)
x_test <- scale(x_test)
y_train <- scale(y_train)
y_test <- scale(y_test)

# Random forest regression
rfr <- randomForest(x = x_train, y = y_train, ntree = 5)