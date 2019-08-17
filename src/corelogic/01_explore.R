library(RPostgreSQL)
library(DBI)
library(sf)
library(here)
library(data.table)
library(dplyr)
library(stargazer)


#
# Connect to database ---------------------------------------------------------------------
#

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                       dbname = "corelogic",
                       host = "localhost",
                       port = "5436",
                       user = Sys.getenv("db_userid"),
                       password = Sys.getenv("db_pwd"))


#
# Get column names --------------------------------------------------------------------------------
#

# Raw data for whole country 2014
deed14us <- dbGetQuery(con, "SELECT * FROM usda_deed_2014 LIMIT 1")
colnames(deed14us)

# Data for Virginia with tract info and geometry (does not include 43 tracts that had no properties with lat/lon)
deed14va <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_tract LIMIT 1")
colnames(deed14va)

# Median sales per tract in Virginia (includes all 1900 tracts with those with no properties to calculate set to NA)
deed14vasale <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_median_sales_tract LIMIT 1")
colnames(deed14vasale)

clva <- st_read(con, query = "SELECT * FROM usda_deed_2014_51_median_sales_tract")


#
# Get tract FCC data --------------------------------------------------------------------------------
#

fcc_file <- here("data", "working", "fcc_processed_tract_25.csv")
fcc <- fread(fcc_file, colClasses = c(state = "character", county = "character", tract = "character")) 

fccva <- fcc %>% filter(State == "VA")


#
# Link tract level FCC VA and CL VA ------------------------------------------------------------------------
#

head(fccva)
head(clva)

sum(fcc$tract %in% clva$GEOID)
sum(clva$GEOID %in% fcc$tract)

fcc_cl <- merge(fccva, clva, by.x = "tract", by.y = "GEOID")

fcc_cl <- st_as_sf(fcc_cl)
plot(st_geometry(fcc_cl))


#
# Regress -----------------------------------------------------------------------------
#

reg_va1 <- lm(median_sale_amount ~ RUCC_2013,
                     data = fcc_cl)
reg_va2 <- lm(median_sale_amount ~ availability_adv,
                     data = fcc_cl)
reg_va3 <- lm(median_sale_amount ~ RUCC_2013 + availability_adv,
              data = fcc_cl)
stargazer(reg_va1, reg_va2, reg_va3, no.space = TRUE, digits = 2, type = "text")


#
# Disconnect -----------------------------------------------------------------------------
#

dbDisconnect(con)
