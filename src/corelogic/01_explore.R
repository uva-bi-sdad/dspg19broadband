library(RPostgreSQL)
library(DBI)
library(sf)


#
# Connect to database ---------------------------------------------------------------------
#

con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                       dbname = "corelogic",
                       host = "localhost",
                       port = "5436",
                       user = "tp2sk",
                       password = "tp2sk")


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


#
# Disconnect -----------------------------------------------------------------------------
#

dbDisconnect(con)
