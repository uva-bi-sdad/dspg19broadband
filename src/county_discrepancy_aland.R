library(dplyr)
library(sp)
library(sf)
library(usmap)
library(data.table)
library(ggplot2)
fcc<- read.csv('~/dspg19broadband/data/working/fcc_processed_county.csv')
acs <- read.csv("data/working/summary_acs_county.csv")

fcc=fcc[, c("fips", "availability_cons")]
acs=acs[, c("FIPS", "B28002_007_per")]

fcc$fips <- ifelse(nchar(fcc$fips)==4,paste0("0",fcc$fips),fcc$fips)
acs$FIPS <- ifelse(nchar(acs$FIPS)==4,paste0("0",acs$FIPS),acs$FIPS)

discr<- merge(fcc, acs, by.x = "fips", by.y="FIPS", all=TRUE)

matched <- intersect(fcc$fips, acs$FIPS)
all <-  union(fcc$fips, acs$FIPS)
non.matched <- all[!all %in% matched]

#02270 is an old FIPS code
#46113 is an old FIPS code
#51515 city is no longer a city, is now part of a county

discr$discr = round((discr$availability_cons*100) - discr$B28002_007_per,1)


con <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(),
                      dbname = "gis",
                      host = "postgis_1",
                      port = "5432",
                      user = "sm9dv",
                      password = "sm9dv")

geo = st_read(con, c("census_cb", "cb_2016_us_county_500k"))
geo$fips = paste(geo$STATEFP, geo$COUNTYFP, sep="")

DBI::dbDisconnect(con)

geo$fips <- paste(geo$STATEFP, geo$COUNTYFP, sep="")
geo <- geo %>% select("fips", "ALAND")

discr = merge(discr, geo, by="fips", all.y=FALSE)


ggplot(discr, aes(log(discr), log(ALAND))) + 
  geom_point()

