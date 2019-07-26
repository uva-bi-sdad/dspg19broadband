library(dplyr)
library(sp)
library(sf)
library(usmap)
library(data.table)
library(ggplot2)
library(RColorBrewer)
fcc_original<- read.csv('~/dspg19broadband/data/working/fcc_processed_county_updated.csv')
acs <- read.csv("data/working/summary_acs_county.csv")

fcc=fcc_original[, c("county", "availability_adv", "Population_2010", "RUCC_2013")]
acs=acs[, c("FIPS", "B28002_007_per")]

fcc$county <- ifelse(nchar(fcc$county)==4,paste0("0",fcc$county),fcc$county)
acs$FIPS <- ifelse(nchar(acs$FIPS)==4,paste0("0",acs$FIPS),acs$FIPS)

discr<- merge(fcc, acs, by.x = "county", by.y="FIPS", all.y=TRUE)

matched <- intersect(fcc$county, acs$FIPS)
all <-  union(fcc$county, acs$FIPS)
non.matched <- all[!all %in% matched]

#02270 is an old FIPS code
#46113 is an old FIPS code
#51515 city is no longer a city, is now part of a county

discr$discr_metr = round((discr$availability_adv*100) - discr$B28002_007_per,1)


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

colnames(discr)[colnames(discr)=="county"] <- "fips"

discr = merge(discr, geo, by="fips", all.y=FALSE)

discr$ru_binary = ifelse(discr$RUCC_2013>3, "nonmetro", "metro")

#discr to ALAND
ggplot(discr, aes(log(ALAND), discr_metr, color=RUCC_2013)) + 
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="Land Area vs Discrepancy", x ="Land Area (log)", y = "Discrepancy")


ggplot(discr, aes(log(ALAND), discr_metr, color=ru_binary, alpha=.1)) + 
  geom_point()+
  labs(title="Land Area vs Discrepancy", x ="Land Area (log)", y = "Discrepancy")

#unemployed
empl<- read.csv("data/working/acs_transform/acs_empl.csv")
unempl<- empl[, c("GEOID", "B23025_005_per")]
unempl$GEOID <- as.character(as.integer(unempl$GEOID))
unempl$GEOID <- ifelse(nchar(unempl$GEOID)==4,paste0("0",unempl$GEOID),unempl$GEOID)
discr_only<- discr[,c("fips", "discr_metr", "RUCC_2013", "ru_binary")]
unempl_discr <- merge(unempl, discr_only, by.x="GEOID", by.y="fips", all.x = TRUE)

ggplot(unempl_discr, aes(B23025_005_per, discr_metr, color=RUCC_2013)) +
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="Unemployment vs Discrepancy", x ="Unemployment (%)", y = "Discrepancy")

ggplot(unempl_discr, aes(B23025_005_per, discr_metr, color=ru_binary, alpha=.1)) + 
  geom_point()+
  labs(title="Unemployment vs Discrepancy", x ="Unemployment (%)", y = "Discrepancy")


#hispanic/latino
ethn<- read.csv("data/working/acs_transform/acs_ethn.csv")
ethn<- ethn[, c("GEOID","B03003_002_per", "B03003_003_per")]
ethn$GEOID <- as.character(as.integer(ethn$GEOID))
ethn$GEOID <- ifelse(nchar(ethn$GEOID)==4,paste0("0",ethn$GEOID),ethn$GEOID)

ethn_discr <- merge(ethn, discr_only, by.x="GEOID", by.y="fips", all.x = TRUE)

ggplot(ethn_discr, aes(B03003_003_per, discr_metr, color=RUCC_2013)) +
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="Percent Hispanic or Latino Population vs Discrepancy", x ="Hispanic or Latino Population (%)", y = "Discrepancy")

ggplot(ethn_discr, aes(B03003_003_per, discr_metr, color=ru_binary, alpha=.1)) + 
  geom_point()+
  labs(title="Percent Hispanic or Latino Population vs Discrepancy", x ="Hispanic or Latino Population (%)", y = "Discrepancy")

#race
race<- read.csv("data/working/acs_transform/acs_race.csv")
race$minority_per <- race$B02001_003_per + race$B02001_004_per + race$B02001_005_per + race$B02001_006_per + race$B02001_007_per + race$B02001_008_per
race$GEOID <- as.character(as.integer(race$GEOID))
race$GEOID <- ifelse(nchar(race$GEOID)==4,paste0("0",race$GEOID),race$GEOID)
race_discr <- merge(race, discr_only, by.x="GEOID", by.y = "fips", all.x = TRUE)

ggplot(race_discr, aes(minority_per, discr_metr, color=RUCC_2013)) +
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="Percent Minority Population vs Discrepancy", x ="Minority Population (%)", y = "Discrepancy")

ggplot(race_discr, aes(minority_per, discr_metr, color=ru_binary)) +
  geom_point()+
  labs(title="Percent Minority Population vs Discrepancy", x ="Minority Population (%)", y = "Discrepancy")

#poverty
pov<- read.csv("data/working/acs_transform/acs_pov.csv")
pov$under_1 <- pov$B17026_002_per + pov$B17026_003_per + pov$B17026_004_per
pov$GEOID <- as.character(as.integer(pov$GEOID))
pov$GEOID <- ifelse(nchar(pov$GEOID)==4,paste0("0",pov$GEOID),pov$GEOID)
pov_discr <- merge(pov, discr_only, by.x="GEOID", by.y = "fips", all.x = TRUE)

ggplot(pov_discr, aes(under_1, discr_metr, color=RUCC_2013)) +
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="Percent of Population with Income to Population Ratio Under 1 vs Discrepancy", x ="Income to Population Ratio Under 1 (%)", y = "Discrepancy")

ggplot(pov_discr, aes(under_1, discr_metr, color=ru_binary)) +
  geom_point()+
  labs(title="Percent of Population with Income to Population Ratio Under 1 vs Discrepancy", x ="Income to Population Ratio Under 1 (%)", y = "Discrepancy")

#education
edu<- read.csv("data/working/acs_transform/acs_edu.csv")
edu$no_hs <- edu$B15003_002_per + edu$B15003_003_per + edu$B15003_004_per + edu$B15003_005_per + edu$B15003_006_per + edu$B15003_007_per + edu$B15003_008_per + edu$B15003_009_per + edu$B15003_010_per + edu$B15003_011_per + edu$B15003_012_per + edu$B15003_013_per + edu$B15003_014_per + edu$B15003_015_per + edu$B15003_016_per
edu$GEOID <- as.character(as.integer(edu$GEOID))
edu$GEOID <- ifelse(nchar(edu$GEOID)==4,paste0("0",edu$GEOID),edu$GEOID)
edu_discr <- merge(edu, discr_only, by.x="GEOID", by.y = "fips", all.x = TRUE)

ggplot(edu_discr, aes(no_hs, discr_metr, color=RUCC_2013)) +
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="Percent of Population Without High School Diploma vs Discrepancy", x ="Population Without High School Diploma (%)", y = "Discrepancy")
ggplot(edu_discr, aes(no_hs, discr_metr, color=ru_binary)) +
  geom_point()+
  labs(title="Percent of Population Without High School Diploma vs Discrepancy", x ="Population Without High School Diploma (%)", y = "Discrepancy")

#FCC vs ACS
ggplot(discr, aes(availability_adv, B28002_007_per, color=RUCC_2013)) +
  geom_point()+
  scale_color_gradient(low="purple", high="yellow")+
  labs(title="FCC Availability Advertised vs ACS Self-Reported Broadband", x ="FCC", y = "ACS")


#Dist. for rural and urban counties
ggplot(discr, aes(x=discr_metr, fill=ru_binary, color=ru_binary)) +
  geom_histogram(position="identity", alpha= 0.2)+
  labs(title="Discrepancy Distribution for Metro and Non-Metro Counties", x ="Discrepancy", y = "Count")

va <- filter(discr %like% '51%')
ggplot(va, aes(x=fips, y=discr_metr, fill=ru_binary, color=ru_binary))+
  geom_bar(stat="identity")
