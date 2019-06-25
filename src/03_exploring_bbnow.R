library(dplyr)
library(stringr)
library(readr)
library(xml2)
library(purrr)
library(data.table)

bbnow <- read.csv("./data/working/bbnow_cities.csv")
uscity <- read.csv("./data/working/uscities.csv")
uscities <- uscity[c(1,3:10)]
#head(bbnow)
#head(uscities)
#city_n <- uscities$city
uscities$city <- tolower(city_n)
uscities$city <- str_replace_all(uscities$city, "\\[upper-alpha 3\\]", "")
uscities$city <- str_replace_all(uscities$city, "\\[upper-alpha 4\\]", "")
uscities$city <- str_replace_all(uscities$city, " ", "-")
head(uscities)

#uscities %>% data.table() %>% dt_mutate(city_names = tolower(city)) %>% dt_mutate(city_names =str_replace_all(city_names, "\\[upper-alpha 3\\]", ""))

head(city_names)
head(uscities)
merged_by_city <- merge(bbnow,uscities,by.x = "city",by.y = "city")
head(bbnow)
