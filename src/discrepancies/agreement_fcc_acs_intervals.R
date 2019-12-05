library(IRanges)

# Prepare data (need two-column dataframes with nothing else)
test_acs <- data %>% select(bbandmin, bbandmax) %>% 
                     st_set_geometry(NULL)
                    
test_fcc <- data %>% select(conn10min, conn10max) %>% 
                     st_set_geometry(NULL)

# Can only work with whole numbers
startacs <- round(test_acs$bbandmin*100)
endacs <- round(test_acs$bbandmax*100)
test_acs <- IRanges(start = startacs, end = endacs)

startfcc<- round(test_fcc$conn10min*100)
endfcc <- round(test_fcc$conn10max*100)
test_fcc <- IRanges(start = startfcc, end = endfcc)

# Test
countOverlaps(test_acs[1], test_fcc[1])
countOverlaps(test_acs[2], test_fcc[2])
countOverlaps(test_acs[3], test_fcc[3])

# Need to do it pairwise (package functions to 1:every)

# Any overlap at all
overlap_any <- as.data.frame(countOverlaps(test_acs[1], test_fcc[1]))
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc[i])
  overlap_any <- rbind(overlap_any, tmp)
}
names(overlap_any)[1] <- "overlap_any"

# ACS is completely within FCC
overlap_acswithinfcc <- as.data.frame(countOverlaps(test_acs[1], test_fcc[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc[i], type = "within")
  overlap_acswithinfcc <- rbind(overlap_acswithinfcc, tmp)
}
names(overlap_acswithinfcc)[1] <- "acs_within_fcc"

# FCC is completely within ACS
overlap_fccwithinacs <- as.data.frame(countOverlaps(test_fcc[1], test_acs[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_fcc[i], test_acs[i], type = "within")
  overlap_fccwithinacs <- rbind(overlap_fccwithinacs, tmp)
}
names(overlap_fccwithinacs)[1] <- "fcc_within_acs"

# Put back 
overlap_geo <- as.data.frame(data$GEOID)
names(overlap_geo)[1] <- "GEOID"
overlap_geo$GEOID <- as.character(data$GEOID)

overlap_df <- cbind(overlap_geo, overlap_any, overlap_acswithinfcc, overlap_fccwithinacs)
