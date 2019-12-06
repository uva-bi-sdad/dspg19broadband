library(IRanges)

#
# Prepare data -------------------------------------------------------------------------------------
#

# Need two-column dataframes with nothing else
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


#
# Get intervals -------------------------------------------------------------------------------------
#

# Any overlap at all
overlap_any <- as.data.frame(countOverlaps(test_acs[1], test_fcc[1]))
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc[i])
  overlap_any <- rbind(overlap_any, tmp)
}
names(overlap_any)[1] <- "overlap_any"
overlap_any$overlap_any <- as.factor(overlap_any$overlap_any)

# ACS is completely within FCC
overlap_acswithinfcc <- as.data.frame(countOverlaps(test_acs[1], test_fcc[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_acs[i], test_fcc[i], type = "within")
  overlap_acswithinfcc <- rbind(overlap_acswithinfcc, tmp)
}
names(overlap_acswithinfcc)[1] <- "acs_within_fcc"
overlap_acswithinfcc$acs_within_fcc <- as.factor(overlap_acswithinfcc$acs_within_fcc)

# FCC is completely within ACS
overlap_fccwithinacs <- as.data.frame(countOverlaps(test_fcc[1], test_acs[1]), type = "within")
for(i in 2:length(test_acs@start)){
  tmp <- countOverlaps(test_fcc[i], test_acs[i], type = "within")
  overlap_fccwithinacs <- rbind(overlap_fccwithinacs, tmp)
}
names(overlap_fccwithinacs)[1] <- "fcc_within_acs"
overlap_fccwithinacs$fcc_within_acs <- as.factor(overlap_fccwithinacs$fcc_within_acs)

#
# Put back -------------------------------------------------------------------------------------
#

# Create df with intervals
overlap_geo <- as.data.frame(data$GEOID)
names(overlap_geo)[1] <- "GEOID"
overlap_geo$GEOID <- as.character(data$GEOID)

overlap_df <- cbind(overlap_geo, overlap_any, overlap_acswithinfcc, overlap_fccwithinacs)

# Create categorical variable
overlap_df <- overlap_df %>% mutate(overlaptype = case_when(fcc_within_acs == 1 ~ "FCC within ACS",
                                                            acs_within_fcc == 1 ~ "ACS within FCC",
                                                            (fcc_within_acs == 0 &  acs_within_fcc == 0 & overlap_any == 1) ~ "Other overlap",
                                                            overlap_any == 0 ~ "No overlap"))
overlap_df$overlaptype <- factor(overlap_df$overlaptype, levels = c("FCC within ACS", "ACS within FCC", "Other overlap", "No overlap"))

# Left join with data (that has geography)
data_int <- left_join(data, overlap_df, by = "GEOID")


#
# Select data -------------------------------------------------------------------------------------
#

int_contig <- data_int %>% filter(STATEFP != "02" & STATEFP != "15" & STATEFP != "60" & STATEFP != "66" & STATEFP != "69" & STATEFP != "72" & STATEFP != "78")
int_alaska <- data_int %>% filter(STATEFP == "02")
int_hawaii <- data_int %>% filter(STATEFP == "15")


#
# Plot: OVERLAP TYPE -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot(data = int_contig) +
  geom_sf(aes(fill = overlaptype), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC Broadband Subscription Estimate Congruence by Tract", 
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_discrete_manual(aesthetics = "fill", name = "Overlap type", values = c("#002A64", "#53596C", "#908B79", "#f0f0f0")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot(data = int_hawaii) +
  geom_sf(aes(fill = overlaptype), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "Overlap type", values = c("#002A64", "#53596C", "#908B79", "#f0f0f0")) +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot(data = int_alaska) +
  geom_sf(aes(fill = overlaptype), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "Overlap type", values = c("#002A64", "#53596C", "#908B79", "#f0f0f0")) +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


#
# Plot: ANY OVERLAP -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot(data = int_contig) +
  geom_sf(aes(fill = overlap_any), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC Broadband Subscription Estimate Congruence by Tract", 
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_discrete_manual(aesthetics = "fill", name = "Any interval overlap", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot(data = int_hawaii) +
  geom_sf(aes(fill = overlap_any), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "Any interval overlap", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot(data = int_alaska) +
  geom_sf(aes(fill = overlap_any), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "Any interval overlap", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


#
# Plot: ACS WITHIN FCC -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot(data = int_contig) +
  geom_sf(aes(fill = acs_within_fcc), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC Broadband Subscription Estimate Congruence by Tract", 
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_discrete_manual(aesthetics = "fill", name = "ACS within FCC interval", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot(data = int_hawaii) +
  geom_sf(aes(fill = acs_within_fcc), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "ACS within FCC interval", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot(data = int_alaska) +
  geom_sf(aes(fill = acs_within_fcc), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "ACS within FCC interval", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


#
# Plot: FCC WITHIN ACS -------------------------------------------------------------------------------------
#

# Plot contiguous states
plot_main <- ggplot(data = int_contig) +
  geom_sf(aes(fill = fcc_within_acs), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000)) +
  labs(title = "ACS and FCC Broadband Subscription Estimate Congruence by Tract", 
       caption = "Note: FCC = Federal Communications Commission, December 2015. ACS = American Community Survey, 2013-17.\nAlaska and Hawaii not to scale.") +
  scale_discrete_manual(aesthetics = "fill", name = "FCC within ACS interval", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "top")

# Plot Hawaii
plot_hawaii <- ggplot(data = int_hawaii) +
  geom_sf(aes(fill = fcc_within_acs), size = 0.001)  +
  theme_map() +
  coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "FCC within ACS interval", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot Alaska
plot_alaska <- ggplot(data = int_alaska) +
  geom_sf(aes(fill = fcc_within_acs), size = 0.001) +
  theme_map() +
  coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000), expand = FALSE) +
  scale_discrete_manual(aesthetics = "fill", name = "FCC within ACS interval", values = c("#f0f0f0", "#3F4788"), labels = c("No", "Yes")) +
  theme(legend.position = "none")

# Plot all
plot_main +
  annotation_custom(grob = ggplotGrob(plot_alaska),
                    xmin = -3350000,
                    xmax = -3350000 + (1600000 - (-2400000))/1.8,
                    ymin = -2450000,
                    ymax = -2450000 + (2500000 - 200000)/1.8) +
  annotation_custom(grob = ggplotGrob(plot_hawaii),
                    xmin = -1700000,
                    xmax = -1700000 + (-154 - (-161))*230000,
                    ymin = -2450000,
                    ymax = -2450000 + (23 - 18)*230000)


