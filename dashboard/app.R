# Read In Packages
for (pkg in c("tableHTML","usmap","shinydashboard", "shiny", "leaflet", "dplyr", "httr", "here", "maptools","gpclib","sp", 'sf', 'ggplot2', 'ggmap', 
              'tidyverse', 'tigris', 'acs', 'data.table', 'maditr', 'viridis', 'ggplot2', 'usmap', 'rapportools', 'RPostgreSQL', 'DBI')) {
  library(pkg, character.only = TRUE)
}

# Create Data for Plotting
acs_fcc_shapes <- function(state, geography, r_u){
  state_fips = usmap::fips(state)
  if(geography == "Block Group"){
    acs_file <- here("data", "working", "summary_acs.csv")
    acs <- fread(acs_file, colClasses=c(state="character",county="character",census_tract="character", block_group = "character"))
    fcc_file <- here("data", "working", "fcc_processed_25.csv")
    fcc <- fread(fcc_file, colClasses=c(state="character",county="character",tract="character", block_group = "character")) 
  } else if(geography == "Census Tract"){
    acs_file <- here("data", "working", "summary_acs_census_tract.csv")
    acs <- fread(acs_file, colClasses=c(state="character",county="character",census_tract="character"))
    fcc_file <- here("data", "working", "fcc_processed_tract_25.csv")
    fcc <- fread(fcc_file, colClasses=c(state_fips="character",county_short="character",county = "character",tract_short="character", tract="character"))   
  }
  
  #merge fcc & acs 
  if(geography =='Block Group'){
    fcc_acs = merge(fcc, acs, by.x = c('state', 'county', 'tract', 'block_group'), by.y = c('state', 'county', 'census_tract', 'block_group')) %>% 
      dt_filter(state==state_fips)
  } else if(geography == "Census Tract") {
    fcc_acs = merge(fcc, acs, by.x = c('state_fips', 'county_short', 'tract_short'), by.y = c('state', 'county', 'census_tract')) %>%
      dt_filter(state_fips==state_fips)  
  }
  #pull shapes for state
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   dbname = "gis",
                   host = "localhost",
                   port = "5434",
                   user = "bband_user",
                   password = "bband")
  # pull shapes on tract or bl level
  if(geography =='Block Group'){
    geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_bg_500k", state_fips)))
  } else if(geography == "Census Tract"){
    geo = st_read(con, c("census_cb", sprintf("cb_2018_%s_tract_500k", state_fips)))  
  }
  
  DBI::dbDisconnect(con)
  
  #merge shapes and data
  if(geography == "Block Group") {
    full <- merge(fcc_acs, geo, by.x = c('state', 'county', 'tract', 'block_group'), by.y =c('STATEFP','COUNTYFP','TRACTCE','BLKGRPCE'))
    full_sf <- full %>% dt_select(state,county,tract, block_group, consumer_has, business_has,
                                  maxaddown, maxcirdown, stateabbr, num_ppl, availability_cons,
                                  availability_bus, pcat_all_pct_min, pcat_all_pct_max, pcat_all_10x1_min,
                                  pcat_all_10x1_max, State, County_Name, Population_2010, RUCC_2013.x, 
                                  B28002_004_per, B28002_007_per, ALAND, AWATER, geometry) %>% 
      dt_mutate(rural_urban = ifelse(RUCC_2013.x < 4, 'Urban', 'Rural')) %>% rename(replace = c("RUCC_2013.x" = "RUCC_2013"))
    
  } else if(geography == "Census Tract"){
    full <- merge(fcc_acs, geo, by.x = c('state_fips', 'county_short', 'tract_short'), by.y =c('STATEFP','COUNTYFP','TRACTCE'))
    full_sf <- full %>% dt_select(state,county,tract, tract_short,
                                  maxaddown,state_fips, availability_cont,
                                  availability_adv, pcat_all_pct_min, pcat_all_pct_max, pcat_all_10x1_min,
                                  pcat_all_10x1_max, State, County_Name, Population_2010, RUCC_2013, 
                                  B28002_004_per, B28002_007_per, ALAND, AWATER, geometry) %>% 
      dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural'))
  }
  
  #allow to filter by rural/urban
  if(r_u == 'Rural') {
    full_sf %>% data.table() %>% dt_filter(rural_urban == 'Rural') %>% st_as_sf()
  } else if (r_u == 'Urban') {
    full_sf %>% data.table() %>% dt_filter(rural_urban == 'Urban') %>% st_as_sf()
  } else if (r_u == 'All') {
    full_sf = full_sf %>% st_as_sf()
  }
}

county_shapes <- function(state, r_u){
  state_fips = usmap::fips(state)
  mic <- here("data", "original", "microsoft", 'microsoft.csv')
  microsoft <- read.csv(mic, colClasses=c(ST="character",COUNTY.ID="character", 
                                          BROADBAND.USAGE = "numeric", BROADBAND.AVAILABILITY.PER.FCC = "numeric"),
                        na.strings = "-")
  microsoft$county<- ifelse(nchar(microsoft$COUNTY.ID) !=5 ,gsub(" ", "", paste("0",microsoft$COUNTY.ID), fixed = TRUE), microsoft$COUNTY.ID)
  
  # merge with fcc
  fcc_data <- here("data", "working", "fcc_processed_county_25.csv")
  fcc_county <- read.csv(fcc_data, colClasses=c(state="character",county="character"))
  fcc_mic = merge(fcc_county, microsoft, by = 'county')
  # get shapes
  con <- dbConnect(drv = RPostgreSQL::PostgreSQL(),
                   dbname = "gis",
                   host = "localhost",
                   port = "5434",
                   user = "bband_user",
                   password = "bband")
  
  geo = st_read(con, c("census_cb", "cb_2016_us_county_500k"))
  DBI::dbDisconnect(con)
  
  # merge with shapes
  full = merge(fcc_mic, geo, by.x = 'county', by.y = 'GEOID') 
  full_st = full[full$STATEFP == state_fips,] %>% data.table() %>%
    dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural')) %>% st_as_sf()
  
  #allow to filter by rural/urban
  if(r_u == 'Rural') {
    full_st %>% data.table() %>% dt_filter(rural_urban == 'Rural') %>% st_as_sf()
  } else if (r_u == 'Urban') {
    full_st %>% data.table() %>% dt_filter(rural_urban == 'Urban') %>% st_as_sf()
  } else if (r_u == 'All') {
    full_st = full_st %>% st_as_sf()
  }
}

make_state_map <- function(state, geography, r_u){
  print("Building Map...")
  if(geography  == 'Block Group'){
    data <- acs_fcc_shapes(state, geography, r_u) %>% st_transform(4326)
    dat <- here('data', 'working', 'merged_by_rural_urban.csv')
    q <- read.csv(dat) %>% data.table() %>% dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural')) %>% 
      dt_filter(as.character(stateid) == unique(data$stateabbr)) %>% dt_filter(rural_urban %in% unique(data$rural_urban))
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$County_Name,
            "<br />",
            "<strong>Tract: </strong>",
            data$tract,
            "<br />",
            "<strong>Block Group: </strong>",
            data$block_group,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format="f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>ACS Coverage: Broadband (Any Type) (004): </strong>",
            round(data$B28002_004_per,1),"%",
            "<br />",
            "<strong>ACS Coverage: Broadband (Excluding Cellular/Satellite) (007): </strong>",
            round(data$B28002_007_per,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage 10x1 (Max): </strong>",
            round(data$pcat_all_10x1_max*100,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage 10x1 (Min): </strong>",
            round(data$pcat_all_10x1_min*100,1),"%",
            "<br />",
            "<strong>ACS Internet in FCC Subs Bin: </strong>",
            data$B28002_007_per < data$pcat_all_10x1_max*100 & data$B28002_007_per > data$pcat_all_10x1_min*100,
            "<br />",
            "<strong>FCC Coverage (Advertised): </strong>",
            round(data$availability_cons*100,1),"%",
            "<br />",
            "<strong>Percent Discrepancy: </strong>",
            abs(round(data$availability_cons*100 - data$B28002_007_per,1)),"%"
      ),
      htmltools::HTML
    )
    #qpal <- colorQuantile("YlOrRd", abs(round(data$availability_cons*100 - data$B28002_007_per,1)), n = 5)
    
    bins <- c(0,20,40,60,80,100)
    binpal<- colorBin("YlOrRd", abs(round(data$availability_adv*100 - data$B28002_007_per,1)),bins = bins,pretty = FALSE)
    m = leaflet(data = data)
    
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = .8,
                     color = 'lightgray',
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black"),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto"
                                                 )),
                     fillColor = ~binpal(abs(round(data$availability_cons*100 - data$B28002_007_per,1))),
                     fillOpacity = 0.7
    )
    
    cl <- c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026")
    
    leg <- c("0%-20%","20%-40%","40%-60%","60%-80%","80%-100%")  
    m <- addLegend(m,
                   position = "bottomleft", colors = cl, values = ~(abs(round(data$availability_cons*100 - data$B28002_007_per,1))),
                   title = "Percent Difference: FCC v ACS",
                   opacity = 1, labels = leg)
    label_cities <- lapply(
      paste("<strong>City: </strong>",
            as.character(q$city),
            "<br />",
            "<strong>Coverage: </strong>",
            q$coverage,"%",
            "<br />",
            "<strong>County: </strong>",
            q$county,
            "<br />",
            "<strong>State: </strong>",
            q$stateid,
            "<br />",
            "<strong>Population (2010): </strong>",
            q$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            q$RUCC_2013,
            "<br />"
      ),
      htmltools::HTML
    )
    
    pal <-  c('#9999ff', '#4c4cff', '#1300ff','#0d00cc', '#070099', '#05007f', '#030065', '#02004b', '#010032')
    qpal_cities <- colorNumeric(pal, q$RUCC_2013)
    
    m <- addCircles(m,lng = q$long, lat = q$lat, label = label_cities, 
                    radius = ~sqrt((as.numeric(q$Population_2010))) * 100,
                    fillOpacity = .4,
                    opacity = 1,
                    weight = 1, 
                    color  =  ~qpal_cities(q$RUCC_2013))
    m
    
  } else if (geography  == 'Census Tract') {
    data <- acs_fcc_shapes(state, geography, r_u) %>% st_transform(4326)
    dat <- here('data', 'working', 'merged_by_rural_urban.csv')
    q <- read.csv(dat) %>% data.table() %>% dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural')) %>% 
      dt_filter(as.character(stateid) == unique(data$state)) %>% dt_filter(rural_urban %in% unique(data$rural_urban))
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$County_Name,
            "<br />",
            "<strong>Tract: </strong>",
            data$tract_short,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format="f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>ACS Coverage: Broadband (Any Type) (004): </strong>",
            round(data$B28002_004_per,1),"%",
            "<br />",
            "<strong>ACS Coverage: Broadband (Excluding Cellular/Satellite) (007): </strong>",
            round(data$B28002_007_per,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage 10x1 (Max): </strong>",
            round(data$pcat_all_10x1_max*100,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage 10x1 (Min): </strong>",
            round(data$pcat_all_10x1_min*100,1),"%",
            "<br />",
            "<strong>ACS Internet in FCC Subs Bin: </strong>",
            data$B28002_007_per < data$pcat_all_10x1_max*100 & data$B28002_007_per > data$pcat_all_10x1_min*100,
            "<br />",
            "<strong>FCC Coverage (Advertised): </strong>",
            round(data$availability_adv*100,1),"%",
            "<br />",
            "<strong>Percent Discrepancy: </strong>",
            abs(round(data$availability_adv*100 - data$B28002_007_per,1)),"%"
      ),
      htmltools::HTML
    )
    
    bins <- c(0,20,40,60,80,100)
    binpal<- colorBin("YlOrRd", abs(round(data$availability_adv*100 - data$B28002_007_per,1)),bins = bins,pretty = FALSE)
    m = leaflet(data = data)
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = 1,
                     color = "lightgray",
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black"),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto",
                                                   offset = c(1, 5)
                                                 )),
                     fillColor = ~binpal(abs(round(data$availability_adv*100 - data$B28002_007_per,1))),
                     fillOpacity = 0.7
    )
    
    cl <- c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026")
    
    leg <- c("0%-20%","20%-40%","40%-60%","60%-80%","80%-100%")
    m <- addLegend(m,
                   position = "bottomleft", colors = cl, values = ~(abs(round(availability_adv*100 - B28002_007_per,1))),
                   title = "Percent Difference: FCC v ACS",
                   opacity = 1, labels = leg)
    label_cities <- lapply(
      paste("<strong>City: </strong>",
            as.character(q$city),
            "<br />",
            "<strong>Coverage: </strong>",
            q$coverage,"%",
            "<br />",
            "<strong>County: </strong>",
            q$county,
            "<br />",
            "<strong>State: </strong>",
            q$stateid,
            "<br />",
            "<strong>Population (2010): </strong>",
            q$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            q$RUCC_2013,
            "<br />"
      ),
      htmltools::HTML
    )
    
    pal <-  c('#9999ff', '#4c4cff', '#1300ff','#0d00cc', '#070099', '#05007f', '#030065', '#02004b', '#010032')
    qpal_cities <- colorNumeric(pal, q$RUCC_2013)
    
    m <- addCircles(m,lng = q$long, lat = q$lat, label = label_cities, 
                    radius = ~sqrt((as.numeric(q$Population_2010))) * 100,
                    fillOpacity = .4,
                    opacity = 1,
                    weight = 1, 
                    color  =  ~qpal_cities(q$RUCC_2013))
    
  } else if (geography  == 'County') {
    data <- county_shapes(state, r_u) %>% st_transform(4326)
    dat <- here('data', 'working', 'merged_by_rural_urban.csv')
    q <- read.csv(dat) %>% data.table() %>% dt_mutate(rural_urban = ifelse(RUCC_2013 < 4, 'Urban','Rural')) %>% 
      dt_filter(as.character(stateid) == unique(data$state)) %>% dt_filter(rural_urban %in% unique(data$rural_urban))
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$COUNTY.NAME,
            "<br />",
            "<strong>Land Area (square meters): </strong>",
            formatC(data$ALAND, format="f", big.mark = ",", digits = 0),
            "<br />",
            "<strong>Population (2010): </strong>",
            data$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            data$RUCC_2013,
            "<br />",
            "<strong>Microsoft Usage: </strong>",
            round(data$BROADBAND.USAGE*100,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage (Max): </strong>",
            round(data$max_pcat_10x1_per*100,1),"%",
            "<br />",
            "<strong>FCC Subscription Coverage (Min): </strong>",
            round(data$min_pcat_10x1_per*100,1),"%",
            "<br />",
            "<strong>Microsoft Usage In FCC Subs Bin: </strong>",
            data$BROADBAND.USAGE*100 < data$max_pcat_10x1_per*100 & data$BROADBAND.USAGE*100 > data$min_pcat_10x1_per*100,
            "<br />",
            "<strong>FCC Coverage (Advertised): </strong>",
            round(data$availability_adv*100,1),"%",
            "<br />",
            "<strong>Percent Discrepancy: </strong>", 
            abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100,1)),"%"
      ),
      htmltools::HTML
    )
    #binpal <- colorBin("Blues", countries$gdp_md_est, 6, pretty = FALSE)
    bins <- c(0,20,40,60,80,100)
    binpal<- colorBin("YlOrRd", abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100,1)),bins = bins,pretty = FALSE)
    #qpal <- colorQuantile("YlOrRd", abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100,1)), n = 5)
    m = leaflet(data = data)
    m <- addPolygons(m,
                     stroke = TRUE,
                     weight = 1,
                     color = "lightgray",
                     smoothFactor = 0.2,
                     label = labels,
                     highlight = highlightOptions(
                       weight = 5,
                       color = "black"),
                     
                     labelOptions = labelOptions(direction = "bottom",
                                                 style = list(
                                                   "font-size" = "12px",
                                                   "border-color" = "rgba(0,0,0,0.5)",
                                                   direction = "auto",
                                                   offset = c(1, 5)
                                                 )),
                     fillColor = ~binpal(abs(round(data$availability_adv*100 - data$BROADBAND.USAGE*100,1))),
                     fillOpacity = 0.7
    ) 
    
    labelOptions = labelOptions(direction = "bottom",
                                style = list(
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)",
                                  direction = "auto"
                                ))
    
    cl <- c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026")
    
    leg <- c("0%-20%","20%-40%","40%-60%","60%-80%","80%-100%")
    
    m <- addLegend(m,
                   position = "bottomleft", colors = cl, values = ~(abs(round(availability_adv*100 - BROADBAND.USAGE*100,1))),
                   title = "Percent Difference: FCC vs Microsoft",
                   opacity = 0.7, labels = leg)
    
    label_cities <- lapply(
      paste("<strong>City: </strong>",
            as.character(q$city),
            "<br />",
            "<strong>Coverage: </strong>",
            q$coverage,"%",
            "<br />",
            "<strong>County: </strong>",
            q$county,
            "<br />",
            "<strong>State: </strong>",
            q$stateid,
            "<br />",
            "<strong>Population (2010): </strong>",
            q$Population_2010,
            "<br />",
            "<strong>Rural-Urban Continuum Code: </strong>",
            q$RUCC_2013,
            "<br />"
      ),
      htmltools::HTML
    )
    
    pal <-  c('#9999ff', '#4c4cff', '#1300ff','#0d00cc', '#070099', '#05007f', '#030065', '#02004b', '#010032')
    qpal_cities <- colorNumeric(pal, q$RUCC_2013)
    
    m <- addCircles(m,lng = q$long, lat = q$lat, label = label_cities, 
                    radius = ~sqrt((as.numeric(q$Population_2010))) * 100,
                    fillOpacity = .4,
                    opacity = 1,
                    weight = 1, 
                    color  =  ~qpal_cities(q$RUCC_2013))
  }
}

server <- function(input,output,session){
  data <- reactive({
    x <- acs_fcc_shapes(input$State, input$Geography, input$R_U) %>% st_transform(4326)
  })
  
  output$mymap <- renderLeaflet({
    make_state_map(input$State, input$Geography, input$R_U)
  })
  
  definitions <- data.table(Geography = c(rep('Census Tract', 6), rep('Block Group', 6), rep('County', 4)),
                            Metric = c('ACS Coverage (Broadband of Any Type)', 
                                       'ACS Coverage (Excluding Cellular/Satellite)', 
                                       'FCC Subscription Coverage',
                                       'ACS Internet in FCC Subs Bin',
                                       'FCC Coverage (Advertised)',
                                       'Percent Discrepancy',
                                       'ACS Coverage (Broadband of Any Type)', 
                                       'ACS Coverage (Excluding Cellular/Satellite)', 
                                       'FCC Subscription Coverage',
                                       'ACS Internet in FCC Subs Bin',
                                       'FCC Coverage (Advertised)',
                                       'Percent Discrepancy',
                                       'Microsoft Usage',
                                       'FCC Subscription Coverage',
                                       'FCC Coverage (Advertised)',
                                       'Percent Discrepancy'
                            ), 
                            Definition = c('Data come from ACS table B28002, which describes the presence and types of internet subscriptions in households.', 
                                           'Data come from ACS table B28002, which describes the presence and types of internet subscriptions in households. This metric excludes cellular data and satellite internet, and we use it in our comparisons with FCC Form 477-reported broadband coverage.', 
                                           'The data show bins (maximum and minimum) for the number of internet connections per 1,000 households on the census tract level. We use the data as of December 31, 2015, and connections of at least 10 mbps downstream in order to create an approximately one-to-one comparison between FCC and ACS data.',
                                           'The metric compares ACS coverage (broadband subscriptions excluding cellular and satellite) and FCC subscription coverage bins. It determines whether ACS self-reported connections fall within the FCC-reported bins internet subscription bins.', 
                                           'The metric uses the maximum advertised downstream speed offered by the provider in the block for consumer service from FCC form 477, as well as  population data from the Decennial Census to calculate the proportion of the census tract population that has access to at least one provider offering at least 25 mbps maximum advertised downstream speed.',
                                           'The metric indicates the discrepancy in percent points between the FCC coverage metric and the ACS coverage (excluding cellular and satellite - item 007) metric.',
                                           'Data come from ACS table B28002 and describe the presence and types of internet subscription in households.', 
                                           'Data come from ACS table B28002 and describe the presence and type of internet subscription in households. This metric excludes cellular data and satellite internet, and we use it for comparison to FCC Form 477-reported coverage.', 
                                           'The data show bins (maximum and minimum) for the number of internet connections per 1,000 households within a block group. We use data as of December 31, 2015, and connections of at least 10 mbps downstream in order to create an approximately one-to-one comparison between FCC and ACS data.',
                                           'The metric compares ACS coverage (excluding cellular and satellite) and FCC subscription coverage bins in to determine whether ACS self-reported connections fall within FCC-reported bins for internet subscriptions.', 
                                           'The metric uses the maximum advertised downstream speed offered by the provider in the block for consumer service from FCC form 477, as well as population data from the decennial census to calculate the proportion of block group population that has access to at least one provider offering at least 25 mbps maximum advertised downstream speed.',
                                           'The metric indicates the discrepancy in percent points between the FCC coverage metric and the ACS coverage (excluding cellular and satellite - item 007) metric.',
                                           'Data come from Microsoft and include the percent of people per county using the internet at 25 mbps downstream.',
                                           'The metric shows bins (maximum and minimum) for the number of internet connections per 1,000 households within the county. We use data as of December 31, 2015, and connections of at least 10 mbps downstream to create an approximately one-to-one comparison between FCC and Microsoft data.',
                                           'The metric uses the maximum advertised downstream speed offered by the provider in the block for consumer service from FCC form 477, as well as population data from the decennial census to calculate the proportion of county population that has access to at least one provider that offers at least 25 mbps maximum advertised downstream speed.',
                                           'The metric indicates the discrepancy in percent points between the FCC coverage metric and the Microsoft usage metric.'
                            ),
                            Source = c('2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 004', 
                                       '2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007',
                                       'Federal Communications Commission Form 477 Residential Fixed Internet Access Service Connections per 1000 Households by Census Tract (December 31, 2015)', 
                                       'Constructed Metric. Primary data sources are 2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007 and Federal Communications Commission Form 477 Residential Fixed Internet Access Service Connections per 1000 Households by Census Tract (December 31, 2015)', 
                                       'Federal Communications Commission Form 477 (2015); Census Decennial Population (2010)', 
                                       'Constructed Metric. Primary data sources are 2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007, Federal Communications Commission Form 477; Max Advertised Downstream Speed (mbps) (2015), and ACS Decennial Population (2010)',
                                       '2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 004', 
                                       '2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007',
                                       'Federal Communications Commission Form 477 Residential Fixed Internet Access Service Connections per 1000 Households by Census Tract (December 31, 2015)', 
                                       'Constructed Metric. Primary data sources are 2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007 and Federal Communications Commission Form 477 Residential Fixed Internet Access Service Connections per 1000 Households by Census Tract (December 31, 2015)', 
                                       'Federal Communications Commission Form 477 (2015); Census Decennial Population (2010)', 
                                       'Constructed Metric. Primary data sources are 2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007, Federal Communications Commission Form 477; Max Advertised Downstream Speed (mbps) (2015), and Census Decennial Population (2010)',
                                       'Microsoft Airband Initiative 2018 Broadband Usage Data',
                                       'Federal Communications Commission Form 477 Residential Fixed Internet Access Service Connections per 1000 Households by Census Tract (December 31, 2015)', 
                                       'Federal Communications Commission Form 477 (2015); Census Decennial Population (2010)', 
                                       'Constructed Metric. Primary data sources are 2013-2017 American Community Survey 5-Year Estimates; Table B28002, Item 007, and Microsoft Airband Initative Broadband Usage Data'
                                       
                            )
  )
  output$table <- renderDataTable(definitions %>% dt_filter(Geography == input$Geography), 
                                  options = list(searching = FALSE,
                                                 paging = FALSE,
                                                 pageLength = 15,
                                                 lengthMenu = list(c(1, 2, 3), c('5', '15', 'All'))
                                  ))
}

ui <- fluidPage(
  theme = "bootstrap.css",
  title = "Broadband DSPG 2019",
  
  fluidRow(width = 5, column(2.5,
                             img(src = 'logo.png', class = 'topimage')
  ),
  column(7, 
         h1('Broadband Coverage Discrepancy Map')),
         br(),
         h5('This dashboard was created as part of the Data Science for Public Good program in the Social and Decision Analytics Division of the Biocomplexity Institute and Initiative
             at the University of Virginia, in partnership with the United States Department of Agriculture Economic Research Service.'),
         p(),
         h5('The dashboard visualizes broadband coverage report discrepancies between three publicly available datasets: the 2015 Federal Communications Commission
             Form 477 provider-reported data, American Community Survey 2013-17 consumer-reported broadband subscription data, and 2018 Microsoft Airband Initiative broadband usage data.'),
         p(),
         h5('We would like to acknowledge the support of', a('BroadbandNow.com,', href = 'https://www.broadbandnow.com/'), 'an internet provider coverage and availability aggregator which enabled us to overlay city-level coverage data.')
  ),
  hr(),
  fluidRow(width = 4,
           column(3,
                  selectInput("State", "Select State", choices = state.abb, selected = 'AL', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
           ),
           column(3, 
                  selectInput("Geography", "Select Geography", c("County", "Census Tract", "Block Group"), 
                              selected = 'County', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
           ),
           column(3,
                  selectInput("R_U", "Select Urban Status", c("Rural", "Urban", "All"), selected = 'All', multiple = FALSE,
                              selectize = TRUE, width = NULL, size = NULL)
           )
  ),
  
  hr(),
  fluidRow(width = 4,leafletOutput("mymap",height = 580, width = 1200)),
  hr(),
  fluidRow(width = 4, 
           h3('Table of Metric Definitions')
  ),
  fluidRow(width = 1, heght = 10,
           dataTableOutput('table')
  )
)

shinyApp(ui = ui, server = server)