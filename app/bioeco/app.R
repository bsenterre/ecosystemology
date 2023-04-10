#bioeco v2.0

#Load libraries
library(pacman)
p_load(data.table, DT, dplyr, stringr, sjmisc, units,
       raster, terra, tidyterra, bit64,
       sf, mapview, leaflet,
       redlistr,
       shiny, shinydashboard, #gridlayout, 
       shinycssloaders) #To get a spinner while loading my leaflet map

#Load datainput
options(DT.options = list(pageLength = 50), scrollX = TRUE, scrollY  = TRUE)

#Set mapview options
mapviewOptions(basemaps = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"), legend = FALSE)

### Get input data

### Read the general GIS data for the Mapping (Tab 3)
KBA = st_read("datainput/Guinea_Liberia_SierraLeone_Ivory Coast_KBA.shp", crs = 4326, quiet = TRUE)
KBA = terra::vect(KBA)

KBASey = sf::st_read("datainput/sey_cons_pas2023.shp", crs = 4326, quiet = TRUE) %>% subset(statusID != 8)
#Correcting an issue (likely from the WDKBA geometries?)
KBASey <- st_make_valid(KBASey)
tmp <- KBASey %>% dplyr::filter(status == "National Park" | status == "Reserve-Special Reserve" | status == "Reserve-Nature Reserve" | status == "Reserve-Bird Reserve")
KBASey <- left_join(KBASey, as.data.frame(tmp) %>% dplyr::select(kbaName, -geometry) %>% mutate(protected = "yes"), by = "kbaName")
KBASey[is.na(KBASey$protected),]$protected <- "no"

#Adding the KBA size (xx to be done in bioEcosystemology on production of KBAStatsAll)
#sf::sf_use_s2(FALSE) #NOOO THIS LEADS TO SERIOUS BUGS!
KBASeyWEA <- sf::st_transform(KBASey, crs=4087)
KBASeyWEA$KBAarea_ha <- round(st_area(KBASeyWEA) / 10000, digits = 1) %>% units::drop_units()
KBASey <- left_join(KBASey, as.data.frame(KBASeyWEA) %>% dplyr::select(kbaName, KBAarea_ha), by = "kbaName")

#Get a kbaList for the UI
KBASey_df <- as.data.frame(KBASey)
kbaList <- sort(unique(KBASey_df$kbaName))


### ecoSpeciesNames for Tab 2

#Load the ecoSpParabiotypes object, i.e. complete list of names incl. p.p.
ecoSpParabiotypes = data.table::fread("datainput/ecoSpParabiotypes.txt", header = TRUE, sep = "\t")
ecoSpParabiotypes$ecoSpeciesID <- as.character(ecoSpParabiotypes$ecoSpeciesID)
ecoSpParabiotypes$synof_ecoSpeciesID <- as.character(ecoSpParabiotypes$synof_ecoSpeciesID)
#Get the IUCNL3ID as a hyperlink for the DT
ecoSpParabiotypes$IUCNL3IDLink <- paste0("<a href='","https://global-ecosystems.org/explore/groups/", ecoSpParabiotypes$IUCNL3ID,"'>",paste0(ecoSpParabiotypes$IUCNL3ID),"</a>")

#Get exactly the table content that I want to show in DT
ecoSpeciesNames <- ecoSpParabiotypes %>% dplyr::select(ecoSpeciesENFR, BIOL4ID, IUCNL3IDLink, lifeZone, ecoGenus, ecoSpeciesReference, nomenclaturalStatus, synof_ecoSpeciesID, BIOL4Ordering, ecoGenusOrdering) %>% dplyr::filter(grepl("Seychelles", ecoSpeciesENFR)) # xxedit
ecoSpeciesNames <- with(ecoSpeciesNames, ecoSpeciesNames[order(BIOL4Ordering, ecoGenusOrdering, lifeZone, synof_ecoSpeciesID, nomenclaturalStatus), ]) %>% rename(ecoSpecies = ecoSpeciesENFR, IUCNL3ID = IUCNL3IDLink, reference = ecoSpeciesReference)


### Get the eco-occurrences for the Mapping (Tab 3) and for Tab 5

#Load the EDlastEident
EDlastEident = data.table::fread("datainput/EDlastEident.txt", header = TRUE, sep = "\t")
EDlastEident$ecoSpeciesID <- as.character(EDlastEident$ecoSpeciesID)

#Get it ready for printing, with just the needed fields
EDlastEidentDT <- EDlastEident %>% dplyr::select(recordedBy, recordNumber, eventDate, country, verbatimLocality, habitat, verbatimElevation, degreeOfInvasion, ecoNaturalness, nplots, ecoOccurrenceID, biotypeStatus, isMappable, ecoUse, decimalLongitude, decimalLatitude, ecoSpeciesID) %>% rename(num = recordNumber, date = eventDate, locality = verbatimLocality, altit = verbatimElevation, invasion = degreeOfInvasion, natural = ecoNaturalness)
EDlastEidentDT <- with(EDlastEidentDT, EDlastEidentDT[order(ecoSpeciesID, ecoUse, biotypeStatus, date), ])

#Get it as a sf object
EDlastEident_sf <- EDlastEidentDT[EDlastEidentDT$isMappable == 1,]
EDlastEident_sf <- st_as_sf(EDlastEident_sf, coords = c("decimalLongitude", "decimalLatitude"), crs=4326)


### Get the item for Tab 4: myKBAStats

KBAStatsAll = data.table::fread("datainput/KBAStatsAll.txt", header = TRUE, sep = "\t")
KBAStatsAll$ecoSpeciesID <- as.character(KBAStatsAll$ecoSpeciesID)
#Add the KBA area in ha
KBAStatsAll <- left_join(KBAStatsAll, as.data.frame(KBASey) %>% dplyr::select(kbaName, KBAarea_ha), by = "kbaName")


### Get the other item for Tab 4: myRLEStats

#Load the ecoSpRLE metadata
ecoSpRLEStats = data.table::fread("datainput/ecoSpRLEStats.txt", header = TRUE, sep = "\t")
ecoSpRLEStats$ecoSpeciesID <- as.character(ecoSpRLEStats$ecoSpeciesID)

#Add key stats to the ecoSp list (Should I add also ratioPreNat? No: can get from others?)
ecoSpeciesData <- left_join(ecoSpeciesNames, ecoSpRLEStats %>% dplyr::select(ecoSpeciesID, nOcc, AreaTotPre_ha, AreaTot_ha, ratioPre, ratioNat, ratioPA, AOO2km, AOO2kmNat, AOO10km, AOO10kmNat, EOO, RLEStatus) %>% rename(synof_ecoSpeciesID = ecoSpeciesID), by = "synof_ecoSpeciesID")

#Filter only the accepted names (and nomen nudum)
#Print a DT of only ecoSp with existing metadata
ecoSpeciesDataDT <- ecoSpeciesData %>% dplyr::filter(grepl("^aa-", nomenclaturalStatus) | grepl("^ab-", nomenclaturalStatus)) %>% dplyr::filter(nOcc > 0 | AreaTot_ha > 0 | nchar(RLEStatus) > 0) %>% dplyr::select(ecoSpecies, nOcc, AreaTotPre_ha, AreaTot_ha, ratioPre, ratioNat, ratioPA, AOO2km, AOO2kmNat, AOO10km, AOO10kmNat, EOO, RLEStatus, synof_ecoSpeciesID) %>% rename(ecoSpeciesID = synof_ecoSpeciesID) #IUCNL3ID, 

### Get the IUCNL3 typology for the Tab6
IUCNL3 = data.table::fread("datainput/IUCNL3.txt", header = TRUE, sep = "\t")
IUCNL3$IUCNL3Link <-paste0("<a href='","https://global-ecosystems.org/explore/groups/", IUCNL3$IUCNL3ID,"'>",paste0(IUCNL3$IUCNL3),"</a>")

### Get alternative Tab6 for the IUCN-BIO crossover and notes (not cleaned yet)
IUCNL3BIOCrossover = data.table::fread("datainput/IUCNL3BIOCrossover.txt", header = TRUE, sep = "\t")
#I will also need the BIOL2, which I used to show gaps in IUCNL3
BIOL2 = data.table::fread("datainput/BIOL2.txt", header = TRUE, sep = "\t")
IUCNL3BIOCrossover <- left_join(BIOL2, IUCNL3BIOCrossover %>% dplyr::select(-BIOL2Ordering), by = "BIOL2ID")
IUCNL3BIOCrossover <- left_join(IUCNL3BIOCrossover, IUCNL3 %>% dplyr::select(IUCNL3ID, IUCNL3), by = "IUCNL3ID")

IUCNL3BIOCrossover <- with(IUCNL3BIOCrossover, IUCNL3BIOCrossover[order(BIOL2Ordering, IUCNL3ID), ])

IUCNL3BIOCrossover$factors <- paste0(IUCNL3BIOCrossover$L2CI, "|", IUCNL3BIOCrossover$L2UW, "|", IUCNL3BIOCrossover$L2Ph, "|", IUCNL3BIOCrossover$L2T)

#This is a crossover BIOL2 - IUCNL3; It covers the full range of the IUCN typology, which largely reinterpreted through BIOL2 and BIOL3.

#Next thing, I want to show, for terrestrial and related, how my approach leads to subdivisions of IUCNL3 into my BIOL3
#BIOL4Ordering	BIO_L2_id	3T	3Anth	3Wet	BIO_L3_id	BIO_L3_systematic_functional_group	4Wet	BIO_L4_id	BIO_L4_systematic_global_ecosystem	IUCN_L3_id	BIO_L4_notes
#So basically: BIOL3, BIOL4, IUCNL3, BIOL4Notes



### Get the BIOL4 typology for the Tab 7
BIOL4 = data.table::fread("datainput/BIOL4.txt", header = TRUE, sep = "\t")
BIOL3 = data.table::fread("datainput/BIOL3.txt", header = TRUE, sep = "\t")
BIOL4 <- left_join(BIOL4, IUCNL3 %>% dplyr::select(IUCNL3ID, IUCNL3Link) %>% rename(IUCNL3_eco_functional_group = IUCNL3Link), by = "IUCNL3ID")
BIOL4 <- left_join(BIOL4, BIOL3 %>% dplyr::select(BIOL3ID, BIOL3) %>% rename(BIOL3_systematic_functional_group = BIOL3), by = "BIOL3ID")

### Get the synthetic KBA Tab on species and ecosystem triggers: Tab 8
#Load the species trigger data produced in bioEcosystemology
spKBAStats = data.table::fread("datainput/spKBAStats.txt", header = TRUE, sep = "\t")
#Filter out exotics
spKBAStats <- spKBAStats %>% dplyr::filter(grepl("native", establishmentMeansNat))
#Compile species and ecosystem potential triggers into a single table
#kbaName, status, KBAarea_ha, ecoSpecies/scientificName, ratioOfTotalNat/NA, ratioOfTotal/ratio, IUCNg/, IUCNnRLEStatus, isTrigger, BIO (ecoSp/sp)
spKBAStats <- left_join(spKBAStats, KBASey %>% dplyr::select(kbaName, status, KBAarea_ha), by = "kbaName") %>%
  mutate(ratioNat = NA, type = "species") %>% dplyr::select(kbaName, status, KBAarea_ha, scientificName, ratioNat, ratio, threatStatusNat, threatStatusGlobal, isTrigger, type, taxonID) %>% rename(biotaName = scientificName, threatNat = threatStatusNat, threatGlo = threatStatusGlobal, biotaID = taxonID)
ecoSpKBAStats <- KBAStatsAll %>% mutate(threatGlo = NA, type = "ecosystem") %>% dplyr::select(kbaName, status, KBAarea_ha, ecoSpecies, ratioOfTotalNat, ratioOfTotal, RLEStatus, threatGlo, isTrigger, type, ecoSpeciesID) %>% rename(biotaName = ecoSpecies, ratioNat = ratioOfTotalNat, ratio = ratioOfTotal, threatNat = RLEStatus, biotaID = ecoSpeciesID)
biotaKBATriggers <- rbind(spKBAStats, ecoSpKBAStats)
#Order them
biotaKBATriggers <- with(biotaKBATriggers, biotaKBATriggers[order(kbaName, -ratio), ])
biotaKBATriggers$kbaName <- as.factor(biotaKBATriggers$kbaName)
#biotaKBATriggers$statusGlo <- as.factor(biotaKBATriggers$statusGlo)
#biotaKBATriggers$statusNat <- as.factor(biotaKBATriggers$statusNat)

#Read the saved layer of all AOO and EOO
AOOEOOAll <- sf::st_read("datainput/AOOEOOAll.shp", crs = 4326, quiet = TRUE)

##########2. Shiny UI
ui <- dashboardPage(
  dashboardHeader(title = "Ecosystemology", titleWidth = "100%"),
  
  dashboardSidebar(disable=TRUE),
  
  dashboardBody(
    tabBox(title = "",id = "tabset1", selected = "1-Readme", height = "99%", width = "99%",
           
           tabPanel("1-Readme", 
                    hr(),
                    h4("Here some tips on how to use this interactive webpage on ecosystems"),
                    hr(),
                    h5("The Tabs 1-5 can be used to explore the whole database. Use the search box using one to several words or part of words separated by spaces."),
                    br(),
                    h5("You can also use the Tab-2 to filter ecosystem 'species' (or eco-species) and then select one particular row. All other Tabs will then be updated to show information on the selected eco-species. Tha map can take a few seconds to load, so just be patient. To un-select, just click again on the selected row."),
                    br(),
                    h5("In the Tabs 6 and 7, we provide some general view on definitions used to classify ecosystem types within a global typology. It includes a summary of the new (2022) IUCN function-based typology. The Tab 7 shows a variation of the IUCN typology in order to make possible the integration from the global to the local, and also to integrate principles of ecosystemology (see ", tags$a(href="https://doi.org/10.1016/j.ecocom.2021.100945", "https://doi.org/10.1016/j.ecocom.2021.100945"), ")."),
                    hr(),
                    h5("Please send your feedback to bsenterre@gmail.com"),
                    h5("This work is based on the following project and is derived from the 'BIO' database: ", tags$a(href="https://www.gbif.org/publisher/5299c15f-07ad-4ef6-8d6a-b3acbb90ce93", "https://www.gbif.org/publisher/5299c15f-07ad-4ef6-8d6a-b3acbb90ce93"))
           ),
           
           tabPanel("2-Eco-species names", 
                    h5("TO filter the ecosystem names to a given area, you can simply use the search box, e.g. with 'Seychelles', or 'Sey rav fo', etc. Click on a row to select one ecosystem name and filter the Tab. 3-5 accordingly."),
                    DTOutput(outputId = "myEcoSpeciesList", width = "98%")
           ),
           
           tabPanel("3-Map", 
                    "Select an ecosystem at Tab.2 and the map will update within a few seconds",
                    br(),
                    #Include this verbatim txt to find out how to make row_selected work
                    h5("The currently selected eco-species ID is: "), verbatimTextOutput("testEcoSpSelected"),
                    leaflet::leafletOutput("myMap", width = "98%", height = "74vh") %>% withSpinner()
           ),
           
           #Tab 4 in simple vertical option
           #tabPanel("4-Main stats", 
           #        splitLayout(cellWidths = c("50%", "50%"),
           #                    DTOutput(outputId = "myKBAStats", width = "98%"),
           #                    DTOutput(outputId = "myRLEStats", width = "98%")
           #        ) #Close the splitLayout
           #),
           
           #Tab 4 built using shinyUI, i.e. gridlayout; Didn't work, but I edited using simply fluidRow
           tabPanel(
             title = "4-Main stats",
             fluidRow(
               column(width = 12,
                      DTOutput(
                        outputId = "myRLEStats",
                        width = "100%"
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      DTOutput(
                        outputId = "myKBAStats",
                        width = "100%"
                      )
               )
             )
           ),
           
           tabPanel("5-Occurrences", 
                    DTOutput(outputId = "myEcoSpeciesOccurrences", width = "98%")
           ),
           
           tabPanel("6-IUCN typology", 
                    #DTOutput(outputId = "myIUCNTypology", width = "98%")#,
                    h5("WORK IN PROGRESS: This page will show how we propose to integrate ecosystem-types conceptualized locally using our approach within the new IUCN typology.")
                    ######leaflet::leafletOutput("myTestMap")
           ),
           
           tabPanel("7-BIO typology", 
                    #DTOutput(outputId = "myBIOTypology", width = "98%")
                    h5("WORK IN PROGRESS: This page will show the correspondances between the 2022 IUCN typology and the variation of it used in our work.")
           ),
           
           tabPanel("8-KBA potential triggers (species & ecosystems)", 
                    #DTOutput(outputId = "myKBATriggers", width = "98%")
                    fluidRow(
                      column(width = 3,
                             selectInput(inputId="kbaSelect", label = "Select a KBA", choices = kbaList, selected="Montagne Planneau proposed National Park")
                      ),
                      column(width = 9,br(), "Table 8. List & Map of potential KBA triggers (species & ecosystems) for the selected KBA. ratio = % of the global distribution; ratio = % of the global distribution still in a natural state; threatNat = National Red List status; threatGlo = Global RL status.")
                    ),
                    fluidRow(
                      splitLayout(cellWidths = c("60%", "40%"),
                                  box(width = 7.2, DTOutput(outputId = "myKBATriggers")),#, width = "98%")),
                                  leaflet::leafletOutput("myKBAMap", width = "98%", height = "65vh") %>% withSpinner()
                      ) #Close the splitLayout
                      #column(width = 6,
                      #       DTOutput(outputId = "myKBATriggers", width = "100%")
                      #       ),
                      #column(width = 6,
                      #       leaflet::leafletOutput("myKBAMap", width = "98%", height = "75vh")#"lalala"#put the KBA map
                      #       )
                    )
           )
           
    ) #Close the tabsetPanel (tabBox in shinydashboard terms)
    
  ) #Close the body
  
) #Closing the UI dashboard page


##########3. Shiny Server
server <- function(session, input, output) {
  
  ###render Tab2
  output$myEcoSpeciesList <- renderDT({
    DT::datatable(ecoSpeciesNames[,1:8], 
                  options = list(pageLength = 10),
                  escape = FALSE,
                  selection = 'single'#, filter = "top"
    ) %>% 
      DT::formatStyle(columns= 1:8, lineHeight='90%')
  })
  
  ###Get the ecoSpeciesID selected as a reactive value
  ecoSpSelected_r <- reactive({
    #Check that there is even a row selected
    if(length(input$myEcoSpeciesList_rows_selected) > 0) {
      #Get the index (n°) of the selected row in input$myEcoSpeciesList
      rowSelected <- input$myEcoSpeciesList_rows_selected
      #Get that row using the index found and the DT table where selection was done
      rowi <- ecoSpeciesNames[as.numeric(rowSelected):as.numeric(rowSelected),]
      rowi$synof_ecoSpeciesID #OK IT IS WORKING
    } else {"NA"}
  })
  
  #Include this verbatim txt to find out how to make row_selected work
  output$testEcoSpSelected <- renderPrint({
    #Check that there is even a row selected
    if(length(input$myEcoSpeciesList_rows_selected) > 0) {
      #Get the index (n°) of the selected row in input$myEcoSpeciesList
      rowSelected <- input$myEcoSpeciesList_rows_selected
      #Get that row using the index found and the DT table where selection was done
      rowi <- ecoSpeciesNames[as.numeric(rowSelected):as.numeric(rowSelected),]
      paste0(rowi$synof_ecoSpeciesID, " (", rowi$ecoSpecies, ")") #OK IT IS WORKING
    } else {NA}
  })
  
  ###Render Tab 4 myKBAStats
  output$myKBAStats <- renderDT({
    req(ecoSpSelected_r())
    if(ecoSpSelected_r() != "NA") {
      KBAStatsAll_ecoSpi <- KBAStatsAll[KBAStatsAll$ecoSpeciesID == ecoSpSelected_r(),]
    } else {
      #If none yet selected, just show a subset of 1000 entries
      KBAStatsAll_ecoSpi <- KBAStatsAll
    }
    
    KBAStatsAll_ecoSpi <- with(KBAStatsAll_ecoSpi, KBAStatsAll_ecoSpi[order(-area_ha), ])
    DT::datatable(KBAStatsAll_ecoSpi %>% dplyr::select(kbaName, status, KBAarea_ha, ecoSpecies, area_ha, ratioOfTotal, ratioOfTotalNat, isTrigger) %>% rename(ratio = ratioOfTotal, ratioNat = ratioOfTotalNat),
                  caption = "Table 4b. Relative importance of each ecosystem-type (eco-species) for each KBA. area_ha = hectares within the KBA; ratio = % of the global distribution located within the KBA; ratioNat = % of the global distribution still in natural/preserved state located within the KBA; isTrigger = list of the KBA criteria triggering a global KBA status.",
                  options = list(pageLength = 4,
                                 scrollX = TRUE,
                                 columnDefs = list(
                                   list(
                                     targets = 3,
                                     render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 70 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 70) + '...</span>' : data;",
                                       "}")))),
                  escape = FALSE,
                  rownames = FALSE) %>% DT::formatStyle(columns= 1:8, lineHeight='90%')
  })
  
  ###Render Tab 4 myRLEStats
  output$myRLEStats <- renderDT({
    req(ecoSpSelected_r())
    if(ecoSpSelected_r() != "NA") {
      ecoSpeciesDataDT_ecoSpi <- ecoSpeciesDataDT[ecoSpeciesDataDT$ecoSpeciesID == ecoSpSelected_r(),]
    } else {
      #If none yet selected, just show a subset of 1000 entries
      ecoSpeciesDataDT_ecoSpi <- ecoSpeciesDataDT
    }
    
    #ecoSpeciesDataDT_ecoSpi <- with(ecoSpeciesDataDT_ecoSpi, ecoSpeciesDataDT_ecoSpi[order(-area_ha), ])
    DT::datatable(ecoSpeciesDataDT_ecoSpi %>% dplyr::select(-ecoSpeciesID), 
                  caption = "Table 4a. Red List parameters and status for the evaluated ecosystem-types. nOcc = Number of occurrences; AreaTotPre_ha = hectares at prehuman state; AreaTot_ha = hectares at current state; ratioPre = % of current cover relative to prehuman state; ratioNat = % of current cover still in a natural/preserved state; ratioPA = % of current area located in Protected Areas; AOO2km = Area Of Occurrence with a 2km grid size; AOO10km: AOO with 10km grid size; AOO2kmNat & AOO10kmNat = idem but considering only natural/preserved occurrences; EOO = Extent Of Occurrence (km²).",
                  options = list(pageLength = 2,
                                 scrollX = TRUE,
                                 columnDefs = list(
                                   list(
                                     targets = 0,
                                     render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 60 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 60) + '...</span>' : data;",
                                       "}")))),
                  escape = FALSE,
                  rownames = FALSE) %>% DT::formatStyle(columns= 1:8, lineHeight='90%')
  })
  
  ###render Tab5
  output$myEcoSpeciesOccurrences <- renderDT({
    #Filter the occurrences of the selected ecoSpi
    req(ecoSpSelected_r())
    if(ecoSpSelected_r() != "NA") {
      EDlastEidentDT_ecoSpi <- EDlastEidentDT[EDlastEidentDT$ecoSpeciesID == ecoSpSelected_r(),]
    } else {
      #If none yet selected, just show a subset of 1000 entries
      EDlastEidentDT_ecoSpi <- EDlastEidentDT[sample(.N, 1000)]
    }
    
    DT::datatable(setDT(EDlastEidentDT_ecoSpi %>% dplyr::select(-isMappable, -biotypeStatus, -decimalLongitude, -decimalLatitude, -ecoSpeciesID)),
                  options = list(scrollX = TRUE,
                                 columnDefs = list(
                                   list(
                                     targets = 4:5,
                                     render = JS(
                                       "function(data, type, row, meta) {",
                                       "return type === 'display' && data.length > 50 ?",
                                       "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                                       "}")))),
                  escape = FALSE,
                  rownames = FALSE) %>% DT::formatStyle(columns= 1:8, lineHeight='90%')
  })
  
  ### Tab 6: myIUCNL3Typology
  output$myIUCNTypology <- renderDT({
    #DT::datatable(IUCNL3 %>% dplyr::select(IUCNL3Link, IUCNL2, IUCNL1) %>% rename(IUCNL3 = IUCNL3Link), 
    #DT::datatable(IUCNL3BIOCrossover[1,] %>% dplyr::select(BIOL1, factors, BIOL2ID, BIOL2, IUCNL3, IUCNL3Notes),
    DT::datatable(IUCNL3BIOCrossover %>% dplyr::select(BIOL1, factors, BIOL2ID, BIOL2, IUCNL3, IUCNL3Notes),
                  rownames = FALSE,
                  caption = "Table 6. TO BE ADDED LATER; Definition of the BIO typology level 2 and its correspondance to the IUCN level 3. Several IUCN level 3 classes are repeated at different places of the table, some are moved to different places, and some are simply renamed to be more explicit and avoid non-mutually exclusive classes.") %>% DT::formatStyle(columns= 1:8, lineHeight='90%')
  })
  
  ### Tab 7: myBIOTypology
  output$myBIOTypology <- renderDT({
    #DT::datatable(BIOL4[1,] %>% dplyr::select(IUCNL3_eco_functional_group, BIOL3_systematic_functional_group, BIOL4, BIOL4Notes) %>% rename(BIOL4_systematic_global_ecosystem = BIOL4), 
    DT::datatable(BIOL4 %>% dplyr::select(IUCNL3_eco_functional_group, BIOL3_systematic_functional_group, BIOL4, BIOL4Notes) %>% rename(BIOL4_systematic_global_ecosystem = BIOL4), 
                  rownames = FALSE,
                  #extensions=list('Buttons'),#, 'RowGroup'),
                  #options = list(pageLength=100,
                  #               dom='Bfrtip',
                  #              buttons=c('copy', 'csv' ,'excel')),
                  #rowGroup = list(1)),
                  escape = FALSE,
                  caption = "Table 7. TO BE ADDED LATER; Here we subdivide the BIO level3, into our level 4, based on another set of factors. The IUCN level 3 is in part describing the level of detail reached here, but not so consistently throughout the typology."
    ) %>% DT::formatStyle(columns= 1:8, lineHeight='90%')
  })
  
  
  ### Tab 8: myKBATriggers
  output$myKBATriggers <- renderDT({
    KBAk <- input$kbaSelect
    biotaKBATriggersMyKBA <- biotaKBATriggers %>% dplyr::filter(kbaName == KBAk, nchar(isTrigger) > 0) %>%
      dplyr::select(-kbaName, -status, -KBAarea_ha, -biotaID)
    DT::datatable(biotaKBATriggersMyKBA, 
                  rownames = FALSE,
                  #filter = "top",
                  #extensions=list('Buttons'),#, 'RowGroup'),
                  options = list(pageLength=10, 
                                 scrollX = T,
                                 autoWidth = TRUE,
                                 #I cannot reduce column width and I don't understand why
                                 columnDefs = list(list(targets=c(0), width='20px'))
                                 #columnDefs = list(list(width = "20%", targets = 0))
                                 #               dom='Bfrtip',
                                 #              buttons=c('copy', 'csv' ,'excel')),
                                 #rowGroup = list(1)
                  ),
                  #escape = FALSE
    ) %>% DT::formatStyle(columns= 1:8,lineHeight='90%') #%>%
    #DT::formatStyle(columns = 0, width='20px')
  })
  
  ### Tab8 myKBAMap (right side of window)
  output$myKBAMap <- renderLeaflet({
    #Get list of ecoSp triggers for the chosen KBAk
    KBAk <- input$kbaSelect #e.g."Collines Du Sud proposed Ecological Reserve"
    mapKBAecoSpTriggers <- biotaKBATriggers %>% dplyr::filter(type == "ecosystem", kbaName == KBAk, nchar(isTrigger) >0)
    
    #Create a Loop on list above
    #Get an empty ecoSpi_cur_cv_Po
    ecoSp_list_Po <- sf::st_read("datainput/ecoSp_20230109130519.shp", crs = 4326, quiet = TRUE) %>% dplyr::filter(nat == 1000)
    
    for (i in 1:nrow(mapKBAecoSpTriggers)){
      #Get the ième ecoSpeciesID of the list
      ecoSpeciesIDi <- mapKBAecoSpTriggers[i,]$biotaID #e.g. 118=lowland mesic forest=20230109130519
      
      if(file.exists(paste0("datainput/ecoSp_",ecoSpeciesIDi,".shp"))) {
        ecoSpi_list_Po <- sf::st_read(paste0("datainput/ecoSp_",ecoSpeciesIDi,".shp"), crs = 4326, quiet = TRUE)
        ecoSp_list_Po <- rbind(ecoSp_list_Po, ecoSpi_list_Po)
      }
    }
    
    #Now get the list of spTriggers
    mapKBAspTriggers <- biotaKBATriggers %>% dplyr::filter(type == "species", kbaName == KBAk, nchar(isTrigger) >0) %>% rename(taxonID = biotaID)
    
    SDint = data.table::fread("datainput/SDint_sensitive.txt", header = TRUE, sep = "\t", dec = ".", encoding = "UTF-8") %>% dplyr::select(taxonID, decimalLatitude, decimalLongitude, recordedBy, recordNumber, eventDate, occurrenceStatus, country)
    #Filter out all mappable records of presence, in Seychelles only
    SDint$isMappable <- ifelse(!is.na(SDint$decimalLatitude) & SDint$decimalLatitude > -90 & SDint$decimalLatitude < 90 & !is.na(SDint$decimalLongitude) & SDint$decimalLongitude > -180 & SDint$decimalLongitude < 180, 1, 0)
    SDint <- SDint %>% dplyr::filter(isMappable == 1 & occurrenceStatus != "absent" & country == "Seychelles")
    SDint_sf <- st_as_sf(SDint, coords = c("decimalLongitude", "decimalLatitude"), crs=4326)
    
    KBAkEDlastEident_sf <- inner_join(SDint_sf, mapKBAspTriggers %>% dplyr::select(taxonID), by = "taxonID")
    
    KBASeyk <- KBASey %>% dplyr::filter(kbaName == KBAk)
    #mapPo <- ifelse(nrow(ecoSp_list_Po) > 0,mapview(ecoSp_list_Po), mapview())
    #mapPt <- ifelse(nrow(KBAkEDlastEident_sf) > 0,mapview(KBAkEDlastEident_sf), mapview())
    #mapKBAk <- mapview(ecoSp_list_Po) + mapview(KBAkEDlastEident_sf) + mapview(KBASeyk)
    if(nrow(ecoSp_list_Po) > 0) {
      mapPo <- mapview(ecoSp_list_Po)
    } else {mapPo <- mapview()}
    if(nrow(KBAkEDlastEident_sf) > 0) {
      mapPt <- mapview(KBAkEDlastEident_sf)
    } else {mapPt <- mapview()}
    mapKBAk <- mapPo + mapPt + mapview(KBASeyk)
    
    #Zoom to the selected KBA
    centoid <- st_centroid(KBASeyk$geometry)
    centroidcoord <- st_coordinates(centoid)
    
    mapKBAk@map %>% setView(centroidcoord[[1]], centroidcoord[[2]], zoom = 13)
    
    #Add distribution of all species that are threatened on the National Red List
    #Colour as fct of RL status and ratio (trigger layer) or just RL status (RL layer)
    #CR-EN = 4 * ratio
    #VU = 2 * ratio
    #other = ratio
  })
  
  output$myTestMap <- renderLeaflet({
    ecoSp_list_PoTest <- sf::st_read("ecoSp_20230109130519.shp", crs = 4326, quiet = TRUE)
    mapview(ecoSp_list_PoTest)@map
  })
  
  #################################################  
  ###render Tab3
  output$myMap <- renderLeaflet({
    req(ecoSpSelected_r())
    if(ecoSpSelected_r() != "NA") {
      
      ecoSpeciesIDi <- ecoSpSelected_r()
      
      # ######### Here I insert my super long script to get the map of a given ecoSpi
      
      EDi <- EDlastEident[EDlastEident$isMappable == 1 & EDlastEident$ecoSpeciesID == ecoSpeciesIDi,]
      
      EDi_Pt <- sf::st_as_sf(EDi, coords = c("decimalLongitude", "decimalLatitude"), crs=4326 )
      #Convert the text attribute 'ecoNaturalness' into a conservation value (cv) index like for raster
      EDi_Pt$nat <- ifelse(
        EDi_Pt$ecoNaturalness == "natural", 10,
        ifelse(EDi_Pt$ecoNaturalness == "semi-natural", 1, NA)
      )
      
      #Get the AOO and EOO to add to map
      ############################## Hope fully replace the next section by the script below
      #Filter the various elements to map for the ecoSpi out of the imported AOOEOOAll
      AOO2kmpolygon <- AOOEOOAll %>% dplyr::filter(ecoSpID == ecoSpeciesIDi, AOOEOO == "AOO2km")
      AOO10kmpolygon <- AOOEOOAll %>% dplyr::filter(ecoSpID == ecoSpeciesIDi, AOOEOO == "AOO10km")
      EOOpolygon <- AOOEOOAll %>% dplyr::filter(ecoSpID == ecoSpeciesIDi, AOOEOO == "EOO")
      AOO2kmpolygonNat <- AOOEOOAll %>% dplyr::filter(ecoSpID == ecoSpeciesIDi, AOOEOO == "AOO2kmNat")
      AOO10kmpolygonNat <- AOOEOOAll %>% dplyr::filter(ecoSpID == ecoSpeciesIDi, AOOEOO == "AOO10kmNat")
      
      #Get the shp saved from Raster ecoSpi
      if(file.exists(paste0("datainput/ecoSp_",ecoSpeciesIDi,".shp"))) {
        ecoSpi_cur_cv_Po <- sf::st_read(paste0("datainput/ecoSp_",ecoSpeciesIDi,".shp"), crs = 4326, quiet = TRUE)
      }
      ###############################
      
      
      # Map it all for the selected ecoSpi
      
      ##Set mapview options
      mapviewOptions(basemaps = c("Esri.WorldTopoMap", "OpenStreetMap", "Esri.WorldImagery"), legend = FALSE)
      
      ##Basemap: eco-occurrences + all other occurrences + exploration tracks + KBA
      if(nrow(EDi_Pt) >0) {
        EDi_Pt_Map <- EDi_Pt %>% dplyr::select(ecoOccurrenceID, recordedBy, recordNumber, eventDate, habitat, degreeOfInvasion, ecoNaturalness, ecoStage, nplots, nomenclaturalStatus, nat)
        EDi_Pt_Map <- mapview(EDi_Pt_Map, zcol = "nomenclaturalStatus") 
      } else {
        EDi_Pt_Map <- mapview()
      }
      
      EDother <- left_join(EDlastEident, EDi %>% dplyr::select(ecoOccurrenceID) %>% mutate(link = 1), by = "ecoOccurrenceID") %>% dplyr::filter(is.na(link) & isMappable == 1)
      EDother_Pt <- sf::st_as_sf(EDother, coords = c("decimalLongitude", "decimalLatitude"), crs=4326 )
      #Extract only those that are within the modeled distribution map
      EDother_Pt_Map <- EDother_Pt %>% dplyr::select(ecoOccurrenceID, recordedBy, recordNumber, eventDate, habitat, degreeOfInvasion, ecoNaturalness, ecoStage, nplots, nomenclaturalStatus)
      if(file.exists(paste0("datainput/ecoSp_",ecoSpeciesIDi,".shp")) & nrow(EDother_Pt) > 0) {
        EDother_Pt_MapFiltered <- st_intersection(EDother_Pt_Map, ecoSpi_cur_cv_Po)
        ecoSpi_cur_cv_Po_Map <- mapview(ecoSpi_cur_cv_Po)
        EDother_Map <- mapview(EDother_Pt_MapFiltered, cex = 4,layer.name="Other likely occurrences", hide = TRUE) #default cex is 8)
      } else {
        
        if(file.exists(paste0("datainput/ecoSp_",ecoSpeciesIDi,".shp"))) {
          ecoSpi_cur_cv_Po_Map <- mapview(ecoSpi_cur_cv_Po)
          EDother_Map <- mapview()
        } else {
          ecoSpi_cur_cv_Po_Map <- mapview()
          EDother_Map <- mapview() 
        }
        
      }
      
      if(nrow(AOO2kmpolygon)>1) {
        AOO2kmpolygon_Map <- mapview(AOO2kmpolygon, color="blue", alpha.regions = 0, lwd=0.5, legend=FALSE, homebutton=FALSE, hide = TRUE)
        AOO10kmpolygon_Map <- mapview(AOO10kmpolygon, color="blue", alpha.regions = 0, lwd=1.5, legend=FALSE, homebutton=FALSE, hide = TRUE)
        EOOpolygon_Map <- mapview(EOOpolygon, color="#50b397", alpha.regions=0, lwd=2, legend=FALSE, homebutton=FALSE)
      } else {
        AOO2kmpolygon_Map <- mapview()
        AOO10kmpolygon_Map <- mapview()
        EOOpolygon_Map <- mapview()
      }
      
      if(nrow(AOO2kmpolygonNat)>1) {
        AOO2kmpolygonNat_Map <- mapview(AOO2kmpolygonNat, color="blue", alpha.regions = 0, lwd=0.5, legend=FALSE, homebutton=FALSE)
        AOO10kmpolygonNat_Map <- mapview(AOO10kmpolygonNat, color="blue", alpha.regions = 0, lwd=1.5, legend=FALSE, homebutton=FALSE)
      } else {
        AOO2kmpolygonNat_Map <- mapview()
        AOO10kmpolygonNat_Map <- mapview()
      }
      
      myMap <- EDi_Pt_Map + EDother_Map + 
        AOO2kmpolygon_Map + AOO10kmpolygon_Map + EOOpolygon_Map +
        AOO2kmpolygonNat_Map + AOO10kmpolygonNat_Map +
        ecoSpi_cur_cv_Po_Map + mapview(KBASey, zcol = "protected", alpha.regions = 0.3, lwd=2, homebutton=FALSE, hide = TRUE) #reduce KBA opacity
      
      myMap <- myMap@map %>% leaflet::setView(55.552, -4.524, zoom = 10)
      myMap
      
    } else {
      
      #If none yet selected, just show a subset of 1000 entries
      EDlastEident_sf_ecoSpi <- EDlastEidentDT[EDlastEidentDT$isMappable == 1,]
      EDlastEident_sf_ecoSpi <- EDlastEident_sf_ecoSpi[sample(.N, 1000)]
      EDlastEident_sf_ecoSpi <- st_as_sf(EDlastEident_sf_ecoSpi, coords = c("decimalLongitude", "decimalLatitude"), crs=4326)
      myMap <- mapview(EDlastEident_sf_ecoSpi)#, zcol = "nomenclaturalStatus")
      myMap@map
    }
    
  })
  
} #Closing the Server function

shinyApp(ui = ui, server = server) 