#' Paper based Annex dashboard
#' Authors : Meklit Chernet, Turry Ouma, IITA
#' Last updated on : November 2021 (to include GH)
#' 
#setwd("C:/Users/User/Documents/ACAI/DASHBOARDS/paper based/Bu")

#C:\Users\User\Documents\ACAI\paper based\PaperbasedDashboard -v4 - Copy
#C:\Users\User\Documents\ACAI\paper based\PaperBasedAnnex - RW
library(tidyr)
require(plyr)
library(rgdal)
library(raster)
library(dismo)
library(maptools)
library(rgeos)
require(RColorBrewer)
require(graphics)
require(rasterVis)
library(sp)
library(shinyalert)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
require(ggrepel)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(grid)
#library(formattable)
library(shinybusy)
library(DT)


### SHINY SERVER ###

server = function(input, output, session) {
  #.............................................................................  
  # Show a modal on initiation of tool
  #.............................................................................
  shinyalert("Paper Based Tools Annex", "This tool contains tables and maps with advice on application rates of urea,
                         NPK fertilizer for cassava, as well as the expected root yield response. Response to fertilizer depends on soil
                         conditions and the time of planting.
                     
                 
                 
                 ", type = "info", timer = 5000, size = 'm',
             closeOnClickOutside = FALSE,
             closeOnEsc = FALSE,
             animation = TRUE,
             html = TRUE,
             
             showConfirmButton = FALSE,
             showCancelButton = FALSE,
             confirmButtonText = "OK",
             confirmButtonCol = "#AEDEF4")
 
  #............................................................................. 
  #spinner before maps are displayed
  #.............................................................................
  observeEvent(input$btn_go, {
  
    shinybusy::show_modal_spinner(
      spin = "cube-grid",
      #spin = "fading-circle",
      #spin = "fading-circle",
      
      color = 	"#228B22",
      #00FF00
      text = "Please wait while the map loads..."
    )
    Sys.sleep(6)
    remove_modal_spinner()
  })
  
  #.............................................................................
  #render select input options
  #.............................................................................

 
  output$province <- renderUI({

    pickerInput("province", "Select Province",
                choices = c("Cibitoke", "Bubanza", "Kayanza", "Kirundo","Ngozi", "Muyinga", "Karuzi", "Cankuzo", "Ruyigi","Muramvya", "Gitega", "Mwaro", "Bujumbura Rural",
                            "Bururi", "Rutana", "Makamba"),
                selected = NULL,
                multiple = TRUE,
                options = pickerOptions(maxOptions = 1))
  })
  
 
  observeEvent(input$province, {
    
    if(!is.null(input$province))  {
  output$plntmth <- renderUI({
        
      pickerInput("plntmth", "Select planting month",
                  choices = c("September","October", "November", "December"),
                  selected = NULL,
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1))
  })
    }
     
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth)) {
      output$costs <- renderUI({
        
        pickerInput("costs", "Would you like to specify your prices for cassava and fertilizers?",
                    choices = c("Yes", "No"),
                    selected = NULL,
                    multiple = TRUE,
                    options = pickerOptions(maxOptions = 1))
      })
  
    }
  })
  
  observeEvent(input$plntmth, {
    if(!is.null(input$plntmth))  {


      
  output$selection2 <- renderUI({
    
    pickerInput("selection2", "Select variable to view",
                                choices = c("FOMI-TOTAHAZA rate", "FOMI-IMBURA rate", "FOMI-BAGARA rate", "Expected yield response"),
                                selected = NULL,
                                multiple = TRUE,
                                options = pickerOptions(maxOptions = 1))
  })


    }
  })


  
  observeEvent(input$province, {
    if(!is.null(input$province))  {
      
      output$unit_loc <- renderUI({
        
        selectInput("unit_loc", "Select unit of land",
                    choices = c("acre", "ha"))
        

      })
    }
  }) 
  

  observeEvent(input$unit_loc, {
    if(!is.null(input$unit_loc))  {
      output$FCY_ha <- renderUI({
        
        selectInput("FCY_ha", "Select Your Current Yield (Tonnes)",
                    choices = c("0-7.5 t/hectare", "7.5-15 t/hectare", "15-22.5 t/hectare", "22.5-30 t/hectare", ">30 t/hectare",  ""),
                    selected = "")            
      })
      
      
      output$FCY_acre <- renderUI({
        
        selectInput("FCY_acre", "Select Your Current Yield (Tonnes)",
                    choices = c("0-3 t/acre", "3-6 t/acre", "6-9 t/acre", "9-12 t/acre", ">12 t/acre", ""),
                    selected = "")
      })
    }
    
  })  
  
  ######################################
#PRICE INCLUSION SCRIPT STARTS HERE
  ######################################
  
  # observeEvent(input$costs, {
  #   if(input$costs == "Yes" ) {
  #  
  # output$CassavaPrice <- renderUI({
  #   
  #   textInput("CassavaPrice", "Price of cassava per ton")
  # })
  #   }
  # })
  # 
  # 
  # 
  # observeEvent(input$costs, {
  #   if(input$costs == "Yes")  {
  #     
  #     output$FOMI_TOTAHAZAPrice <- renderUI({
  #       
  #       textInput("FOMI_TOTAHAZAPrice", "Cost of FOMI-TOTAHAZA per 50Kg bag")
  #     })
  #     
  #     output$FOMI_IMBURAPrice <- renderUI({
  #       
  #       textInput("FOMI_IMBURAPrice", "Cost of FOMI-IMBURAPrice per 50Kg bag")
  #     })
  #     
  #     output$FOMI_BAGARAPrice <- renderUI({
  #       
  #       textInput("FOMI_BAGARAPrice", "Cost of FOMI-BAGARAPrice per 50Kg bag")
  #     })
  #     
  #   }
  # })
  
    # 
    # observeEvent(input$costs, {
    #   if(input$costs == "Yes") {
    #   output$UreaPrice <- renderUI({
    #     
    #     textInput("UreaPrice", "Cost of Urea per 50Kg bag")
    #   })
    # }
    # })
# 
#   observeEvent(input$costs, {
#     if(input$costs == "Yes") {  
#       output$btn_go <- renderUI({
#         actionButton("btn_go", "Get Maps & Tables", icon("map"),
#                      style="color: #fff; background-color: green; border-color: #2e6da4")
#         
#       })
#     }else if(input$costs == "No"){
#       output$btn_go <- renderUI({
#         actionButton("btn_go", "Get Maps & Tables", icon("map"),
#                      style="color: #fff; background-color: green; border-color: #2e6da4")
#         
#       })
#     }
#     
#     
#   })
  
  
  observeEvent(input$selection2, {
    if(!is.null(input$selection2)) {  
      output$btn_go <- renderUI({
        actionButton("btn_go", "Get Maps & Tables", icon("map"),
                     style="color: #fff; background-color: green; border-color: #2e6da4")
        
      })
    }
      
    
  })
  
  
 #.................................................................................................................
  

  
#######################################################################################
      ## Read the GIS layers
#######################################################################################
  ###########################################################################
  ##  Burundi fertilizer recom for FCY 1:5
  ###########################################################################
  
  
  boundaryBu <- readOGR(dsn=getwd(), layer="gadm36_BDI_1")
  BuRegion <- readOGR(dsn=getwd(), layer="gadm36_BDI_2")
  
  FCY_FRData <- readRDS("Bu_FR_CassavaPaperBased.RDS") 
  FCY_FRData <- FCY_FRData[FCY_FRData$harvMonth == 12, ]
  
  
  FR_Bu_FCY1 <- FCY_FRData[FCY_FRData$FCY == "level1", ]
  FR_Bu_FCY2 <- FCY_FRData[FCY_FRData$FCY == "level2", ]
  FR_Bu_FCY3 <- FCY_FRData[FCY_FRData$FCY == "level3", ]
  FR_Bu_FCY4 <- FCY_FRData[FCY_FRData$FCY == "level4", ]
  FR_Bu_FCY5 <- FCY_FRData[FCY_FRData$FCY == "level5", ]
  head(FCY_FRData)
  
  unique(FCY_FRData$plw)
  
  FCY_FRData[FR_Bu_FCY2$plw == 42 & FCY_FRData$Commune == "Mugina" & FCY_FRData$FCY == "level2", ]

    addplm <- function(ds, country){
    ds$respY <- ds$TargetY - ds$CurrentY
    ds$groRev <- ds$NR + ds$TC
    ds$plm <- as.factor(ds$plw)
    
 
      ds$plm  <- ifelse(ds$plm %in% c(2), "January", 
                        ifelse(ds$plm %in% c(33), "August",
                               ifelse(ds$plm == 38, "September", 
                                      ifelse(ds$plm == 42, "October",
                                             ifelse(ds$plm == 46, "November", "December")))))
  
 
      ds$rateFOMI_TOTAHAZA <- ds$FOMI_TOTAHAZA
      ds$rateFOMI_IMBURA <- ds$FOMI_IMBURA
      ds$rateFOMI_BAGARA <- ds$FOMI_BAGARA 
    
    return(ds)
  }

  FR_Bu_FCY1_plm <- addplm(ds=FR_Bu_FCY1, country = "Bu") ## Bu if user current yield is level 1
  FR_Bu_FCY2_plm <- addplm(ds=FR_Bu_FCY2, country = "Bu") 
  FR_Bu_FCY3_plm <- addplm(ds=FR_Bu_FCY3, country = "Bu") 
  FR_Bu_FCY4_plm <- addplm(ds=FR_Bu_FCY4, country = "Bu") 
  FR_Bu_FCY5_plm <- addplm(ds=FR_Bu_FCY5, country = "Bu") 

 
      ###########################################################################
      ## select FCY and read the corresponding file 
      ## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
      ###########################################################################
      
      #.................................................................................................................
      #Dashboard activity starts here
      #............................................................................. 
      ## Determine platform type and set working directory accordingly
   
      
      
    observeEvent(input$btn_go, {
    
    #define reactive values
   # country <- input$country
  
    FCY_acre <- input$FCY_acre
    FCY_ha <- input$FCY_ha
   

    Selection2 <- input$selection2

    usecase <- input$usecase
    plantMonth <- input$plntmth
    state <- input$province
  
    lga_Groups <- input$province
  
    plantMonth <- input$plntmth
    cities <- input$city
    
    unit <- input$unit_loc
    UreaPrice <- as.numeric(input$UreaPrice)
    DAPPrice <- as.numeric(input$DAPPrice)

    NPK171717Price <- as.numeric(input$NPK171717Price)
    
    CassavaPrice <- as.numeric(input$CassavaPrice)

    costs <- input$costs


    print(unit)
    print(plantMonth)
    print(FCY_acre)
    
    #define the state category
    # if(state == "Southern"){
    #   lgaGroups <- "Amajyepfo"
    # }else if(state == "Kigali City"){
    #   lgaGroups <- "Umujyi wa Kigali"
    # }else if(state == "Western"){
    #   lgaGroups <- "Iburengerazuba"
    # }else if (state == "Eastern"){
    #   lgaGroups <- "Iburasirazuba"
    # }else if (state == "Nothern"){
    #   lgaGroups <- "Amajyaruguru"
    # }

    #specify yield categories
    
    if(unit == 'hectare'){
      yield_level <- ifelse( FCY_ha == "0-7.5 t/hectare", "a low yield level",
                             ifelse( FCY_ha == "7.5-15 t/hectare","a normal yield level",
                                     ifelse( FCY_ha == "15-22.5 t/hectare","a medium yield level", 
                                             ifelse( FCY_ha == "22.5-30 t/hectare","a high yield level",
                                                     ifelse( FCY_ha == ">30 t/hectare","a very high yield level"
                                                     )))))
    }else if(unit == 'acre'){ 
      yield_level <- ifelse( FCY_acre == "0-3 t/acre","a low yield level",
                             ifelse( FCY_acre == "3-6 t/acre","a normal yield level",
                                     ifelse( FCY_acre == "6-9 t/acre","a medium yield level",
                                             ifelse( FCY_acre == "9-12 t/acre","a high yield level",
                                                     ifelse( FCY_acre == ">12 t/acre","a very high yield level")
                                             ))))
    }
    
    print(yield_level)
    
    #lgaGroups <- "Cibitoke"
    lgaGroups <- input$province
    
    lgaGroups2 <- input$province
    
    #define the yield category
    if (unit == "hectare"){
      FCY <- FCY_ha
      
      if(FCY == "7.5-15 t/hectare" ){
        ds <- FR_Bu_FCY2_plm
      }else if(FCY == "0-7.5 t/hectare" ){
        ds <- FR_Bu_FCY1_plm
      }else if(FCY == "15-22.5 t/hectare" ){
        ds <- FR_Bu_FCY3_plm
      }else if(FCY == "22.5-30 t/hectare"){
        ds <- FR_Bu_FCY4_plm
      }else if(FCY == ">30 t/hectare" ){
        ds <- FR_Bu_FCY5_plm
      }
    }else if(unit == "acre"){
      FCY <- FCY_acre
      
      if(FCY == "3-6 t/acre" ){
        ds <- FR_Bu_FCY2_plm
      }else if(FCY == "0-3 t/acre" ){
        ds <- FR_Bu_FCY1_plm
      }else if(FCY == "6-9 t/acre" ){
        ds <- FR_Bu_FCY3_plm
      }else if(FCY == "9-12 t/acre" ){
        ds <- FR_Bu_FCY4_plm
      }else if(FCY == ">12 t/acre" ){
        ds <- FR_Bu_FCY5_plm
        
      }
    }
      
    #ds <- FR_RW_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm
    #unique(ds$STATE)
     ###########################################################################
     ## Burundi
     ## Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
     ###########################################################################
     #ds <- FR_Bu_FCY2_plm
     #unique(ds$Province)
     ds$STATE <- ds$REGION <- ds$Province
     Cibitoke <- droplevels(ds[ds$STATE %in% c("Cibitoke", "Bubanza", "Kayanza"), ])
     Cibitokelabel <- data.frame(state= c("Cibitoke", "Bubanza", "Kayanza"), lat=c(-2.61, -3.28, -2.77), lon=c(29.32, 29.4, 29.6))
     
     Kirundo <- droplevels(ds[ds$STATE %in% c("Kirundo", "Ngozi", "Muyinga"), ])
     Kirundolabel <- data.frame(state= c("Kirundo", "Ngozi", "Muyinga"), lat=c(-2.35, -2.7, -3.0), lon=c(30.15, 29.8, 30.2))
     
     Karuzi <- droplevels(ds[ds$STATE %in% c("Karuzi", "Cankuzo", "Ruyigi"), ])
     Karuzilabel <- data.frame(state= c("Karuzi", "Cankuzo","Ruyigi"), lat=c(-2.95, -2.9, -3.58), lon=c(30.0, 30.73, 30.6))
     
     Muramvya <- droplevels(ds[ds$STATE %in%  c("Muramvya", "Gitega", "Mwaro", "Bujumbura Rural"), ])
     Muramvyalabel <- data.frame(state = c("Muramvya", "Gitega", "Mwaro", "Bujumbura Rural"), lat=c(-3.05, -3.8, -3.7, -3.2), lon=c(29.7, 30.06, 29.6, 29.4))
     
     Bururi <- droplevels(ds[ds$STATE %in% c("Bururi", "Rutana", "Makamba"), ])
     Bururilabel <- data.frame(state= c("Bururi", "Rutana", "Makamba"), lat=c(-3.65, -3.6, -4.25), lon=c(29.35, 30.1, 30.2))
     
     
     Cibitokecity <- data.frame(REGION = c("Cibitoke", "Bubanza", "Kayanza"),name=c("Cibitoke", "Bubanza", "Kayanza"), lat=c(-2.9, -3.1, -2.9), lon = c(29.1, 29.4, 29.6))
     Kirundocity <- data.frame(REGION = c("Kirundo","Ngozi", "Muyinga"),name=c("Kirundo","Ngozi", "Muyinga"), lat=c(-2.6, -2.9, -2.8), lon = c(30.1, 29.8, 30.3))
     Karuzicity <- data.frame(REGION = c("Karuzi", "Cankuzo", "Ruyigi"),name=c("Karuzi", "Cankuzo", "Ruyigi"), lat=c(-3.1, -3.2, -3.5), lon = c(30.1, 30.5, 30.2))
     Muramvyacity <- data.frame(REGION = c("Muramvya", "Gitega", "Mwaro", "Bujumbura Rural"),name=c("Muramvya", "Gitega", "Mwaro", "Isale"), lat=c(-3.3, -3.4, -3.5, -3.3), lon = c(29.6, 29.9, 29.7, 29.5))
     Bururicity <- data.frame(REGION = c("Bururi", "Rutana", "Makamba"),name=c("Bururi", "Rutana", "Makamba"), lat=c(-3.9, -3.9, -4.1), lon = c(29.6, 30.0, 29.8))
     

     ##############################################################################################################
     ## RW: mapping: for every Region, maps will be made per planting month and based on user selection for ha or acre
     ##############################################################################################################   

    if(lgaGroups == "Cibitoke"){
      LGApoints <- Cibitoke
      stateLabel <- Cibitokelabel
      textangle<-0
      cities = Cibitokecity
      engname = "Cibitoke"
    }else if(lgaGroups == "Bubanza"){
      LGApoints <- Cibitoke
      stateLabel <- Cibitokelabel
      textangle<-0
      cities = Cibitokecity
      engname = "Bubanza"
    }else if(lgaGroups == "Kayanza"){
      LGApoints <- Cibitoke
      stateLabel <- Cibitokelabel
      textangle<-0
      cities = Cibitokecity
      engname = "Kayanza"
    } else if(lgaGroups =="Kirundo"){
      LGApoints <- Kirundo
      stateLabel <- Kirundolabel
      textangle<-0
      cities = Kirundocity
      couple <- "One"
      engname = "Kirundo"

      }else if(lgaGroups =="Ngozi"){
        LGApoints <- Kirundo
        stateLabel <- Kirundolabel
        textangle<-0
        cities = Kirundocity
        couple <- "One"
        engname = "Ngozi"
        
      }else if(lgaGroups =="Muyinga"){
        LGApoints <- Kirundo
        stateLabel <- Kirundolabel
        textangle<-0
        cities = Kirundocity
        couple <- "One"
        engname = "Muyinga"
        
      }else if(lgaGroups =="Karuzi"){
      LGApoints <- Karuzi
      stateLabel <- Karuzilabel
      textangle<-0
      cities = Karuzicity
      couple <- "One"
      engname = "Karuzi"

     }  else if(lgaGroups =="Cankuzo"){
       LGApoints <- Karuzi
       stateLabel <- Karuzilabel
       textangle<-0
       cities = Karuzicity
       couple <- "One"
       engname = "Cankuzo"
       
     }  else if(lgaGroups =="Ruyigi"){
       LGApoints <- Karuzi
       stateLabel <- Karuzilabel
       textangle<-0
       cities = Karuzicity
       couple <- "One"
       engname = "Ruyigi"
       
     }else if(lgaGroups =="Muramvya"){
      LGApoints <- Muramvya
      stateLabel <- Muramvyalabel
      textangle<-0
      cities = Muramvyacity
      couple <- "One"
      engname = "Muramvya"

     }else if(lgaGroups =="Gitega"){
       LGApoints <- Muramvya
       stateLabel <- Muramvyalabel
       textangle<-0
       cities = Muramvyacity
       couple <- "One"
       engname = "Gitega"
       
     }else if(lgaGroups =="Mwaro"){
       LGApoints <- Muramvya
       stateLabel <- Muramvyalabel
       textangle<-0
       cities = Muramvyacity
       couple <- "One"
       engname = "Mwaro"
       
     }else if(lgaGroups =="Bujumbura Rural"){
       LGApoints <- Muramvya
       stateLabel <- Muramvyalabel
       textangle<-0
       cities = Muramvyacity
       couple <- "One"
       engname = "Bujumbura Rural"
       
     }else if(lgaGroups =="Bururi"){
       LGApoints <- Bururi
       stateLabel <- Bururilabel
       textangle<-0
       cities = Bururicity
       couple <- "One"
       engname = "Bururi"
       
     }else if(lgaGroups =="Rutana"){
       LGApoints <- Bururi
       stateLabel <- Bururilabel
       textangle<-0
       cities = Bururicity
       couple <- "One"
       engname = "Rutana"
       
     }else if(lgaGroups =="Makamba"){
       LGApoints <- Bururi
       stateLabel <- Bururilabel
       textangle<-0
       cities = Bururicity
       couple <- "One"
       engname = "Makamba"
       
     }
     
     #plantMonth <- "November"
     plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$REGION %in% lgaGroups , ])
     
     
     AOI <- lgaGroups
     AOIMapS <- subset(boundaryBu, NAME_1 %in% AOI ) 
     
     AOIMap <- subset(BuRegion, NAME_1 %in% AOI )
     AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
     LGAnames <- as.data.frame(AOIMap)
     LGAnames <- cbind(LGAnames, coordinates(AOIMap))
     colnames(LGAnames) <- c("REGION","Commune","long","lat")
     crop_ngstate <- subset(BuRegion, NAME_1 %in% AOI )
     
     
     ## take REGION average
     LGAaverage <- ddply(plotData, .( Commune, REGION), summarize,
                         LGAFOMI_TOTAHAZA = round(mean(rateFOMI_TOTAHAZA), digits=0),
                         LGAFOMI_IMBURA = round(mean(rateFOMI_IMBURA), digits=0),
                         LGAFOMI_BAGARA = round(mean(rateFOMI_BAGARA), digits=0),
                         LGAdY = round(mean(respY), digits=0))
     
     
     LGAaverage$LGAFOMI_TOTAHAZA <- ifelse(LGAaverage$LGAFOMI_TOTAHAZA <25, 0, LGAaverage$LGAFOMI_TOTAHAZA)
     LGAaverage$LGAFOMI_IMBURA <- ifelse(LGAaverage$LGAFOMI_IMBURA <25, 0, LGAaverage$LGAFOMI_IMBURA)
     LGAaverage$LGAFOMI_BAGARA <- ifelse(LGAaverage$LGAFOMI_BAGARA <25, 0, LGAaverage$LGAFOMI_BAGARA)
     
     
     plotData <- merge(plotData, LGAaverage, by=c("Commune", "REGION"))
     
     
     plotData$FOMI_TOTAHAZA <- round(plotData$LGAFOMI_TOTAHAZA/10)*10
     plotData$FOMI_IMBURA <- round(plotData$LGAFOMI_IMBURA/10)*10
     plotData$FOMI_BAGARA <- round(plotData$LGAFOMI_BAGARA /10)*10
     plotData$dY <- round(plotData$LGAdY/1)*1
     
     
     fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
     
     AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA", "LGAdY")]),
                      by.x=c("NAME_1","NAME_2") ,by.y=c("REGION","Commune"))
     AOIMap2$month <- plantMonth
     # AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
     plotData$month <- plantMonth
     tt_bu <- unique(as.data.frame(plotData[, c("REGION","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA", "LGAdY")]))
     tt_bu <- tt_bu[order(tt_bu$REGION, tt_bu$Commune), ]
     colnames(tt_bu) <- c("Province","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA", "Expected yield response (t)")
    # write.csv(tt_bu, paste(getwd(), fileNameCsv, sep="/"), row.names = FALSE)
     AOIMap3 <- st_as_sf(AOIMap2)
    #
     #TABLES
     Currency <- "BIF"
     output$tabletext_bu <- renderText({
       
       
       paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
       
     })
     
     output$mytable <- renderDT({tt_bu},
                                rownames = FALSE, 
                                extensions = c('Buttons','FixedColumns'), 
                                options = list(dom = 'Bfrtip',
                                               pageLength = nrow(tt_bu),
                                               initComplete = DT::JS(
                                                 "function(settings, json) {",
                                                 "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
                                                 "}"),
                                               
                                               buttons = list(
                                                 list(extend = 'excel', 
                                                      filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                      title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY, ".", sep="")),
                                                 list(extend = 'pdf',
                                                      filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
                                                      title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  ".", sep=""),
                                                      header = TRUE)
                                               )
                                               
                                )
     )
     
     #PRICE TABLES START HERE
     
     # if(costs == "No"){
     #   output$tabletext_bu <- renderText({
     #     
     #     
     #     paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY, ".", sep="")
     #     
     #   })
     #   
     #   output$mytable <- renderDT({tt_bu},
     #                              rownames = FALSE, 
     #                              extensions = c('Buttons','FixedColumns'), 
     #                              options = list(dom = 'Bfrtip',
     #                                             pageLength = nrow(tt_bu),
     #                                             initComplete = DT::JS(
     #                                               "function(settings, json) {",
     #                                               "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
     #                                               "}"),
     #                                             
     #                                             buttons = list(
     #                                               list(extend = 'excel', 
     #                                                    filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
     #                                                    title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY, ".", sep="")),
     #                                               list(extend = 'pdf',
     #                                                    filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
     #                                                    title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  ".", sep=""),
     #                                                    header = TRUE)
     #                                             )
     #                                             
     #                              )
     #   )
     #   
     # }else if (costs == "Yes"){
     #   #colnames(tt) <- c("State","LGA", "Recommended urea rate (kg/ha)", "NPK15_15_15 rate", "Expected yield response", "Planting month")
     #   tt_dataframe2 <- reactive({
     #     
     #     # df_tt2 <- data.frame(FOMI_TOTAHAZAPrice=4000,CassavaPrice=5000,FOMI_IMBURAPrice=6000,
     #     #                      FOMI_BAGARAPrice=3000,Province="Cibitoke")
     #     # 
     #    
     #     df_tt2 <- data.frame(FOMI_TOTAHAZAPrice=as.numeric(input$FOMI_TOTAHAZAPrice),CassavaPrice=as.numeric(input$CassavaPrice),FOMI_IMBURAPrice=as.numeric(input$FOMI_IMBURAPrice),
     #                          FOMI_BAGARAPrice=as.numeric(input$FOMI_BAGARAPrice),Province=input$province)
     #     
     #     return(df_tt2)
     #   })
     #   
     #   #tt_merge_bu <- merge(tt_bu, df_tt2,by="Province")
     #   
     #   tt_merge_bu <- merge(tt_bu, tt_dataframe2(),by="Province")
     #   colnames(tt_merge_bu) <- c("Province","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA", "LGAdY", "FOMI_TOTAHAZAPrice", "CassavaPrice", "FOMI_IMBURAPrice", "FOMI_BAGARAPrice")
     #   
     #   tt_merge_bu$totalSalePrice = as.numeric(tt_merge_bu$LGAdY)  * as.numeric(tt_merge_bu$CassavaPrice)
     #   tt_merge_bu$totalCost = (as.numeric(tt_merge_bu$FOMI_BAGARAPrice)/50 * as.numeric(tt_merge_bu$FOMI_BAGARA))+
     #     (as.numeric(tt_merge_bu$FOMI_IMBURAPrice)/50 * as.numeric(tt_merge_bu$FOMI_IMBURA))+
     #     
     #     (as.numeric(tt_merge_bu$FOMI_IMBURAPrice)/50 * as.numeric(tt_merge_bu$FOMI_IMBURA))
     #   
     #   tt_merge_bu$NetRevenue = as.numeric(tt_merge_bu$totalSalePrice) - as.numeric(tt_merge_bu$totalCost)
     #   
     #   tt_merge_bu2 <- dplyr::select(tt_merge_bu, c(Province, Commune, FOMI_TOTAHAZA, FOMI_IMBURA, FOMI_BAGARA, LGAdY, CassavaPrice, totalSalePrice, totalCost, NetRevenue))
     #   colnames(tt_merge_bu2) <- c("Province","Commune","FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA","Expected yield increase (t)", 
     #                               "Cassava Price", "Total sale (BIF)", "Fertilizer cost (BIF)", "Profit (BIF)")
     #   
     #   
     #   write.csv(tt_merge_bu2, fileNameCsv, row.names = FALSE)
     #   
     #   AOIMap3 <- st_as_sf(AOIMap2)
     #   
     #   output$tabletext_bu <- renderText({
     #     
     #     
     #     paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")
     #     
     #   })
     #   
     #   output$mytable2 <- renderDT({tt_merge_bu2},
     #                               rownames = FALSE, 
     #                               extensions = c('Buttons','FixedColumns'), 
     #                               options = list(dom = 'Bfrtip',
     #                                              pageLength = nrow(tt_merge_bu2),
     #                                              initComplete = DT::JS(
     #                                                "function(settings, json) {",
     #                                                "$(this.api().table().header()).css({'background-color': 'black', 'color': '#fff'});",
     #                                                "}"),
     #                                              
     #                                              buttons = list(
     #                                                list(extend = 'excel', 
     #                                                     filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
     #                                                     title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep="")),
     #                                                list(extend = 'pdf',
     #                                                     filename = paste('AKILIMO advice', '_', lgaGroups2, '_', plantMonth),
     #                                                     title = paste("AKILIMO advice for planting in ", plantMonth, ". Your current yield is between ", FCY,  " and the cassava price is ", CassavaPrice, " ", Currency, ".", sep=""),
     #                                                     header = TRUE)
     #                                              )
     #                                              
     #                               )
     #   )
     #   
     # }
     # 
     
      # --------------------------------------------------------------------------
      #side by side maps
      # --------------------------------------------------------------------------
      AOIMap3 <- st_as_sf(AOIMap2)
      ############################################################################   
      #FOMI_IMBURA plot
      #############################################################################
      #reactive title based on unit of land
     
     
      ttFOMI_IMBURA <- reactive({
        
        if(unit == "ha"){
          
          ttFOMI_IMBURA <- paste("Recommended FOMI-IMBURA rate (kg/ha)")
        }else {
          
          ttFOMI_IMBURA <- paste("Recommended FOMI-IMBURA rate(kg/acre)")
        }
      })
      
      
      
     FOMI_IMBURAsclae <- unique(AOIMap3$FOMI_IMBURA)
      kev <- as.character(FOMI_IMBURAsclae[order(FOMI_IMBURAsclae)])
      AOIMap3$FOMI_IMBURA <- factor(AOIMap3$FOMI_IMBURA)
      levels(AOIMap3$FOMI_IMBURA) <- kev
      
      #npk plot
      observeEvent(ttFOMI_IMBURA(),
                   {
                     
                     output$FOMI_IMBURAplot <- renderTmap({
                       
                       
                       
                       sm2 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "FOMI_IMBURA",
                           title = ttFOMI_IMBURA(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Purples")+
                         tm_text(text = "NAME_2")
                            
                       sm2
                       
                     }) 
                   })
      
      
      ############################################################################   
      #FOMI_TOTAHAZA  plot
      #############################################################################
      #reactive title based on unit of land  
      
      ttFOMI_TOTAHAZA <- reactive({
        
        if(unit == "ha"){
          
          ttFOMI_TOTAHAZA  <- paste("Recommended FOMI-TOTAHAZA  (kg/ha)")
        }else {
          
          ttFOMI_TOTAHAZA  <- paste("Recommended FOMI-TOTAHAZA  (kg/acre)")
        }
      })
      
      
      
      FOMI_TOTAHAZAsclae <- unique(AOIMap3$FOMI_TOTAHAZA)
      keFOMI_TOTAHAZA <- as.factor(FOMI_TOTAHAZAsclae[order(FOMI_TOTAHAZAsclae)])
      AOIMap3$FOMI_TOTAHAZA <- factor(AOIMap3$FOMI_TOTAHAZA)
      levels(AOIMap3$FOMI_TOTAHAZA) <- keFOMI_TOTAHAZA
      
      #FOMI_TOTAHAZA plot
      observeEvent(ttFOMI_TOTAHAZA(),
                   {
                     
                     output$FOMI_TOTAHAZAplot <- renderTmap({
                       
                       
                       
                       sm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "FOMI_TOTAHAZA",
                           title = ttFOMI_TOTAHAZA(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "Oranges", direction = -1)+
                         tm_text(text = "NAME_2")
                       sm4
                       
                     }) 
                   })
      
      ############################################################################   
      #FOMI_BAGARA plot
      #############################################################################
      #reactive title based on unit of land  
   
      ttFOMI_BAGARA <- reactive({
        
        if(unit == "ha"){
          
          ttmop <- paste("Recommended FOMI_BAGARA (kg/ha)")
        }else {
          
          ttmop <- paste("Recommended FOMI_BAGARA (kg/acre)")
        }
      })
      
      
      
      FOMI_BAGARAsclae <- unique(AOIMap3$FOMI_BAGARA)
      keFOMI_BAGARA <- as.factor(FOMI_BAGARAsclae[order(FOMI_BAGARAsclae)])
      AOIMap3$FOMI_BAGARA <- factor(AOIMap3$FOMI_BAGARA)
      levels(AOIMap3$FOMI_BAGARA) <- keFOMI_BAGARA
      
      #FOMI_BAGARA plot
      observeEvent(ttFOMI_BAGARA(),
                   {
                     
                     output$FOMI_BAGARAplot <- renderTmap({
                       
                       
                       
                       sm4 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "FOMI_BAGARA",
                           title = ttFOMI_BAGARA(),
                           #tm_borders(lwd = 1, col = "black", alpha = .5) +
                           # breaks = c(100, 110, 120, 130),
                           # labels = c("Low", "Medium", "High"),
                           palette = "GnBu", direction = -1)+
                         tm_text(text = "NAME_2")
                       sm4
                       
                     }) 
                   })
      
      ############################################################################   
      #yield plot
      #############################################################################
      #reactive title based on unit of land
      
      ttha <- reactive({
        
        if(unit == "ha"){
          
          ttha <- paste("Recommended Yield response (t/ha)")
        }else {
          
          ttha <- paste("Recommended Yield response (t/acre)")
          
        }
      })
      

      
      #yield plot
      observeEvent(ttha(),
                   {
                     
                     
                     output$yieldplot <- renderTmap({
                       
                       
                       sm3 = tm_shape(AOIMap3) +
                         tm_polygons(
                           col = "dY",
                           title = ttha(),
                           #breaks = c(3, 4, 5, 6),
                           #labels = c("Low", "Medium", "High"),
                           palette = "YlGnBu")+
                         tm_text(text = "NAME_2")
                       
                       sm3
                       
                     })
                   })
      
      #-------------------------------
      #generate downloadable maps
      #-------------------------------
      
      #generate color pallette
      ggFOMI_IMBURA <- NULL
      ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
      if(any(as.numeric(as.character(AOIMap3$FOMI_IMBURA))>0)){
        
        if(unit == "ha"){
          FOMI_IMBURAcols <- c("0"="#FFFFFF","30"= "#FFF7BC", "40"= "#FEE391", "50"= "#FEC44F", "60"= "#FE9929", 
                               "70"= "#EC7014", "80"= "#CC4C02")
          tt <- "FOMI-IMBURA (kg/ha)"
        }else{
          FOMI_IMBURAcols <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                               "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
          tt <- "FOMI-IMBURA (kg/acre)"
        }
        
        FOMI_IMBURAcols <- FOMI_IMBURAcols[names(FOMI_IMBURAcols) %in% AOIMap3$FOMI_IMBURA]
        
        FOMI_IMBURAsclae <- unique(AOIMap3$FOMI_IMBURA)
        kev <- as.character(FOMI_IMBURAsclae[order(FOMI_IMBURAsclae)])
        AOIMap3$FOMI_IMBURA <- factor(AOIMap3$FOMI_IMBURA)
        levels(AOIMap3$FOMI_IMBURA) <- kev
        
        ggFOMI_IMBURA <- ggplot(AOIMap3) +
          geom_sf(aes(fill=FOMI_IMBURA), col="darkgrey") +
          scale_fill_manual(values = FOMI_IMBURAcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=Commune, fontface=1, angle=textangle), size=4, segment.size = NA) + 
          geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
        
      }
      

      ggFOMI_BAGARA<- NULL
      if(any(as.numeric(as.character(AOIMap3$FOMI_BAGARA)) > 0)){
        FOMI_BAGARApalette <- brewer.pal(9,"YlGnBu")
        if(unit == "ha"){
          FOMI_BAGARAcols <- c("0"="#FFFFFF", "30" = "#FFFFD9" ,"40" ="#EDF8B1" ,"50"= "#C7E9B4", "60"= "#7FCDBB", "70"= "#41B6C4",
                               "80"= "#1D91C0","90"="#9999FF", "100"="#0000FF", "110"= "#225EA8", "120"= "#253494", "130"= "#081D58", "140" = "#FFFF00FF")
          tt <- "FOMI-BAGARA (kg/ha)"
        }else{
          FOMI_BAGARAcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                               "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
          tt <- "FOMI-BAGARA (kg/acre)"
        }
        
        FOMI_BAGARAcols <- FOMI_BAGARAcols[names(FOMI_BAGARAcols) %in% AOIMap3$FOMI_BAGARA]
        
        FOMI_BAGARAsclae <- unique(AOIMap3$FOMI_BAGARA)
        kev <- as.character(FOMI_BAGARAsclae[order(FOMI_BAGARAsclae)])
        AOIMap3$FOMI_BAGARA <- factor(AOIMap3$FOMI_BAGARA)
        levels(AOIMap3$FOMI_BAGARA) <- kev
        
        ggFOMI_BAGARA <- ggplot(AOIMap3) +
          geom_sf(aes(fill=FOMI_BAGARA), col="darkgrey") +
          scale_fill_manual(values = FOMI_BAGARAcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=Commune, fontface=1, angle=textangle), size=4, segment.size = NA) +
          geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
          xlab("") + ylab("") +
          ggtitle(tt) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8))
      }
      

      ggFOMI_TOTAHAZA <- NULL
      if(any(as.numeric(as.character(AOIMap3$FOMI_TOTAHAZA))>0)){
        if(unit == "ha"){
          # FOMI_TOTAHAZAcols <- c("0" = "#FFFFFF", "110"= "#E5F5E0", "120"= "#C7E9C0", "130"= "#A1D99B", "140"= "#74C476",
          #                        "150"= "#41AB5D", "160"= "#238B45", "170"="#006D2C", "180"= "#00441B", "190" = "#993404", "200"= "#662506")
          # 
          FOMI_TOTAHAZAcols <-  c("230"="#440154FF", "220"="#481A6CFF","210"="#472F7DFF", "200"="#414487FF", "190"="#39568CFF",
                                  "180"="#31688EFF", "170"="#2A788EFF", "160"="#23888EFF", "150"="#1F988BFF", "140"="#22A884FF",
                                  "130"="#35B779FF", "120"="#54C568FF", "110"="#7AD151FF", "110"="#A5DB36FF", "100"="#D2E21BFF", 
                                  "90"="#FDE725FF")
          
          ttz <- "FOMI-TOTAHAZA (kg/ha)"
        }else{#Meklit
          FOMI_TOTAHAZAcols <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                               "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
          ttz <- "FOMI-TOTAHAZA (kg/acre)"
        }
        
        FOMI_TOTAHAZAcols <- FOMI_TOTAHAZAcols[names(FOMI_TOTAHAZAcols) %in% AOIMap3$FOMI_TOTAHAZA]
        
        
        FOMI_TOTAHAZAsclae <- unique(AOIMap3$FOMI_TOTAHAZA)
        keU <- as.character(FOMI_TOTAHAZAsclae[order(FOMI_TOTAHAZAsclae)])
        AOIMap3$FOMI_TOTAHAZA <- factor(AOIMap3$FOMI_TOTAHAZA)
        levels(AOIMap3$FOMI_TOTAHAZA) <- keU
        
        require(ggrepel) 
        
        ggFOMI_TOTAHAZA <- ggplot(AOIMap3) +
          geom_sf(aes(fill=FOMI_TOTAHAZA), col="darkgrey") +
          scale_fill_manual(values = FOMI_TOTAHAZAcols, guide = guide_legend(reverse=TRUE))+
          geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.4) +
          geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
          geom_text_repel(data=LGAnames, aes(long, lat, label=Commune, fontface=1, angle=textangle), size=4, segment.size = NA) + 
          geom_text(data=stateLabel, aes(lon, lat, label=state, fontface=2), col='black', size=6)+
          geom_point(data=cities, aes(x=lon, y=lat), shape=19,  size=3) +
          annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
          annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                                 style = north_arrow_fancy_orienteering) +
          xlab("") + ylab("") +
          ggtitle(ttz) +
          theme_bw() +
          theme(legend.position="right", legend.title=element_blank(),
                plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
                axis.text = element_text(size=8)) 
      }
      
      
      
      
      
      if(unit == "ha"){
        Ydcols <- c( "17"= "#E8FF00FF", "16"="#A2FF00FF", "15"= "#5DFF00FF", "14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                     "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                     "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                     "1"= "#FF008BFF", "0"= "#FFFFFF")
        tt <- "Yield increase (t/ha)"
      }else{
        Ydcols <- c("14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                    "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                    "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                    "1"= "#FF008BFF", "0"= "#FFFFFF")
        tt <- "Yield increase (t/acre)"
      }
      
      
      
      Ysclae <- unique(AOIMap3$LGAdY)
      keY <- as.factor(Ysclae[order(Ysclae)])
      AOIMap3$dY <- factor(AOIMap3$LGAdY)
      levels(AOIMap3$dY) <- keY
      
      Ydcols <- Ydcols[names(Ydcols) %in% AOIMap3$dY]
      
      ggYield <- ggplot(AOIMap3) +
        geom_sf(aes(fill=dY), col="darkgrey") +
        scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
        geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
        geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
        geom_text_repel(data=LGAnames, aes(long, lat, label=Commune, fontface=1, angle=textangle), size=4, segment.size = NA) + 
        #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
        #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
        geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
        xlab("") + ylab("") +
        ggtitle(tt) +
        theme_bw() +
        theme(legend.position="right", legend.title=element_blank(),
              plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
              axis.text = element_text(size=8))
      
      
      #Combine plots together in pdf
      fileName <- paste("maps", ".pdf", sep="")
      pdf(fileName, onefile = TRUE, height = 14, width=12)
      #pdf.options(paper = "a4")
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.6, 4, 4, 0.4), "null"))))   
      grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
      print(ggFOMI_TOTAHAZA, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
      print(ggFOMI_IMBURA, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      print(ggFOMI_BAGARA, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
      print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
      dev.off()
      
      # Ureapalette <- brewer.pal(9,"Greens")
      # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])

      # #-------------------------------------------------------------------------
      # #front page dynamic tmap
      # #-------------------------------------------------------------------------
      # 

      #reactive selection of variable to view
      filt_select <- reactive({
        print(Selection2)
        if(Selection2 == "Expected yield response"){
          filt_select <- "Expected yield response"
        }else if (Selection2 == "FOMI-TOTAHAZA rate"){
          filt_select <- "FOMI-TOTAHAZA rate"
        }else if (Selection2 == "FOMI-IMBURA rate"){
          filt_select <- "FOMI-IMBURA rate"
        }else if(Selection2 == "FOMI-BAGARA rate"){
          filt_select <- "FOMI-BAGARA rate"
        }

      })


      #show map based on selection of variable but retaining single name

      #filter by variable selected and unit for color pallette
     if(filt_select() == "FOMI-TOTAHAZA rate"){

          ttFOMI_TOTAHAZA <- reactive({

            if(unit == "ha"){

              ttFOMI_TOTAHAZA <- paste("Recommended FOMI-TOTAHAZA rate (kg/ha)")
            }else {

              ttFOMI_TOTAHAZA <- paste("Recommended FOMI-TOTAHAZA rate (kg/are)")
            }
          })



          FOMI_TOTAHAZAsclae <- unique(AOIMap3$FOMI_TOTAHAZA)
          kev <- as.character(FOMI_TOTAHAZAsclae[order(FOMI_TOTAHAZAsclae)])
          AOIMap3$FOMI_TOTAHAZA <- factor(AOIMap3$FOMI_TOTAHAZA)
          levels(AOIMap3$FOMI_TOTAHAZA) <- kev

          observeEvent(ttFOMI_TOTAHAZA(),
                       {

                         output$tmapplot <- renderTmap({



                           sm2 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "FOMI_TOTAHAZA",
                               title = ttFOMI_TOTAHAZA(),
                               palette = "Oranges")+
                             tm_text(text = "NAME_2")

                           sm2

                         })
                       })

        }else if(filt_select() == "FOMI-IMBURA rate"){
          ttFOMI_IMBURA <- reactive({

            if(unit == "ha"){

              ttFOMI_IMBURA <- paste("Recommended FOMI-IMBURA rate (kg/ha)")
            }else {

              ttFOMI_IMBURA <- paste("Recommended FOMI-IMBURA rate (kg/are)")
            }
          })



          FOMI_IMBURAsclae <- unique(AOIMap3$FOMI_IMBURA)
          keFOMI_IMBURA <- as.factor(FOMI_IMBURAsclae[order(FOMI_IMBURAsclae)])
          AOIMap3$FOMI_IMBURA <- factor(AOIMap3$FOMI_IMBURA)
          levels(AOIMap3$FOMI_IMBURA) <- keFOMI_IMBURA

          observeEvent(ttFOMI_IMBURA(),
                       {

                         output$tmapplot <- renderTmap({



                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "FOMI_IMBURA",
                               title = ttFOMI_IMBURA(),
                               palette = "Purples", direction = -1)+
                             tm_text(text = "NAME_2")
                           sm4

                         })
                       })

        }else if(filt_select() == "FOMI-BAGARA rate"){
          ttFOMI_BAGARA <- reactive({
            
            if(unit == "ha"){
              
              ttFOMI_BAGARA <- paste("Recommended FOMI-BAGARA rate (kg/ha)")
            }else {
              
              ttFOMI_BAGARA <- paste("Recommended FOMI-BAGARA rate (kg/are)")
            }
          })
          
 
          FOMI_BAGARAsclae <- unique(AOIMap3$FOMI_BAGARA)
          kev <- as.character(FOMI_BAGARAsclae[order(FOMI_BAGARAsclae)])
          AOIMap3$FOMI_BAGARA <- factor(AOIMap3$FOMI_BAGARA)
          levels(AOIMap3$FOMI_BAGARA) <- kev
          
          observeEvent(ttFOMI_BAGARA(),
                       {
                         
                         output$tmapplot <- renderTmap({
                           
                           
                           
                           sm4 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "FOMI_BAGARA",
                               title = ttFOMI_BAGARA(),
                               palette = "GnBu", direction = -1)+
                             tm_text(text = "NAME_2")
                           sm4
                           
                         })
                       })
          
        }else if(filt_select() == "Expected yield response"){
          ttha <- reactive({

            if(unit == "ha"){

              ttha <- paste("Recommended yield increase (t/ha)")
            }else {

              ttha <- paste("Recommended yield increase (t/are)")

            }
          })


          # Ydcols <- unique(AOIMap3$dY)
          # keY <- as.factor(Ydcols[order(Ydcols)])
          # AOIMap3$dY <- factor(AOIMap3$dY)
          # levels(AOIMap3$dY) <- keY

          observeEvent(ttha(),
                       {


                         output$tmapplot <- renderTmap({


                           sm3 = tm_shape(AOIMap3) +
                             tm_polygons(
                               col = "dY",
                               title = ttha(),
                               palette = "YlGnBu")+
                             tm_text(text = "NAME_2")

                           sm3

                         })
                       })



        }


      
      
      if (unit == "acre"){
        
        #download acre printable guides
        output$downloadDatafr <- 
          
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (acre)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Acre latest.pdf", file)
            },
            contentType = "application/pdf"
          )
        
      }else if(unit == "ha"){
        #download hectare printable guides
        output$downloadDatafr <- 
          downloadHandler(
            filename <- function() {
              paste("FR Printable guides (ha)",  ".pdf", sep="")
            },
            
            content <- function(file) {
              file.copy("data/Tailored fertilizer application recommendations for cassava - Tanzania Hectare latest.pdf", file)
            },
            contentType = "application/pdf"
          ) 
        
        
      } 
 
    
    
    output$sidetext <- renderText({

 
      # paste0('<span style=\"background-color:', "color", '\ ">',text,' #<span style=\"font-size:8px;font-weight:bold;background-color:white;">',"ent_type",'</span></span>')
      paste("Maps and tables below present fertilizer recommendations for cassava planted in", plantMonth, "in", lgaGroups2, "province, in a field with", yield_level,
            ". Recommendations are optimized to obtain a maximal return on investment, assuming cassava will be harvested after 12 months.
              ")
    
    })
    

    
  
    #download maps
    output$downloadData <- downloadHandler(
      filename <- function() {
        paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
      },
      
      content <- function(file) {
        file.copy("maps.pdf", file)
      },
      contentType = "application/pdf"
    )
    
    #download tables
    output$downloadcsv <- downloadHandler(
      filename <- function() {
        paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
      },
      
      content <- function(file) {
        file.copy("tables.csv", file)
      },
      contentType = "application/csv"
    )
    
 
     })
  #})
  
}


#runApp(shinyApp(ui, server), launch.browser = TRUE)
#shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
