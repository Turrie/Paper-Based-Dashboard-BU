

library(dplyr)
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
require(sp)
library(ggthemes)
require(ggplot2)
library(gridExtra) 
library(hexbin)
library(viridis)
library(sf)
library(ggspatial)
library(grid)
require(qpdf)

###########################################################################
##  Burundi fertilizer recom for FCY 1:5
###########################################################################
FCY_FRData <- readRDS("/home/akilimo/projects/PaperbasedDashboard_v2/Bu_FR_CassavaPaperBased.RDS") 
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
  
  if(country == "Bu"){
    ds$plm  <- ifelse(ds$plm %in% c(2), "January", 
                      ifelse(ds$plm %in% c(33), "August",
                             ifelse(ds$plm == 38, "September", 
                                    ifelse(ds$plm == 42, "October",
                                           ifelse(ds$plm == 46, "November", "December")))))
  }else if(country == "RW"){
    ds$plm  <- ifelse(ds$plm %in% c(6,7,8,9), "February", 
                      ifelse(ds$plm %in% c(10,11,12,13,14), "March",
                             ifelse(ds$plm %in% c(37,38,39,40), "September", "October")))
    
  }else{
    levels(ds$plm)[levels(ds$plm) %in% 1:4]   <- "January"
    levels(ds$plm)[levels(ds$plm) %in% 5:8]   <- "February"
    levels(ds$plm)[levels(ds$plm) %in% 9:13]  <- "March"
    levels(ds$plm)[levels(ds$plm) %in% 14:17] <- "April"
    levels(ds$plm)[levels(ds$plm) %in% 18:22] <- "May"
    levels(ds$plm)[levels(ds$plm) %in% 23:26] <- "June"
    levels(ds$plm)[levels(ds$plm) %in% 27:30] <- "July"
    levels(ds$plm)[levels(ds$plm) %in% 31:35] <- "August"
    levels(ds$plm)[levels(ds$plm) %in% 36:39] <- "September"
    levels(ds$plm)[levels(ds$plm) %in% 40:43] <- "October"
    levels(ds$plm)[levels(ds$plm) %in% 44:48] <- "November"
    levels(ds$plm)[levels(ds$plm) %in% 49:53] <- "December"
  }
  
  
  if(country=="NG"){
    ds$rateUrea <- ds$urea
    ds$rateNPK151515 <- ds$NPK15_15_15
  }else if (country == "TZ"){
    ds$rateUrea <- ds$urea
    ds$rateNPK171717 <- ds$NPK17_17_17
    ds$rateDAP <- ds$DAP
  }else if (country == "GH"){
    ds$rateUrea <- ds$Urea
    ds$rateNPK112221 <- ds$NPK112221
    ds$rateNPK251010 <- ds$NPK251010
    ds$rateNPK152020 <- ds$NPK152020
    ds$rateNPK123017 <- ds$NPK123017  
  }else if (country == "RW"){
    ds$rateUrea <- ds$Urea
    ds$rateNPK171717 <- ds$NPK17_17_17
    ds$rateMOP <- ds$MOP
    ds$rateNDAP <- ds$DAP
  }else if (country == "Bu"){
    ds$rateFOMI_TOTAHAZA <- ds$FOMI_TOTAHAZA
    ds$rateFOMI_IMBURA <- ds$FOMI_IMBURA
    ds$rateFOMI_BAGARA <- ds$FOMI_BAGARA 
  }
  return(ds)
}






FR_Bu_FCY1_plm <- addplm(ds=FR_Bu_FCY1, country = "Bu") ## Bu if user current yield is level 1
FR_Bu_FCY2_plm <- addplm(ds=FR_Bu_FCY2, country = "Bu") 
FR_Bu_FCY3_plm <- addplm(ds=FR_Bu_FCY3, country = "Bu") 
FR_Bu_FCY4_plm <- addplm(ds=FR_Bu_FCY4, country = "Bu") 
FR_Bu_FCY5_plm <- addplm(ds=FR_Bu_FCY5, country = "Bu") 



###########################################################################
## Burundi
## Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
###########################################################################
ds <- FR_Bu_FCY2_plm
unique(ds$Province)
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
setwd("/home/akilimo/lintul/lintul/dataSources/GIS_layers")
boundaryBu <- readOGR(dsn=getwd(), layer="gadm36_BDI_1")
BuRegion <- readOGR(dsn=getwd(), layer="gadm36_BDI_2")


LGAMaps_Bu <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple, engname){
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
  tt <- unique(as.data.frame(plotData[, c("REGION","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA", "LGAdY", "month")]))
  tt <- tt[order(tt$REGION, tt$Commune), ]
  colnames(tt) <- c("PROVINCE","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA","FOMI_BAGARA", "LGAdY", "month")
  write.csv(tt, paste(getwd(), fileNameCsv, sep="/"), row.names = FALSE)
  AOIMap3 <- st_as_sf(AOIMap2)
  
  
  
  
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
      geom_text(data=stateLabel, aes(lon, lat, label=stateLabel$state, fontface=2), col='black', size=6)+
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
  
  
  fileName <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
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
  
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/Cibitoke_Bubanza_Kayanza_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in c("August", "September", "October", "November", "December", "January")){
  LGAMaps_Bu(plantMonth=m,  cities = Cibitokecity, lgaGroups = c("Cibitoke", "Bubanza", "Kayanza"), engname = c("Cibitoke", "Bubanza", "Kayanza"),
             LGApoints = Cibitoke, stateLabel = Cibitokelabel, textangle=0, unit="ha")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/Kirundo_Ngozi_Muyinga_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in c("August", "September", "October", "November", "December", "January")){
  LGAMaps_Bu(plantMonth=m,  cities = Kirundocity, lgaGroups = c("Kirundo", "Ngozi", "Muyinga"), engname = c("Kirundo", "Ngozi", "Muyinga"),
             LGApoints = Kirundo, stateLabel = Kirundolabel, textangle=0, unit="ha")
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/Karuzi_Cankuzo_Ruyigi_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in c("August", "September", "October", "November", "December", "January")){
  LGAMaps_Bu(plantMonth=m,  cities = Karuzicity, lgaGroups = c("Karuzi", "Cankuzo", "Ruyigi"), engname = c("Karuzi", "Cankuzo", "Ruyigi"),
             LGApoints = Karuzi, stateLabel = Karuzilabel, textangle=0, unit="ha")
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/Muramvya_Gitega_Mwaro_Bujumbura Rural_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in c("August", "September", "October", "November", "December", "January")){
  LGAMaps_Bu(plantMonth=m,  cities = Muramvyacity, lgaGroups = c("Muramvya", "Gitega", "Mwaro", "Bujumbura Rural"), 
             engname = c("Muramvya", "Gitega", "Mwaro", "Bujumbura Rural"),
             LGApoints = Muramvya, stateLabel = Muramvyalabel, textangle=0, unit="ha")
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/Bururi_Rutana_Makamba_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in c("August", "September", "October", "November", "December", "January")){
  LGAMaps_Bu(plantMonth=m,  cities = Bururicity, lgaGroups = c("Bururi", "Rutana", "Makamba"), 
             engname = c("Bururi", "Rutana", "Makamba"),
             LGApoints = Bururi, stateLabel = Bururilabel, textangle=0, unit="ha")
}



# ggUrea, ggNPK152020, ggNPK123017, ggYield
pdfTables_BU <- function(FERTtABLE, plantMonth, unit, REGION){
  row.names(FERTtABLE) = NULL
  FERTtABLE <- subset(FERTtABLE, select=-c(month))
  FERTtABLE$LGAdY <- round(FERTtABLE$LGAdY, digits=0)
  
  FERTtABLE <- FERTtABLE[ c("PROVINCE","Commune", "FOMI_TOTAHAZA", "FOMI_IMBURA", "FOMI_BAGARA", "LGAdY")]
  
  colnames(FERTtABLE) <- c("PROVINCE", "Commune", "FOMI-TOTAHAZA \n (kg/ha)", "FOMI-IMBURA \n (kg/ha)", 
                           "FOMI-BAGARA \n (kg/ha)", "Yield increase \n (t/ha)")
  
  FERTtABLE <- FERTtABLE[order(FERTtABLE$PROVINCE, FERTtABLE$Commune), ]
  fileNameR <- paste("Table","_", plantMonth, "_", REGION, ".pdf", sep="")
  
  pdf(fileNameR, width = 8, height = 12)
  pdf.options(paper = "a4")
  
  hj <- matrix(c(0, 0, -0.5,-0.5, -0.5, -0.5, -0.5), ncol=7, nrow=nrow(FERTtABLE), byrow=TRUE)
  tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
                        colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
                        rowhead = list(fg_params=list(cex = 0.8)))
  table <- tableGrob(FERTtABLE, rows = NULL, theme = tt1)
  
  title <- textGrob(paste("Planting in ", plantMonth, sep=""), gp = gpar(fontsize = 14))
  padding <- unit(0.4,"line")
  table <- gtable_add_rows(
    table, heights = grobHeight(title) + padding, pos = 0
  )
  table <- gtable_add_grob(
    table, list(title),
    t = 1, l = 1, r = ncol(table)
  )
  grid.newpage()
  grid.draw(table)
  dev.off()
  
}

tablepdf_BU <- function(unit, fname, REGION){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/", fname, sep=""))
  listcsv <- list.files(getwd(), "csv")
  library(gtable)
  for(files in c(listcsv, listcsv[1])){
    plantMonth <-  strsplit(files, '_')[[1]][1]
    csvtables <- unique(read.csv(files))
    pdfTables_BU(FERTtABLE = csvtables, plantMonth, unit=unit, REGION=REGION)
  }
}


tablepdf_BU(unit="ha", fname="Cibitoke_Bubanza_Kayanza_ha", REGION="Cibitoke_Bubanza_Kayanza")
tablepdf_BU(unit="ha", fname="Kirundo_Ngozi_Muyinga_ha", REGION="Kirundo_Ngozi_Muyinga")
tablepdf_BU(unit="ha", fname="Karuzi_Cankuzo_Ruyigi_ha", REGION="Karuzi_Cankuzo_Ruyigi")
tablepdf_BU(unit="ha", fname="Muramvya_Gitega_Mwaro_Bujumbura Rural_ha", REGION="Muramvya_Gitega_Mwaro_Bujumbura Rural")
tablepdf_BU(unit="ha", fname="Bururi_Rutana_Makamba_ha", REGION="Bururi_Rutana_Makamba")


lgaGroups <- c("Cibitoke_Bubanza_Kayanza", "Kirundo_Ngozi_Muyinga", "Karuzi_Cankuzo_Ruyigi",
               "Muramvya_Gitega_Mwaro_Bujumbura Rural", "Bururi_Rutana_Makamba")

for(p in lgaGroups){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/", p, "_ha", sep=""))
  getwd()
  p <- toupper(p)
  fileName_p <- paste(p,  ".pdf", sep="")
  print(fileName_p)
  b = "This tool contains tables and maps with advice for fertilizers application 
for cassva. The advice consider access to FOMI-TOTAHAZA, FOMI-IMBURA, and 
FOMI-BAGARA. Response to fertilizer depends on soil conditions and the 
time of planting. Tables and maps show the recommended fertilizer 
rates by district and month of planting for harvest after 
12 months." 
  
  
  pdf(fileName_p, paper="a4", pagecentre=FALSE, width=12,height=14)
  plot(NA, xlim=c(0.2,6), ylim=c(0,6), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  #text(0.4, 5, a, pos=4, cex=2.5) ## for Kagera etc
  text(0, 5, p, pos=4, cex=2)
  text(0, 1,b, pos=4, cex=1.1)
  #points(rep(1,4),1:4, pch=15)
  dev.off()
}


Combined_pdf_BU <- function(regionName, unit){
  
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/Bu/", regionName, "_ha", sep=""))#
  
  Availbalemonths <- list.files(path = getwd(), pattern = "Table")
  AM <- c()
  for(k in 1:length(Availbalemonths)){
    AM <- c(AM, strsplit(Availbalemonths, "_")[[k]][2])
  }
  AM <- month.name[month.name %in% AM]
  
  csvpdfs <- paste(paste("Table", AM, regionName, sep="_"), ".pdf", sep="")
  mappdfs <- paste(AM, "_",regionName, ".pdf", sep="")
  
  
  outputpdf <- paste(regionName,"_" ,unit, "_MapsTables.pdf", sep="")
  
  pdflist <- c()
  for(k in 1:length(Availbalemonths)){
    pdflist <- c(pdflist, csvpdfs[k], mappdfs[k])
  }
  
  pdflist <- pdflist[!is.na(pdflist)]
  pdf_combine(c(paste(toupper(regionName), ".pdf", sep=""), pdflist), output = outputpdf)
}


require(qpdf)
for(snames in lgaGroups){
  print(snames)
  Combined_pdf_BU(regionName = snames, unit="ha")
}


