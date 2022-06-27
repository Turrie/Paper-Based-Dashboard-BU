#setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard")
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


#######################################################################################
## Read the GIS layers
#######################################################################################

TownsNG <- readOGR(dsn = ".", layer = "Places_towns")
RiversNG <- readOGR(dsn = ".", layer = "Rivers")

boundaryNG <- readOGR(dsn=getwd(), layer="gadm36_NGA_1")
ngstate <- readOGR(dsn=getwd(), layer="gadm36_NGA_2")

boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")
tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")




###################################################################################################
## NG fertilizer recom for FCY 1:5
###################################################################################################
FR_NG_FCY1 <- readRDS("/home/akilimo/projects/PaperbasedDashboard_v2/FRrecom_lga_level1_NG_2020.RDS")
FR_NG_FCY2 <- readRDS("/home/akilimo/projects/PaperbasedDashboard_v2/FRrecom_lga_level2_NG_2020.RDS")
FR_NG_FCY3 <- readRDS("FRrecom_lga_level3_NG_2020.RDS")
FR_NG_FCY4 <- readRDS("FRrecom_lga_level4_NG_2020.RDS")
FR_NG_FCY5 <- readRDS("FRrecom_lga_level5_NG_2020.RDS")



###########################################################################
##  TZ fertilizer recom for FCY 1:5
###########################################################################
FR_TZ_FCY1 <- readRDS("FRrecom_lga_level1_TZ_2020.RDS")
FR_TZ_FCY2 <- readRDS("FRrecom_lga_level2_TZ_2020.RDS")
FR_TZ_FCY3 <- readRDS("FRrecom_lga_level3_TZ_2020.RDS")
FR_TZ_FCY4 <- readRDS("FRrecom_lga_level4_TZ_2020.RDS")
FR_TZ_FCY5 <- readRDS("FRrecom_lga_level5_TZ_2020.RDS")


###########################################################################
##  Ghana fertilizer recom for FCY 1:5
###########################################################################
FCY_FRData <- readRDS("/home/akilimo/projects/PaperbasedDashboard_v2/GH_FR_CassavaPaperBased.RDS") 

FR_GH_FCY1 <- FCY_FRData[FCY_FRData$FCY == "level1", ]
FR_GH_FCY2 <- FCY_FRData[FCY_FRData$FCY == "level2", ]
FR_GH_FCY3 <- FCY_FRData[FCY_FRData$FCY == "level3", ]
FR_GH_FCY4 <- FCY_FRData[FCY_FRData$FCY == "level4", ]
FR_GH_FCY5 <- FCY_FRData[FCY_FRData$FCY == "level5", ]
head(FCY_FRData)


ghRegion

unique(FR_GH_FCY2[FR_GH_FCY2$Regions == "Central",]$Districts)



###########################################################################
##  Rwanda fertilizer recom for FCY 1:5
###########################################################################
FCY_FRData <- readRDS("/home/akilimo/projects/PaperbasedDashboard_v2/RW_CassavaPaperBased.RDS") 
names(FCY_FRData)[18] <- "DISTRICT"
FCY_FRData <- FCY_FRData[FCY_FRData$harvMonth == 12, ]


FR_RW_FCY1 <- FCY_FRData[FCY_FRData$FCY == "level1", ]
FR_RW_FCY2 <- FCY_FRData[FCY_FRData$FCY == "level2", ]
FR_RW_FCY3 <- FCY_FRData[FCY_FRData$FCY == "level3", ]
FR_RW_FCY4 <- FCY_FRData[FCY_FRData$FCY == "level4", ]
FR_RW_FCY5 <- FCY_FRData[FCY_FRData$FCY == "level5", ]
head(FCY_FRData)

unique(FCY_FRData$STATE)
unique(FCY_FRData$District)




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

###########################################################################
##  adding planting month
###########################################################################
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


FR_NG_FCY1_plm <- addplm(ds=FR_NG_FCY1, country = "NG") ## NG if user current yield is level 1
FR_NG_FCY2_plm <- addplm(ds=FR_NG_FCY2, country = "NG") ## NG if user current yield is level 2
FR_NG_FCY3_plm <- addplm(ds=FR_NG_FCY3, country = "NG") ## NG if user current yield is level 3
FR_NG_FCY4_plm <- addplm(ds=FR_NG_FCY4, country = "NG") ## NG if user current yield is level 4
FR_NG_FCY5_plm <- addplm(ds=FR_NG_FCY5, country = "NG") ## NG if user current yield is level 5


FR_TZ_FCY1_plm <- addplm(ds=FR_TZ_FCY1, country = "TZ") ## TZ if user current yield is level 1
FR_TZ_FCY2_plm <- addplm(ds=FR_TZ_FCY2, country = "TZ") ## TZ if user current yield is level 1
FR_TZ_FCY3_plm <- addplm(ds=FR_TZ_FCY3, country = "TZ") ## TZ if user current yield is level 1
FR_TZ_FCY4_plm <- addplm(ds=FR_TZ_FCY4, country = "TZ") ## TZ if user current yield is level 1
FR_TZ_FCY5_plm <- addplm(ds=FR_TZ_FCY5, country = "TZ") ## TZ if user current yield is level 1


FR_GH_FCY1_plm <- addplm(ds=FR_GH_FCY1, country = "GH") ## GH if user current yield is level 1
FR_GH_FCY2_plm <- addplm(ds=FR_GH_FCY2, country = "GH") ## GH if user current yield is level 2
FR_GH_FCY3_plm <- addplm(ds=FR_GH_FCY3, country = "GH") ## GH if user current yield is level 3
FR_GH_FCY4_plm <- addplm(ds=FR_GH_FCY4, country = "GH") ## GH if user current yield is level 4
FR_GH_FCY5_plm <- addplm(ds=FR_GH_FCY5, country = "GH") ## GH if user current yield is level 5


FR_RW_FCY1_plm <- addplm(ds=FR_RW_FCY1, country = "RW") ## RW if user current yield is level 1
FR_RW_FCY2_plm <- addplm(ds=FR_RW_FCY2, country = "RW") ## RW if user current yield is level 2
FR_RW_FCY3_plm <- addplm(ds=FR_RW_FCY3, country = "RW") ## RW if user current yield is level 3
FR_RW_FCY4_plm <- addplm(ds=FR_RW_FCY4, country = "RW") ## RW if user current yield is level 4
FR_RW_FCY5_plm <- addplm(ds=FR_RW_FCY5, country = "RW") ## RW if user current yield is level 5


FR_Bu_FCY1_plm <- addplm(ds=FR_Bu_FCY1, country = "Bu") ## Bu if user current yield is level 1
FR_Bu_FCY2_plm <- addplm(ds=FR_Bu_FCY2, country = "Bu") 
FR_Bu_FCY3_plm <- addplm(ds=FR_Bu_FCY3, country = "Bu") 
FR_Bu_FCY4_plm <- addplm(ds=FR_Bu_FCY4, country = "Bu") 
FR_Bu_FCY5_plm <- addplm(ds=FR_Bu_FCY5, country = "Bu") 

FR_Bu_FCY2_plm[FR_Bu_FCY2_plm$plw == 42 & FR_Bu_FCY2_plm$Commune == "Mugina" & FR_Bu_FCY2_plm$FCY == "level2", ]

###########################################################################
## select FCY and read the corresponding file 
## NG: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
###########################################################################
ds <- FR_NG_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm

Oyo <- droplevels(ds[ds$STATE == "Oyo", ])
Oyolabel <- data.frame(state= c("Oyo"), lon=c(3.3), lat=c(9))

Ogun <- droplevels(ds[ds$STATE == "Ogun", ])
Ogunlabel <- data.frame(state= c("Ogun"), lon=c(3.4), lat=c(7.65))

Kogi <- droplevels(ds[ds$STATE == "Kogi", ])
Kogilabel <- data.frame(state= c("Kogi"), lon=c(6.63), lat=c(8.56))

Kwara <- droplevels(ds[ds$STATE %in% c("Kwara"), ])
Kwaralabel <- data.frame(state= c( "Kwara"), lon=c(4.9), lat=c(9.5))

Taraba <- droplevels(ds[ds$STATE %in% c("Taraba"), ])
Tarabalabel <- data.frame(state= c( "Taraba"), lon=c(10.2), lat=c(9.05))

Benue_CR <- droplevels(ds[ds$STATE %in% c("Benue","Cross River"), ])
Benue_CRlabel <- data.frame(state= c("Benue", "Cross River"), lon=c(8, 9.5), lat=c(8.2, 5.8))

Edo <- droplevels(ds[ds$STATE %in% c("Edo"), ])
Edolabel <- data.frame(state= c("Edo"), lon=c(5.3), lat=c(7))

Delta_Edo <- droplevels(ds[ds$STATE %in% c("Delta","Edo"), ])
Delta_Edolabel <- data.frame(state= c( "Delta","Edo"), lon=c(6, 5.3), lat=c(5,7))

Akwa_Ibom <- droplevels(ds[ds$STATE %in% c("Akwa Ibom"), ])
Akwa_Ibomlabel <- data.frame(state= c( "Akwa Ibom"), lon=c(8), lat=c(5.45))

Imo <- droplevels(ds[ds$STATE %in% c("Imo"), ])
Imolabel <- data.frame(state= c( "Imo"), lon=c(6.9), lat=c(5.9))

Abia <- droplevels(ds[ds$STATE %in% c("Abia"), ])
Abialabel <- data.frame(state= c( "Abia"), lon=c(7.7), lat=c(5.9))

AkwaIbom_Imo_Abia <- droplevels(ds[ds$STATE %in% c("Akwa Ibom", "Imo", "Abia"), ])
AkwaIbom_Imo_AbiAlabel <- data.frame(state= c( "Akwa Ibom", "Imo", "Abia"), lon=c(7.3, 7, 7.8), lat=c(4.4, 6, 6))

Ondo <- droplevels(ds[ds$STATE %in% c("Ondo"), ])
Ondolabel <- data.frame(state= c( "Ondo"), lon=c(5.3), lat=c(6.6))

Ekiti <- droplevels(ds[ds$STATE %in% c("Ekiti"), ])
Ekitilabel <- data.frame(state= c( "Ekiti"), lon=c(5.3), lat=c(8.1))

Osun <- droplevels(ds[ds$STATE == "Osun", ])
Osunlabel <- data.frame(state= c("Osun"), lon=c(4.2), lat=c(8.05))

Ekiti_Ondo_Osun <- droplevels(ds[ds$STATE %in% c("Ekiti", "Ondo", "Osun"), ])
Ekitil_Ondo_Osunabel <- data.frame(state= c( "Ekiti", "Ondo", "Osun"), lon=c(5.3, 5.3, 4.2), lat=c(8.1, 6.6, 8.05))

Anambra <- droplevels(ds[ds$STATE %in% c("Anambra"), ])
Anambralabel <- data.frame(state= c( "Anambra"), lon=c(7.15), lat=c(6.45))

Ebonyi <- droplevels(ds[ds$STATE %in% c("Ebonyi"), ])
Ebonyilabel <- data.frame(state= c( "Ebonyi"), lon=c(7.83), lat=c(6.67))

Enugu <- droplevels(ds[ds$STATE %in% c("Enugu"), ])
Enugulabel <- data.frame(state= c( "Enugu"), lon=c(7.1), lat=c(6.95))

Anambra_Enugu_Ebonyi <- droplevels(ds[ds$STATE %in% c("Anambra", "Enugu", "Ebonyi"), ])
Anambra_Enugu_Ebonyilabel <- data.frame(state= c( "Anambra", "Enugu", "Ebonyi"), lon=c(6.7, 7, 8.25), lat=c(5.9, 7.1, 6.9))


##############################################################################################################
## NG: mapping: for every State, maps will be made per planting month and based on user selection for ha or acre
## the LGAMaps function produces a pdf file with three maps and csn files and it saves both in the directory as set before running the function
##############################################################################################################
setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/BenueCrossRiver_Acre")

for(m in month.name){
  LGAMaps(plantMonth="January",  cities = c("Calabar", "Makurdi"), lgaGroups = "Benue_Cross River",
          LGApoints = Benue_CR, stateLabel = Benue_CRlabel, textangle=0, unit="acre", couple = "Two")
}


LGAMaps <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple){
  plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
  
  if(couple == "Two"){
    lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
  }
  
  if(couple == "Three"){
    lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
  }
  
  plotData <- droplevels(plotData[plotData$STATE %in% lgaGroups, ])
  
  AOI <- lgaGroups
  AOIMapS <- subset(boundaryNG, NAME_1 %in% AOI ) 
  
  AOIMap <- subset(ngstate, NAME_1 %in% AOI )
  AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
  LGAnames <- as.data.frame(AOIMap)
  LGAnames <- cbind(LGAnames, coordinates(AOIMap))
  colnames(LGAnames) <- c("STATE","LGA","long","lat"  )
  LGAnames <- LGAnames[!LGAnames$LGA %in% c("IbadanNorth-West","IbadanNorth-East","IbadanSouth-West", "IbadanSouth-East"),]
  LGAnames$LGA <- gsub("Egbado /", "", LGAnames$LGA )
  
  
  crop_ngstate <- subset(ngstate, NAME_1 %in% AOI )
  towns <- as.data.frame(TownsNG)
  towns <- towns[towns$name %in% cities & towns$fclass %in% c("town", "city"),]
  crop_RiversNG <-  crop(RiversNG, extent(crop_ngstate))
  crop_RiversNG <- crop_RiversNG[crop_RiversNG$fclass == "river", ]
  

  LGAaverage <- ddply(plotData, .(LGA, STATE), summarize,
                      LGAUrea = round(mean(rateUrea), digits=0),
                      LGANPK151515 = round(mean(rateNPK151515), digits=0),
                      LGAdY = round(mean(respY), digits=0))
  
  dss <- LGAaverage
  dss$LGAUrea <- dss$LGAUrea / 2.47105
  dss$LGANPK151515 <- dss$LGANPK151515 / 2.47105
  dss$LGAdY <- dss$LGAdY / 2.47105
  
  if(unit == 'acre'){
    LGAaverage <- dss
  }
  
  plotData <- merge(plotData, LGAaverage, by=c("LGA", "STATE"))
  
  if(unit == "ha"){
    plotData$Urea <- round(plotData$LGAUrea/25)*25
    plotData$NPK15_15_15 <- round(plotData$LGANPK151515/50)*50
    plotData$dY <- round(plotData$LGAdY/2)*2
  }else{
    plotData$Urea <- round(plotData$LGAUrea/10)*10
    plotData$NPK15_15_15 <- round(plotData$LGANPK151515/20)*20
    plotData$dY <- round(plotData$LGAdY/1)*1
  }
  
  fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
  
  AOIMap2 <- merge(AOIMap, unique(plotData[, c("LGA", "Urea", "NPK15_15_15","dY", "LGAdY")]),by.x="NAME_2" ,by.y="LGA")
  AOIMap2$month <- plantMonth
  AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
  plotData$month <- plantMonth
  tt <- unique(as.data.frame(plotData[, c("STATE","LGA", "Urea", "NPK15_15_15", "LGAdY", "month")]))
  tt <- tt[order(tt$STATE, tt$LGA), ]
  write.csv(tt, fileNameCsv, row.names = FALSE)

  AOIMap3 <- st_as_sf(AOIMap2)
  
  # Ureapalette <- brewer.pal(9,"Greens")
  # colpallets <- c(mypalette[c((9-length(unique(plotData$Urea))): length(mypalette))])
  if(unit == "ha"){
    ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                  "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
    tt <- "Urea (kg/ha)"
  }else {
    ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                  "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
    tt <- "Urea (kg/acre)"
  }
  ureasclae <- unique(AOIMap3$Urea)
  keU <- as.character(ureasclae[order(ureasclae)])
  AOIMap3$Urea <- factor(AOIMap3$Urea)
  levels(AOIMap3$Urea) <- keU
  
  require(ggrepel) 
  
  ggUrea <- ggplot(AOIMap3) +
    geom_sf(aes(fill=Urea), col="grey30") +
    scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) +
    geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
    geom_text(data=stateLabel, aes(lon, lat, label=state, fontface=2), col='black', size=6)+
    geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
    geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
    annotation_scale(location = "br", width_hint = 0.3, line_width = 0.4) +
    annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8)) 
  
 
    ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
  if(unit == "ha"){
    NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                 "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
    tt <- "NPK 15-15-15 (kg/ha)"
  }else{
    NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                 "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
    tt <- "NPK 15-15-15 (kg/acre)"
  }
  
  
  mopsclae <- unique(AOIMap3$NPK15_15_15)
  kev <- as.character(mopsclae[order(mopsclae)])
  AOIMap3$NPK15_15_15 <- factor(AOIMap3$NPK15_15_15)
  levels(AOIMap3$NPK15_15_15) <- kev
  
  require(plotly)
  ggNPK <- ggplot(AOIMap3) +
    geom_sf(aes(fill=NPK15_15_15), col="grey30") +
    scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
    # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
    #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
    geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
    geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8))
 
  
  if(unit == "ha"){
    tt <- "Yield increase (t/ha)"
    Ydcols <- c("21"="#FF0000FF", "20"= "#FF8B00FF", "19"= "#E8FF00FF",
                "18"= "#5DFF00FF",  "17"= "#00FF2EFF", "16"="#00FFB9FF", "15"= "#00B9FFFF", "14"= "#002EFFFF",
                "12"= "#5D00FFFF", "11"= "#E800FFFF", "10"= "#FF008BFF", "9"= "#FFFFFF")
  }else{
    tt <- "Yield increase (t/acre)"
    Ydcols <- c("9"="#FF0000FF", "8"= "#FF8B00FF", "7"= "#E8FF00FF",
                "6"= "#5DFF00FF",  "5"= "#00FF2EFF", "4"="#00FFB9FF", "3"= "#00B9FFFF", "4"= "#002EFFFF",
                "3"= "#5D00FFFF", "2"= "#E800FFFF", "1"= "#FF008BFF", "0"= "#FFFFFF")
  }
  
  Ysclae <- unique(AOIMap3$dY)
  keY <- as.factor(Ysclae[order(Ysclae)])
  AOIMap3$dY <- factor(AOIMap3$dY)
  levels(AOIMap3$dY) <- keY
  
  ggYield <- ggplot(AOIMap3) +
    geom_sf(aes(fill=dY), col="grey30") +
    scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3, segment.size = NA) + 
    #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
    #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
    geom_path(data=crop_RiversNG, aes(x=long, y=lat, group=group), color="dodgerblue1", size=0.3) +
    geom_point(data=towns, aes(x=coords.x1, y=coords.x2), shape=16,  size=3) +
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
  pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5,0.8), "null"))))   
  grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  #print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  dev.off()
  
}



setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/BenueCrossRiver_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Calabar", "Makurdi"), lgaGroups = "Benue_Cross River",
          LGApoints = Benue_CR, stateLabel = Benue_CRlabel, textangle=0, unit="acre", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/BenueCrossRiver")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Calabar", "Makurdi"), lgaGroups = "Benue_Cross River",
          LGApoints = Benue_CR, stateLabel = Benue_CRlabel, textangle=0, unit="ha", couple = "Two")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Delta")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Asaba"), lgaGroups = "Delta",
          LGApoints = Delta, stateLabel = Deltalabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Delta_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Asaba"), lgaGroups = "Delta",
          LGApoints = Delta, stateLabel = Deltalabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Edo")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Benin City"), lgaGroups = "Edo",
          LGApoints = Edo, stateLabel = Edolabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Edo_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Benin City"), lgaGroups = "Edo",
          LGApoints = Edo, stateLabel = Edolabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Imo")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Owerri"), lgaGroups = "Imo",
          LGApoints = Imo, stateLabel = Imolabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Imo_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Owerri"), lgaGroups = "Imo",
          LGApoints = Imo, stateLabel = Imolabel, textangle=0, unit="acre", couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Abia")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Umuahia"), lgaGroups = "Abia",
          LGApoints = Abia, stateLabel = Abialabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Abia_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Umuahia"), lgaGroups = "Abia",
          LGApoints = Abia, stateLabel = Abialabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/AkwaIbom")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Uyo"), lgaGroups = "Akwa Ibom",
          LGApoints = Akwa_Ibom, stateLabel = Akwa_Ibomlabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/AkwaIbom_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Uyo"), lgaGroups = "Akwa Ibom",
          LGApoints = Akwa_Ibom, stateLabel = Akwa_Ibomlabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ekiti")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Ado Ekiti"), lgaGroups = "Ekiti",
          LGApoints = Ekiti, stateLabel = Ekitilabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ekiti_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Ado Ekiti"), lgaGroups = "Ekiti",
          LGApoints = Ekiti, stateLabel = Ekitilabel, textangle=0, unit="acre", couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ondo")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Akure"), lgaGroups = "Ondo",
          LGApoints = Ondo, stateLabel = Ondolabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ondo_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Akure"), lgaGroups = "Ondo",
          LGApoints = Ondo, stateLabel = Ondolabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Osun")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Osogbo"), lgaGroups = "Osun",
          LGApoints = Osun, stateLabel = Osunlabel, textangle=0, unit="ha", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Osun_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Osogbo"), lgaGroups = "Osun",
          LGApoints = Osun, stateLabel = Osunlabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Anambra")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Awka"), lgaGroups = "Anambra",
          LGApoints = Anambra, stateLabel = Anambralabel, textangle=0, unit="ha",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Anambra_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Awka"), lgaGroups = "Anambra",
          LGApoints = Anambra, stateLabel = Anambralabel, textangle=0, unit="acre",couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Enugu")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Enugu"), lgaGroups = "Enugu",
          LGApoints = Enugu, stateLabel = Enugulabel, textangle=0, unit="ha",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Enugu_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Enugu"), lgaGroups = "Enugu",
          LGApoints = Enugu, stateLabel = Enugulabel, textangle=0, unit="acre",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ebonyi")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Ebonyi"), lgaGroups = "Ebonyi",
          LGApoints = Ebonyi, stateLabel = Ebonyilabel, textangle=0, unit="ha",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ebonyi_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = c("Ebonyi"), lgaGroups = "Ebonyi",
          LGApoints = Ebonyi, stateLabel = Ebonyilabel, textangle=0, unit="acre",couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Taraba")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Taraba", lgaGroups = "Taraba", LGApoints = Taraba, 
          stateLabel = Tarabalabel, textangle=0, unit="ha",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Taraba_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Taraba", lgaGroups = "Taraba", LGApoints = Taraba, 
          stateLabel = Tarabalabel, textangle=0, unit="acre",couple = "One")
}

("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Kogi")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Kogi", lgaGroups = "Kogi", LGApoints = Kogi, stateLabel = Kogilabel,
          textangle=0, unit="ha",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Kogi_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Kogi", lgaGroups = "Kogi", LGApoints = Kogi, stateLabel = Kogilabel,
          textangle=0, unit="acre",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Kwara_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Kwara", lgaGroups = "Kwara", LGApoints = Kwara, 
          stateLabel = Kwaralabel, textangle=0, unit="acre",couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Kwara")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Kwara", lgaGroups = "Kwara", LGApoints = Kwara, 
          stateLabel = Kwaralabel, textangle=0, unit="ha" ,couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Oyo_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Oyo", lgaGroups = "Oyo", LGApoints = Oyo, 
          stateLabel = Oyolabel, textangle=0, unit="acre" ,couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Oyo")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Oyo", lgaGroups = "Oyo", LGApoints = Oyo, 
          stateLabel = Oyolabel, textangle=0, unit="ha" ,couple = "One")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ogun")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Abeokuta", lgaGroups = "Ogun", LGApoints = Ogun,
          stateLabel = Ogunlabel, textangle=0, unit="ha" ,couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/Ogun_Acre")
for(m in month.name){
  LGAMaps(plantMonth=m,  cities = "Abeokuta", lgaGroups = "Ogun", LGApoints = Ogun, 
          stateLabel = Ogunlabel, textangle=0, unit="acre" ,couple = "One")
}


##############################################################################################################
## NG: The two functions are used to convert the csv files to pdf files
##############################################################################################################


pdfTables <- function(FERTtABLE, plantMonth, unit, state){
  row.names(FERTtABLE) = NULL
  FERTtABLE <- subset(FERTtABLE, select=-c(month))
  FERTtABLE$LGAdY <- round(FERTtABLE$LGAdY, digits=0)
  # if(unit=="acre" & state == "CrossRiver_Benue"){
  #   colnames(FERTtABLE) <- c("LGA", "STATE","Urea \n (kg/acre)","MOP \n (kg/acre)","DAP \n (kg/acre)", "Yield increase \n (t/acre)")
  # }else{
  #   colnames(FERTtABLE) <- c("LGA","STATE", "Urea \n (kg/ha)","MOP \n (kg/ha)","DAP \n (kg/ha)", "Yield increase \n (t/ha)")
  # }
  # if(unit=="acre" & state != "CrossRiver_Benue"){
  #   colnames(FERTtABLE) <- c("STATE","LGA", "Urea \n (kg/acre)","MOP \n (kg/acre)","DAP \n (kg/acre)", "Yield increase \n (t/acre)")
  # }else if (unit=="ha" & state != "CrossRiver_Benue"){
  #   colnames(FERTtABLE) <- c("STATE", "LGA", "Urea \n (kg/ha)","MOP \n (kg/ha)","DAP \n (kg/ha)", "Yield increase \n (t/ha)")
  # }
  # 
  
  
  if(unit=="acre"){
    colnames(FERTtABLE) <- c("STATE","LGA", "Urea \n (kg/acre)", "NPK 15-15-15 \n (kg/acre)", "Yield increase \n (t/acre)")
  }else if (unit=="ha"){
    colnames(FERTtABLE) <- c("STATE", "LGA", "Urea \n (kg/ha)", "NPK 15-15-15 \n (kg/ha)", "Yield increase \n (t/ha)")
  }
  
  
  FERTtABLE <- FERTtABLE[order(FERTtABLE$STATE, FERTtABLE$LGA), ]
  
  if(state=="CrossRiver_Benue"){
    fileNameR <- paste("Table","_", plantMonth, "_", "Benue_CrossRiver", ".pdf", sep="")
  }else{
    fileNameR <- paste("Table","_", plantMonth, "_", state, ".pdf", sep="")
  }
  
  
  
  pdf(fileNameR, width = 8, height = 12)
  pdf.options(paper = "a4")
  
  # if(state  %in% c("Oyo", "Benue_Cross River", "Osun", "Akwa Ibom", "Imo")){
  #   pdf.options(paper = "a4")
  # }else{
  #   pdf.options(paper = "a4r")
  # }
  # 
  hj <- matrix(c(0, 0, -0.5,-0.5, -0.5, -0.5), ncol=6, nrow=nrow(FERTtABLE), byrow=TRUE)
  if(state  %in% c("Oyo", "Benue_Cross River", "Osun")){
    tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
                          colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.8)),
                          rowhead = list(fg_params=list(cex = 0.7)))
  }else{
    tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
                          colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
                          rowhead = list(fg_params=list(cex = 0.8)))
  }
  
  # hj <- matrix(c(0, 0, -0.6,-0.6, -0.9, -2.0), ncol=6, nrow=nrow(FERTtABLE), byrow=TRUE)
  # tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
  #                       colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
  #                       rowhead = list(fg_params=list(cex = 0.9)))
  
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

tablepdf <- function(unit, fname, state){
  setwd(paste("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/NG/", fname, sep=""))
  listcsv <- list.files(getwd(), "csv")
  library(gtable)
  for(files in c(listcsv, listcsv[1])){
    plantMonth <-  strsplit(files, '_')[[1]][1]
    csvtables <- unique(read.csv(files))
    pdfTables(FERTtABLE = csvtables, plantMonth, unit=unit, state=state)
  }
}

tablepdf(unit="acre", fname="BenueCrossRiver_Acre", state="Benue_Cross River")
tablepdf(unit="ha", fname="BenueCrossRiver", state="Benue_Cross River")

tablepdf(unit="acre", fname="Abia_Acre", state="Abia")
tablepdf(unit="ha", fname="Abia", state="Abia")

tablepdf(unit="acre", fname="AkwaIbom_Acre", state="Akwa Ibom")
tablepdf(unit="ha", fname="AkwaIbom", state="Akwa Ibom")

tablepdf(unit="acre", fname="Anambra_Acre", state="Anambra")
tablepdf(unit="ha", fname="Anambra", state="Anambra")

tablepdf(unit="acre", fname="Delta_Acre", state="Delta")
tablepdf(unit="ha", fname="Delta", state="Delta")

tablepdf(unit="acre", fname="Ebonyi_Acre", state="Ebonyi")
tablepdf(unit="ha", fname="Ebonyi", state="Ebonyi")

tablepdf(unit="acre", fname="Edo_Acre", state="Edo")
tablepdf(unit="ha", fname="Edo", state="Edo")

tablepdf(unit="acre", fname="Ekiti_Acre", state="Ekiti")
tablepdf(unit="ha", fname="Ekiti", state="Ekiti")

tablepdf(unit="acre", fname="Enugu_Acre", state="Enugu")
tablepdf(unit="ha", fname="Enugu", state="Enugu")

tablepdf(unit="acre", fname="Imo_Acre", state="Imo")
tablepdf(unit="ha", fname="Imo", state="Imo")

tablepdf(unit="acre", fname="Ondo_Acre", state="Ondo")
tablepdf(unit="ha", fname="Ondo", state="Ondo")

tablepdf(unit="acre", fname="Osun_Acre", state="Osun")
tablepdf(unit="ha", fname="Osun", state="Osun")

tablepdf(unit="acre", fname="Ogun_Acre", state="Ogun")
tablepdf(unit="ha", fname="Ogun", state="Ogun")

tablepdf(unit="acre", fname="Oyo_Acre", state="Oyo")
tablepdf(unit="ha", fname="Oyo", state="Oyo")

tablepdf(unit="acre", fname="Kwara_Acre", state="Kwara")
tablepdf(unit="ha", fname="Kwara", state="Kwara")

tablepdf(unit="acre", fname="Kogi_Acre", state="Kogi")
tablepdf(unit="ha", fname="Kogi", state="Kogi")

tablepdf(unit="acre", fname="Taraba_Acre", state="Taraba")
tablepdf(unit="ha", fname="Taraba", state="Taraba")



############################################################################################################################################################################################################################
############################################################################################################################################################################################################################
############################################################################################################################################################################################################################
## ## select FCY and read the corresponding file 
## TZ: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
##############################################################################################################
ds_TZ <- FR_TZ_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm

Mara <- droplevels(ds[ds$REGION %in% c("Mara", "Simiyu"), ])
Maralabel <- data.frame(REGION= c("Mara", "Simiyu"), lon=c(34.7, 33.7), lat=c(-1.2, -3.7))

Kagera <- droplevels(ds[ds$REGION %in% c("Kagera", "Geita", "Kigoma"), ])
Kageralabel <- data.frame(REGION= c("Kagera", "Geita", "Kigoma"), lon=c(30.25, 32.4, 31), lat=c(-2.1, -4, -6.1))

Mwanza <- droplevels(ds[ds$REGION %in% c("Mwanza","Shinyanga"), ])
Mwanzalabel <- data.frame(REGION= c("Mwanza","Shinyanga"), lon=c(33.65, 33.2), lat=c(-2.1, -4.1))

Pwani <- droplevels(ds[ds$REGION %in% c("Tanga","Pwani"), ])
Pwanilabel <- data.frame(REGION= c("Tanga","Pwani"), lon=c(37.6, 37.9), lat=c(-4.8, -7.3))

Mtwara <- droplevels(ds[ds$REGION %in% c("Mtwara", "Lindi"), ])
Mtwaralabel <- data.frame(REGION= c("Mtwara", "Lindi"), lon=c(37.5, 38.7), lat=c(-11.2,-8.1))

Zanzibar <- droplevels(ds[ds$REGION %in% c("Zanzibar South and Central", "Zanzibar West", "Zanzibar North"), ])
Zanzibarlabel <- data.frame(REGION= c("Zanzibar"), lon=c(39.5), lat=c(-5.95))

Maracity <- data.frame(REGION = c("Mara", "Simiyu"),name=c("Musoma","Bariadi"), lat=c(-1.5,-2.8), lon = c(33.8, 33.98))

Kageracity <- data.frame(REGION = c("Kagera", "Geita", "Kigoma"), name=c("Bukoba","Geita","Kigoma"), 
                         lat=c(-1.33, -2.87, -4.88), lon = c(31.82, 32.23,29.63))

Pwaniacity <- data.frame(REGION = c("Pwani", "Tanga"),name=c("Kibaha","Tanga"), 
                         lat=c(-6.77, -5.07), lon = c(38.92, 39.01))

Mwanzacity <- data.frame(REGION = c("Mwanza", "Shinyanga"),name=c("Mwanza", "Shinyanga"), 
                         lat=c(-2.52, -3.66), lon = c(32.9, 33.42))

Mtwaraacity <- data.frame(REGION = c("Mtwara","Lindi"),name=c("Mtwara","Lindi"),
                          lat=c(-10.27, -9.99), lon = c(40.18, 39.71))
Zanzibarcity <- data.frame(REGION = "Zanzibar",name="Zanzibar", lat=-6.17, lon = 39.2)



##############################################################################################################
## TZ: mapping: for every Region, maps will be made per planting month and based on user selection for ha or acre
##############################################################################################################

setwd("E:/QUEFTS/mtcGISData/Tanzania")
boundaryTZ <- readOGR(dsn=getwd(), layer="gadm36_TZA_1")
tzRegion <- readOGR(dsn=getwd(), layer="gadm36_TZA_2")


LGAMaps_TZ <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple){
  plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth, ])
  
  if(couple == "Two"){
    lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
  }
  
  if(couple == "Three"){
    lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
  }
  
  plotData <- droplevels(plotData[plotData$REGION %in% lgaGroups, ])
  
  AOI <- lgaGroups
  AOIMapS <- subset(boundaryTZ, NAME_1 %in% AOI ) 
  
  AOIMap <- subset(tzRegion, NAME_1 %in% AOI )
  AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
  LGAnames <- as.data.frame(AOIMap)
  LGAnames <- cbind(LGAnames, coordinates(AOIMap))
  colnames(LGAnames) <- c("REGION","DISTRICT","long","lat"  )
  crop_ngstate <- subset(tzRegion, NAME_1 %in% AOI )
  
  
  ## take REGION average
  LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                      LGAUrea = round(mean(rateUrea), digits=0),
                      LGANPK171717 = round(mean(rateNPK171717), digits=0),
                      LGADAP = round(mean(rateDAP), digits=0),
                      LGAdY = round(mean(respY), digits=0))
  
  
  dss <- LGAaverage
  dss$LGAUrea <- dss$LGAUrea / 2.47105
  dss$LGANPK171717 <- dss$LGANPK171717 / 2.47105
  dss$LGADAP <- dss$LGADAP / 2.47105
  dss$LGAdY <- dss$LGAdY / 2.47105
  
  if(unit == 'acre'){
    LGAaverage <- dss
  }
  
  plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
  
  if(unit == "ha"){
    plotData$Urea <- round(plotData$LGAUrea/25)*25
    plotData$NPK17_17_17 <- round(plotData$LGANPK171717/50)*50
    plotData$DAP <- round(plotData$LGADAP/25)*25
    plotData$dY <- round(plotData$LGAdY/2)*2
  }else{
    plotData$Urea <- round(plotData$LGAUrea/10)*10
    plotData$NPK17_17_17 <- round(plotData$LGANPK171717/20)*20
    plotData$DAP <- round(plotData$LGADAP/10)*10
    plotData$dY <- round(plotData$LGAdY/1)*1
  }
  
  fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
  
  AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","DAP","dY", "LGAdY")]),
                   by.x=c("NAME_1","NAME_2") ,by.y=c("REGION","DISTRICT"))
  AOIMap2$month <- plantMonth
  AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
  plotData$month <- plantMonth
  tt <- unique(as.data.frame(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","DAP", "LGAdY", "month")]))
  tt <- tt[order(tt$REGION, tt$DISTRICT), ]
  write.csv(tt, fileNameCsv, row.names = FALSE)
  AOIMap3 <- st_as_sf(AOIMap2)
  
  if(unit == "ha"){
    ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                  "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
    ttz <- "Urea (kg/ha)"
  }else {
    ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                  "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
    ttz <- "Urea (kg/acre)"
  }
  ureasclae <- unique(AOIMap3$Urea)
  keU <- as.character(ureasclae[order(ureasclae)])
  AOIMap3$Urea <- factor(AOIMap3$Urea)
  levels(AOIMap3$Urea) <- keU
  
  require(ggrepel) 
  
  ggUrea <- ggplot(AOIMap3) +
    geom_sf(aes(fill=Urea), col="darkgrey") +
    scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
    geom_text(data=stateLabel, aes(lon, lat, label=REGION, fontface=2), col='black', size=6)+
    geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
    annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
    annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                           style = north_arrow_fancy_orienteering) +
    # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
    xlab("") + ylab("") +
    ggtitle(ttz) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8)) 
  
  
  ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
  if(unit == "ha"){
    NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                 "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
    tt <- "NPK 17-17-17 (kg/ha)"
  }else{
    NPKcols <- c("0"="#FFFFFF","20"= "#FFF7BC", "40"= "#FEE391", "60"= "#FEC44F", "80"= "#FE9929", 
                 "100"= "#EC7014", "120"= "#CC4C02", "140" ="#993404","160" = "#662506")
    tt <- "NPK 17-17-17 (kg/acre)"
  }
  
  
  mopsclae <- unique(AOIMap3$NPK17_17_17)
  kev <- as.character(mopsclae[order(mopsclae)])
  AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
  levels(AOIMap3$NPK17_17_17) <- kev
  
  ggNPK <- ggplot(AOIMap3) +
    geom_sf(aes(fill=NPK17_17_17), col="darkgrey") +
    
    scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
    # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
    #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
    geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8))
  
  
  
  DAPPpalette <- brewer.pal(9,"YlGnBu")
  if(unit == "ha"){
    DAPcols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                 "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
    tt <- "DAP (kg/ha)"
  }else{
    DAPcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                 "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
    tt <- "DAP (kg/acre)"
  }
  
  dapsclae <- unique(AOIMap3$DAP)
  kedap <- as.factor(dapsclae[order(dapsclae)])
  AOIMap3$DAP <- factor(AOIMap3$DAP)
  levels(AOIMap3$DAP) <- kedap
  
  ggDAP <- ggplot(AOIMap3) +
    geom_sf(aes(fill=DAP), col="darkgrey") +
    
    scale_fill_manual(values = DAPcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
    geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8))
  
  
  # brewer.pal(9,"heat")
  Ydcols <- c("21"="#FF0000FF","20" = "#FF4600FF", "19"= "#FF8B00FF", "18"= "#FFD100FF", "17"= "#E8FF00FF",
              "16"="#A2FF00FF", "15"= "#5DFF00FF", "14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
              "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
              "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
              "1"= "#FF008BFF", "0"= "#FFFFFF")
  
  if(unit == "ha"){
    tt <- "Yield increase (t/ha)"
  }else{
    tt <- "Yield increase (t/acre)"
  }
  
  Ysclae <- unique(AOIMap3$dY)
  keY <- as.factor(Ysclae[order(Ysclae)])
  AOIMap3$dY <- factor(AOIMap3$dY)
  levels(AOIMap3$dY) <- keY
  
  ggYield <- ggplot(AOIMap3) +
    geom_sf(aes(fill=dY), col="darkgrey") +
    
    scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
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
  pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5, 0.8), "null"))))   
  grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  print(ggNPK, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(ggDAP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  dev.off()
  
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Mtwara_Lindi_Acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Mtwaraacity, lgaGroups = c("Mtwara_Lindi"),
             LGApoints = Mtwara, stateLabel = Mtwaralabel, textangle=0, unit="acre", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Mtwara_Lindi")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Mtwaraacity, lgaGroups = "Mtwara_Lindi",
             LGApoints = Mtwara, stateLabel = Mtwaralabel, textangle=0, unit="ha", couple = "Two")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Pwani_Tanga_Acre")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Pwaniacity, lgaGroups = c("Pwani_Tanga"),
             LGApoints = Pwani, stateLabel = Pwanilabel, textangle=0, unit="acre", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Pwani_Tanga")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Pwaniacity, lgaGroups = "Pwani_Tanga",
             LGApoints = Pwani, stateLabel = Pwanilabel, textangle=0, unit="ha", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Mwanza_Shinyanga_Acre")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Mwanzacity, lgaGroups = c("Mwanza_Shinyanga"),
             LGApoints = Mwanza, stateLabel = Mwanzalabel, textangle=0, unit="acre", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Mwanza_Shinyanga")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Mwanzacity, lgaGroups = "Mwanza_Shinyanga",
             LGApoints = Mwanza, stateLabel = Mwanzalabel, textangle=0, unit="ha", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Mara_Simiyu_Acre")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Maracity, lgaGroups = "Mara_Simiyu",
             LGApoints = Mara, stateLabel = Maralabel, textangle=0, unit="acre", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Mara_Simiyu")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Maracity, lgaGroups = "Mara_Simiyu",
             LGApoints = Mara, stateLabel = Maralabel, textangle=0, unit="ha", couple = "Two")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Kagera_Geita_Kigoma_Acre")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Kageracity, lgaGroups = c("Kagera_Geita_Kigoma"),
             LGApoints = Kagera,stateLabel = Kageralabel, textangle=0, unit="acre", couple = "Three")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Kagera_Geita_Kigoma")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Kageracity, lgaGroups = "Kagera_Geita_Kigoma",
             LGApoints = Kagera, stateLabel = Kageralabel, textangle=0, unit="ha", couple = "Three")
}


setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Zanzibar_Acre")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Zanzibarcity, lgaGroups = c("Zanzibar South and Central", "Zanzibar West", "Zanzibar North"),
             LGApoints = Zanzibar, stateLabel = Zanzibarlabel, textangle=0, unit="acre", couple = "One")
}

setwd("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/Zanzibar")
for(m in month.name){
  LGAMaps_TZ(plantMonth=m,  cities = Zanzibarcity, lgaGroups = c("Zanzibar South and Central", "Zanzibar West", "Zanzibar North"),
             LGApoints = Zanzibar, stateLabel = Zanzibarlabel, textangle=0, unit="ha", couple = "One")
}



##############################################################################################################
## TZ: The two functions are used to convert the csv files to pdf files
##############################################################################################################

pdfTables_TZ <- function(FERTtABLE, plantMonth, unit, REGION){
  row.names(FERTtABLE) = NULL
  FERTtABLE <- subset(FERTtABLE, select=-c(month))
  FERTtABLE$LGAdY <- round(FERTtABLE$LGAdY, digits=0)
  
  
  if(unit=="acre"){
    colnames(FERTtABLE) <- c("REGION","DISTRICT", "Urea \n (kg/acre)", "NPK 17-17-17 \n (kg/acre)", 
                             "DAP \n (kg/acre)", "Yield increase \n (t/acre)")
  }else if (unit=="ha"){
    colnames(FERTtABLE) <- c("REGION", "DISTRICT", "Urea \n (kg/ha)", "NPK 17-17-17 \n (kg/ha)", 
                             "DAP \n (kg/acre)","Yield increase \n (t/ha)")
  }
  
  
  FERTtABLE <- FERTtABLE[order(FERTtABLE$REGION, FERTtABLE$DISTRICT), ]
  
  fileNameR <- paste("Table","_", plantMonth, "_", REGION, ".pdf", sep="")
  
  
  pdf(fileNameR, width = 8, height = 12)
  pdf.options(paper = "a4")
  
  # if(REGION  %in% c("Oyo", "Benue_Cross River", "Osun", "Akwa Ibom", "Imo")){
  #   pdf.options(paper = "a4")
  # }else{
  # pdf.options(paper = "a4r")
  # }
  
  hj <- matrix(c(0, 0, -0.5,-0.5, -0.5, -0.5, -0.5), ncol=7, nrow=nrow(FERTtABLE), byrow=TRUE)
  # if(REGION  %in% c("Oyo", "Benue_Cross River", "Osun")){
  #   tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
  #                         colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.8)),
  #                         rowhead = list(fg_params=list(cex = 0.7)))
  # }else{
  tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
                        colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
                        rowhead = list(fg_params=list(cex = 0.8)))
  # }
  
  # hj <- matrix(c(0, 0, -0.6,-0.6, -0.9, -2.0), ncol=6, nrow=nrow(FERTtABLE), byrow=TRUE)
  # tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
  #                       colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
  #                       rowhead = list(fg_params=list(cex = 0.9)))
  
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

tablepdf_TZ <- function(unit, fname, REGION){
  setwd(paste("C:/Users/User/Documents/ACAI/paper based/PaperbasedDashboard/FR/TZ/", fname, sep=""))
  listcsv <- list.files(getwd(), "csv")
  library(gtable)
  for(files in c(listcsv, listcsv[1])){
    plantMonth <-  strsplit(files, '_')[[1]][1]
    csvtables <- unique(read.csv(files))
    pdfTables_TZ(FERTtABLE = csvtables, plantMonth, unit=unit, REGION=REGION)
  }
}

tablepdf_TZ(unit="acre", fname="Kagera_Geita_Kigoma_Acre", REGION="Kagera_Geita_Kigoma")
tablepdf_TZ(unit="ha", fname="Kagera_Geita_Kigoma", REGION="Kagera_Geita_Kigoma")

tablepdf_TZ(unit="acre", fname="Mara_Simiyu_Acre", REGION="Mara_Simiyu")
tablepdf_TZ(unit="ha", fname="Mara_Simiyu", REGION="Mara_Simiyu")

tablepdf_TZ(unit="acre", fname="Mtwara_Lindi_Acre", REGION="Mtwara_Lindi")
tablepdf_TZ(unit="ha", fname="Mtwara_Lindi", REGION="Mtwara_Lindi")

tablepdf_TZ(unit="acre", fname="Mwanza_Shinyanga_Acre", REGION="Mwanza_Shinyanga")
tablepdf_TZ(unit="ha", fname="Mwanza_Shinyanga", REGION="Mwanza_Shinyanga")

tablepdf_TZ(unit="acre", fname="Pwani_Tanga_Acre", REGION="Pwani_Tanga")
tablepdf_TZ(unit="ha", fname="Pwani_Tanga", REGION="Pwani_Tanga")

tablepdf_TZ(unit="acre", fname="Zanzibar_Acre", REGION="Zanzibar South and Central_Zanzibar West_Zanzibar North")
tablepdf_TZ(unit="ha", fname="Zanzibar", REGION="Zanzibar South and Central_Zanzibar West_Zanzibar North")












############################################################################################################################################################################################################################


############################################################################################################################################################################################################################
############################################################################################################################################################################################################################
## ## select FCY and read the corresponding file 
## Ghana: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
##############################################################################################################
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH")
ds <- FR_GH_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm
ds$REGION <- ds$Regions
ds$DISTRICT <- ds$Districts
head(ds)


## Brong Ahafo = Bono





Savannah <- droplevels(ds[ds$REGION %in% c("Savannah"), ])
Savannahlabel <-  data.frame(REGION= c("Savannah"), lon=c(-0.75), lat=c(9.5))

Bono_East <- droplevels(ds[ds$REGION %in% c("Bono East"), ])
Bono_Eastlabel <- data.frame(REGION= c("Bono East"), lon=c(-0.65), lat=c(8.5))

Bono <- droplevels(ds[ds$REGION %in% c("Bono"), ])
Bonolabel <- data.frame(REGION= c("Brong Ahafo"), lon=c(-2.5), lat=c(8.68))


Central <- droplevels(ds[ds$REGION %in% c("Central"), ])
Centrallabel <- data.frame(REGION= c("Central"), lon=c(-1.0), lat=c( 6.1))

Ashanti <- droplevels(ds[ds$REGION %in% c("Ashanti"), ])
Ashantilabel <- data.frame(REGION= c("Ashanti"), lon=c(-2.5), lat=c(7.53))

Ahafo <- droplevels(ds[ds$REGION %in% c("Ahafo"), ])
Ahafolabel <- data.frame(REGION= c("Ahafo"), lon=c(-2.8), lat=c(6.5))


Eastern <- droplevels(ds[ds$REGION %in% c("Eastern"), ])
Eastlabel <- data.frame(REGION= c("Eastern"), lon=c(-0.8), lat=c(7.21))

Volta <- droplevels(ds[ds$REGION %in% c("Volta"), ])
Voltalabel <- data.frame(REGION= c("Volta"), lon=c(0.7), lat=c(7.3))


Savannahcity <- data.frame(REGION = c("Savannah"),name=c("Damongo"), lat=c(9.08), lon = c(-1.82))

Centralcity <- data.frame(REGION = c("Central"),name=c("Cape Coast"), lat=c(5.12), lon = c(-1.27))

Bonocity <- data.frame(REGION = c("Brong Ahafo"), name=c("Sunyani"), 
                                lat=c(7.34), lon = c(-2.32))

Bono_Eastcity <- data.frame(REGION = c("Bono East"), name=c("Techiman"), 
                       lat=c(7.58), lon = c(-1.93))

Ashanti_city <- data.frame(REGION = c("Ashanti"), name=c("Kumasi"), 
                         lat=c(6.68), lon = c(-1.63))

Ahafocity <- data.frame(REGION = c("Ahafo"), name=c("Goaso"), 
                                lat=c(6.8), lon = c(-2.52))


Eastcity <- data.frame(REGION = c("Eastern"),name=c("Koforidua"), 
                         lat=c(6.1), lon = c(-0.26))


Voltacity <- data.frame(REGION = c("Volta"),name=c("Ho"), 
                             lat=c(6.6), lon = c(0.47))





##############################################################################################################
## GH: mapping: for every Region, maps will be made per planting month and based on user selection for ha or acre
##############################################################################################################
setwd("/home/akilimo/lintul/lintul/dataSources/GIS_layers")
boundaryGH <- readOGR(dsn=getwd(), layer="gha_admbnda_adm1_gss_20210308")
ghRegion <- readOGR(dsn=getwd(), layer="gha_admbnda_adm2_gss_20210308")




LGAMaps_GH <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple){
  plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$REGION %in% lgaGroups , ])
  
  # if(couple == "Two"){
  #   lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
  # }
  # 
  # if(couple == "Three"){
  #   lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
  # }
  
  # plotData <- droplevels(plotData[plotData$REGION %in% lgaGroups, ])
  
  AOI <- lgaGroups
  AOIMapS <- subset(boundaryGH, ADM1_EN %in% AOI ) 
  
  AOIMap <- subset(ghRegion, ADM1_EN %in% AOI )
  AOIMap <- AOIMap[,c("ADM1_EN", "ADM2_EN")]
  LGAnames <- as.data.frame(AOIMap)
  LGAnames <- cbind(LGAnames, coordinates(AOIMap))
  colnames(LGAnames) <- c("REGION","DISTRICT","long","lat")
  crop_ngstate <- subset(ghRegion, ADM1_EN %in% AOI )
  
  
  ## take REGION average
  LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                      LGAUrea = round(mean(rateUrea), digits=0),
                      LGANPK112221 = round(mean(rateNPK112221 ), digits=0),
                      LGANPK251010= round(mean(rateNPK251010), digits=0),
                      LGANPK152020 = round(mean(rateNPK152020), digits=0),
                      LGANPK123017 = round(mean(rateNPK123017), digits=0),
                      LGAdY = round(mean(respY), digits=0))
  
  
  LGAaverage$LGAUrea <- ifelse(LGAaverage$LGAUrea <25, 0, LGAaverage$LGAUrea)
  LGAaverage$LGANPK112221 <- ifelse(LGAaverage$LGANPK112221 <25, 0, LGAaverage$LGANPK112221)
  LGAaverage$LGANPK251010 <- ifelse(LGAaverage$LGANPK251010 <25, 0, LGAaverage$LGANPK251010)
  LGAaverage$LGANPK152020 <- ifelse(LGAaverage$LGANPK152020 <25, 0, LGAaverage$LGANPK152020)
  LGAaverage$LGANPK123017 <- ifelse(LGAaverage$LGANPK123017 <25, 0, LGAaverage$LGANPK123017)
  


  if(unit == 'acre'){
    
    dss <- LGAaverage
    dss$LGAUrea <- dss$LGAUrea / 2.47105
    dss$LGANPK112221 <- dss$LGANPK112221 / 2.47105
    dss$LGANPK251010 <- dss$LGANPK251010 / 2.47105
    dss$LGANPK152020 <- dss$LGANPK152020 / 2.47105
    dss$LGANPK123017 <- dss$LGANPK123017 / 2.47105
    dss$LGAdY <- dss$LGAdY / 2.47105
    
    LGAaverage <- dss
  }
  
  plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
  
  if(unit == "ha"){
    plotData$Urea <- round(plotData$LGAUrea/50)*50
    plotData$NPK112221 <- round(plotData$LGANPK112221/50)*50
    plotData$NPK251010 <- round(plotData$LGANPK251010 /25)*25
    plotData$NPK152020 <- round(plotData$LGANPK152020/50)*50
    plotData$NPK123017 <- round(plotData$LGANPK123017 /25)*25
    plotData$dY <- round(plotData$LGAdY/2)*2
  }else{
    plotData$Urea <- round(plotData$LGAUrea/10)*10
    plotData$NPK112221 <- round(plotData$LGANPK112221/10)*10
    plotData$NPK251010 <- round(plotData$LGANPK251010 /10)*10
    plotData$NPK152020 <- round(plotData$LGANPK152020/10)*10
    plotData$NPK123017 <- round(plotData$LGANPK123017 /10)*10
    plotData$dY <- round(plotData$LGAdY/1)*1
  }
  
  fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
  
  AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017","dY", "LGAdY")]),
                   by.x=c("ADM1_EN","ADM2_EN") ,by.y=c("REGION","DISTRICT"))
  AOIMap2$month <- plantMonth
  AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
  plotData$month <- plantMonth
  tt <- unique(as.data.frame(plotData[, c("REGION","DISTRICT", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017", "LGAdY", "month")]))
  tt <- tt[order(tt$REGION, tt$DISTRICT), ]
  
  write.csv(tt, fileNameCsv, row.names = FALSE)
  AOIMap3 <- st_as_sf(AOIMap2)
  
  
  ggUrea <- NULL
  if(any(AOIMap3$Urea>0)){
    if(unit == "ha"){
      ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                    "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
      ttz <- "Urea (kg/ha)"
    }else {
      ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                    "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
      ttz <- "Urea (kg/acre)"
    }
    ureasclae <- unique(AOIMap3$Urea)
    keU <- as.character(ureasclae[order(ureasclae)])
    AOIMap3$Urea <- factor(AOIMap3$Urea)
    levels(AOIMap3$Urea) <- keU
    
    require(ggrepel) 
    
    ggUrea <- ggplot(AOIMap3) +
      geom_sf(aes(fill=Urea), col="darkgrey") +
      scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) + 
      geom_text(data=stateLabel, aes(lon, lat, label=REGION, fontface=2), col='black', size=6)+
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
      annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering) +
      # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
      #                        style = north_arrow_fancy_orienteering) +
      # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
      xlab("") + ylab("") +
      ggtitle(ttz) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8)) 
  }
  


  
  ggNPK152020 <- NULL
  ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
  if(any(AOIMap3$NPK152020>0)){
    
    if(unit == "ha"){
      NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                   "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
      tt <- "NPK 15:20:20 (kg/ha)"
    }else{
      NPKcols <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                   "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
      tt <- "NPK 15:20:20 (kg/acre)"
    }
    
    
    NPK152020sclae <- unique(AOIMap3$NPK152020)
    kev <- as.character(NPK152020sclae[order(NPK152020sclae)])
    AOIMap3$NPK152020 <- factor(AOIMap3$NPK152020)
    levels(AOIMap3$NPK152020) <- kev
    
    ggNPK152020 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK152020), col="darkgrey") +
      
      scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) + 
      # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
      #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
    
  }
  

  
  ggNPK123017 <- NULL
  if(any(AOIMap3$NPK123017>0)){
    DAPPpalette <- brewer.pal(9,"YlGnBu")
    if(unit == "ha"){
      NPK123017cols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                         "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
      tt <- "NPK 12:30:17 (kg/ha)"
    }else{
      NPK123017cols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                        "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
      tt <- "NPK 12:30:17 (kg/acre)"
    }
    
    NPK123017sclae <- unique(AOIMap3$NPK123017)
    keNPK123017 <- as.factor(NPK123017sclae[order(NPK123017sclae)])
    AOIMap3$NPK123017 <- factor(AOIMap3$NPK123017)
    levels(AOIMap3$NPK123017) <- keNPK123017
    
    ggNPK123017 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK123017), col="darkgrey") +
      
      scale_fill_manual(values = NPK123017cols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }

  

  
  ggNPK112221 <- NULL
  if(any(AOIMap3$NPK112221>0)){
    DAPPpalette <- brewer.pal(9,"YlGn")
    if(unit == "ha"){
      NPK112221cols <- c("0"="#FFFFFF","25"= "#D9F0A3", "50"= "#ADDD8E", "75"= "#78C679",
                         "100"= "#41AB5D", "125"= "#238443", "150"= "#006837", "175"= "#004529")
      tt <- "NPK 11:22:21 (kg/ha)"
    }else{
      NPK112221cols <- c("0"="#FFFFFF","10"= "#D9F0A3", "20"= "#ADDD8E", "30"= "#78C679",
                         "40"= "#41AB5D", "50"= "#238443", "60"= "#006837", "70"= "#004529")
      
      tt <- "NPK 11:22:21 (kg/acre)"
    }
    
    NPK112221sclae <- unique(AOIMap3$NPK112221)
    keNPK112221<- as.factor(NPK112221sclae[order(NPK112221sclae)])
    AOIMap3$NPK112221 <- factor(AOIMap3$NPK112221)
    levels(AOIMap3$NPK112221) <- keNPK112221
    
    ggNPK112221 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK112221), col="darkgrey") +
      scale_fill_manual(values = NPK112221cols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }
  
  

 
  
  ggNPK251010 <- NULL
  if(any(AOIMap3$NPK251010 > 0)){
    DAPPpalette <- brewer.pal(9,"BuPu")
    if(unit == "ha"){
      NPK251010cols <- c("0"="#FFFFFF","25"= "#BFD3E6", "50"= "#9EBCDA", "75"= "#8C96C6",
                         "100"= "#8C6BB1", "125"= "#88419D", "150"= "#810F7C", "175"= "#4D004B")
      tt <- "NPK 25:10:10 (kg/ha)"
    }else{
      NPK251010cols <- c("0"="#FFFFFF","10"= "#BFD3E6", "20"= "#9EBCDA", "30"= "#8C96C6",
                         "40"= "#8C6BB1", "50"= "#88419D", "60"= "#810F7C", "70"= "#4D004B")
      tt <- "NPK 25:10:10 (kg/acre)"
    }
    
    NPK251010sclae <- unique(AOIMap3$NPK251010)
    keNPK251010<- as.factor(NPK251010sclae[order(NPK251010sclae)])
    AOIMap3$NPK251010 <- factor(AOIMap3$NPK251010)
    levels(AOIMap3$NPK251010) <- keNPK251010
    
    ggNPK251010 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK251010), col="darkgrey") +
      scale_fill_manual(values = NPK251010cols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }
  
  
  
  # brewer.pal(9,"heat")
  

  if(unit == "ha"){
    Ydcols <- c( "34"= "#E8FF00FF", "32"="#A2FF00FF", "30"= "#5DFF00FF", "28"= "#17FF00FF", "26"= "#00FF2EFF", "24"= "#00FF74FF",
                "22"="#00FFB9FF", "20"= "#00FFFFFF", "18"= "#00B9FFFF", "16"= "#0074FFFF", "14"= "#002EFFFF",
                "12"="#1700FFFF", "10"= "#5D00FFFF", "8"= "#A200FFFF", "6"= "#E800FFFF", "4"= "#FF00D1FF",
                "2"= "#FF008BFF", "0"= "#FFFFFF")
    tt <- "Yield increase (t/ha)"
  }else{
    Ydcols <- c("14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                "1"= "#FF008BFF", "0"= "#FFFFFF")
    tt <- "Yield increase (t/acre)"
  }
  
  Ysclae <- unique(AOIMap3$dY)
  keY <- as.factor(Ysclae[order(Ysclae)])
  AOIMap3$dY <- factor(AOIMap3$dY)
  levels(AOIMap3$dY) <- keY
  
  ggYield <- ggplot(AOIMap3) +
    geom_sf(aes(fill=dY), col="darkgrey") +
    
    scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
    #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
    #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
    geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8))
  
  
  # ggUrea, ggNPK152020, ggNPK123017, ggNPK112221, ggNPK251010, ggYield
  

  fileName <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
  pdf(fileName, onefile = TRUE, height = 14, width=12)
  #pdf.options(paper = "a4")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5, 0.8), "null"))))   
  grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  print(ggNPK152020, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(ggNPK123017, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  dev.off()
  
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Savannah_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Savannahcity, lgaGroups = c("Savannah"),
             LGApoints = Savannah, stateLabel = Savannahlabel, textangle=0, unit="acre", couple = "Two")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Savannah_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Savannahcity, lgaGroups = c("Savannah"),
             LGApoints = Savannah, stateLabel = Savannahlabel, textangle=0, unit="ha", couple = "Two")
}


Bono
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bonocity, lgaGroups = c("Bono"),
             LGApoints = Bono, stateLabel = Bonolabel, textangle=0, unit="acre", couple = "Two")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bonocity, lgaGroups = c("Bono"),
             LGApoints = Bono, stateLabel = Bonolabel, textangle=0, unit="ha", couple = "Two")
}



Bono_East
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_East_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bono_Eastcity, lgaGroups = c("Bono East"),
             LGApoints = Bono_East, stateLabel = Bono_Eastlabel, textangle=0, unit="acre", couple = "Two")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_East_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bono_Eastcity, lgaGroups = c("Bono East"),
             LGApoints = Bono_East, stateLabel = Bono_Eastlabel, textangle=0, unit="ha", couple = "Two")
}


Central


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Central_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Centralcity, lgaGroups = c("Central"),
             LGApoints = Central, stateLabel = Centrallabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Central_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Centralcity, lgaGroups = c("Central"),
             LGApoints = Central, stateLabel = Centrallabel, textangle=0, unit="ha", couple = "Two")
}


Ashanti
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ashanti_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ashanti_city, lgaGroups = c("Ashanti"),
             LGApoints = Ashanti, stateLabel = Ashantilabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ashanti_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ashanti_city, lgaGroups = c("Ashanti"),
             LGApoints = Ashanti, stateLabel = Ashantilabel, textangle=0, unit="ha", couple = "Two")
}


Ahafo
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ahafo_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ahafocity, lgaGroups = c("Ahafo"),
             LGApoints = Ahafo, stateLabel = Ahafolabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ahafo_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ahafocity, lgaGroups = c("Ahafo"),
             LGApoints = Ahafo, stateLabel = Ahafolabel, textangle=0, unit="ha", couple = "Two")
}


Eastern
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Eastern_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Eastcity, lgaGroups = c("Eastern"),
             LGApoints = Eastern, stateLabel = Eastlabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Eastern_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Eastcity, lgaGroups = c("Eastern"),
             LGApoints = Eastern, stateLabel = Eastlabel, textangle=0, unit="ha", couple = "Two")
}



Volta
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Volta_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Voltacity, lgaGroups = c("Volta"),
             LGApoints = Volta, stateLabel = Voltalabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Volta_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = East_Voltacity, lgaGroups = c("Volta"),
             LGApoints = Volta, stateLabel = Voltalabel, textangle=0, unit="ha", couple = "Two")
}


##############################################################################################################
## GH: The two functions are used to convert the csv files to pdf files
##############################################################################################################

# ggUrea, ggNPK152020, ggNPK123017, ggYield
pdfTables_GH <- function(FERTtABLE, plantMonth, unit, REGION){
  row.names(FERTtABLE) = NULL
  FERTtABLE <- subset(FERTtABLE, select=-c(month))
  FERTtABLE$LGAdY <- round(FERTtABLE$LGAdY, digits=0)
  
  FERTtABLE <- FERTtABLE[ c("REGION","DISTRICT", "Urea","NPK152020", "NPK123017", "LGAdY")]
  
  if(unit=="acre"){
    colnames(FERTtABLE) <- c("REGION","DISTRICT", "Urea \n (kg/acre)", "NPK 15:20:20 \n (kg/acre)", 
                             "NPK 12:30:17 \n (kg/acre)", "Yield increase \n (t/acre)")
  }else if (unit=="ha"){
    colnames(FERTtABLE) <- c("REGION", "DISTRICT", "Urea \n (kg/ha)", "NPK 15:20:20 \n (kg/ha)", 
                             "NPK 12:30:17 \n (kg/acre)","Yield increase \n (t/ha)")
  }
  
  
  FERTtABLE <- FERTtABLE[order(FERTtABLE$REGION, FERTtABLE$DISTRICT), ]
  
  if(REGION == "East"){
    fileNameR <- paste("Table","_", plantMonth, "_Eastern", ".pdf", sep="")
  }else{
    fileNameR <- paste("Table","_", plantMonth, "_", REGION, ".pdf", sep="")
  }
  
 
  pdf(fileNameR, width = 8, height = 12)
  pdf.options(paper = "a4")
  

  hj <- matrix(c(0, 0, -0.5,-0.5, -0.5, -0.5, -0.5), ncol=7, nrow=nrow(FERTtABLE), byrow=TRUE)
  # if(REGION  %in% c("Oyo", "Benue_Cross River", "Osun")){
  #   tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
  #                         colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.8)),
  #                         rowhead = list(fg_params=list(cex = 0.7)))
  # }else{
  tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
                        colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
                        rowhead = list(fg_params=list(cex = 0.8)))
  # }
  
  # hj <- matrix(c(0, 0, -0.6,-0.6, -0.9, -2.0), ncol=6, nrow=nrow(FERTtABLE), byrow=TRUE)
  # tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
  #                       colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
  #                       rowhead = list(fg_params=list(cex = 0.9)))
  
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

tablepdf_GH <- function(unit, fname, REGION){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", fname, sep=""))
  listcsv <- list.files(getwd(), "csv")
  library(gtable)
  for(files in c(listcsv, listcsv[1])){
    plantMonth <-  strsplit(files, '_')[[1]][1]
    csvtables <- unique(read.csv(files))
    pdfTables_GH(FERTtABLE = csvtables, plantMonth, unit=unit, REGION=REGION)
  }
}



tablepdf_GH(unit="acre", fname="Ahafo_acre", REGION="Ahafo")
tablepdf_GH(unit="ha", fname="Ahafo_ha", REGION="Ahafo")

tablepdf_GH(unit="acre", fname="Ashanti_acre", REGION="Ashanti")
tablepdf_GH(unit="ha", fname="Ashanti_ha", REGION="Ashanti")


tablepdf_GH(unit="acre", fname="Bono_East_acre", REGION="Bono_East")
tablepdf_GH(unit="ha", fname="Bono_East_ha", REGION="Bono_East")

tablepdf_GH(unit="acre", fname="Bono_acre", REGION="Bono")
tablepdf_GH(unit="ha", fname="Bono_ha", REGION="Bono")


tablepdf_GH(unit="acre", fname="Central_acre", REGION="Central")
tablepdf_GH(unit="ha", fname="Central_ha", REGION="Central")


tablepdf_GH(unit="acre", fname="Eastern_acre", REGION="East")
tablepdf_GH(unit="ha", fname="Eastern_ha", REGION="East")

tablepdf_GH(unit="acre", fname="Volta_acre", REGION="Volta")
tablepdf_GH(unit="ha", fname="Volta_ha", REGION="Volta")


tablepdf_GH(unit="acre", fname="Savannah_acre", REGION="Savannah")
tablepdf_GH(unit="ha", fname="Savannah_ha", REGION="Savannah")

####################################################################################################################
####################################################################################################################


#GENERATE FIRST PAGE SAVANNAH

lgaGroups <- c("Savannah", "Bono_East", "Bono", "Central", "Ashanti", "Ahafo", "Eastern", "Volta")

for(p in lgaGroups){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", p, "_acre", sep=""))
  # setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", p, "_ha", sep=""))

  p <- toupper(p)
  fileName_p <- paste(p,  ".pdf", sep="")
  
  b = "This tool contains tables and maps with advice on application rates of Urea, 
NPK 15:20:20 and NPK 12:30:17 fertilizer for cassava. Response to fertilizer  depends on 
soil conditions and the time of planting. Tables are provided that specify 
the recommended fertilizer application rates by district and month of 
planting, as well as the expected root yield response. Maps 
are also provided to show how fertilizer rates vary 
across the Districts"
  
  pdf(fileName_p, paper="a4", pagecentre=FALSE, width=12,height=14)
  plot(NA, xlim=c(0.2,6), ylim=c(0,6), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  #text(0.4, 5, a, pos=4, cex=2.5) ## for Kagera etc
  text(1.4, 5, p, pos=4, cex=3)
  text(0,0.5,b, pos=4, cex=1.1)
  #points(rep(1,4),1:4, pch=15)
  dev.off()
}


Combined_pdf_GH <- function(regionName, unit){
  if(unit == "acre"){
    setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", regionName, "_acre", sep=""))#
  }else{
    setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", regionName, "_ha", sep=""))#
  }
  
  
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
for(snames in lgaGroups[4]){
  print(snames)
  Combined_pdf_GH(regionName = snames, unit="acre")
  Combined_pdf_GH(regionName = snames, unit="ha")
}









############################################################################################################################################################################################################################
############################################################################################################################################################################################################################
## ## select FCY and read the corresponding file 
## Rwanda: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
##############################################################################################################
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH")
ds <- FR_GH_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm
ds$REGION <- ds$Regions
ds$DISTRICT <- ds$Districts
head(ds)


## Brong Ahafo = Bono





Savannah <- droplevels(ds[ds$REGION %in% c("Savannah"), ])
Savannahlabel <-  data.frame(REGION= c("Savannah"), lon=c(-0.75), lat=c(9.5))

Bono_East <- droplevels(ds[ds$REGION %in% c("Bono East"), ])
Bono_Eastlabel <- data.frame(REGION= c("Bono East"), lon=c(-0.65), lat=c(8.5))

Bono <- droplevels(ds[ds$REGION %in% c("Bono"), ])
Bonolabel <- data.frame(REGION= c("Brong Ahafo"), lon=c(-2.5), lat=c(8.68))


Central <- droplevels(ds[ds$REGION %in% c("Central"), ])
Centrallabel <- data.frame(REGION= c("Central"), lon=c(-1.0), lat=c( 6.1))

Ashanti <- droplevels(ds[ds$REGION %in% c("Ashanti"), ])
Ashantilabel <- data.frame(REGION= c("Ashanti"), lon=c(-2.5), lat=c(7.53))

Ahafo <- droplevels(ds[ds$REGION %in% c("Ahafo"), ])
Ahafolabel <- data.frame(REGION= c("Ahafo"), lon=c(-2.8), lat=c(6.5))


Eastern <- droplevels(ds[ds$REGION %in% c("Eastern"), ])
Eastlabel <- data.frame(REGION= c("Eastern"), lon=c(-0.8), lat=c(7.21))

Volta <- droplevels(ds[ds$REGION %in% c("Volta"), ])
Voltalabel <- data.frame(REGION= c("Volta"), lon=c(0.7), lat=c(7.3))


Savannahcity <- data.frame(REGION = c("Savannah"),name=c("Damongo"), lat=c(9.08), lon = c(-1.82))

Centralcity <- data.frame(REGION = c("Central"),name=c("Cape Coast"), lat=c(5.12), lon = c(-1.27))

Bonocity <- data.frame(REGION = c("Brong Ahafo"), name=c("Sunyani"), 
                       lat=c(7.34), lon = c(-2.32))

Bono_Eastcity <- data.frame(REGION = c("Bono East"), name=c("Techiman"), 
                            lat=c(7.58), lon = c(-1.93))

Ashanti_city <- data.frame(REGION = c("Ashanti"), name=c("Kumasi"), 
                           lat=c(6.68), lon = c(-1.63))

Ahafocity <- data.frame(REGION = c("Ahafo"), name=c("Goaso"), 
                        lat=c(6.8), lon = c(-2.52))


Eastcity <- data.frame(REGION = c("Eastern"),name=c("Koforidua"), 
                       lat=c(6.1), lon = c(-0.26))


Voltacity <- data.frame(REGION = c("Volta"),name=c("Ho"), 
                        lat=c(6.6), lon = c(0.47))





##############################################################################################################
## GH: mapping: for every Region, maps will be made per planting month and based on user selection for ha or acre
##############################################################################################################
setwd("/home/akilimo/lintul/lintul/dataSources/GIS_layers")
boundaryGH <- readOGR(dsn=getwd(), layer="gha_admbnda_adm1_gss_20210308")
ghRegion <- readOGR(dsn=getwd(), layer="gha_admbnda_adm2_gss_20210308")




LGAMaps_GH <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple){
  plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$REGION %in% lgaGroups , ])
  
  # if(couple == "Two"){
  #   lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2])
  # }
  # 
  # if(couple == "Three"){
  #   lgaGroups <- c(strsplit(lgaGroups, "_")[[1]][1], strsplit(lgaGroups, "_")[[1]][2], strsplit(lgaGroups, "_")[[1]][3])
  # }
  
  # plotData <- droplevels(plotData[plotData$REGION %in% lgaGroups, ])
  
  AOI <- lgaGroups
  AOIMapS <- subset(boundaryGH, ADM1_EN %in% AOI ) 
  
  AOIMap <- subset(ghRegion, ADM1_EN %in% AOI )
  AOIMap <- AOIMap[,c("ADM1_EN", "ADM2_EN")]
  LGAnames <- as.data.frame(AOIMap)
  LGAnames <- cbind(LGAnames, coordinates(AOIMap))
  colnames(LGAnames) <- c("REGION","DISTRICT","long","lat")
  crop_ngstate <- subset(ghRegion, ADM1_EN %in% AOI )
  
  
  ## take REGION average
  LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                      LGAUrea = round(mean(rateUrea), digits=0),
                      LGANPK112221 = round(mean(rateNPK112221 ), digits=0),
                      LGANPK251010= round(mean(rateNPK251010), digits=0),
                      LGANPK152020 = round(mean(rateNPK152020), digits=0),
                      LGANPK123017 = round(mean(rateNPK123017), digits=0),
                      LGAdY = round(mean(respY), digits=0))
  
  
  LGAaverage$LGAUrea <- ifelse(LGAaverage$LGAUrea <25, 0, LGAaverage$LGAUrea)
  LGAaverage$LGANPK112221 <- ifelse(LGAaverage$LGANPK112221 <25, 0, LGAaverage$LGANPK112221)
  LGAaverage$LGANPK251010 <- ifelse(LGAaverage$LGANPK251010 <25, 0, LGAaverage$LGANPK251010)
  LGAaverage$LGANPK152020 <- ifelse(LGAaverage$LGANPK152020 <25, 0, LGAaverage$LGANPK152020)
  LGAaverage$LGANPK123017 <- ifelse(LGAaverage$LGANPK123017 <25, 0, LGAaverage$LGANPK123017)
  
  
  
  if(unit == 'acre'){
    
    dss <- LGAaverage
    dss$LGAUrea <- dss$LGAUrea / 2.47105
    dss$LGANPK112221 <- dss$LGANPK112221 / 2.47105
    dss$LGANPK251010 <- dss$LGANPK251010 / 2.47105
    dss$LGANPK152020 <- dss$LGANPK152020 / 2.47105
    dss$LGANPK123017 <- dss$LGANPK123017 / 2.47105
    dss$LGAdY <- dss$LGAdY / 2.47105
    
    LGAaverage <- dss
  }
  
  plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
  
  if(unit == "ha"){
    plotData$Urea <- round(plotData$LGAUrea/50)*50
    plotData$NPK112221 <- round(plotData$LGANPK112221/50)*50
    plotData$NPK251010 <- round(plotData$LGANPK251010 /25)*25
    plotData$NPK152020 <- round(plotData$LGANPK152020/50)*50
    plotData$NPK123017 <- round(plotData$LGANPK123017 /25)*25
    plotData$dY <- round(plotData$LGAdY/2)*2
  }else{
    plotData$Urea <- round(plotData$LGAUrea/10)*10
    plotData$NPK112221 <- round(plotData$LGANPK112221/10)*10
    plotData$NPK251010 <- round(plotData$LGANPK251010 /10)*10
    plotData$NPK152020 <- round(plotData$LGANPK152020/10)*10
    plotData$NPK123017 <- round(plotData$LGANPK123017 /10)*10
    plotData$dY <- round(plotData$LGAdY/1)*1
  }
  
  fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
  
  AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017","dY", "LGAdY")]),
                   by.x=c("ADM1_EN","ADM2_EN") ,by.y=c("REGION","DISTRICT"))
  AOIMap2$month <- plantMonth
  AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
  plotData$month <- plantMonth
  tt <- unique(as.data.frame(plotData[, c("REGION","DISTRICT", "Urea", "NPK112221","NPK251010", "NPK152020", "NPK123017", "LGAdY", "month")]))
  tt <- tt[order(tt$REGION, tt$DISTRICT), ]
  
  write.csv(tt, fileNameCsv, row.names = FALSE)
  AOIMap3 <- st_as_sf(AOIMap2)
  
  
  ggUrea <- NULL
  if(any(AOIMap3$Urea>0)){
    if(unit == "ha"){
      ureacols <- c("0" = "#FFFFFF", "25"= "#E5F5E0", "50"= "#C7E9C0", "75"= "#A1D99B", "100"= "#74C476",
                    "125"= "#41AB5D", "150"= "#238B45", "175"="#006D2C", "200"= "#00441B")
      ttz <- "Urea (kg/ha)"
    }else {
      ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                    "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
      ttz <- "Urea (kg/acre)"
    }
    ureasclae <- unique(AOIMap3$Urea)
    keU <- as.character(ureasclae[order(ureasclae)])
    AOIMap3$Urea <- factor(AOIMap3$Urea)
    levels(AOIMap3$Urea) <- keU
    
    require(ggrepel) 
    
    ggUrea <- ggplot(AOIMap3) +
      geom_sf(aes(fill=Urea), col="darkgrey") +
      scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) + 
      geom_text(data=stateLabel, aes(lon, lat, label=REGION, fontface=2), col='black', size=6)+
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      annotation_scale(location = "bl", width_hint = 0.3, line_width = 0.4) +
      annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                             style = north_arrow_fancy_orienteering) +
      # annotation_north_arrow(location = "tr", which_north = "true", pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"),
      #                        style = north_arrow_fancy_orienteering) +
      # annotation_scale(location = "tr", width_hint = 0.2, line_width = 0.4) +
      xlab("") + ylab("") +
      ggtitle(ttz) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8)) 
  }
  
  
  
  
  ggNPK152020 <- NULL
  ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
  if(any(AOIMap3$NPK152020>0)){
    
    if(unit == "ha"){
      NPKcols <- c("0"="#FFFFFF","50"= "#FFF7BC", "100"= "#FEE391", "150"= "#FEC44F", "200"= "#FE9929", 
                   "250"= "#EC7014", "300"= "#CC4C02","350" = "#993404", "400"= "#662506")
      tt <- "NPK 15:20:20 (kg/ha)"
    }else{
      NPKcols <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                   "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
      tt <- "NPK 15:20:20 (kg/acre)"
    }
    
    
    NPK152020sclae <- unique(AOIMap3$NPK152020)
    kev <- as.character(NPK152020sclae[order(NPK152020sclae)])
    AOIMap3$NPK152020 <- factor(AOIMap3$NPK152020)
    levels(AOIMap3$NPK152020) <- kev
    
    ggNPK152020 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK152020), col="darkgrey") +
      
      scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) + 
      # geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
      #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
    
  }
  
  
  
  ggNPK123017 <- NULL
  if(any(AOIMap3$NPK123017>0)){
    DAPPpalette <- brewer.pal(9,"YlGnBu")
    if(unit == "ha"){
      NPK123017cols <- c("0"="#FFFFFF","25"= "#C7E9B4", "50"= "#7FCDBB", "75"= "#41B6C4",
                         "100"= "#1D91C0", "125"= "#225EA8", "150"= "#253494", "175"= "#081D58")
      tt <- "NPK 12:30:17 (kg/ha)"
    }else{
      NPK123017cols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                         "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
      tt <- "NPK 12:30:17 (kg/acre)"
    }
    
    NPK123017sclae <- unique(AOIMap3$NPK123017)
    keNPK123017 <- as.factor(NPK123017sclae[order(NPK123017sclae)])
    AOIMap3$NPK123017 <- factor(AOIMap3$NPK123017)
    levels(AOIMap3$NPK123017) <- keNPK123017
    
    ggNPK123017 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK123017), col="darkgrey") +
      
      scale_fill_manual(values = NPK123017cols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }
  
  
  
  
  ggNPK112221 <- NULL
  if(any(AOIMap3$NPK112221>0)){
    DAPPpalette <- brewer.pal(9,"YlGn")
    if(unit == "ha"){
      NPK112221cols <- c("0"="#FFFFFF","25"= "#D9F0A3", "50"= "#ADDD8E", "75"= "#78C679",
                         "100"= "#41AB5D", "125"= "#238443", "150"= "#006837", "175"= "#004529")
      tt <- "NPK 11:22:21 (kg/ha)"
    }else{
      NPK112221cols <- c("0"="#FFFFFF","10"= "#D9F0A3", "20"= "#ADDD8E", "30"= "#78C679",
                         "40"= "#41AB5D", "50"= "#238443", "60"= "#006837", "70"= "#004529")
      
      tt <- "NPK 11:22:21 (kg/acre)"
    }
    
    NPK112221sclae <- unique(AOIMap3$NPK112221)
    keNPK112221<- as.factor(NPK112221sclae[order(NPK112221sclae)])
    AOIMap3$NPK112221 <- factor(AOIMap3$NPK112221)
    levels(AOIMap3$NPK112221) <- keNPK112221
    
    ggNPK112221 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK112221), col="darkgrey") +
      scale_fill_manual(values = NPK112221cols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=2, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }
  
  
  
  
  
  ggNPK251010 <- NULL
  if(any(AOIMap3$NPK251010 > 0)){
    DAPPpalette <- brewer.pal(9,"BuPu")
    if(unit == "ha"){
      NPK251010cols <- c("0"="#FFFFFF","25"= "#BFD3E6", "50"= "#9EBCDA", "75"= "#8C96C6",
                         "100"= "#8C6BB1", "125"= "#88419D", "150"= "#810F7C", "175"= "#4D004B")
      tt <- "NPK 25:10:10 (kg/ha)"
    }else{
      NPK251010cols <- c("0"="#FFFFFF","10"= "#BFD3E6", "20"= "#9EBCDA", "30"= "#8C96C6",
                         "40"= "#8C6BB1", "50"= "#88419D", "60"= "#810F7C", "70"= "#4D004B")
      tt <- "NPK 25:10:10 (kg/acre)"
    }
    
    NPK251010sclae <- unique(AOIMap3$NPK251010)
    keNPK251010<- as.factor(NPK251010sclae[order(NPK251010sclae)])
    AOIMap3$NPK251010 <- factor(AOIMap3$NPK251010)
    levels(AOIMap3$NPK251010) <- keNPK251010
    
    ggNPK251010 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK251010), col="darkgrey") +
      scale_fill_manual(values = NPK251010cols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }
  
  
  
  # brewer.pal(9,"heat")
  
  
  if(unit == "ha"){
    Ydcols <- c( "34"= "#E8FF00FF", "32"="#A2FF00FF", "30"= "#5DFF00FF", "28"= "#17FF00FF", "26"= "#00FF2EFF", "24"= "#00FF74FF",
                 "22"="#00FFB9FF", "20"= "#00FFFFFF", "18"= "#00B9FFFF", "16"= "#0074FFFF", "14"= "#002EFFFF",
                 "12"="#1700FFFF", "10"= "#5D00FFFF", "8"= "#A200FFFF", "6"= "#E800FFFF", "4"= "#FF00D1FF",
                 "2"= "#FF008BFF", "0"= "#FFFFFF")
    tt <- "Yield increase (t/ha)"
  }else{
    Ydcols <- c("14"= "#17FF00FF", "13"= "#00FF2EFF", "12"= "#00FF74FF",
                "11"="#00FFB9FF", "10"= "#00FFFFFF", "9"= "#00B9FFFF", "8"= "#0074FFFF", "7"= "#002EFFFF",
                "6"="#1700FFFF", "5"= "#5D00FFFF", "4"= "#A200FFFF", "3"= "#E800FFFF", "2"= "#FF00D1FF",
                "1"= "#FF008BFF", "0"= "#FFFFFF")
    tt <- "Yield increase (t/acre)"
  }
  
  Ysclae <- unique(AOIMap3$dY)
  keY <- as.factor(Ysclae[order(Ysclae)])
  AOIMap3$dY <- factor(AOIMap3$dY)
  levels(AOIMap3$dY) <- keY
  
  ggYield <- ggplot(AOIMap3) +
    geom_sf(aes(fill=dY), col="darkgrey") +
    
    scale_fill_manual(values = Ydcols, guide = guide_legend(reverse=TRUE))+
    geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
    geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
    #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
    #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
    geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8))
  
  
  # ggUrea, ggNPK152020, ggNPK123017, ggNPK112221, ggNPK251010, ggYield
  
  
  fileName <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
  pdf(fileName, onefile = TRUE, height = 14, width=12)
  #pdf.options(paper = "a4")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.8, 5, 5, 0.8), "null"))))   
  grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  print(ggNPK152020, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(ggNPK123017, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(ggYield, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  dev.off()
  
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Savannah_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Savannahcity, lgaGroups = c("Savannah"),
             LGApoints = Savannah, stateLabel = Savannahlabel, textangle=0, unit="acre", couple = "Two")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Savannah_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Savannahcity, lgaGroups = c("Savannah"),
             LGApoints = Savannah, stateLabel = Savannahlabel, textangle=0, unit="ha", couple = "Two")
}


Bono
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bonocity, lgaGroups = c("Bono"),
             LGApoints = Bono, stateLabel = Bonolabel, textangle=0, unit="acre", couple = "Two")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bonocity, lgaGroups = c("Bono"),
             LGApoints = Bono, stateLabel = Bonolabel, textangle=0, unit="ha", couple = "Two")
}



Bono_East
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_East_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bono_Eastcity, lgaGroups = c("Bono East"),
             LGApoints = Bono_East, stateLabel = Bono_Eastlabel, textangle=0, unit="acre", couple = "Two")
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Bono_East_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Bono_Eastcity, lgaGroups = c("Bono East"),
             LGApoints = Bono_East, stateLabel = Bono_Eastlabel, textangle=0, unit="ha", couple = "Two")
}


Central


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Central_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Centralcity, lgaGroups = c("Central"),
             LGApoints = Central, stateLabel = Centrallabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Central_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Centralcity, lgaGroups = c("Central"),
             LGApoints = Central, stateLabel = Centrallabel, textangle=0, unit="ha", couple = "Two")
}


Ashanti
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ashanti_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ashanti_city, lgaGroups = c("Ashanti"),
             LGApoints = Ashanti, stateLabel = Ashantilabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ashanti_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ashanti_city, lgaGroups = c("Ashanti"),
             LGApoints = Ashanti, stateLabel = Ashantilabel, textangle=0, unit="ha", couple = "Two")
}


Ahafo
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ahafo_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ahafocity, lgaGroups = c("Ahafo"),
             LGApoints = Ahafo, stateLabel = Ahafolabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Ahafo_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Ahafocity, lgaGroups = c("Ahafo"),
             LGApoints = Ahafo, stateLabel = Ahafolabel, textangle=0, unit="ha", couple = "Two")
}


Eastern
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Eastern_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Eastcity, lgaGroups = c("Eastern"),
             LGApoints = Eastern, stateLabel = Eastlabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Eastern_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Eastcity, lgaGroups = c("Eastern"),
             LGApoints = Eastern, stateLabel = Eastlabel, textangle=0, unit="ha", couple = "Two")
}



Volta
setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Volta_acre") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = Voltacity, lgaGroups = c("Volta"),
             LGApoints = Volta, stateLabel = Voltalabel, textangle=0, unit="acre", couple = "Two")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/Volta_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in month.name){
  LGAMaps_GH(plantMonth=m,  cities = East_Voltacity, lgaGroups = c("Volta"),
             LGApoints = Volta, stateLabel = Voltalabel, textangle=0, unit="ha", couple = "Two")
}


##############################################################################################################
## GH: The two functions are used to convert the csv files to pdf files
##############################################################################################################

# ggUrea, ggNPK152020, ggNPK123017, ggYield
pdfTables_GH <- function(FERTtABLE, plantMonth, unit, REGION){
  row.names(FERTtABLE) = NULL
  FERTtABLE <- subset(FERTtABLE, select=-c(month))
  FERTtABLE$LGAdY <- round(FERTtABLE$LGAdY, digits=0)
  
  FERTtABLE <- FERTtABLE[ c("REGION","DISTRICT", "Urea","NPK152020", "NPK123017", "LGAdY")]
  
  if(unit=="acre"){
    colnames(FERTtABLE) <- c("REGION","DISTRICT", "Urea \n (kg/acre)", "NPK 15:20:20 \n (kg/acre)", 
                             "NPK 12:30:17 \n (kg/acre)", "Yield increase \n (t/acre)")
  }else if (unit=="ha"){
    colnames(FERTtABLE) <- c("REGION", "DISTRICT", "Urea \n (kg/ha)", "NPK 15:20:20 \n (kg/ha)", 
                             "NPK 12:30:17 \n (kg/acre)","Yield increase \n (t/ha)")
  }
  
  
  FERTtABLE <- FERTtABLE[order(FERTtABLE$REGION, FERTtABLE$DISTRICT), ]
  
  if(REGION == "East"){
    fileNameR <- paste("Table","_", plantMonth, "_Eastern", ".pdf", sep="")
  }else{
    fileNameR <- paste("Table","_", plantMonth, "_", REGION, ".pdf", sep="")
  }
  
  
  pdf(fileNameR, width = 8, height = 12)
  pdf.options(paper = "a4")
  
  
  hj <- matrix(c(0, 0, -0.5,-0.5, -0.5, -0.5, -0.5), ncol=7, nrow=nrow(FERTtABLE), byrow=TRUE)
  # if(REGION  %in% c("Oyo", "Benue_Cross River", "Osun")){
  #   tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
  #                         colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.8)),
  #                         rowhead = list(fg_params=list(cex = 0.7)))
  # }else{
  tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.7)),
                        colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
                        rowhead = list(fg_params=list(cex = 0.8)))
  # }
  
  # hj <- matrix(c(0, 0, -0.6,-0.6, -0.9, -2.0), ncol=6, nrow=nrow(FERTtABLE), byrow=TRUE)
  # tt1 <- ttheme_default(core=list(fg_params=list(hjust= as.vector(hj), x=0.05, cex = 0.9)),
  #                       colhead=list(fg_params=list(hjust=0, x=0.1, cex = 0.9)),
  #                       rowhead = list(fg_params=list(cex = 0.9)))
  
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

tablepdf_GH <- function(unit, fname, REGION){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", fname, sep=""))
  listcsv <- list.files(getwd(), "csv")
  library(gtable)
  for(files in c(listcsv, listcsv[1])){
    plantMonth <-  strsplit(files, '_')[[1]][1]
    csvtables <- unique(read.csv(files))
    pdfTables_GH(FERTtABLE = csvtables, plantMonth, unit=unit, REGION=REGION)
  }
}



tablepdf_GH(unit="acre", fname="Ahafo_acre", REGION="Ahafo")
tablepdf_GH(unit="ha", fname="Ahafo_ha", REGION="Ahafo")

tablepdf_GH(unit="acre", fname="Ashanti_acre", REGION="Ashanti")
tablepdf_GH(unit="ha", fname="Ashanti_ha", REGION="Ashanti")


tablepdf_GH(unit="acre", fname="Bono_East_acre", REGION="Bono_East")
tablepdf_GH(unit="ha", fname="Bono_East_ha", REGION="Bono_East")

tablepdf_GH(unit="acre", fname="Bono_acre", REGION="Bono")
tablepdf_GH(unit="ha", fname="Bono_ha", REGION="Bono")


tablepdf_GH(unit="acre", fname="Central_acre", REGION="Central")
tablepdf_GH(unit="ha", fname="Central_ha", REGION="Central")


tablepdf_GH(unit="acre", fname="Eastern_acre", REGION="East")
tablepdf_GH(unit="ha", fname="Eastern_ha", REGION="East")

tablepdf_GH(unit="acre", fname="Volta_acre", REGION="Volta")
tablepdf_GH(unit="ha", fname="Volta_ha", REGION="Volta")


tablepdf_GH(unit="acre", fname="Savannah_acre", REGION="Savannah")
tablepdf_GH(unit="ha", fname="Savannah_ha", REGION="Savannah")

####################################################################################################################
####################################################################################################################


#GENERATE FIRST PAGE SAVANNAH

lgaGroups <- c("Savannah", "Bono_East", "Bono", "Central", "Ashanti", "Ahafo", "Eastern", "Volta")

for(p in lgaGroups){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", p, "_acre", sep=""))
  # setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", p, "_ha", sep=""))
  
  p <- toupper(p)
  fileName_p <- paste(p,  ".pdf", sep="")
  
  b = "This tool contains tables and maps with advice on application rates of Urea, 
NPK 15:20:20 and NPK 12:30:17 fertilizer for cassava. Response to fertilizer  depends on 
soil conditions and the time of planting. Tables are provided that specify 
the recommended fertilizer application rates by district and month of 
planting, as well as the expected root yield response. Maps 
are also provided to show how fertilizer rates vary 
across the Districts"
  
  pdf(fileName_p, paper="a4", pagecentre=FALSE, width=12,height=14)
  plot(NA, xlim=c(0.2,6), ylim=c(0,6), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  #text(0.4, 5, a, pos=4, cex=2.5) ## for Kagera etc
  text(1.4, 5, p, pos=4, cex=3)
  text(0,0.5,b, pos=4, cex=1.1)
  #points(rep(1,4),1:4, pch=15)
  dev.off()
}


Combined_pdf_GH <- function(regionName, unit){
  if(unit == "acre"){
    setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", regionName, "_acre", sep=""))#
  }else{
    setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/GH/", regionName, "_ha", sep=""))#
  }
  
  
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
for(snames in lgaGroups[4]){
  print(snames)
  Combined_pdf_GH(regionName = snames, unit="acre")
  Combined_pdf_GH(regionName = snames, unit="ha")
}





###########################################################################
## select FCY and read the corresponding file 
## RW: Subsetting for the user defined Region and selecting a coordinate to put the state name in the map
###########################################################################
ds <- FR_RW_FCY2_plm ## ds will be defined based on the current yield. e.g. if user idicate that thier current yield is yield level 2 ds  = FR_NG_FCY2_plm
unique(ds$STATE)
ds$REGION <- ds$STATE
Amajyaruguru <- droplevels(ds[ds$STATE == "Amajyaruguru", ])
Amajyarugurulabel <- data.frame(state= c("Amajyaruguru"), lat=c(-1.34), lon=c(30.05))

Amajyepfo <- droplevels(ds[ds$STATE == "Amajyepfo", ])
Amajyepfolabel <- data.frame(state= c("Amajyepfo"), lat=c(-1.8), lon=c(29.48))

Iburasirazuba <- droplevels(ds[ds$STATE == "Iburasirazuba", ])
Iburasirazubalabel <- data.frame(state= c("Iburasirazuba"), lat=c(-1.1), lon=c(30.4))

Iburengerazuba <- droplevels(ds[ds$STATE == "Iburengerazuba", ])
Iburengerazubalabel <- data.frame(state= c("Iburengerazuba"), lat=c(-1.5), lon=c(29.19))

Kigali <- droplevels(ds[ds$STATE == "Umujyi wa Kigali", ])
Kigalilabel <- data.frame(state= c("Umujyi wa Kigali"), lat=c(-1.79), lon=c(30.1))


Amajyarugurucity <- data.frame(REGION = c("Amajyaruguru"),name=c("Byumba"), lat=c(-1.57), lon = c(30.06))
Amajyepfocity <- data.frame(REGION = c("Amajyepfo"),name=c("Nyanza"), lat=c(-2.35), lon = c(29.75))
Iburasirazubacity <- data.frame(REGION = c("Iburasirazuba"),name=c("Rwamagana"), lat=c(-1.95), lon = c(30.43))
Iburengerazubacity <- data.frame(REGION = c("Iburengerazuba"),name=c("Kibuye"), lat=c(-2.06), lon = c(29.34))
Kigalicity <- data.frame(REGION = c("Umujyi wa Kigali"),name=c("Kigali"), lat=c(-1.95), lon = c(30.07))




##############################################################################################################
## RW: mapping: for every Region, maps will be made per planting month and based on user selection for ha or acre
##############################################################################################################
setwd("/home/akilimo/lintul/lintul/dataSources/GIS_layers")
boundaryRW <- readOGR(dsn=getwd(), layer="gadm36_RWA_1")
RWRegion <- readOGR(dsn=getwd(), layer="gadm36_RWA_2")

plot(RWRegion)



LGAMaps_RW <- function(plantMonth, cities, lgaGroups, LGApoints, stateLabel, textangle, unit, couple, engname){
  plotData <- droplevels(LGApoints[LGApoints$plm == plantMonth & LGApoints$REGION %in% lgaGroups , ])
  
  
  AOI <- lgaGroups
  AOIMapS <- subset(boundaryRW, NAME_1 %in% AOI ) 
  
  AOIMap <- subset(RWRegion, NAME_1 %in% AOI )
  AOIMap <- AOIMap[,c("NAME_1", "NAME_2")]
  LGAnames <- as.data.frame(AOIMap)
  LGAnames <- cbind(LGAnames, coordinates(AOIMap))
  colnames(LGAnames) <- c("REGION","DISTRICT","long","lat")
  crop_ngstate <- subset(RWRegion, NAME_1 %in% AOI )
  
  
  ## take REGION average
  LGAaverage <- ddply(plotData, .(DISTRICT, REGION), summarize,
                      LGAUrea = round(mean(rateUrea), digits=0),
                      LGANPK171717 = round(mean(rateNPK171717 ), digits=0),
                      LGAMOP = round(mean(rateMOP), digits=0),
                      LGADAP = round(mean(rateNDAP), digits=0),
                      LGAdY = round(mean(respY), digits=0))
  
  
  LGAaverage$LGAUrea <- ifelse(LGAaverage$LGAUrea <25, 0, LGAaverage$LGAUrea)
  LGAaverage$LGANPK171717 <- ifelse(LGAaverage$LGANPK171717 <25, 0, LGAaverage$LGANPK171717)
  LGAaverage$LGAMOP <- ifelse(LGAaverage$LGAMOP <25, 0, LGAaverage$LGAMOP)
  LGAaverage$LGADAP <- ifelse(LGAaverage$LGADAP <25, 0, LGAaverage$LGADAP)


  plotData <- merge(plotData, LGAaverage, by=c("DISTRICT", "REGION"))
  

    plotData$Urea <- round(plotData$LGAUrea/10)*10
    plotData$NPK17_17_17 <- round(plotData$LGANPK171717/10)*10
    plotData$MOP <- round(plotData$LGAMOP /5)*5
    plotData$DAP <- round(plotData$LGADAP/5)*5
    plotData$dY <- round(plotData$LGAdY/1)*1
 
  
  fileNameCsv <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".csv", sep="")
  
  AOIMap2 <- merge(AOIMap, unique(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","MOP", "DAP", "LGAdY")]),
                   by.x=c("NAME_1","NAME_2") ,by.y=c("REGION","DISTRICT"))
  AOIMap2$month <- plantMonth
  AOIMap2 <- AOIMap2[!is.na(AOIMap2$Urea), ]
  plotData$month <- plantMonth
  tt <- unique(as.data.frame(plotData[, c("REGION","DISTRICT", "Urea", "NPK17_17_17","MOP", "DAP", "LGAdY", "month")]))
  tt <- tt[order(tt$REGION, tt$DISTRICT), ]
  colnames(tt) <- c("PROVINCE","DISTRICT", "Urea", "NPK17_17_17","MOP", "DAP", "LGAdY", "month")
  write.csv(tt, paste(getwd(), fileNameCsv, sep="/"), row.names = FALSE)
  AOIMap3 <- st_as_sf(AOIMap2)
  
  
  ggUrea <- NULL
  if(any(AOIMap3$Urea>0)){
    if(unit == "ha"){
      ureacols <- c("0" = "#FFFFFF", "90"= "#E5F5E0", "100"= "#C7E9C0", "110"= "#A1D99B", "120"= "#74C476",
                    "130"= "#41AB5D", "135"= "#238B45", "140"="#006D2C", "150"= "#00441B")
      ttz <- "Urea (kg/ha)"
    }else {
      ureacols <- c("0" = "#FFFFFF", "10"= "#E5F5E0", "20"= "#C7E9C0", "30"= "#A1D99B", "40"= "#74C476",
                    "50"= "#41AB5D", "60"= "#238B45", "70"="#006D2C", "80"= "#00441B")
      ttz <- "Urea (kg/acre)"
    }
    
    ureacols <- ureacols[names(ureacols) %in% AOIMap3$Urea]
    
   
    ureasclae <- unique(AOIMap3$Urea)
    keU <- as.character(ureasclae[order(ureasclae)])
    AOIMap3$Urea <- factor(AOIMap3$Urea)
    levels(AOIMap3$Urea) <- keU
    
    require(ggrepel) 
    
    ggUrea <- ggplot(AOIMap3) +
      geom_sf(aes(fill=Urea), col="darkgrey") +
      scale_fill_manual(values = ureacols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
      geom_text(data=stateLabel, aes(lon, lat, label=engname, fontface=2), col='black', size=6)+
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
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
  
  
  
  
  ggNPK171717 <- NULL
  ### NPK 151515 palette <- brewer.pal(9,"YlOrBr")
  if(any(AOIMap3$NPK17_17_17>0)){
    
    if(unit == "ha"){
      NPKcols <- c("0"="#FFFFFF","35"= "#FFF7BC", "40"= "#FEE391", "45"= "#FEC44F", "50"= "#FE9929", 
                   "55"= "#EC7014", "60"= "#CC4C02","65" = "#993404", "70"= "#662506")
      tt <- "NPK 17:17:17 (kg/ha)"
    }else{
      NPKcols <- c("40"="#FFFFFF","50"= "#FFF7BC", "60"= "#FEE391", "70"= "#FEC44F", "80"= "#FE9929", 
                   "90"= "#EC7014", "100"= "#CC4C02", "110" ="#993404","120" = "#662506")
      tt <- "NPK 15:20:20 (kg/acre)"
    }
    
    NPKcols <- NPKcols[names(NPKcols) %in% AOIMap3$NPK17_17_17]
    
    NPK17_17_17sclae <- unique(AOIMap3$NPK17_17_17)
    kev <- as.character(NPK17_17_17sclae[order(NPK17_17_17sclae)])
    AOIMap3$NPK17_17_17 <- factor(AOIMap3$NPK17_17_17)
    levels(AOIMap3$NPK17_17_17) <- kev
    
    ggNPK171717 <- ggplot(AOIMap3) +
      geom_sf(aes(fill=NPK17_17_17), col="darkgrey") +
      
      scale_fill_manual(values = NPKcols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
    
  }
  
  
  
  ggMOP <- NULL
  if(any(AOIMap3$MOP>0)){
    DAPPpalette <- brewer.pal(9,"YlGnBu")
    if(unit == "ha"){
     MOPcols <- c("0"="#FFFFFF", "40" = "#FFFFD9" ,"45" ="#EDF8B1" ,"50"= "#C7E9B4", "55"= "#7FCDBB", "60"= "#41B6C4",
                         "65"= "#1D91C0","70"="#9999FF", "75"="#0000FF", "80"= "#225EA8", "85"= "#253494", "90"= "#081D58")
      tt <- "MOP (kg/ha)"
    }else{
      MOPcols <- c("0"="#FFFFFF","10"= "#C7E9B4", "20"= "#7FCDBB", "30"= "#41B6C4",
                         "40"= "#1D91C0", "50"= "#225EA8", "60"= "#253494", "70"= "#081D58")
      tt <- "NPK 12:30:17 (kg/acre)"
    }
    
    MOPcols <- MOPcols[names(MOPcols) %in% AOIMap3$MOP]
    
    MOPsclae <- unique(AOIMap3$MOP)
    kev <- as.character(MOPsclae[order(MOPsclae)])
    AOIMap3$MOP <- factor(AOIMap3$MOP)
    levels(AOIMap3$MOP) <- kev
    
    ggMOP <- ggplot(AOIMap3) +
      geom_sf(aes(fill=MOP), col="darkgrey") +
      
      scale_fill_manual(values = MOPcols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
      geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
      xlab("") + ylab("") +
      ggtitle(tt) +
      theme_bw() +
      theme(legend.position="right", legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
            axis.text = element_text(size=8))
  }
  
  
  
  
  ggDAP <- NULL
  if(any(AOIMap3$DAP>0)){
    DAPPpalette <- brewer.pal(9,"YlGn")
    if(unit == "ha"){
     DAPcols <- c("0"="#FFFFFF","45"= "#F7FCB9" ,"50"= "#D9F0A3", "55"= "#ADDD8E", "60"= "#78C679",
                         "65"= "#41AB5D", "70"= "#238443", "75"= "#006837", "80"= "#004529")
      tt <- "DAP (kg/ha)"
    }else{
      DAPcols <- c("0"="#FFFFFF","10"= "#D9F0A3", "20"= "#ADDD8E", "30"= "#78C679",
                         "40"= "#41AB5D", "50"= "#238443", "60"= "#006837", "70"= "#004529")
      
      tt <- "NPK 11:22:21 (kg/acre)"
    }
    
    DAPcols <- DAPcols[names(DAPcols) %in% AOIMap3$DAP]
    
    
   DAPsclae <- unique(AOIMap3$DAP)
    keDAP <- as.factor(DAPsclae[order(DAPsclae)])
    AOIMap3$DAP <- factor(AOIMap3$DAP)
    levels(AOIMap3$DAP) <- keDAP
    
    ggDAP <- ggplot(AOIMap3) +
      geom_sf(aes(fill=DAP), col="darkgrey") +
      scale_fill_manual(values = DAPcols, guide = guide_legend(reverse=TRUE))+
      geom_path(data=crop_ngstate, aes(x=long, y=lat, group=group), size=0.5) +
      geom_path(data=AOIMapS, aes(x=long, y=lat, group=group), size=1, col="darkgrey") +
      geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) +
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
    geom_text_repel(data=LGAnames, aes(long, lat, label=DISTRICT, fontface=1, angle=textangle), size=4, segment.size = NA) + 
    #geom_text(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=2.5)+
    #geom_text_repel(data=LGAnames, aes(long, lat, label=LGA, fontface=1, angle=textangle), size=3) + 
    geom_point(data=cities, aes(x=lon, y=lat), shape=16,  size=3) +
    xlab("") + ylab("") +
    ggtitle(tt) +
    theme_bw() +
    theme(legend.position="right", legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5, size=16, face ='bold'),
          axis.text = element_text(size=8))
  
  
  # ggUrea, ggNPK152020, ggNPK123017, ggNPK112221, ggNPK251010, ggYield
  
  
  fileName <- paste(plantMonth, "_", paste(lgaGroups, collapse="_"), ".pdf", sep="")
  pdf(fileName, onefile = TRUE, height = 14, width=12)
  #pdf.options(paper = "a4")
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(4, 2, heights = unit(c(0.6, 4, 4, 4, 0.4), "null"))))   
  grid.text(paste("Planting in", plantMonth, sep=" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  print(ggUrea, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))         
  print(ggNPK171717, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(ggMOP, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(ggDAP, vp = viewport(layout.pos.row = 3, layout.pos.col = 2))
  print(ggYield, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
  dev.off()
  
}



setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/Amajyaruguru_ha") ## maps and table for the selected using will be stored in the defined director for every region
for(m in c("February", "March", "September", "October")){
  LGAMaps_RW(plantMonth=m,  cities = Amajyarugurucity, lgaGroups = c("Amajyaruguru"), engname = "Northern",
             LGApoints = Amajyaruguru, stateLabel = Amajyarugurulabel, textangle=0, unit="ha")
}

setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/Amajyepfo_ha")
for(m in c("February", "March", "September", "October")){
  LGAMaps_RW(plantMonth=m,  cities = Amajyepfocity, lgaGroups = c("Amajyepfo"),engname = "Southern",
             LGApoints = Amajyepfo, stateLabel = Amajyepfolabel, textangle=0, unit="ha")
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/Iburasirazuba_ha")
for(m in c("February", "March", "September", "October")){
  LGAMaps_RW(plantMonth=m,  cities = Iburasirazubacity, lgaGroups = c("Iburasirazuba"),engname = "Eastern",
             LGApoints = Iburasirazuba, stateLabel = Iburasirazubalabel, textangle=0, unit="ha")
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/Iburengerazuba_ha")
for(m in c("February", "March", "September", "October")){
  LGAMaps_RW(plantMonth=m,  cities = Iburengerazubacity, lgaGroups = c("Iburengerazuba"),engname = "Western",
             LGApoints = Iburengerazuba, stateLabel = Iburengerazubalabel, textangle=0, unit="ha")
}


setwd("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/Kigali_ha")
for(m in c("February", "March", "September", "October")){
  LGAMaps_RW(plantMonth=m,  cities = Kigalicity, lgaGroups = c("Umujyi wa Kigali"),engname = "Kigali",
             LGApoints = Kigali, stateLabel = Kigalilabel, textangle=0, unit="ha")
}


# ggUrea, ggNPK152020, ggNPK123017, ggYield
pdfTables_RW <- function(FERTtABLE, plantMonth, unit, REGION){
  row.names(FERTtABLE) = NULL
  FERTtABLE <- subset(FERTtABLE, select=-c(month))
  FERTtABLE$LGAdY <- round(FERTtABLE$LGAdY, digits=0)
  
  FERTtABLE <- FERTtABLE[ c("PROVINCE","DISTRICT", "Urea","NPK17_17_17", "MOP", "DAP","LGAdY")]
  
  colnames(FERTtABLE) <- c("PROVINCE", "DISTRICT", "Urea \n (kg/ha)", "NPK 17:17:17 \n (kg/ha)", 
                             "NPK MOP \n (kg/ha)", "MOP \n (kg/ha)", "Yield increase \n (t/ha)")
  
  
  FERTtABLE$PROVINCE <- ifelse(FERTtABLE$PROVINCE == "Amajyaruguru", "Northern",
                               ifelse(FERTtABLE$PROVINCE == "Amajyepfo", "Southern", 
                                      ifelse(FERTtABLE$PROVINCE == "Iburasirazuba", "Eastern", 
                                             ifelse(FERTtABLE$PROVINCE == "Iburengerazuba", "Western", "Kigali"))))

  
  FERTtABLE <- FERTtABLE[order(FERTtABLE$PROVINCE, FERTtABLE$DISTRICT), ]
  
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

tablepdf_RW <- function(unit, fname, REGION){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/", fname, sep=""))
  listcsv <- list.files(getwd(), "csv")
  library(gtable)
  for(files in c(listcsv, listcsv[1])){
    plantMonth <-  strsplit(files, '_')[[1]][1]
    csvtables <- unique(read.csv(files))
    pdfTables_RW(FERTtABLE = csvtables, plantMonth, unit=unit, REGION=REGION)
  }
}


tablepdf_RW(unit="ha", fname="Amajyaruguru_ha", REGION="Amajyaruguru")
tablepdf_RW(unit="ha", fname="Amajyepfo_ha", REGION="Amajyepfo")
tablepdf_RW(unit="ha", fname="Iburasirazuba_ha", REGION="Iburasirazuba")
tablepdf_RW(unit="ha", fname="Iburengerazuba_ha", REGION="Iburengerazuba")
tablepdf_RW(unit="ha", fname="Kigali_ha", REGION="Kigali")






lgaGroups <- c("Amajyaruguru", "Amajyepfo", "Iburasirazuba", "Iburengerazuba", "Kigali")

for(p in lgaGroups){
  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/", p, "_ha", sep=""))
getwd()
    p <- toupper(p)
  fileName_p <- paste(p,  ".pdf", sep="")
  print(fileName_p)
  b = "This tool contains tables and maps with advice for fertilizers application 
for cassva. The advice consider access to Urea, NPK 17:17:17, MOP and DAP. 
Response to fertilizer depends on soil conditions and the time of planting. 
Tables and maps show the recommended fertilizer rates by district and month
of planting for harvest after 12 months." 

  
  pdf(fileName_p, paper="a4", pagecentre=FALSE, width=12,height=14)
  plot(NA, xlim=c(0.2,6), ylim=c(0,6), bty='n',
       xaxt='n', yaxt='n', xlab='', ylab='')
  #text(0.4, 5, a, pos=4, cex=2.5) ## for Kagera etc
  text(0, 5, p, pos=4, cex=3)
  text(0, 1,b, pos=4, cex=1.1)
  #points(rep(1,4),1:4, pch=15)
  dev.off()
}


Combined_pdf_RW <- function(regionName, unit){

  setwd(paste("/home/akilimo/projects/PaperbasedDashboard_v2/FR/RW/", regionName, "_ha", sep=""))#
  
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
  Combined_pdf_RW(regionName = snames, unit="ha")
}





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





