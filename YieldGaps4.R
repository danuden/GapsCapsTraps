#####Load libraries
library(sp)
library(rgdal)
library(raster)
library(BAMMtools)

#####Set directory path
path.root <- "E:/Manuscripts/PREPARATION/NatureSustainability"
setwd(path.root)

#####Read in GYGA yield gap data
###Rainfed cereal grains
#Maize
RainMaize <- read.csv("GygaRainfedMaize.csv", header = TRUE)
dim(RainMaize)
head(RainMaize)

#Rice
RainRice <- read.csv("GygaRainfedRice.csv", header = TRUE)
dim(RainRice)
head(RainRice)

#Wheat
RainWheat <- read.csv("GygaRainfedWheat.csv", header = TRUE)
dim(RainWheat)
head(RainWheat)

#Give rainfed yield gap variables crop-specific names
RainMaize$YW.Maize <- RainMaize$YW
RainMaize$YA.Maize <- RainMaize$YA
RainMaize$YW.YA.Maize <- RainMaize$YW.YA
head(RainMaize)
RainRice$YW.Rice <- RainRice$YW
RainRice$YA.Rice <- RainRice$YA
RainRice$YW.YA.Rice <- RainRice$YW.YA
head(RainRice)
RainWheat$YW.Wheat <- RainWheat$YW
RainWheat$YA.Wheat <- RainWheat$YA
RainWheat$YW.YA.Wheat <- RainWheat$YW.YA
head(RainWheat)

#Trim data frames
retain <- c("COUNTRY", "YW.Maize", "YA.Maize", "YW.YA.Maize", "TOTAL_AREA_HA")
RainMaize <- RainMaize[retain]
head(RainMaize)
retain <- c("COUNTRY", "YW.Rice", "YA.Rice", "YW.YA.Rice", "TOTAL_AREA_HA")
RainRice <- RainRice[retain]
head(RainRice)
retain <- c("COUNTRY", "YW.Wheat", "YA.Wheat", "YW.YA.Wheat", "TOTAL_AREA_HA")
RainWheat <- RainWheat[retain]
head(RainWheat)

#Give total areas crop-specific names
RainMaize$MaizeHaYG <- RainMaize$TOTAL_AREA_HA
RainRice$RiceHaYG <- RainRice$TOTAL_AREA_HA
RainWheat$WheatHaYG <- RainWheat$TOTAL_AREA_HA

#Merge rainfed crop datasets
RainGaps <- merge(RainMaize, RainRice, by.x = "COUNTRY", by.y = "COUNTRY", all.x = TRUE, all.y = TRUE)
dim(RainGaps)
head(RainGaps)
RainGaps <- merge(RainGaps, RainWheat, by.x = "COUNTRY", by.y = "COUNTRY", all.x = TRUE, all.y = TRUE)
dim(RainGaps)
head(RainGaps)

#Delete unnecessary columns
RainGaps <- within(RainGaps, rm(TOTAL_AREA_HA, TOTAL_AREA_HA.x, TOTAL_AREA_HA.y))
head(RainGaps)

#####Read in world countries shapefile
World <- readOGR("F:/GIS/TM_WORLD_BORDERS-0.3/TM_WORLD_BORDERS-0.3.shp",
"TM_WORLD_BORDERS-0.3", verbose = TRUE, p4s = NULL)
plot(World)
dim(World@data)
head(World@data)

#####Write rain gaps to new csv file
write.csv(RainGaps, "GlobalRainfedCerealYieldGaps.csv")

#####Merge World shapefile and crop data
###Rainfed crops
WorldRain <- merge(World, RainGaps, by.x = "NAME", by.y = "COUNTRY", all.x = FALSE, all.y = TRUE)
dim(WorldRain@data)
head(WorldRain@data)

#####Calculate relative yield gap (current yield as percentage of potential yield) for each crop
###Maize
head(WorldRain@data)
WorldRain@data$R_YG_M <- 1 - (WorldRain@data$YA.Maize / WorldRain@data$YW.Maize)
head(WorldRain@data)
summary(WorldRain@data$R_YG_M)

###Rice
WorldRain@data$R_YG_R <- 1 - (WorldRain@data$YA.Rice / WorldRain@data$YW.Rice)
head(WorldRain@data)
summary(WorldRain@data$R_YG_R)

###Wheat
WorldRain@data$R_YG_W <- 1 - (WorldRain@data$YA.Wheat / WorldRain@data$YW.Wheat)
head(WorldRain@data)
summary(WorldRain@data$R_YG_W)

#####Calculate proportion of total grain production area for each of three crops
###Total cereal grain area
WorldRain@data$CerealHaYG <- NA
head(WorldRain@data)
class(WorldRain@data$MaizeHaYG)
for(x in 1:nrow(WorldRain@data)){
	WorldRain@data$CerealHaYG[x] <- sum(WorldRain@data$MaizeHaYG[x], WorldRain@data$RiceHaYG[x],
	WorldRain@data$WheatHaYG[x], na.rm = TRUE)
}
head(WorldRain@data)

###Proportion area maize, rice, and wheat
WorldRain@data$MaizeHaYGP <- WorldRain@data$MaizeHaYG / WorldRain@data$CerealHaYG
WorldRain@data$RiceHaYGP <- WorldRain@data$RiceHaYG / WorldRain@data$CerealHaYG
WorldRain@data$WheatHaYGP <- WorldRain@data$WheatHaYG / WorldRain@data$CerealHaYG
head(WorldRain@data)

#####Calculate area-weighted average relative cereal grain yield gaps for each country
###All three crops at once, using the weighted mean function
#Create new dataframe for relative yield gaps of 3 crops
CerealYG <- data.frame(MaizeRYG = WorldRain@data$R_YG_M)
CerealYG$RiceRYG <- WorldRain@data$R_YG_R
CerealYG$WheatRYG <- WorldRain@data$R_YG_W
dim(CerealYG)
head(CerealYG)
CerealYG2 <- t(CerealYG)
dim(CerealYG2)
head(CerealYG2)

#Create new dataframe of proportional land area weights
CerealAreas <- data.frame(MaizeRA = WorldRain@data$MaizeHaYGP)
CerealAreas$RiceRA <- WorldRain@data$RiceHaYGP
CerealAreas$WheatRA <- WorldRain@data$WheatHaYGP
dim(CerealAreas)
head(CerealAreas)
CerealAreas2 <- t(CerealAreas)
dim(CerealAreas2)
head(CerealAreas2)

#Calculate weighted mean among 3 crops in each country
WorldRain@data$CerealRYG <- NA
for(x in 1:nrow(WorldRain@data)){
	WorldRain@data$CerealRYG[x] <- weighted.mean(CerealYG2[,x], CerealAreas2[,x], na.rm = TRUE)
}
head(WorldRain@data)

#####Write result to ESRI Shapefile
writeOGR(WorldRain, "WorldRain", "WorldRain", driver = 'ESRI Shapefile', overwrite = TRUE)

#####Important note: 'NA' values will be converted to '0' in ArcGIS, so
#####manually correct for it in ArcGIS by changing 0's to 'NoData'. There are
#####currently no true 0 values in the "CerealRYG", "WheatHaYGP", "RiceHaYGP",
#####or "MaizeHaYGP" columns.

#####Retain only desired columns
retain <- c("NAME", "FIPS", "UN", "POP2005",
"YW.Maize", "YA.Maize", "YW.YA.Maize", "R_YG_M", "MaizeHaYG",
"YW.Rice", "YA.Rice", "YW.YA.Rice", "R_YG_R", "RiceHaYG",
"YW.Wheat", "YA.Wheat", "YW.YA.Wheat", "R_YG_W", "WheatHaYG",
"CerealHaYG", "MaizeHaYGP", "RiceHaYGP", "WheatHaYGP",
"CerealRYG")
WorldRain@data <- WorldRain@data[retain]
head(WorldRain@data)

#####Write ESRI shapefile with updated attribute table
###Rainfed
writeOGR(WorldRain, "WorldRain", "WorldRain", driver = 'ESRI Shapefile', overwrite = TRUE)

#####Write attribute table to csv file
write.csv(WorldRain@data, "WorldRainGaps.csv")

