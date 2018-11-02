#####Load libraries
library(sp)
library(rgdal)
library(raster)
library(BAMMtools)

#####Set directory path
path.root <- "E:/Manuscripts/PREPARATION/NatureSustainability"
setwd(path.root)

#####Read in P and N data (fertilizer use for crops + livestock pasture + aquaculture)
Nut <- read.csv("FAOAgNPGapCountries2002To2015.csv", header = TRUE)
dim(Nut)
head(Nut)

#####Read in landuse data from FAO
Area <- read.csv("FAOAreaGapCountries1961to2015.csv", header = TRUE)
dim(Area)
head(Area)

#####Trim datasets to most recent decade (2006-2015)
###Nutrients
Nut2 <- subset(Nut, Year >= 2006)
dim(Nut2)
head(Nut2)

###Landuse
Area2 <- subset(Area, Year >= 2006)
dim(Area2)
head(Area2)

#Convert 1000s of hectares to hectares
Area2$Hectares <- Area2$Value * 1000
head(Area2)

#Pull out subset area datasets
unique(Area2$Item)

#Agricultural area (arable land + permanent crops + permanent meadows and pastures)
AreaAg <- subset(Area2, Item == "Agricultural area")
dim(AreaAg)
head(AreaAg)

#####Break phosphorus and nitrogen into separate datasets
#Phosphorus
P <- subset(Nut2, Item == "Nutrient phosphate P2O5 (total)")
dim(P)
head(P)

#Nitrogen
N <- subset(Nut2, Item == "Nutrient nitrogen N (total)")
dim(N)
head(N)

#####Convert phosphorus and nitrogen values from tonnes to kilograms
#Phosphorus
P$PTotal <- P$Value * 1000
head(P)

#Nitrogen
N$NTotal <- N$Value * 1000
head(N)

#####Merge cropland area and nutrient datasets according to area code and year
###Phosphorus
PArea <- merge(P, AreaAg, by.x = c("Area.Code", "Year"), by.y = c("Area.Code", "Year"))
dim(PArea)
head(PArea)

###Nitrogen
NArea <- merge(N, AreaAg, by.x = c("Area.Code", "Year"), by.y = c("Area.Code", "Year"))
dim(NArea)
head(NArea)

#####Rename selected columns
###Phosphorus
PArea$Country <- PArea$Area.x
NArea$Country <- NArea$Area.x

#####Retain only desired columns
#Phosphorus
retainP <- c("Area.Code", "Year", "Country", "PTotal", "Hectares")
PArea2 <- PArea[retainP]
head(PArea2)

#Nitrogen
retainN <- c("Area.Code", "Year", "Country", "NTotal", "Hectares")
NArea2 <- NArea[retainN]
head(NArea2)

#####Create new data frame for each nutrient with row for each country
###Phosphorus
PArea3 <- data.frame(Country = unique(PArea2$Country))
dim(PArea3)
head(PArea3)

###Nitrogen
NArea3 <- data.frame(Country = unique(NArea2$Country))
dim(NArea3)
head(NArea3)

#####Calculate mean values for variables of interest
###Phosphorus
for(x in 1:nrow(PArea3)){
	Subset <- subset(PArea2, Country == PArea3$Country[x])
	PArea3$PKg[x] <- mean(Subset$PTotal, na.rm = TRUE)
	PArea3$Ha[x] <- mean(Subset$Hectares, na.rm = TRUE)
}
dim(PArea3)
head(PArea3)

###Nitrogen
for(x in 1:nrow(NArea3)){
	Subset <- subset(NArea2, Country == NArea3$Country[x])
	NArea3$PKg[x] <- mean(Subset$NTotal, na.rm = TRUE)
	NArea3$Ha[x] <- mean(Subset$Hectares, na.rm = TRUE)
}
dim(NArea3)
head(NArea3)

#####Calculate mean P and N inputs per year (kg/ha/year)
###Phosphorus
for(x in 1:nrow(PArea3)){
	PArea3$PAnn[x] <- PArea3$PKg[x] / PArea3$Ha[x]
}	
head(PArea3)

###Nitrogen
for(x in 1:nrow(NArea3)){
	NArea3$NAnn[x] <- NArea3$PKg[x] / NArea3$Ha[x]
}	
head(NArea3)

#####Add columns indicating if countries have surpassed regional planetary boundary values of Steffen et al. (2015)
###Phosphorus
PArea3$PCap <- NA
PArea3$PCap <- ifelse(PArea3$PAnn > 7.5, 1, 0)
head(PArea3)

###Nitrogen
NArea3$NCap <- NA
NArea3$NCap <- ifelse(NArea3$NAnn > 55.0, 1, 0)
head(NArea3)

#####Subset nations over the lower and upper P and N planetary boundary range estimates of Steffen et al. (2015).
###Phosphorus range of 4.1 - 7.5 kg/ha/year
PLower <- subset(PArea3, PAnn > 4.1)
PLower
nrow(PLower) / nrow(PArea3)

PUpper <- subset(PArea3, PAnn > 7.5)
PUpper
nrow(PUpper) / nrow(PArea3)

###Nitrogen range of 41.0 - 55.0 kg/ha/year
NLower <- subset(NArea3, NAnn > 41.0)
NLower
nrow(NLower) / nrow(NArea3)

NUpper <- subset(NArea3, NAnn > 55.0)
NUpper
nrow(NUpper) / nrow(NArea3)

#####Read in shapefile of world countries
World <- readOGR("G:/GIS/TM_WORLD_BORDERS-0.3/WorldCopy.shp",
"WorldCopy", verbose = TRUE, p4s = NULL)
plot(World)
dim(World@data)
head(World@data)

#####Merge World shapefile with P and N datasets
###Phosphorus
WorldP <- merge(World, PArea3, by.x = "NAME", by.y = "Country")
dim(WorldP@data)
head(WorldP@data)

###Nitrogen
WorldPN <- merge(WorldP, NArea3, by.x = "NAME", by.y = "Country")
dim(WorldPN@data)
head(WorldPN@data)

#####Eliminate non-focal countries
WorldPN
dim(WorldPN)
WorldPN@data$ISNA <- is.na(WorldPN@data$PAnn)
head(WorldPN@data)
WorldPN2 <- subset(WorldPN, ISNA == 'FALSE')
dim(WorldPN2)
head(WorldPN2)

#####Write results to ESRI shapefiles
writeOGR(WorldPN2, "WorldPN", "WorldPN", driver = 'ESRI Shapefile', overwrite = TRUE)


