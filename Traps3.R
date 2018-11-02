#####Load libraries
library(sp)
library(rgdal)
library(raster)
library(BAMMtools)

#####Set directory path
path.root <- "E:/Manuscripts/PREPARATION/NatureSustainability"
setwd(path.root)

#####Read in shapefile of world countries
World <- readOGR("G:/GIS/TM_WORLD_BORDERS-0.3/WorldCopy.shp",
"WorldCopy", verbose = TRUE, p4s = NULL)
plot(World)
dim(World@data)
head(World@data)

#####Read in poverty data
#####Values of "<2.5" for undernourishment converted to "2.5"
#####Therefore, human undernourishment averages may be slightly inflated for these countries
Poverty <- read.csv("FAOPovertyGapCountries2000To2015.csv", header = TRUE)
dim(Poverty)
head(Poverty)
unique(Poverty$Item)

###Rename 3 year range column to single (middle) year.
###For example, "1999.2001" year range renamed to "2000" year.
Poverty$Year1 <- substr(Poverty$Year.Code, 1, 4)
Poverty$Year <- as.numeric(Poverty$Year1) + 1
head(Poverty)

###Subset out variables of interest
#Percent undernourishment
Under <- subset(Poverty, Item == "Prevalence of undernourishment (%) (3-year average)")
dim(Under)
head(Under)
unique(Under$Year)

###Trim dataset to 2006-2015 decade
Under2 <- subset(Under, Year > 2005 & Year < 2016)
unique(Under2$Year)

###Give undernourishment variable new name
head(Under2)
Under2$HungryPerc <- Under2$Value
summary(Under2$HungryPerc)

###Manually change NA value to 2.5
unique(Under2$Value)
Under2$Value

###Write to csv for manually setting undernourishment values of "< 2.5" to "2.5".
write.csv(Under2, "FAOPovertyGapCountries2000To2015a.csv")

###Read revised version back in
Under2 <- read.csv("FAOPovertyGapCountries2000To2015a.csv", header = TRUE)

###Create a new data frame with a row for each country
Under3 <- data.frame(Country = unique(Under2$Area))
head(Under3)

###Calculate mean undernourishment percentage each year
for(x in 1:nrow(Under3)){
	Subset <- subset(Under2, Area == Under3$Country[x])
	Under3$HungryPerc[x] <- mean(Subset$HungryPerc, na.rm = TRUE)
}
dim(Under3)
head(Under3)

###Merge countries shapefile with undernourishment data
WorldUnder <- merge(World, Under3, by.x = "NAME", by.y = "Country")
dim(WorldUnder@data)
head(WorldUnder@data)

###Eliminate non-focal countries
WorldUnder@data$ISNA <- is.na(WorldUnder$HungryPerc)
head(WorldUnder@data)
WorldUnder2 <- subset(WorldUnder, ISNA == "FALSE")
dim(WorldUnder2)
head(WorldUnder2)

###Create breaks at levels set in https://www.ifpri.org/sites/default/files/ghi/2015/feature_3702.html
WorldUnder2@data$HungryPerc.Level <- NA
for(x in 1:nrow(WorldUnder2@data)){
	if(WorldUnder2@data$HungryPerc[x] < 10.00)
		WorldUnder2@data$HungryPerc.Level[x] <- 1
	else if(WorldUnder2@data$HungryPerc[x] > 20.00)
		WorldUnder2@data$HungryPerc.Level[x] <- 3
	else
		WorldUnder2@data$HungryPerc.Level[x] <- 2
}
head(WorldUnder2@data)

###Write result to ESRI shapefile
writeOGR(WorldUnder2, "WorldUnder", "WorldUnder", driver = 'ESRI Shapefile', overwrite = TRUE)

