##%######################################################%##
#                                                          #
####                    Get GBIF data                   ####
#                                                          #
##%######################################################%##
## Note:
## This file shows how to extract thermal affinity for fishes but doesn´t run without extracting the occurrence data yourself

install.packages("rgbif")
library(rgbif)
install.packages("stringr")
library(stringr)
library(dplyr)
library(tidyverse)

### UsageKey manual extraction Gbif
library(readr)
GbifListFish <- read_csv("output_data/GbifSpeciesListFish.csv") 
GbifListFish<- GbifListFish[which(GbifListFish$matchType  =='EXACT'),]
GbifListFish <- GbifListFish[c(2,18)]

#### Occurences
occ_download(
  pred_in("taxonKey", GbifListFish$usageKey),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred_gte("year", 1900),
  format = "SIMPLE_CSV",
  user="add username", pwd="add password" , email="add your mail"
)

Occurences <- occ_download_get('0008036-250920141307145') %>%
  occ_download_import()
#write.csv(Occurences ,file='output_data/GBIFFish.csv') (To big of a file for GitHub)

##%######################################################%##
#                                                          #
####           Load and use occurrence data             ####
#                                                          #
##%######################################################%##

library(devtools)
library(CoordinateCleaner)
library(readr)
library(ggplot2)
library(spThin)

### Basis of record - leave certain methods out?
Occurences <- read.delim("C:/Users/Lammerant/Downloads/GbifFish/0008036-250920141307145.csv")
table(Occurences$basisOfRecord)
# take out fossil species
Occurences <- Occurences[Occurences$basisOfRecord != c('FOSSIL_SPECIMEN'),]

### Clean coordinates
flags <- clean_coordinates(x = Occurences, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           species = "species",
                           tests = c("capitals", "centroids",
                                     "equal", "zeros", "gbif", "institutions","seas")) # most test are on by default


### Subset coordinates that fall in the sea (110 075 occurences cleaned)

flags_Cleaned <- flags[flags$.zer != c('FALSE'),]
flags_Cleaned <- flags_Cleaned[flags_Cleaned$.cap != c('FALSE'),]
flags_Cleaned <- flags_Cleaned[flags_Cleaned$.cen != c('FALSE'),]
flags_Cleaned <- flags_Cleaned[flags_Cleaned$.inst != c('FALSE'),]
flags_Cleaned <- flags_Cleaned[flags_Cleaned$.sea != c('TRUE'),]

#plot data to get an overview
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = flags_Cleaned,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()

### Spatial tinning of data for every species in the data set
### repeat for each species
#### Doesn´t work on really large occurance amounts 30k+
#### So run on subsets of data and all the thinned subsets together to get the same result
 

Species <- as.data.frame(table(flags_Cleaned$species))

ZeusFaber <- flags_Cleaned[flags_Cleaned$species == c('Zeus faber'),]
  
thin(
  ZeusFaber ,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par = 5,
  reps = 1,
  write.files = TRUE,
  max.files = 1,
  out.dir = "output_data/FishThinnedOccurences"
)

### Import Thinned spatial datasets
### Link them to environmental data
### Repeat this code for all 120 species and save as csv files
library(readr)
library(sp)
XyrichtysNovacula <- read_csv("output_data/FishThinnedOccurences/AbudefdufSaxatilis_Thinned.csv")
summary(XyrichtysNovacula)

###  Biooracle temperature and link to coordinates
library(biooracler)
x <-list_layers("Ocean Temperature")
info_layer("thetao_baseline_2000_2019_depthsurf")

#### download layers
dataset_id <- "thetao_baseline_2000_2019_depthsurf"
time = c('2000-01-01T00:00:00Z','2010-01-01T00:00:00Z')
latitude = c(-75, 75)
longitude = c(-179,179.5)
constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")
variables = c("thetao_mean")
layers <- download_layers(dataset_id, variables, constraints,fmt = "csv")
RecentLayer <- layers[layers$time == c('2010-01-01T00:00:00Z'),]

# Here our working projection
setProj<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
xy.dt<- XyrichtysNovacula[,2:3]
quanti.sp<- SpatialPointsDataFrame(coords = xy.dt, 
                                   data = XyrichtysNovacula,
                                   proj4string = CRS(setProj))
library(raster)
TemperatureFrame <- cbind(RecentLayer$longitude,RecentLayer$latitude, RecentLayer$thetao_mean)
TempRaster <- rasterFromXYZ(TemperatureFrame)
quanti.sp@data$Temp <-raster::extract(TempRaster, quanti.sp)

# Back to dataframe and exclude NA datapoints that were missed in the cleaning process
# The cleaning package struggled with points in the Fjords, bays etc
# Or no raster coords exist for it as to close to land
df<- quanti.sp@data
summary(df$Temp)
df <- na.omit(df)

write.csv(df, file='output_data/FishThermalAffinity/ZeusFaber.csv')

### Load all csv files with temp data into one dataframe
library(data.table)  
files <- list.files(path = "output_data/FishThermalAffinity",pattern = ".csv", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
write.csv(data, file='output_data/FishThermalAffinity/OverallThermalAffinity.csv')

### Clean out Mediterannean data

### Clean out Mediterannean data
### Choose longitude sections of 5 degrees and clean out mediterannean latitude
### Merge after

Subset_minusToZero <- OverallThermalAffinity[OverallThermalAffinity$decimalLongitude > -6 & OverallThermalAffinity$decimalLongitude < 0,]
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = NoMed_minusToZeroB,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()
NoMed_minusToZeroA <- Subset_minusToZero[Subset_minusToZero$decimalLatitude < 30,]
NoMed_minusToZeroB <- Subset_minusToZero[Subset_minusToZero$decimalLatitude > 40,]

Subset_ZeroToTwentyfive <- OverallThermalAffinity[OverallThermalAffinity$decimalLongitude > 0 & OverallThermalAffinity$decimalLongitude < 25,]
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = NoMed_ZeroToTwentyfiveB,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()
NoMed_ZeroToTwentyfiveA <- Subset_ZeroToTwentyfive[Subset_ZeroToTwentyfive$decimalLatitude < 30,]
NoMed_ZeroToTwentyfiveB <- Subset_ZeroToTwentyfive[Subset_ZeroToTwentyfive$decimalLatitude > 46,]

Subset_TwentyfiveToFourty <- OverallThermalAffinity[OverallThermalAffinity$decimalLongitude > 25 & OverallThermalAffinity$decimalLongitude < 40,]
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = NoMed_TwentyfiveToFourtyB,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()

NoMed_TwentyfiveToFourtyA <- Subset_TwentyfiveToFourty[Subset_TwentyfiveToFourty$decimalLatitude < 31,]
NoMed_TwentyfiveToFourtyB <- Subset_TwentyfiveToFourty[Subset_TwentyfiveToFourty$decimalLatitude > 41.5,]

Subset_NomedA <- OverallThermalAffinity[OverallThermalAffinity$decimalLongitude > -180 & OverallThermalAffinity$decimalLongitude < -6,]
Subset_NomedB <- OverallThermalAffinity[OverallThermalAffinity$decimalLongitude > 40 & OverallThermalAffinity$decimalLongitude < 180,]

### Combine all the subsets
### Calculate thermal affinity molluscs (Mean, Max)
ThermalAffinityWithoutMed <- rbind(NoMed_TwentyfiveToFourtyA,
                                   NoMed_TwentyfiveToFourtyB,NoMed_ZeroToTwentyfiveA,
                                   NoMed_ZeroToTwentyfiveB, NoMed_minusToZeroA,
                                   NoMed_minusToZeroB,Subset_NomedB,Subset_NomedA)

wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = ThermalAffinityWithoutMed,
             aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkred",
             size = 0.5) +
  theme_bw()
ThermalAffinityFishMax <- aggregate(ThermalAffinityWithoutMed$Temp, 
                                       list(ThermalAffinityWithoutMed$species), FUN=max)
colnames(ThermalAffinityFishMax) <- c("Species","MaxThermal")
ThermalAffinityFishMean <- aggregate(ThermalAffinityWithoutMed$Temp, 
                                        list(ThermalAffinityWithoutMed$species), FUN=mean)
colnames(ThermalAffinityFishMean) <- c("Species","MeanThermal")

ThermalAffinityFish<- as.data.frame(cbind(ThermalAffinityFishMax$Species,ThermalAffinityFishMax$MaxThermal,
                                             ThermalAffinityFishMean$MeanThermal))
colnames(ThermalAffinityFish) <- c("Species","MaxThermal","MeanThermal" )
ThermalAffinityFish$MaxThermal <- as.numeric(as.character(ThermalAffinityFish$MaxThermal))
ThermalAffinityFish$MeanThermal <- as.numeric(as.character(ThermalAffinityFish$MeanThermal))
write_csv(ThermalAffinityFish, file='output_data/FishThermalAffinityIndices.csv')