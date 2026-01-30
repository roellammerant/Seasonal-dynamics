##%######################################################%##
#                                                          #
####                    Get GBIF data                   ####
#                                                          #
##%######################################################%##
## Note:
## This file shows how to extract thermal affinity for molluscs but doesn´t run without extracting the occurrence data yourself

install.packages("rgbif")
library(rgbif)
install.packages("stringr")
library(stringr)
library(dplyr)
library(tidyverse)

### UsageKey manual extraction Gbif
IsraelGBiFList <- read.csv2("~/GitHub/ViennaSeasonality/output_data/GbifSpeciesListMollusc.csv")
IsraelGBiFList  <- IsraelGBiFList[which(IsraelGBiFList$matchType  =='EXACT'),]
IsraelTaxonKeys <- IsraelGBiFList[c(2,18)]

#### Occurances
occ_download(
  pred_in("taxonKey", IsraelTaxonKeys$usageKey),
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("occurrenceStatus", "PRESENT"),
  pred_gte("year", 1900),
  format = "SIMPLE_CSV",
  user="add username", pwd="add password" , email="add mail"
)

Occurences <- occ_download_get('0015783-250827131500795') %>%
  occ_download_import()
#write.csv(Occurences ,file='output_data/GBIFMollusc.csv') (To big of a file for GitHub)

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
Occurences <- read_csv("output_data/GBIFMollusc.csv")
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

SpheniaRueppellii<- flags_Cleaned[flags_Cleaned$species == c('Sphenia rueppellii'),]

thin(
  SpheniaRueppellii,
  lat.col = "decimalLatitude",
  long.col = "decimalLongitude",
  spec.col = "species",
  thin.par = 5,
  reps = 1,
  write.files = TRUE,
  max.files = 1,
  out.dir = "output_data/MolluscThinnedOccurences"
)

### Import Thinned spatial datasets
### Link them to environmental data
### Repeat this code for all 120 species and save as csv files
library(readr)
library(sp)
ModiolulaPhaseolina <- read_csv("output_data/MolluscThinnedOccurences/SpheniaRueppellii_Thinned.csv")
summary(ModiolulaPhaseolina)

###  Biooracle temperature and link to coordinates
library(biooracler)
x <-list_layers("Ocean Temperature")
info_layer("thetao_baseline_2000_2019_depthsurf")

#### download layers
dataset_id <- "thetao_baseline_2000_2019_depthsurf"
time = c('2000-01-01T00:00:00Z','2010-01-01T00:00:00Z')
latitude = c(-33, 60)
longitude = c(-17,74)
constraints = list(time, latitude, longitude)
names(constraints) = c("time", "latitude", "longitude")
variables = c("thetao_mean")
layers <- download_layers(dataset_id, variables, constraints,fmt = "csv")
RecentLayer <- layers[layers$time == c('2010-01-01T00:00:00Z'),]

# Here our working projection
setProj<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
xy.dt<- ModiolulaPhaseolina[,2:3]
quanti.sp<- SpatialPointsDataFrame(coords = xy.dt, 
                                   data = ModiolulaPhaseolina,
                                   proj4string = CRS(setProj))
library(raster)
TemperatureFrame <- cbind(RecentLayer$longitude,RecentLayer$latitude, RecentLayer$thetao_mean)
TempRaster <- rasterFromXYZ(TemperatureFrame)
quanti.sp@data$Temp <-raster::extract(TempRaster, quanti.sp)

# Back to dataframe and exclude NA datapoints that were missed in the cleaning process
# The cleaning package struggled with points in the Fjords etc
df<- quanti.sp@data
summary(df$Temp)
df <- na.omit(df)

write.csv(df, file='output_data/MolluscThermalAffinity/SpheniaRueppellii.csv')

### Load all csv files with temp data into one dataframe
library(data.table)  
files <- list.files(path = "output_data/MolluscThermalAffinity",pattern = ".csv", full.names = TRUE)
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp )
write.csv(data, file='output_data/MolluscThermalAffinity/OverallThermalAffinity.csv')

### Clean out Mediterannean data
### Choose longitude sections of 5 degrees and clean out mediterannean latitude
### Merge after

Subset_minusToZero <- OverallThermalAffinity[OverallThermalAffinity$decimalLongitude > -6 & OverallThermalAffinity$decimalLongitude < 0,]
wm <- borders("world", colour = "gray50", fill = "gray50")
ggplot() +
  coord_fixed() +
  wm +
  geom_point(data = NoMed_minusToZeroA,
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
ThermalAffinityMolluscMax <- aggregate(ThermalAffinityWithoutMed$Temp, 
                      list(ThermalAffinityWithoutMed$species), FUN=max)
colnames(ThermalAffinityMolluscMax) <- c("Species","MaxThermal")
ThermalAffinityMolluscMean <- aggregate(ThermalAffinityWithoutMed$Temp, 
                                       list(ThermalAffinityWithoutMed$species), FUN=mean)
colnames(ThermalAffinityMolluscMean) <- c("Species","MeanThermal")

ThermalAffinityMollusc<- as.data.frame(cbind(ThermalAffinityMolluscMax$Species,ThermalAffinityMolluscMax$MaxThermal,
                                   ThermalAffinityMolluscMean$MeanThermal))
colnames(ThermalAffinityMollusc) <- c("Species","MaxThermal","MeanThermal" )
ThermalAffinityMollusc$MaxThermal <- as.numeric(as.character(ThermalAffinityMollusc$MaxThermal))
ThermalAffinityMollusc$MeanThermal <- as.numeric(as.character(ThermalAffinityMollusc$MeanThermal))
write_csv(ThermalAffinityMollusc, file='output_data/MolluscThermalAffinityIndices.csv')
