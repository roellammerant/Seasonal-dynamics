#%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'dplyr', 'data.table','SingleCaseES')
ipak(packages)

##%######################################################%##
#                                                          #
####        Load ViaMaris dataset and summarise         ####
#                                                          #
##%######################################################%##
ViaMaris <- read.csv("DATA/ViaMaris.csv", 
                     na = "NA")
SpViaMaris <- read_excel("output_data/ViaMaris_InvasiveStatus.xlsx", 
                         na = "NA")


### turn data into presence absence
ViaMarisCategories <- ViaMaris[c(2:5)]
ViaMarisSpecies <- ViaMaris[-c(1:5)]

### Create categories to merch replicates
ViaMarisCategories$Merched_Category <- factor(paste(ViaMaris$eventYear, 
                                            ViaMaris$eventSeason,
                                            sep = "_"))

### Create Community level abundance columns
ViaMarisTranspose <- t(ViaMarisSpecies)
ViaMarisTranspose <- as.data.frame(ViaMarisTranspose)
ViaMarisTranspose$Invasive <- SpViaMaris$Invasive

ViaMarisNIS  <- ViaMarisTranspose[which(ViaMarisTranspose$Invasive  =='1'),]
ViaMarisNAT  <- ViaMarisTranspose[which(ViaMarisTranspose$Invasive  =='0'),]
ViaMarisNIS <- ViaMarisNIS %>% 
  mutate_if(is.character, as.numeric)
ViaMarisNAT <- ViaMarisNAT %>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(ViaMarisNIS))
AbundanceNAT <- as.data.frame(colSums(ViaMarisNAT))

ViaMarisCategories$AbundanceNIS <- AbundanceNIS$`colSums(ViaMarisNIS)`[-c(859)]
ViaMarisCategories$AbundanceNAT <- AbundanceNAT$`colSums(ViaMarisNAT)`[-c(859)]

### Mean values per Year*Season 
Mean_NIS <- aggregate(ViaMarisCategories$AbundanceNIS, 
                      list(ViaMarisCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(ViaMarisCategories$AbundanceNAT, 
                      list(ViaMarisCategories$Merched_Category), FUN=mean)
### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio2009 <-Ratios_NIS$`2009_Autumn`/Ratios_NIS$`2009_Spring`
Ratios_NIS$Ratio2010 <-Ratios_NIS$`2010_Autumn`/Ratios_NIS$`2010_Spring`
Ratios_NIS$Ratio2011 <-Ratios_NIS$`2011_Autumn`/Ratios_NIS$`2011_Spring`
Ratios_NIS$Ratio2012 <-Ratios_NIS$`2012_Autumn`/Ratios_NIS$`2012_Spring`
Ratios_NIS$Ratio2015<-Ratios_NIS$`2015_Autumn`/Ratios_NIS$`2015_Spring`
Ratios_NIS$Ratio2016 <-Ratios_NIS$`2016_Autumn`/Ratios_NIS$`2016_Spring`
Ratios_NIS$Ratio2017 <-Ratios_NIS$`2017_Autumn`/Ratios_NIS$`2017_Spring`
Ratios_NIS$Ratio2018<-Ratios_NIS$`2018_Autumn`/Ratios_NIS$`2018_Spring`
Ratios_NIS$Ratio2019 <-Ratios_NIS$`2019_Autumn`/Ratios_NIS$`2019_Spring`
Ratios_NIS$Ratio2020 <-Ratios_NIS$`2020_Autumn`/Ratios_NIS$`2020_Spring`
Ratios_NIS$Ratio2021<-Ratios_NIS$`2021_Autumn`/Ratios_NIS$`2021_Spring`
Ratios_NIS$Ratio2022 <-Ratios_NIS$`2022_Autumn`/Ratios_NIS$`2022_Spring`
Ratios_NIS$Ratio2024 <-Ratios_NIS$`2024_Autumn`/Ratios_NIS$`2024_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),-c(1)]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio2009 <-Ratios_NAT$`2009_Autumn`/Ratios_NAT$`2009_Spring`
Ratios_NAT$Ratio2010 <-Ratios_NAT$`2010_Autumn`/Ratios_NAT$`2010_Spring`
Ratios_NAT$Ratio2011 <-Ratios_NAT$`2011_Autumn`/Ratios_NAT$`2011_Spring`
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2015 <-Ratios_NAT$`2015_Autumn`/Ratios_NAT$`2015_Spring`
Ratios_NAT$Ratio2016 <-Ratios_NAT$`2016_Autumn`/Ratios_NAT$`2016_Spring`
Ratios_NAT$Ratio2017 <-Ratios_NAT$`2017_Autumn`/Ratios_NAT$`2017_Spring`
Ratios_NAT$Ratio2018<-Ratios_NAT$`2018_Autumn`/Ratios_NAT$`2018_Spring`
Ratios_NAT$Ratio2019 <-Ratios_NAT$`2019_Autumn`/Ratios_NAT$`2019_Spring`
Ratios_NAT$Ratio2020 <-Ratios_NAT$`2020_Autumn`/Ratios_NAT$`2020_Spring`
Ratios_NAT$Ratio2021<-Ratios_NAT$`2021_Autumn`/Ratios_NAT$`2021_Spring`
Ratios_NAT$Ratio2022 <-Ratios_NAT$`2022_Autumn`/Ratios_NAT$`2022_Spring`
Ratios_NAT$Ratio2024 <-Ratios_NAT$`2024_Autumn`/Ratios_NAT$`2024_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_Data <- ViaMarisCategories[ViaMarisCategories$eventYear != c('2008'),]
StdError_Data <- StdError_Data[StdError_Data$eventYear != c('2013'),]
StdError_Data <- StdError_Data[StdError_Data$eventYear != c('2023'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(29:41)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(29:41)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
ViaMaris_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
ViaMaris_Comm_Abundance$Ratio <- log(ViaMaris_Comm_Abundance$Ratio)
