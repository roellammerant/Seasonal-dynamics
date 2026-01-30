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
ReplicateNumber <- as.data.frame(table(ViaMarisCategories$Merched_Category))

ViaMarisSpecies$Merched_Category <- ViaMarisCategories$Merched_Category

### Create Community level abundance columns
ViaMarisTranspose <- t(ViaMarisSpecies)
ViaMarisTranspose <- as.data.frame(ViaMarisTranspose)
ViaMarisTranspose <- ViaMarisTranspose[-c(138),]
ViaMarisTranspose$Invasive <- SpViaMaris$Invasive
ViaMarisNIS  <- ViaMarisTranspose[which(ViaMarisTranspose$Invasive  =='1'),]
ViaMarisNAT  <- ViaMarisTranspose[which(ViaMarisTranspose$Invasive  =='0'),]
ViaMarisNIS <- ViaMarisNIS %>% 
  mutate_if(is.character, as.numeric)
ViaMarisNAT <- ViaMarisNAT %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
ViaMarisNIS <- t(ViaMarisNIS)
ViaMarisNIS <- as.data.frame(ViaMarisNIS)
ViaMarisNIS <- ViaMarisNIS[-c(859),]
ViaMarisNIS$Merched_Category <- ViaMarisCategories$Merched_Category
Mean_NIS <- aggregate(ViaMarisNIS[c(1:44)], 
                      list(ViaMarisNIS$Merched_Category), FUN=mean) 

ViaMarisNAT <- t(ViaMarisNAT)
ViaMarisNAT <- as.data.frame(ViaMarisNAT)
ViaMarisNAT <- ViaMarisNAT[-c(859),]
ViaMarisNAT$Merched_Category <- ViaMarisCategories$Merched_Category
Mean_NAT <- aggregate(ViaMarisNAT[c(1:86)], 
                      list(ViaMarisNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
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
Ratios_NAT[Ratios_NAT == 0] <- 0.001
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
ViaMarisNIS$eventYear <- ViaMarisCategories$eventYear
ViaMarisNIS$eventSeason <-  ViaMarisCategories$eventSeason
StdError_DataNIS <- ViaMarisNIS[ViaMarisNIS$eventYear != c('2008'),]
StdError_DataNIS <- StdError_DataNIS[StdError_DataNIS$eventYear != c('2013'),]
StdError_DataNIS <- StdError_DataNIS[StdError_DataNIS$eventYear != c('2023'),]
StdError_DataNIS[StdError_DataNIS == 0] <- 0.1

StdError_B001 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B001, ES = "LRRi")
StdError_B001$Ratio <- log(Ratios_NIS$B001[c(29:41)])
StdError_B001$TempPrefMax <- "30.43"
StdError_B001$TempPrefMean <- "23.15"

StdError_B007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B007, ES = "LRRi")
StdError_B007$Ratio <- log(Ratios_NIS$B007[c(29:41)])
StdError_B007$TempPrefMax <- "25.43"
StdError_B007$TempPrefMean <- "25.43"

StdError_B029 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B029, ES = "LRRi")
StdError_B029$Ratio <- log(Ratios_NIS$B029[c(29:41)])
StdError_B029$TempPrefMax <- "29.67"
StdError_B029$TempPrefMean <- "25.15"

StdError_B030 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B030, ES = "LRRi")
StdError_B030$Ratio <- log(Ratios_NIS$B030[c(29:41)])
StdError_B030$TempPrefMax <- "27.94"
StdError_B030$TempPrefMean <- "25.20"

StdError_B031 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B031, ES = "LRRi")
StdError_B031$Ratio <- log(Ratios_NIS$B031[c(29:41)])
StdError_B031$TempPrefMax <- "NA"
StdError_B031$TempPrefMean <- "NA"

StdError_B079 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B079, ES = "LRRi")
StdError_B079$Ratio <- log(Ratios_NIS$B079[c(29:41)])
StdError_B079$TempPrefMax <- "NA"
StdError_B079$TempPrefMean <- "NA"

StdError_B094 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B094, ES = "LRRi")
StdError_B094$Ratio <- log(Ratios_NIS$B094[c(29:41)])
StdError_B094$TempPrefMax <- "NA"
StdError_B094$TempPrefMean <- "NA"

StdError_B124 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(29:41)])
StdError_B124$TempPrefMax <- "28.36"
StdError_B124$TempPrefMean <- "22.87"

StdError_B202 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B202, ES = "LRRi")
StdError_B202$Ratio <- log(Ratios_NIS$B202[c(29:41)])
StdError_B202$TempPrefMax <- "29.06"
StdError_B202$TempPrefMean <- "28.42"

StdError_B208 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B208, ES = "LRRi")
StdError_B208$Ratio <- log(Ratios_NIS$B208[c(29:41)])
StdError_B208$TempPrefMax <- "30.39"
StdError_B208$TempPrefMean <- "26.20"

StdError_B220 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B220, ES = "LRRi")
StdError_B220$Ratio <- log(Ratios_NIS$B220[c(29:41)])
StdError_B220$TempPrefMax <- "NA"
StdError_B220$TempPrefMean <- "NA"

StdError_B227 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(29:41)])
StdError_B227$TempPrefMax <- "NA"
StdError_B227$TempPrefMean <- "NA"

StdError_B232 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B232, ES = "LRRi")
StdError_B232$Ratio <- log(Ratios_NIS$B232[c(29:41)])
StdError_B232$TempPrefMax <- "29.72"
StdError_B232$TempPrefMean <- "16.50"

StdError_B465 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B465, ES = "LRRi")
StdError_B465$Ratio <- log(Ratios_NIS$B465[c(29:41)])
StdError_B465$TempPrefMax <- "29.37"
StdError_B465$TempPrefMean <- "26.67"

StdError_G001 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G001, ES = "LRRi")
StdError_G001$Ratio <- log(Ratios_NIS$G001[c(29:41)])
StdError_G001$TempPrefMax <- "NA"
StdError_G001$TempPrefMean <- "NA"

StdError_G002 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G002, ES = "LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(29:41)])
StdError_G002$TempPrefMax <- "30.37"
StdError_G002$TempPrefMean <- "26.44"

StdError_G003 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G003, ES = "LRRi")
StdError_G003$Ratio <- log(Ratios_NIS$G003[c(29:41)])
StdError_G003$TempPrefMax <- "NA"
StdError_G003$TempPrefMean <- "NA"

StdError_G004 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G004, ES = "LRRi")
StdError_G004$Ratio <- log(Ratios_NIS$G004[c(29:41)])
StdError_G004$TempPrefMax <- "NA"
StdError_G004$TempPrefMean <- "NA"

StdError_G005 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G005, ES = "LRRi")
StdError_G005$Ratio <- log(Ratios_NIS$G005[c(29:41)])
StdError_G005$TempPrefMax <- "29.93"
StdError_G005$TempPrefMean <- "27.64"

StdError_G006 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G006, ES = "LRRi")
StdError_G006$Ratio <- log(Ratios_NIS$G006[c(29:41)])
StdError_G006$TempPrefMax <- "NA"
StdError_G006$TempPrefMean <- "NA"

StdError_G007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(29:41)])
StdError_G007$TempPrefMax <- "NA"
StdError_G007$TempPrefMean <- "NA"

StdError_G008 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G008, ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(29:41)])
StdError_G008$TempPrefMax <- "NA"
StdError_G008$TempPrefMean <- "NA"

StdError_G009 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G009, ES = "LRRi")
StdError_G009$Ratio <- log(Ratios_NIS$G009[c(29:41)])
StdError_G009$TempPrefMax <- "NA"
StdError_G009$TempPrefMean <- "NA"

StdError_G012 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G012, ES = "LRRi")
StdError_G012$Ratio <- log(Ratios_NIS$G012[c(29:41)])
StdError_G012$TempPrefMax <- "NA"
StdError_G012$TempPrefMean <- "NA"

StdError_G023 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G023, ES = "LRRi")
StdError_G023$Ratio <- log(Ratios_NIS$G023[c(29:41)])
StdError_G023$TempPrefMax <- "NA"
StdError_G023$TempPrefMean <- "NA"

StdError_G063 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G063, ES = "LRRi")
StdError_G063$Ratio <- log(Ratios_NIS$G063[c(29:41)])
StdError_G063$TempPrefMax <- "NA"
StdError_G063$TempPrefMean <- "NA"

StdError_G081 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G081, ES = "LRRi")
StdError_G081$Ratio <- log(Ratios_NIS$G081[c(29:41)])
StdError_G081$TempPrefMax <- "NA"
StdError_G081$TempPrefMean <- "NA"

StdError_G089 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G089, ES = "LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(29:41)])
StdError_G089$TempPrefMax <- "28.27"
StdError_G089$TempPrefMean <- "27.26"

StdError_G102 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G102, ES = "LRRi")
StdError_G102$Ratio <- log(Ratios_NIS$G102[c(29:41)])
StdError_G102$TempPrefMax <- "30.43"
StdError_G102$TempPrefMean <- "29.16"

StdError_G104 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G104, ES = "LRRi")
StdError_G104$Ratio <- log(Ratios_NIS$G104[c(29:41)])
StdError_G104$TempPrefMax <- "NA"
StdError_G104$TempPrefMean <- "NA"

StdError_G133 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G133, ES = "LRRi")
StdError_G133$Ratio <- log(Ratios_NIS$G133[c(29:41)])
StdError_G133$TempPrefMax <- "NA"
StdError_G133$TempPrefMean <- "NA"

StdError_G237 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G237, ES = "LRRi")
StdError_G237$Ratio <- log(Ratios_NIS$G237[c(29:41)])
StdError_G237$TempPrefMax <- "30.43"
StdError_G237$TempPrefMean <- "26.84"

StdError_G254 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G254, ES = "LRRi")
StdError_G254$Ratio <- log(Ratios_NIS$G254[c(29:41)])
StdError_G254$TempPrefMax <- "NA"
StdError_G254$TempPrefMean <- "NA"

StdError_G307 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G307, ES = "LRRi")
StdError_G307$Ratio <- log(Ratios_NIS$G307[c(29:41)])
StdError_G307$TempPrefMax <- "NA"
StdError_G307$TempPrefMean <- "NA"

StdError_G322 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G322, ES = "LRRi")
StdError_G322$Ratio <- log(Ratios_NIS$G322[c(29:41)])
StdError_G322$TempPrefMax <- "NA"
StdError_G322$TempPrefMean <- "NA"

StdError_G343 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G343, ES = "LRRi")
StdError_G343$Ratio <- log(Ratios_NIS$G343[c(29:41)])
StdError_G343$TempPrefMax <- "NA"
StdError_G343$TempPrefMean <- "NA"

StdError_G354 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G354, ES = "LRRi")
StdError_G354$Ratio <- log(Ratios_NIS$G354[c(29:41)])
StdError_G354$TempPrefMax <- "NA"
StdError_G354$TempPrefMean <- "NA"

StdError_G414 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G414, ES = "LRRi")
StdError_G414$Ratio <- log(Ratios_NIS$G414[c(29:41)])
StdError_G414$TempPrefMax <- "NA"
StdError_G414$TempPrefMean <- "NA"

StdError_G466 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G466, ES = "LRRi")
StdError_G466$Ratio <- log(Ratios_NIS$G466[c(29:41)])
StdError_G466$TempPrefMax <- "NA"
StdError_G466$TempPrefMean <- "NA"

StdError_G520 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G520, ES = "LRRi")
StdError_G520$Ratio <- log(Ratios_NIS$G520[c(29:41)])
StdError_G520$TempPrefMax <- "NA"
StdError_G520$TempPrefMean <- "NA"

StdError_G549 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G549, ES = "LRRi")
StdError_G549$Ratio <- log(Ratios_NIS$G549[c(29:41)])
StdError_G549$TempPrefMax <- "NA"
StdError_G549$TempPrefMean <- "NA"

StdError_G815 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G815, ES = "LRRi")
StdError_G815$Ratio <- log(Ratios_NIS$G815[c(29:41)])
StdError_G815$TempPrefMax <- "NA"
StdError_G815$TempPrefMean <- "NA"

StdError_G887 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G887, ES = "LRRi")
StdError_G887$Ratio <- log(Ratios_NIS$G887[c(29:41)])
StdError_G887$TempPrefMax <- "NA"
StdError_G887$TempPrefMean <- "NA"

StdError_G889 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G889, ES = "LRRi")
StdError_G889$Ratio <- log(Ratios_NIS$G889[c(29:41)])
StdError_G889$TempPrefMax <- "NA"
StdError_G889$TempPrefMean <- "NA"

StdError_NIS<- rbind(StdError_B001,StdError_B007,StdError_B029,StdError_B030,StdError_B031,StdError_B079,StdError_B094,StdError_B124,
                     StdError_B202,StdError_B208,StdError_B220,StdError_B227,StdError_B232,StdError_B465,StdError_G001,StdError_G002,
                     StdError_G003,StdError_G004,StdError_G005,StdError_G006,StdError_G007,StdError_G008,StdError_G009,StdError_G012,
                     StdError_G023,StdError_G063,StdError_G081,StdError_G089,StdError_G102,StdError_G104,StdError_G133,StdError_G237,
                     StdError_G254,StdError_G307,StdError_G322,StdError_G343,StdError_G354,StdError_G414,StdError_G466,StdError_G520,
                     StdError_G549,StdError_G815,StdError_G887,StdError_G889)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NIS species ####
ViaMarisNAT$eventYear <- ViaMarisCategories$eventYear
ViaMarisNAT$eventSeason <-  ViaMarisCategories$eventSeason
StdError_DataNAT <- ViaMarisNAT[ViaMarisNAT$eventYear != c('2008'),]
StdError_DataNAT <- StdError_DataNAT[StdError_DataNAT$eventYear != c('2013'),]
StdError_DataNAT <- StdError_DataNAT[StdError_DataNAT$eventYear != c('2023'),]
StdError_DataNAT[StdError_DataNAT == 0] <- 0.1

StdError_B004 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B004, ES = "LRRi")
StdError_B004$Ratio <- log(Ratios_NAT$B004[c(29:41)])
StdError_B004$TempPrefMax <- "27.43"
StdError_B004$TempPrefMean <- "11.44"

StdError_B005 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(29:41)])
StdError_B005$TempPrefMax <- "16.87"
StdError_B005$TempPrefMean <- "11.55"

StdError_B006 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B006, ES = "LRRi")
StdError_B006$Ratio <- log(Ratios_NAT$B006[c(29:41)])
StdError_B006$TempPrefMax <- "28.14"
StdError_B006$TempPrefMean <- "11.22"

StdError_B013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B013, ES = "LRRi")
StdError_B013$Ratio <- log(Ratios_NAT$B013[c(29:41)])
StdError_B013$TempPrefMax <- "29.71"
StdError_B013$TempPrefMean <- "11.00"

StdError_B014 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B014, ES = "LRRi")
StdError_B014$Ratio <- log(Ratios_NAT$B014[c(29:41)])
StdError_B014$TempPrefMax <- "NA"
StdError_B014$TempPrefMean <- "NA"

StdError_B016 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B016, ES = "LRRi")
StdError_B016$Ratio <- log(Ratios_NAT$B016[c(29:41)])
StdError_B016$TempPrefMax <- "25.35"
StdError_B016$TempPrefMean <- "19.41"

StdError_B019 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(29:41)])
StdError_B019$TempPrefMax <- "25.69"
StdError_B019$TempPrefMean <- "14.39"

StdError_B020 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B020, ES = "LRRi")
StdError_B020$Ratio <- log(Ratios_NAT$B020[c(29:41)])
StdError_B020$TempPrefMax <- "28.041"
StdError_B020$TempPrefMean <- "14.77"

StdError_B022 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B022, ES = "LRRi")
StdError_B022$Ratio <- log(Ratios_NAT$B022[c(29:41)])
StdError_B022$TempPrefMax <- "22.22"
StdError_B022$TempPrefMean <- "17.38"

StdError_B023 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(29:41)])
StdError_B023$TempPrefMax <- "20.53"
StdError_B023$TempPrefMean <- "13.49"

StdError_B033 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(29:41)])
StdError_B033$TempPrefMax <- "24.45"
StdError_B033$TempPrefMean <- "11.58"

StdError_B039 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B039, ES = "LRRi")
StdError_B039$Ratio <- log(Ratios_NAT$B039[c(29:41)])
StdError_B039$TempPrefMax <- "NA"
StdError_B039$TempPrefMean <- "NA"

StdError_B044 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B044, ES = "LRRi")
StdError_B044$Ratio <- log(Ratios_NAT$B044[c(29:41)])
StdError_B044$TempPrefMax <- "20.20"
StdError_B044$TempPrefMean <- "15.00"

StdError_B046 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(29:41)])
StdError_B046$TempPrefMax <- "28.59"
StdError_B046$TempPrefMean <- "12.02"

StdError_B052 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B052, ES = "LRRi")
StdError_B052$Ratio <- log(Ratios_NAT$B052[c(29:41)])
StdError_B052$TempPrefMax <- "20.53"
StdError_B052$TempPrefMean <- "12.36"

StdError_B055 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(29:41)])
StdError_B055$TempPrefMax <- "29.24"
StdError_B055$TempPrefMean <- "13.61"

StdError_B061 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B061, ES = "LRRi")
StdError_B061$Ratio <- log(Ratios_NAT$B061[c(29:41)])
StdError_B061$TempPrefMax <- "NA"
StdError_B061$TempPrefMean <- "NA"

StdError_B064 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B064, ES = "LRRi")
StdError_B064$Ratio <- log(Ratios_NAT$B064[c(29:41)])
StdError_B064$TempPrefMax <- "27.19"
StdError_B064$TempPrefMean <- "18.80"

StdError_B071 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B071, ES = "LRRi")
StdError_B071$Ratio <- log(Ratios_NAT$B071[c(29:41)])
StdError_B071$TempPrefMax <- "18.78"
StdError_B071$TempPrefMean <- "12.37"

StdError_B074 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B074, ES = "LRRi")
StdError_B074$Ratio <- log(Ratios_NAT$B074[c(29:41)])
StdError_B074$TempPrefMax <- "27.47"
StdError_B074$TempPrefMean <- "18.41"

StdError_B077 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B077, ES = "LRRi")
StdError_B077$Ratio <- log(Ratios_NAT$B077[c(29:41)])
StdError_B077$TempPrefMax <- "18.63"
StdError_B077$TempPrefMean <- "14.50"

StdError_B084 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B084, ES = "LRRi")
StdError_B084$Ratio <- log(Ratios_NAT$B084[c(29:41)])
StdError_B084$TempPrefMax <- "19.43"
StdError_B084$TempPrefMean <- "13.31"

StdError_B085 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B085, ES = "LRRi")
StdError_B085$Ratio <- log(Ratios_NAT$B085[c(29:41)])
StdError_B085$TempPrefMax <- "NA"
StdError_B085$TempPrefMean <- "NA"

StdError_B118 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B118, ES = "LRRi")
StdError_B118$Ratio <- log(Ratios_NAT$B118[c(29:41)])
StdError_B118$TempPrefMax <- "25.08"
StdError_B118$TempPrefMean <- "10.93"

StdError_B121 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B121, ES = "LRRi")
StdError_B121$Ratio <- log(Ratios_NAT$B121[c(29:41)])
StdError_B121$TempPrefMax <- "18.78"
StdError_B121$TempPrefMean <- "11.28"

StdError_B136 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B136, ES = "LRRi")
StdError_B136$Ratio <- log(Ratios_NAT$B136[c(29:41)])
StdError_B136$TempPrefMax <- "24.38"
StdError_B136$TempPrefMean <- "15.02"

StdError_B158 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B158, ES = "LRRi")
StdError_B158$Ratio <- log(Ratios_NAT$B158[c(29:41)])
StdError_B158$TempPrefMax <- "29.78"
StdError_B158$TempPrefMean <- "17.30"

StdError_B173 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B173, ES = "LRRi")
StdError_B173$Ratio <- log(Ratios_NAT$B173[c(29:41)])
StdError_B173$TempPrefMax <- "18.63"
StdError_B173$TempPrefMean <- "14.56"

StdError_B186<- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                              outcome = B186, ES = "LRRi")
StdError_B186$Ratio <- log(Ratios_NAT$B186[c(29:41)])
StdError_B186$TempPrefMax <- "NA"
StdError_B186$TempPrefMean <- "nA"

StdError_B187 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B187, ES = "LRRi")
StdError_B187$Ratio <- log(Ratios_NAT$B187[c(29:41)])
StdError_B187$TempPrefMax <- "18.63"
StdError_B187$TempPrefMean <- "13.36"

StdError_B262 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B262, ES = "LRRi")
StdError_B262$Ratio <- log(Ratios_NAT$B262[c(29:41)])
StdError_B262$TempPrefMax <- "20.42"
StdError_B262$TempPrefMean <- "12.93"

StdError_B273 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B273, ES = "LRRi")
StdError_B273$Ratio <- log(Ratios_NAT$B273[c(29:41)])
StdError_B273$TempPrefMax <- "18.78"
StdError_B273$TempPrefMean <- "17.35"

StdError_B283 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B283, ES = "LRRi")
StdError_B283$Ratio <- log(Ratios_NAT$B283[c(29:41)])
StdError_B283$TempPrefMax <- "20.20"
StdError_B283$TempPrefMean <- "11.87" 

StdError_B284 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B284, ES = "LRRi")
StdError_B284$Ratio <- log(Ratios_NAT$B284[c(29:41)])
StdError_B284$TempPrefMax <- "19.70"
StdError_B284$TempPrefMean <- "16.68"

StdError_B318 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B318, ES = "LRRi")
StdError_B318$Ratio <- log(Ratios_NAT$B318[c(29:41)])
StdError_B318$TempPrefMax <- "28.04"
StdError_B318$TempPrefMean <- "12.19"

StdError_B321 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B321, ES = "LRRi")
StdError_B321$Ratio <- log(Ratios_NAT$B321[c(29:41)])
StdError_B321$TempPrefMax <- "NA"
StdError_B321$TempPrefMean <- "NA"

StdError_B335 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B335, ES = "LRRi")
StdError_B335$Ratio <- log(Ratios_NAT$B335[c(29:41)])
StdError_B335$TempPrefMax <- "18.02"
StdError_B335$TempPrefMean <- "11.20"

StdError_B343 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B343, ES = "LRRi")
StdError_B343$Ratio <- log(Ratios_NAT$B343[c(29:41)])
StdError_B343$TempPrefMax <- "28.26"
StdError_B343$TempPrefMean <- "11.72"

StdError_B344 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B344, ES = "LRRi")
StdError_B344$Ratio <- log(Ratios_NAT$B344[c(29:41)])
StdError_B344$TempPrefMax <- "27.12"
StdError_B344$TempPrefMean <- "17.41"

StdError_B349 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B349, ES = "LRRi")
StdError_B349$Ratio <- log(Ratios_NAT$B349[c(29:41)])
StdError_B349$TempPrefMax <- "NA"
StdError_B349$TempPrefMean <- "NA"

StdError_B453 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B453, ES = "LRRi")
StdError_B453$Ratio <- log(Ratios_NAT$B453[c(29:41)])
StdError_B453$TempPrefMax <- "28.11"
StdError_B453$TempPrefMean <- "23.86"

StdError_B454 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B454, ES = "LRRi")
StdError_B454$Ratio <- log(Ratios_NAT$B454[c(29:41)])
StdError_B454$TempPrefMax <- "20.78"
StdError_B454$TempPrefMean <- "16.67"

StdError_G010 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(29:41)])
StdError_G010$TempPrefMax <- "NA"
StdError_G010$TempPrefMean <- "NA"

StdError_G013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G013, ES = "LRRi")
StdError_G013$Ratio <- log(Ratios_NAT$G013[c(29:41)])
StdError_G013$TempPrefMax <- "NA"
StdError_G013$TempPrefMean <- "NA"

StdError_G015 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G015, ES = "LRRi")
StdError_G015$Ratio <- log(Ratios_NAT$G015[c(29:41)])
StdError_G015$TempPrefMax <- "18.78"
StdError_G015$TempPrefMean <- "12.35"

StdError_G017 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(29:41)])
StdError_G017$TempPrefMax <- "21.80"
StdError_G017$TempPrefMean <- "19.23"

StdError_G021 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G021, ES = "LRRi")
StdError_G021$Ratio <- log(Ratios_NAT$G021[c(29:41)])
StdError_G021$TempPrefMax <- "NA"
StdError_G021$TempPrefMean <- "NA"

StdError_G027 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(29:41)])
StdError_G027$TempPrefMax <- "NA"
StdError_G027$TempPrefMean <- "NA"

StdError_G041 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G041, ES = "LRRi")
StdError_G041$Ratio <- log(Ratios_NAT$G041[c(29:41)])
StdError_G041$TempPrefMax <- "NA"
StdError_G041$TempPrefMean <- "NA"

StdError_G045 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G045, ES = "LRRi")
StdError_G045$Ratio <- log(Ratios_NAT$G045[c(29:41)])
StdError_G045$TempPrefMax <- "20.50"
StdError_G045$TempPrefMean <- "10.77"

StdError_G053 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G053, ES = "LRRi")
StdError_G053$Ratio <- log(Ratios_NAT$G053[c(29:41)])
StdError_G053$TempPrefMax <- "18.63"
StdError_G053$TempPrefMean <- "16.64"

StdError_G061 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G061, ES = "LRRi")
StdError_G061$Ratio <- log(Ratios_NAT$G061[c(29:41)])
StdError_G061$TempPrefMax <- "NA"
StdError_G061$TempPrefMean <- "NA"

StdError_G066 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G066, ES = "LRRi")
StdError_G066$Ratio <- log(Ratios_NAT$G066[c(29:41)])
StdError_G066$TempPrefMax <- "14.73"
StdError_G066$TempPrefMean <- "11.45"


StdError_G075 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G075, ES = "LRRi")
StdError_G075$Ratio <- log(Ratios_NAT$G075[c(29:41)])
StdError_G075$TempPrefMax <- "23.25"
StdError_G075$TempPrefMean <- "11.86"

StdError_G080 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G080, ES = "LRRi")
StdError_G080$Ratio <- log(Ratios_NAT$G080[c(29:41)])
StdError_G080$TempPrefMax <- "NA"
StdError_G080$TempPrefMean <- "NA"

StdError_G097 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G097, ES = "LRRi")
StdError_G097$Ratio <- log(Ratios_NAT$G097[c(29:41)])
StdError_G097$TempPrefMax <- "NA"
StdError_G097$TempPrefMean <- "NA"

StdError_G105 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G105, ES = "LRRi")
StdError_G105$Ratio <- log(Ratios_NAT$G105[c(29:41)])
StdError_G105$TempPrefMax <- "NA"
StdError_G105$TempPrefMean <- "NA"

StdError_G132 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G132, ES = "LRRi")
StdError_G132$Ratio <- log(Ratios_NAT$G132[c(29:41)])
StdError_G132$TempPrefMax <- "28.72"
StdError_G132$TempPrefMean <- "10.77"

StdError_G134 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G134, ES = "LRRi")
StdError_G134$Ratio <- log(Ratios_NAT$G134[c(29:41)])
StdError_G134$TempPrefMax <- "NA"
StdError_G134$TempPrefMean <- "NA"

StdError_G152 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G152, ES = "LRRi")
StdError_G152$Ratio <- log(Ratios_NAT$G152[c(29:41)])
StdError_G152$TempPrefMax <- "NA"
StdError_G152$TempPrefMean <- "NA"

StdError_G192 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G192, ES = "LRRi")
StdError_G192$Ratio <- log(Ratios_NAT$G192[c(29:41)])
StdError_G192$TempPrefMax <- "NA"
StdError_G192$TempPrefMean <- "NA"

StdError_G209 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G209, ES = "LRRi")
StdError_G209$Ratio <- log(Ratios_NAT$G209[c(29:41)])
StdError_G209$TempPrefMax <- "28.99"
StdError_G209$TempPrefMean <- "26.04"

StdError_G275 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G275, ES = "LRRi")
StdError_G275$Ratio <- log(Ratios_NAT$G275[c(29:41)])
StdError_G275$TempPrefMax <- "NA"
StdError_G275$TempPrefMean <- "NA"

StdError_G292 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G292, ES = "LRRi")
StdError_G292$Ratio <- log(Ratios_NAT$G292[c(29:41)])
StdError_G292$TempPrefMax <- "NA"
StdError_G292$TempPrefMean <- "NA"

StdError_G295<- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                              outcome = G295, ES = "LRRi")
StdError_G295$Ratio <- log(Ratios_NAT$G295[c(29:41)])
StdError_G295$TempPrefMax <- "21.43"
StdError_G295$TempPrefMean <- "20.70"

StdError_G338 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(29:41)])
StdError_G338$TempPrefMax <- "NA"
StdError_G338$TempPrefMean <- "NA"

StdError_G349 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G349, ES = "LRRi")
StdError_G349$Ratio <- log(Ratios_NAT$G349[c(29:41)])
StdError_G349$TempPrefMax <- "NA"
StdError_G349$TempPrefMean <- "NA"

StdError_G357 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G357, ES = "LRRi")
StdError_G357$Ratio <- log(Ratios_NAT$G357[c(29:41)])
StdError_G357$TempPrefMax <- "NA"
StdError_G357$TempPrefMean <- "NA"

StdError_G401 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G401, ES = "LRRi")
StdError_G401$Ratio <- log(Ratios_NAT$G401[c(29:41)])
StdError_G401$TempPrefMax <- "NA"
StdError_G401$TempPrefMean <- "NA"

StdError_G467 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G467, ES = "LRRi")
StdError_G467$Ratio <- log(Ratios_NAT$G467[c(29:41)])
StdError_G467$TempPrefMax <- "NA"
StdError_G467$TempPrefMean <- "NA"

StdError_G481 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G481, ES = "LRRi")
StdError_G481$Ratio <- log(Ratios_NAT$G481[c(29:41)])
StdError_G481$TempPrefMax <- "NA"
StdError_G481$TempPrefMean <- "NA"

StdError_G514<- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                              outcome = G514, ES = "LRRi")
StdError_G514$Ratio <- log(Ratios_NAT$G514[c(29:41)])
StdError_G514$TempPrefMax <- "NA"
StdError_G514$TempPrefMean <- "NA"

StdError_G600 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G600, ES = "LRRi")
StdError_G600$Ratio <- log(Ratios_NAT$G600[c(29:41)])
StdError_G600$TempPrefMax <- "NA"
StdError_G600$TempPrefMean <- "NA"

StdError_G649 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G649, ES = "LRRi")
StdError_G649$Ratio <- log(Ratios_NAT$G649[c(29:41)])
StdError_G649$TempPrefMax <- "20.09"
StdError_G649$TempPrefMean <- "10.98"

StdError_G673 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G673, ES = "LRRi")
StdError_G673$Ratio <- log(Ratios_NAT$G673[c(29:41)])
StdError_G673$TempPrefMax <- "NA"
StdError_G673$TempPrefMean <- "NA"

StdError_G699 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(29:41)])
StdError_G699$TempPrefMax <- "NA"
StdError_G699$TempPrefMean <- "NA"

StdError_G707 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G707, ES = "LRRi")
StdError_G707$Ratio <- log(Ratios_NAT$G707[c(29:41)])
StdError_G707$TempPrefMax <- "NA"
StdError_G707$TempPrefMean <- "NA"

StdError_G711 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G711, ES = "LRRi")
StdError_G711$Ratio <- log(Ratios_NAT$G711[c(29:41)])
StdError_G711$TempPrefMax <- "NA"
StdError_G711$TempPrefMean <- "NA"

StdError_G712 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G712, ES = "LRRi")
StdError_G712$Ratio <- log(Ratios_NAT$G712[c(29:41)])
StdError_G712$TempPrefMax <- "NA"
StdError_G712$TempPrefMean <- "NA"

StdError_G714 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G714, ES = "LRRi")
StdError_G714$Ratio <- log(Ratios_NAT$G714[c(29:41)])
StdError_G714$TempPrefMax <- "NA"
StdError_G714$TempPrefMean <- "NA"

StdError_G724 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G724, ES = "LRRi")
StdError_G724$Ratio <- log(Ratios_NAT$G724[c(29:41)])
StdError_G724$TempPrefMax <- "NA"
StdError_G724$TempPrefMean <- "NA"

StdError_G725 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G725, ES = "LRRi")
StdError_G725$Ratio <- log(Ratios_NAT$G725[c(29:41)])
StdError_G725$TempPrefMax <- "NA"
StdError_G725$TempPrefMean <- "NA"

StdError_G849 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G849, ES = "LRRi")
StdError_G849$Ratio <- log(Ratios_NAT$G849[c(29:41)])
StdError_G849$TempPrefMax <- "NA"
StdError_G849$TempPrefMean <- "NA"

StdError_G913 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G913, ES = "LRRi")
StdError_G913$Ratio <- log(Ratios_NAT$G913[c(29:41)])
StdError_G913$TempPrefMax <- "NA"
StdError_G913$TempPrefMean <- "NA"

StdError_G916 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G916, ES = "LRRi")
StdError_G916$Ratio <- log(Ratios_NAT$G916[c(29:41)])
StdError_G916$TempPrefMax <- "NA"
StdError_G916$TempPrefMean <- "NA"

StdError_S009 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = S009, ES = "LRRi")
StdError_S009$Ratio <- log(Ratios_NAT$S009[c(29:41)])
StdError_S009$TempPrefMax <- "18.78"
StdError_S009$TempPrefMean <- "17.92"

StdError_NAT<- rbind(StdError_B004,StdError_B005,StdError_B006,StdError_B013,StdError_B014,StdError_B016,StdError_B019,StdError_B020,
                     StdError_B022,StdError_B023,StdError_B033,StdError_B039,StdError_B044,StdError_B046,StdError_B052,StdError_B055,
                     StdError_B061,StdError_B064,StdError_B071,StdError_B074,StdError_B077,StdError_B084,StdError_B085,StdError_B118,
                     StdError_B121,StdError_B136,StdError_B158,StdError_B173,StdError_B186,StdError_B187,StdError_B262,StdError_B273,
                     StdError_B283,StdError_B284,StdError_B318,StdError_B321,StdError_B335,StdError_B343,StdError_B344,StdError_B349,
                     StdError_B453,StdError_B454,StdError_G010,StdError_G013,StdError_G015,StdError_G017,StdError_G021,StdError_G027,
                     StdError_G041,StdError_G045,StdError_G053,StdError_G061,StdError_G066,StdError_G075,StdError_G080,StdError_G097,
                     StdError_G105,StdError_G132,StdError_G134,StdError_G152,StdError_G192,StdError_G209,StdError_G275,StdError_G292,
                     StdError_G295,StdError_G338,StdError_G349,StdError_G357,StdError_G401,StdError_G467,StdError_G481,StdError_G514,
                     StdError_G600,StdError_G649,StdError_G673,StdError_G699,StdError_G707,StdError_G711,StdError_G712,StdError_G714,
                     StdError_G724,StdError_G725,StdError_G849,StdError_G913,StdError_G916,StdError_S009)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]


### Create dataframe
ViaMaris_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9,10)],StdError_NAT[c(1,4,7,8,9,10)])
ViaMaris_Species_Abundance <- ViaMaris_Species_Abundance[ViaMaris_Species_Abundance$TempPrefMax != c('NA'),]
