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
StdError_B001$Species <- "B001"

StdError_B007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B007, ES = "LRRi")
StdError_B007$Ratio <- log(Ratios_NIS$B007[c(29:41)])
StdError_B007$Species <- "B007"

StdError_B029 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B029, ES = "LRRi")
StdError_B029$Ratio <- log(Ratios_NIS$B029[c(29:41)])
StdError_B029$Species <- "B029"

StdError_B030 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B030, ES = "LRRi")
StdError_B030$Ratio <- log(Ratios_NIS$B030[c(29:41)])
StdError_B030$Species <- "B030"

StdError_B031 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B031, ES = "LRRi")
StdError_B031$Ratio <- log(Ratios_NIS$B031[c(29:41)])
StdError_B031$Species <- "B031"

StdError_B079 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B079, ES = "LRRi")
StdError_B079$Ratio <- log(Ratios_NIS$B079[c(29:41)])
StdError_B079$Species <- "B079"

StdError_B094 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B094, ES = "LRRi")
StdError_B094$Ratio <- log(Ratios_NIS$B094[c(29:41)])
StdError_B094$Species <- "B094"

StdError_B124 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(29:41)])
StdError_B124$Species <- "B124"

StdError_B202 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B202, ES = "LRRi")
StdError_B202$Ratio <- log(Ratios_NIS$B202[c(29:41)])
StdError_B202$Species <- "B202"

StdError_B208 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B208, ES = "LRRi")
StdError_B208$Ratio <- log(Ratios_NIS$B208[c(29:41)])
StdError_B208$Species <- "B208"

StdError_B220 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B220, ES = "LRRi")
StdError_B220$Ratio <- log(Ratios_NIS$B220[c(29:41)])
StdError_B220$Species <- "B220"

StdError_B227 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(29:41)])
StdError_B227$Species <- "B227"

StdError_B232 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B232, ES = "LRRi")
StdError_B232$Ratio <- log(Ratios_NIS$B232[c(29:41)])
StdError_B232$Species <- "B232"

StdError_B465 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B465, ES = "LRRi")
StdError_B465$Ratio <- log(Ratios_NIS$B465[c(29:41)])
StdError_B465$Species <- "B465"

StdError_G001 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G001, ES = "LRRi")
StdError_G001$Ratio <- log(Ratios_NIS$G001[c(29:41)])
StdError_G001$Species <- "G001"

StdError_G002 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G002, ES = "LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(29:41)])
StdError_G002$Species <- "G002"

StdError_G003 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G003, ES = "LRRi")
StdError_G003$Ratio <- log(Ratios_NIS$G003[c(29:41)])
StdError_G003$Species <- "G003"

StdError_G004 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G004, ES = "LRRi")
StdError_G004$Ratio <- log(Ratios_NIS$G004[c(29:41)])
StdError_G004$Species <- "G004"

StdError_G005 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G005, ES = "LRRi")
StdError_G005$Ratio <- log(Ratios_NIS$G005[c(29:41)])
StdError_G005$Species <- "G005"

StdError_G006 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G006, ES = "LRRi")
StdError_G006$Ratio <- log(Ratios_NIS$G006[c(29:41)])
StdError_G006$Species <- "G006"

StdError_G007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(29:41)])
StdError_G007$Species <- "G007"

StdError_G008 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G008, ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(29:41)])
StdError_G008$Species <- "G008"

StdError_G009 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G009, ES = "LRRi")
StdError_G009$Ratio <- log(Ratios_NIS$G009[c(29:41)])
StdError_G009$Species <- "G009"

StdError_G012 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G012, ES = "LRRi")
StdError_G012$Ratio <- log(Ratios_NIS$G012[c(29:41)])
StdError_G012$Species <- "G012"

StdError_G023 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G023, ES = "LRRi")
StdError_G023$Ratio <- log(Ratios_NIS$G023[c(29:41)])
StdError_G023$Species <- "G023"

StdError_G063 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G063, ES = "LRRi")
StdError_G063$Ratio <- log(Ratios_NIS$G063[c(29:41)])
StdError_G063$Species <- "G063"

StdError_G081 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G081, ES = "LRRi")
StdError_G081$Ratio <- log(Ratios_NIS$G081[c(29:41)])
StdError_G081$Species <- "G081"

StdError_G089 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G089, ES = "LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(29:41)])
StdError_G089$Species <- "G089"

StdError_G102 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G102, ES = "LRRi")
StdError_G102$Ratio <- log(Ratios_NIS$G102[c(29:41)])
StdError_G102$Species <- "G102"

StdError_G104 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G104, ES = "LRRi")
StdError_G104$Ratio <- log(Ratios_NIS$G104[c(29:41)])
StdError_G104$Species <- "G104"

StdError_G133 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G133, ES = "LRRi")
StdError_G133$Ratio <- log(Ratios_NIS$G133[c(29:41)])
StdError_G133$Species <- "G133"

StdError_G237 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G237, ES = "LRRi")
StdError_G237$Ratio <- log(Ratios_NIS$G237[c(29:41)])
StdError_G237$Species <- "G237"

StdError_G254 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G254, ES = "LRRi")
StdError_G254$Ratio <- log(Ratios_NIS$G254[c(29:41)])
StdError_G254$Species <- "G254"

StdError_G307 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G307, ES = "LRRi")
StdError_G307$Ratio <- log(Ratios_NIS$G307[c(29:41)])
StdError_G307$Species <- "G307"

StdError_G322 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G322, ES = "LRRi")
StdError_G322$Ratio <- log(Ratios_NIS$G322[c(29:41)])
StdError_G322$Species <- "G322"

StdError_G343 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G343, ES = "LRRi")
StdError_G343$Ratio <- log(Ratios_NIS$G343[c(29:41)])
StdError_G343$Species <- "G343"

StdError_G354 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G354, ES = "LRRi")
StdError_G354$Ratio <- log(Ratios_NIS$G354[c(29:41)])
StdError_G354$Species <- "G354"

StdError_G414 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G414, ES = "LRRi")
StdError_G414$Ratio <- log(Ratios_NIS$G414[c(29:41)])
StdError_G414$Species <- "G414"

StdError_G466 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G466, ES = "LRRi")
StdError_G466$Ratio <- log(Ratios_NIS$G466[c(29:41)])
StdError_G466$Species <- "G466"

StdError_G520 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G520, ES = "LRRi")
StdError_G520$Ratio <- log(Ratios_NIS$G520[c(29:41)])
StdError_G520$Species <- "G520"

StdError_G549 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G549, ES = "LRRi")
StdError_G549$Ratio <- log(Ratios_NIS$G549[c(29:41)])
StdError_G549$Species <- "G549"

StdError_G815 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G815, ES = "LRRi")
StdError_G815$Ratio <- log(Ratios_NIS$G815[c(29:41)])
StdError_G815$Species <- "G815"

StdError_G887 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G887, ES = "LRRi")
StdError_G887$Ratio <- log(Ratios_NIS$G887[c(29:41)])
StdError_G887$Species <- "G887"

StdError_G889 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G889, ES = "LRRi")
StdError_G889$Ratio <- log(Ratios_NIS$G889[c(29:41)])
StdError_G889$Species <- "G889"

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
StdError_B004$Species <- "B004"

StdError_B005 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(29:41)])
StdError_B005$Species <- "B005"

StdError_B006 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B006, ES = "LRRi")
StdError_B006$Ratio <- log(Ratios_NAT$B006[c(29:41)])
StdError_B006$Species <- "B006"

StdError_B013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B013, ES = "LRRi")
StdError_B013$Ratio <- log(Ratios_NAT$B013[c(29:41)])
StdError_B013$Species <- "B013"

StdError_B014 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B014, ES = "LRRi")
StdError_B014$Ratio <- log(Ratios_NAT$B014[c(29:41)])
StdError_B014$Species <- "B014"

StdError_B016 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B016, ES = "LRRi")
StdError_B016$Ratio <- log(Ratios_NAT$B016[c(29:41)])
StdError_B016 $Species <- "B016 "

StdError_B019 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(29:41)])
StdError_B019$Species <- "B019"

StdError_B020 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B020, ES = "LRRi")
StdError_B020$Ratio <- log(Ratios_NAT$B020[c(29:41)])
StdError_B020$Species <- "B020"

StdError_B022 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B022, ES = "LRRi")
StdError_B022$Ratio <- log(Ratios_NAT$B022[c(29:41)])
StdError_B022$Species <- "B022"

StdError_B023 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(29:41)])
StdError_B023$Species <- "B023"

StdError_B033 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(29:41)])
StdError_B033$Species <- "B033"

StdError_B039 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B039, ES = "LRRi")
StdError_B039$Ratio <- log(Ratios_NAT$B039[c(29:41)])
StdError_B039$Species <- "B039"

StdError_B044 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B044, ES = "LRRi")
StdError_B044$Ratio <- log(Ratios_NAT$B044[c(29:41)])
StdError_B044$Species <- "B044"

StdError_B046 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(29:41)])
StdError_B046$Species <- "B046"

StdError_B052 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B052, ES = "LRRi")
StdError_B052$Ratio <- log(Ratios_NAT$B052[c(29:41)])
StdError_B052$Species <- "B052"

StdError_B055 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(29:41)])
StdError_B055$Species <- "B055"

StdError_B061 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B061, ES = "LRRi")
StdError_B061$Ratio <- log(Ratios_NAT$B061[c(29:41)])
StdError_B061$Species <- "B061"

StdError_B064 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B064, ES = "LRRi")
StdError_B064$Ratio <- log(Ratios_NAT$B064[c(29:41)])
StdError_B064$Species <- "B064"

StdError_B071 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B071, ES = "LRRi")
StdError_B071$Ratio <- log(Ratios_NAT$B071[c(29:41)])
StdError_B071$Species <- "B071"

StdError_B074 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B074, ES = "LRRi")
StdError_B074$Ratio <- log(Ratios_NAT$B074[c(29:41)])
StdError_B074$Species <- "B074"

StdError_B077 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B077, ES = "LRRi")
StdError_B077$Ratio <- log(Ratios_NAT$B077[c(29:41)])
StdError_B077$Species <- "B077"

StdError_B084 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B084, ES = "LRRi")
StdError_B084$Ratio <- log(Ratios_NAT$B084[c(29:41)])
StdError_B084$Species <- "B084"

StdError_B085 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B085, ES = "LRRi")
StdError_B085$Ratio <- log(Ratios_NAT$B085[c(29:41)])
StdError_B085$Species <- "B085"

StdError_B118 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B118, ES = "LRRi")
StdError_B118$Ratio <- log(Ratios_NAT$B118[c(29:41)])
StdError_B118$Species <- "B118"

StdError_B121 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B121, ES = "LRRi")
StdError_B121$Ratio <- log(Ratios_NAT$B121[c(29:41)])
StdError_B121$Species <- "B121"

StdError_B136 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B136, ES = "LRRi")
StdError_B136$Ratio <- log(Ratios_NAT$B136[c(29:41)])
StdError_B136$Species <- "B136"

StdError_B158 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B158, ES = "LRRi")
StdError_B158$Ratio <- log(Ratios_NAT$B158[c(29:41)])
StdError_B158$Species <- "B158"

StdError_B173 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B173, ES = "LRRi")
StdError_B173$Ratio <- log(Ratios_NAT$B173[c(29:41)])
StdError_B173$Species <- "B173"

StdError_B186<- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B186, ES = "LRRi")
StdError_B186$Ratio <- log(Ratios_NAT$B186[c(29:41)])
StdError_B186$Species <- "B186"

StdError_B187 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B187, ES = "LRRi")
StdError_B187$Ratio <- log(Ratios_NAT$B187[c(29:41)])
StdError_B187$Species <- "B187"

StdError_B262 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B262, ES = "LRRi")
StdError_B262$Ratio <- log(Ratios_NAT$B262[c(29:41)])
StdError_B262$Species <- "B262"

StdError_B273 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B273, ES = "LRRi")
StdError_B273$Ratio <- log(Ratios_NAT$B273[c(29:41)])
StdError_B273$Species <- "B273"

StdError_B283 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B283, ES = "LRRi")
StdError_B283$Ratio <- log(Ratios_NAT$B283[c(29:41)])
StdError_B283$Species <- "B283"

StdError_B284 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B284, ES = "LRRi")
StdError_B284$Ratio <- log(Ratios_NAT$B284[c(29:41)])
StdError_B284$Species <- "B284"

StdError_B318 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B318, ES = "LRRi")
StdError_B318$Ratio <- log(Ratios_NAT$B318[c(29:41)])
StdError_B318$Species <- "B318"

StdError_B321 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B321, ES = "LRRi")
StdError_B321$Ratio <- log(Ratios_NAT$B321[c(29:41)])
StdError_B321$Species <- "B321"

StdError_B335 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B335, ES = "LRRi")
StdError_B335$Ratio <- log(Ratios_NAT$B335[c(29:41)])
StdError_B335$Species <- "B335"

StdError_B343 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B343, ES = "LRRi")
StdError_B343$Ratio <- log(Ratios_NAT$B343[c(29:41)])
StdError_B343$Species <- "B343"

StdError_B344 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B344, ES = "LRRi")
StdError_B344$Ratio <- log(Ratios_NAT$B344[c(29:41)])
StdError_B344$Species <- "B344"

StdError_B349 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B349, ES = "LRRi")
StdError_B349$Ratio <- log(Ratios_NAT$B349[c(29:41)])
StdError_B349$Species <- "B349"

StdError_B453 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B453, ES = "LRRi")
StdError_B453$Ratio <- log(Ratios_NAT$B453[c(29:41)])
StdError_B453$Species <- "B453"

StdError_B454 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B454, ES = "LRRi")
StdError_B454$Ratio <- log(Ratios_NAT$B454[c(29:41)])
StdError_B454 $Species <- "B454 "

StdError_G010 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(29:41)])
StdError_G010$Species <- "G010"

StdError_G013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G013, ES = "LRRi")
StdError_G013$Ratio <- log(Ratios_NAT$G013[c(29:41)])
StdError_G013$Species <- "G013"

StdError_G015 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G015, ES = "LRRi")
StdError_G015$Ratio <- log(Ratios_NAT$G015[c(29:41)])
StdError_G015$Species <- "G015"

StdError_G017 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(29:41)])
StdError_G017$Species <- "G017"

StdError_G021 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G021, ES = "LRRi")
StdError_G021$Ratio <- log(Ratios_NAT$G021[c(29:41)])
StdError_G021$Species <- "G021"

StdError_G027 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(29:41)])
StdError_G027$Species <- "G027"

StdError_G041 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G041, ES = "LRRi")
StdError_G041$Ratio <- log(Ratios_NAT$G041[c(29:41)])
StdError_G041$Species <- "G041"

StdError_G045 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G045, ES = "LRRi")
StdError_G045$Ratio <- log(Ratios_NAT$G045[c(29:41)])
StdError_G045$Species <- "G045"

StdError_G053 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G053, ES = "LRRi")
StdError_G053$Ratio <- log(Ratios_NAT$G053[c(29:41)])
StdError_G053$Species <- "G053"

StdError_G061 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G061, ES = "LRRi")
StdError_G061$Ratio <- log(Ratios_NAT$G061[c(29:41)])
StdError_G061$Species <- "G061"

StdError_G066 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G066, ES = "LRRi")
StdError_G066$Ratio <- log(Ratios_NAT$G066[c(29:41)])
StdError_G066$Species <- "G066"

StdError_G075 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G075, ES = "LRRi")
StdError_G075$Ratio <- log(Ratios_NAT$G075[c(29:41)])
StdError_G075$Species <- "G075"

StdError_G080 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G080, ES = "LRRi")
StdError_G080$Ratio <- log(Ratios_NAT$G080[c(29:41)])
StdError_G080$Species <- "G080"

StdError_G097 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G097, ES = "LRRi")
StdError_G097$Ratio <- log(Ratios_NAT$G097[c(29:41)])
StdError_G097$Species <- "G097"

StdError_G105 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G105, ES = "LRRi")
StdError_G105$Ratio <- log(Ratios_NAT$G105[c(29:41)])
StdError_G105$Species <- "G105"

StdError_G132 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G132, ES = "LRRi")
StdError_G132$Ratio <- log(Ratios_NAT$G132[c(29:41)])
StdError_G132$Species <- "G132"

StdError_G134 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G134, ES = "LRRi")
StdError_G134$Ratio <- log(Ratios_NAT$G134[c(29:41)])
StdError_G134$Species <- "G134"

StdError_G152 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G152, ES = "LRRi")
StdError_G152$Ratio <- log(Ratios_NAT$G152[c(29:41)])
StdError_G152$Species <- "G152"

StdError_G192 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G192, ES = "LRRi")
StdError_G192$Ratio <- log(Ratios_NAT$G192[c(29:41)])
StdError_G192$Species <- "G192"

StdError_G209 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G209, ES = "LRRi")
StdError_G209$Ratio <- log(Ratios_NAT$G209[c(29:41)])
StdError_G209$Species <- "G209"

StdError_G275 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G275, ES = "LRRi")
StdError_G275$Ratio <- log(Ratios_NAT$G275[c(29:41)])
StdError_G275$Species <- "G275"

StdError_G292 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G292, ES = "LRRi")
StdError_G292$Ratio <- log(Ratios_NAT$G292[c(29:41)])
StdError_G292$Species <- "G292"

StdError_G295<- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G295, ES = "LRRi")
StdError_G295$Ratio <- log(Ratios_NAT$G295[c(29:41)])
StdError_G295$Species <- "G295"

StdError_G338 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(29:41)])
StdError_G338$Species <- "G338"

StdError_G349 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G349, ES = "LRRi")
StdError_G349$Ratio <- log(Ratios_NAT$G349[c(29:41)])
StdError_G349$Species <- "G349"

StdError_G357 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G357, ES = "LRRi")
StdError_G357$Ratio <- log(Ratios_NAT$G357[c(29:41)])
StdError_G357$Species <- "G357"

StdError_G401 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G401, ES = "LRRi")
StdError_G401$Ratio <- log(Ratios_NAT$G401[c(29:41)])
StdError_G401$Species <- "G401"

StdError_G467 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G467, ES = "LRRi")
StdError_G467$Ratio <- log(Ratios_NAT$G467[c(29:41)])
StdError_G467$Species <- "G467"

StdError_G481 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G481, ES = "LRRi")
StdError_G481$Ratio <- log(Ratios_NAT$G481[c(29:41)])
StdError_G481$Species <- "G481"

StdError_G514<- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G514, ES = "LRRi")
StdError_G514$Ratio <- log(Ratios_NAT$G514[c(29:41)])
StdError_G514$Species <- "G514"

StdError_G600 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G600, ES = "LRRi")
StdError_G600$Ratio <- log(Ratios_NAT$G600[c(29:41)])
StdError_G600$Species <- "G600"

StdError_G649 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G649, ES = "LRRi")
StdError_G649$Ratio <- log(Ratios_NAT$G649[c(29:41)])
StdError_G649$Species <- "G649"

StdError_G673 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G673, ES = "LRRi")
StdError_G673$Ratio <- log(Ratios_NAT$G673[c(29:41)])
StdError_G673$Species <- "G673"

StdError_G699 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(29:41)])
StdError_G699$Species <- "G699"

StdError_G707 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G707, ES = "LRRi")
StdError_G707$Ratio <- log(Ratios_NAT$G707[c(29:41)])
StdError_G707$Species <- "G707"

StdError_G711 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G711, ES = "LRRi")
StdError_G711$Ratio <- log(Ratios_NAT$G711[c(29:41)])
StdError_G711$Species <- "G711"

StdError_G712 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G712, ES = "LRRi")
StdError_G712$Ratio <- log(Ratios_NAT$G712[c(29:41)])
StdError_G712$Species <- "G712"

StdError_G714 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G714, ES = "LRRi")
StdError_G714$Ratio <- log(Ratios_NAT$G714[c(29:41)])
StdError_G714$Species <- "G714"

StdError_G724 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G724, ES = "LRRi")
StdError_G724$Ratio <- log(Ratios_NAT$G724[c(29:41)])
StdError_G724$Species <- "G724"

StdError_G725 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G725, ES = "LRRi")
StdError_G725$Ratio <- log(Ratios_NAT$G725[c(29:41)])
StdError_G725$Species <- "G725"

StdError_G849 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G849, ES = "LRRi")
StdError_G849$Ratio <- log(Ratios_NAT$G849[c(29:41)])
StdError_G849$Species <- "G849"

StdError_G913 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G913, ES = "LRRi")
StdError_G913$Ratio <- log(Ratios_NAT$G913[c(29:41)])
StdError_G913$Species <- "G913"

StdError_G916 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G916, ES = "LRRi")
StdError_G916$Ratio <- log(Ratios_NAT$G916[c(29:41)])
StdError_G916$Species <- "G916"

StdError_S009 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = S009, ES = "LRRi")
StdError_S009$Ratio <- log(Ratios_NAT$S009[c(29:41)])
StdError_S009$Species <- "S009"


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
ViaMaris_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9)],StdError_NAT[c(1,4,7,8,9)])
