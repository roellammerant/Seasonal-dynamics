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
####        Load SOREQ dataset and summarise            ####
#                                                          #
##%######################################################%##
SOREQ <- read.csv("DATA/SOREQ.csv", 
                  na = "NA")
SpSoreq <- read_excel("output_data/SOREQ_InvasiveStatus.xlsx", 
                      na = "NA")

### turn data into presence absence
SoreqCategories <- SOREQ[c(2:5)]
SoreqSpecies <- SOREQ[-c(1:5)]

### Create categories to merch replicates
SoreqCategories$Merched_Category <- factor(paste(SoreqCategories$eventYear, 
                                                 SoreqCategories$eventSeason,
                                                 sep = "_"))

SoreqSpecies$Merched_Category <- SoreqCategories$Merched_Category

### Create Community level abundance columns
SoregTranspose <- t(SoreqSpecies)
SoregTranspose <- as.data.frame(SoregTranspose)
SoregTranspose <- SoregTranspose[-c(108),]
SoregTranspose$Invasive <- SpSoreq$...2
SoreqNIS  <- SoregTranspose[which(SoregTranspose$Invasive  =='1'),]
SoreqNAT  <- SoregTranspose[which(SoregTranspose$Invasive  =='0'),]
SoreqNIS <- SoreqNIS %>% 
  mutate_if(is.character, as.numeric)
SoreqNAT <- SoreqNAT %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
SoreqNIS <- t(SoreqNIS)
SoreqNIS <- as.data.frame(SoreqNIS)
SoreqNIS <- SoreqNIS[-c(391),]
SoreqNIS$Merched_Category <- SoreqCategories$Merched_Category
Mean_NIS <- aggregate(SoreqNIS[c(1:36)], 
                      list(SoreqNIS$Merched_Category), FUN=mean) 

SoreqNAT <- t(SoreqNAT)
SoreqNAT <- as.data.frame(SoreqNAT)
SoreqNAT <- SoreqNAT[-c(391),]
SoreqNAT$Merched_Category <- SoreqCategories$Merched_Category
Mean_NAT <- aggregate(SoreqNAT[c(1:67)], 
                      list(SoreqNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$Ratio2012 <-Ratios_NIS$`2012_Autumn`/Ratios_NIS$`2012_Spring`
Ratios_NIS$Ratio2015 <-Ratios_NIS$`2015_Autumn`/Ratios_NIS$`2015_Spring`
Ratios_NIS$Ratio2016 <-Ratios_NIS$`2016_Autumn`/Ratios_NIS$`2016_Spring`
Ratios_NIS$Ratio2017 <-Ratios_NIS$`2017_Autumn`/Ratios_NIS$`2017_Spring`
Ratios_NIS$Ratio2018 <-Ratios_NIS$`2018_Autumn`/Ratios_NIS$`2018_Spring`
Ratios_NIS$Ratio2019 <-Ratios_NIS$`2019_Autumn`/Ratios_NIS$`2019_Spring`
Ratios_NIS$Ratio2020 <-Ratios_NIS$`2020_Autumn`/Ratios_NIS$`2020_Spring`
Ratios_NIS$Ratio2021 <-Ratios_NIS$`2021_Autumn`/Ratios_NIS$`2021_Spring`
Ratios_NIS$Ratio2022 <-Ratios_NIS$`2022_Autumn`/Ratios_NIS$`2022_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2015 <-Ratios_NAT$`2015_Autumn`/Ratios_NAT$`2015_Spring`
Ratios_NAT$Ratio2016 <-Ratios_NAT$`2016_Autumn`/Ratios_NAT$`2016_Spring`
Ratios_NAT$Ratio2017 <-Ratios_NAT$`2017_Autumn`/Ratios_NAT$`2017_Spring`
Ratios_NAT$Ratio2018 <-Ratios_NAT$`2018_Autumn`/Ratios_NAT$`2018_Spring`
Ratios_NAT$Ratio2019 <-Ratios_NAT$`2019_Autumn`/Ratios_NAT$`2019_Spring`
Ratios_NAT$Ratio2020 <-Ratios_NAT$`2020_Autumn`/Ratios_NAT$`2020_Spring`
Ratios_NAT$Ratio2021 <-Ratios_NAT$`2021_Autumn`/Ratios_NAT$`2021_Spring`
Ratios_NAT$Ratio2022 <-Ratios_NAT$`2022_Autumn`/Ratios_NAT$`2022_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

##### StdError values for ratios NIS species #####
SoreqNIS$eventYear <- SoreqCategories$eventYear
SoreqNIS$eventSeason <-  SoreqCategories$eventSeason
SoreqNIS[SoreqNIS == 0] <- 0.1

StdError_B001 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B001, ES = "LRRi")
StdError_B001$Ratio <- log(Ratios_NIS$B001[c(19:27)])
StdError_B001$TempPrefMax <- "30.43"
StdError_B001$TempPrefMean <- "23.15"


StdError_B007 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B007, ES = "LRRi")
StdError_B007$Ratio <- log(Ratios_NIS$B007[c(19:27)])
StdError_B007$TempPrefMax <- "25.43"
StdError_B007$TempPrefMean <- "25.43"

StdError_B011 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B011, ES = "LRRi")
StdError_B011$Ratio <- log(Ratios_NIS$B011[c(19:27)])
StdError_B011$TempPrefMax <- "NA"
StdError_B011$TempPrefMean <- "NA"

StdError_B029 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B029, ES = "LRRi")
StdError_B029$Ratio <- log(Ratios_NIS$B029[c(19:27)])
StdError_B029$TempPrefMax <- "29.67"
StdError_B029$TempPrefMean <- "25.15"

StdError_B030 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B030, ES = "LRRi")
StdError_B030$Ratio <- log(Ratios_NIS$B030[c(19:27)])
StdError_B030$TempPrefMax <- "27.94"
StdError_B030$TempPrefMean <- "25.20"

StdError_B031 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B031, ES = "LRRi")
StdError_B031$Ratio <- log(Ratios_NIS$B031[c(19:27)])
StdError_B031$TempPrefMax <- "NA"
StdError_B031$TempPrefMean <- "NA"

StdError_B078 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(19:27)])
StdError_B078$TempPrefMax <- "30.24"
StdError_B078$TempPrefMean <- "27.18"

StdError_B094 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B094, ES = "LRRi")
StdError_B094$Ratio <- log(Ratios_NIS$B094[c(19:27)])
StdError_B094$TempPrefMax <- "NA"
StdError_B094$TempPrefMean <- "NA"

StdError_B208 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B208, ES = "LRRi")
StdError_B208$Ratio <- log(Ratios_NIS$B208[c(19:27)])
StdError_B208$TempPrefMax <- "30.39"
StdError_B208$TempPrefMean <- "26.20"

StdError_B227 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(19:27)])
StdError_B227$TempPrefMax <- "NA"
StdError_B227$TempPrefMean <- "NA"

StdError_B482 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B482, ES = "LRRi")
StdError_B482$Ratio <- log(Ratios_NIS$B482[c(19:27)])
StdError_B482$TempPrefMax <- "29.54"
StdError_B482$TempPrefMean <- "21.59"

StdError_G001 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G001, ES = "LRRi")
StdError_G001$Ratio <- log(Ratios_NIS$G001[c(19:27)])
StdError_G001$TempPrefMax <- "NA"
StdError_G001$TempPrefMean <- "NA"

StdError_G002 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G002, ES = "LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(19:27)])
StdError_G002$TempPrefMax <- "30.37"
StdError_G002$TempPrefMean <- "26.44"

StdError_G003 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G003, ES = "LRRi")
StdError_G003$Ratio <- log(Ratios_NIS$G003[c(19:27)])
StdError_G003$TempPrefMax <- "NA"
StdError_G003$TempPrefMean <- "NA"

StdError_G004 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G004, ES = "LRRi")
StdError_G004$Ratio <- log(Ratios_NIS$G004[c(19:27)])
StdError_G004$TempPrefMax <- "NA"
StdError_G004$TempPrefMean <- "NA"

StdError_G005 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G005, ES = "LRRi")
StdError_G005$Ratio <- log(Ratios_NIS$G005[c(19:27)])
StdError_G005$TempPrefMax <- "29.93"
StdError_G005$TempPrefMean <- "27.64"

StdError_G006 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G006, ES = "LRRi")
StdError_G006$Ratio <- log(Ratios_NIS$G006[c(19:27)])
StdError_G006$TempPrefMax <- "NA"
StdError_G006$TempPrefMean <- "NA"

StdError_G007 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(19:27)])
StdError_G007$TempPrefMax <- "NA"
StdError_G007$TempPrefMean <- "NA"

StdError_G008 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G008, ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(19:27)])
StdError_G008$TempPrefMax <- "NA"
StdError_G008$TempPrefMean <- "NA"

StdError_G009 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G009, ES = "LRRi")
StdError_G009$Ratio <- log(Ratios_NIS$G009[c(19:27)])
StdError_G009$TempPrefMax <- "NA"
StdError_G009$TempPrefMean <- "NA"

StdError_G012 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G012, ES = "LRRi")
StdError_G012$Ratio <- log(Ratios_NIS$G012[c(19:27)])
StdError_G012$TempPrefMax <- "NA"
StdError_G012$TempPrefMean <- "NA"

StdError_G023 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G023, ES = "LRRi")
StdError_G023$Ratio <- log(Ratios_NIS$G023[c(19:27)])
StdError_G023$TempPrefMax <- "NA"
StdError_G023$TempPrefMean <- "NA"

StdError_G063 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G063, ES = "LRRi")
StdError_G063$Ratio <- log(Ratios_NIS$G063[c(19:27)])
StdError_G063$TempPrefMax <- "NA"
StdError_G063$TempPrefMean <- "NA"

StdError_G081 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G081, ES = "LRRi")
StdError_G081$Ratio <- log(Ratios_NIS$G081[c(19:27)])
StdError_G081$TempPrefMax <- "NA"
StdError_G081$TempPrefMean <- "NA"

StdError_G089 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G089, ES = "LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(19:27)])
StdError_G089$TempPrefMax <- "28.27"
StdError_G089$TempPrefMean <- "27.26"

StdError_G102 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G102, ES = "LRRi")
StdError_G102$Ratio <- log(Ratios_NIS$G102[c(19:27)])
StdError_G102$TempPrefMax <- "30.43"
StdError_G102$TempPrefMean <- "29.16"

StdError_G104 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G104, ES = "LRRi")
StdError_G104$Ratio <- log(Ratios_NIS$G104[c(19:27)])
StdError_G104$TempPrefMax <- "NA"
StdError_G104$TempPrefMean <- "NA"

StdError_G149 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G149, ES = "LRRi")
StdError_G149$Ratio <- log(Ratios_NIS$G149[c(19:27)])
StdError_G149$TempPrefMax <- "NA"
StdError_G149$TempPrefMean <- "NA"

StdError_G237 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G237, ES = "LRRi")
StdError_G237$Ratio <- log(Ratios_NIS$G237[c(19:27)])
StdError_G237$TempPrefMax <- "30.43"
StdError_G237$TempPrefMean <- "26.84"

StdError_G247<- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                              outcome = G247, ES = "LRRi")
StdError_G247$Ratio <- log(Ratios_NIS$G247[c(19:27)])
StdError_G247$TempPrefMax <- "NA"
StdError_G247$TempPrefMean <- "NA"

StdError_G254 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G254, ES = "LRRi")
StdError_G254$Ratio <- log(Ratios_NIS$G254[c(19:27)])
StdError_G254$TempPrefMax <- "NA"
StdError_G254$TempPrefMean <- "NA"

StdError_G307 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G307, ES = "LRRi")
StdError_G307$Ratio <- log(Ratios_NIS$G307[c(19:27)])
StdError_G307$TempPrefMax <- "NA"
StdError_G307$TempPrefMean <- "NA"

StdError_G322 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G322, ES = "LRRi")
StdError_G322$Ratio <- log(Ratios_NIS$G322[c(19:27)])
StdError_G322$TempPrefMax <- "NA"
StdError_G322$TempPrefMean <- "NA"

StdError_G343 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G343, ES = "LRRi")
StdError_G343$Ratio <- log(Ratios_NIS$G343[c(19:27)])
StdError_G343$TempPrefMax <- "NA"
StdError_G343$TempPrefMean <- "NA"

StdError_G550 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G550, ES = "LRRi")
StdError_G550$Ratio <- log(Ratios_NIS$G550[c(19:27)])
StdError_G550$TempPrefMax <- "NA"
StdError_G550$TempPrefMean <- "NA"

StdError_G858 <- batch_calc_ES(dat = SoreqNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G858, ES = "LRRi")
StdError_G858$Ratio <- log(Ratios_NIS$G858[c(19:27)])
StdError_G858$TempPrefMax <- "NA"
StdError_G858$TempPrefMean <- "NA"

StdError_NIS<- rbind(StdError_B001,StdError_B007,StdError_B011,StdError_B029,StdError_B030,StdError_B031,StdError_B078,
                     StdError_B094,StdError_B208,StdError_B227,StdError_B482,StdError_G001,StdError_G002,StdError_G003,
                     StdError_G004,StdError_G005,StdError_G006,StdError_G007,StdError_G008,StdError_G009,StdError_G012,
                     StdError_G023,StdError_G063,StdError_G081,StdError_G089,StdError_G102,StdError_G104,StdError_G149,
                     StdError_G237,StdError_G247,StdError_G254,StdError_G307,StdError_G322,StdError_G343,StdError_G550,
                     StdError_G858)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NIS species ####
SoreqNAT$eventYear <- SoreqCategories$eventYear
SoreqNAT$eventSeason <-  SoreqCategories$eventSeason
SoreqNAT[SoreqNAT == 0] <- 0.1

StdError_B004 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B004, ES = "LRRi")
StdError_B004$Ratio <- log(Ratios_NAT$B004[c(19:27)])
StdError_B004$TempPrefMax <- "27.43"
StdError_B004$TempPrefMean <- "11.44"

StdError_B005 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(19:27)])
StdError_B005$TempPrefMax <- "16.87"
StdError_B005$TempPrefMean <- "11.55"

StdError_B006 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B006, ES = "LRRi")
StdError_B006$Ratio <- log(Ratios_NAT$B006[c(19:27)])
StdError_B006$TempPrefMax <- "28.14"
StdError_B006$TempPrefMean <- "11.22"

StdError_B013 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B013, ES = "LRRi")
StdError_B013$Ratio <- log(Ratios_NAT$B013[c(19:27)])
StdError_B013$TempPrefMax <- "29.71"
StdError_B013$TempPrefMean <- "11.00"

StdError_B014 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B014, ES = "LRRi")
StdError_B014$Ratio <- log(Ratios_NAT$B014[c(19:27)])
StdError_B014$TempPrefMax <- "NA"
StdError_B014$TempPrefMean <- "NA"

StdError_B016 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B016, ES = "LRRi")
StdError_B016$Ratio <- log(Ratios_NAT$B016[c(19:27)])
StdError_B016$TempPrefMax <- "25.35"
StdError_B016$TempPrefMean <- "19.41"

StdError_B020 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B020, ES = "LRRi")
StdError_B020$Ratio <- log(Ratios_NAT$B020[c(19:27)])
StdError_B020$TempPrefMax <- "28.041"
StdError_B020$TempPrefMean <- "14.77"

StdError_B022 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B022, ES = "LRRi")
StdError_B022$Ratio <- log(Ratios_NAT$B022[c(19:27)])
StdError_B022$TempPrefMax <- "22.22"
StdError_B022$TempPrefMean <- "17.38"

StdError_B023 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(19:27)])
StdError_B023$TempPrefMax <- "20.53"
StdError_B023$TempPrefMean <- "13.49"

StdError_B032 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B032, ES = "LRRi")
StdError_B032$Ratio <- log(Ratios_NAT$B032[c(19:27)])
StdError_B032$TempPrefMax <- "NA"
StdError_B032$TempPrefMean <- "NA"

StdError_B033 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(19:27)])
StdError_B033$TempPrefMax <- "24.45"
StdError_B033$TempPrefMean <- "11.58"

StdError_B039 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B039, ES = "LRRi")
StdError_B039$Ratio <- log(Ratios_NAT$B039[c(19:27)])
StdError_B039$TempPrefMax <- "NA"
StdError_B039$TempPrefMean <- "NA"

StdError_B044 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B044, ES = "LRRi")
StdError_B044$Ratio <- log(Ratios_NAT$B044[c(19:27)])
StdError_B044$TempPrefMax <- "20.20"
StdError_B044$TempPrefMean <- "15.00"

StdError_B045 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B045, ES = "LRRi")
StdError_B045$Ratio <- log(Ratios_NAT$B045[c(19:27)])
StdError_B045$TempPrefMax <- "24.34"
StdError_B045$TempPrefMean <- "11.80"

StdError_B046 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(19:27)])
StdError_B046$TempPrefMax <- "28.59"
StdError_B046$TempPrefMean <- "12.02"

StdError_B052 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B052, ES = "LRRi")
StdError_B052$Ratio <- log(Ratios_NAT$B052[c(19:27)])
StdError_B052$TempPrefMax <- "20.53"
StdError_B052$TempPrefMean <- "12.36"

StdError_B064 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B064, ES = "LRRi")
StdError_B064$Ratio <- log(Ratios_NAT$B064[c(19:27)])
StdError_B064$TempPrefMax <- "27.19"
StdError_B064$TempPrefMean <- "18.80"

StdError_B084 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B084, ES = "LRRi")
StdError_B084$Ratio <- log(Ratios_NAT$B084[c(19:27)])
StdError_B084$TempPrefMax <- "19.43"
StdError_B084$TempPrefMean <- "13.31"

StdError_B088 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B088, ES = "LRRi")
StdError_B088$Ratio <- log(Ratios_NAT$B088[c(19:27)])
StdError_B088$TempPrefMax <- "NA"
StdError_B088$TempPrefMean <- "NA"

StdError_B118 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B118, ES = "LRRi")
StdError_B118$Ratio <- log(Ratios_NAT$B118[c(19:27)])
StdError_B118$TempPrefMax <- "25.08"
StdError_B118$TempPrefMean <- "10.93"

StdError_B121 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B121, ES = "LRRi")
StdError_B121$Ratio <- log(Ratios_NAT$B121[c(19:27)])
StdError_B121$TempPrefMax <- "18.78"
StdError_B121$TempPrefMean <- "11.28"

StdError_B136 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B136, ES = "LRRi")
StdError_B136$Ratio <- log(Ratios_NAT$B136[c(19:27)])
StdError_B136$TempPrefMax <- "24.38"
StdError_B136$TempPrefMean <- "15.02"

StdError_B141 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B141, ES = "LRRi")
StdError_B141$Ratio <- log(Ratios_NAT$B141[c(19:27)])
StdError_B141$TempPrefMax <- "15.63"
StdError_B141$TempPrefMean <- "9.43"

StdError_B158 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B158, ES = "LRRi")
StdError_B158$Ratio <- log(Ratios_NAT$B158[c(19:27)])
StdError_B158$TempPrefMax <- "29.78"
StdError_B158$TempPrefMean <- "17.30"

StdError_B184 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B184, ES = "LRRi")
StdError_B184$Ratio <- log(Ratios_NAT$B184[c(19:27)])
StdError_B184$TempPrefMax <- "NA"
StdError_B184$TempPrefMean <- "NA"

StdError_B186 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B186, ES = "LRRi")
StdError_B186$Ratio <- log(Ratios_NAT$B186[c(19:27)])
StdError_B186$TempPrefMax <- "NA"
StdError_B186$TempPrefMean <- "NA"

StdError_B187 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B187, ES = "LRRi")
StdError_B187$Ratio <- log(Ratios_NAT$B187[c(19:27)])
StdError_B187$TempPrefMax <- "18.63"
StdError_B187$TempPrefMean <- "13.36"

StdError_B252 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B252, ES = "LRRi")
StdError_B252$Ratio <- log(Ratios_NAT$B252[c(19:27)])
StdError_B252$TempPrefMax <- "28.01"
StdError_B252$TempPrefMean <- "11.36"

StdError_B284 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B284, ES = "LRRi")
StdError_B284$Ratio <- log(Ratios_NAT$B284[c(19:27)])
StdError_B284$TempPrefMax <- "19.70"
StdError_B284$TempPrefMean <- "16.68"

StdError_B318 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B318, ES = "LRRi")
StdError_B318$Ratio <- log(Ratios_NAT$B318[c(19:27)])
StdError_B318$TempPrefMax <- "28.04"
StdError_B318$TempPrefMean <- "12.19"

StdError_B321 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B321, ES = "LRRi")
StdError_B321$Ratio <- log(Ratios_NAT$B321[c(19:27)])
StdError_B321$TempPrefMax <- "NA"
StdError_B321$TempPrefMean <- "NA"

StdError_B343 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B343, ES = "LRRi")
StdError_B343$Ratio <- log(Ratios_NAT$B343[c(19:27)])
StdError_B343$TempPrefMax <- "28.26"
StdError_B343$TempPrefMean <- "11.72"

StdError_B453 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B453, ES = "LRRi")
StdError_B453$Ratio <- log(Ratios_NAT$B453[c(19:27)])
StdError_B453$TempPrefMax <- "28.11"
StdError_B453$TempPrefMean <- "23.86"

StdError_B503 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B503, ES = "LRRi")
StdError_B503$Ratio <- log(Ratios_NAT$B503[c(19:27)])
StdError_B503$TempPrefMax <- "NA"
StdError_B503$TempPrefMean <- "NA"

StdError_G010 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(19:27)]) 
StdError_G010$TempPrefMax <- "NA"
StdError_G010$TempPrefMean <- "NA"

StdError_G013 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G013, ES = "LRRi")
StdError_G013$Ratio <- log(Ratios_NAT$G013[c(19:27)]) 
StdError_G013$TempPrefMax <- "NA"
StdError_G013$TempPrefMean <- "NA"

StdError_G027 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(19:27)]) 
StdError_G027$TempPrefMax <- "NA"
StdError_G027$TempPrefMean <- "NA"

StdError_G045 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G045, ES = "LRRi")
StdError_G045$Ratio <- log(Ratios_NAT$G045[c(19:27)]) 
StdError_G045$TempPrefMax <- "20.50"
StdError_G045$TempPrefMean <- "10.77"

StdError_G053 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G053, ES = "LRRi")
StdError_G053$Ratio <- log(Ratios_NAT$G053[c(19:27)]) 
StdError_G053$TempPrefMax <- "18.63"
StdError_G053$TempPrefMean <- "16.64"

StdError_G056 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G056, ES = "LRRi")
StdError_G056$Ratio <- log(Ratios_NAT$G056[c(19:27)]) 
StdError_G056$TempPrefMax <- "29.85"
StdError_G056$TempPrefMean <- "25.37"

StdError_G066 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G066, ES = "LRRi")
StdError_G066$Ratio <- log(Ratios_NAT$G066[c(19:27)]) 
StdError_G066$TempPrefMax <- "14.73"
StdError_G066$TempPrefMean <- "11.45"

StdError_G075 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G075, ES = "LRRi")
StdError_G075$Ratio <- log(Ratios_NAT$G075[c(19:27)]) 
StdError_G075$TempPrefMax <- "23.25"
StdError_G075$TempPrefMean <- "11.86"

StdError_G080 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G080, ES = "LRRi")
StdError_G080$Ratio <- log(Ratios_NAT$G080[c(19:27)]) 
StdError_G080$TempPrefMax <- "NA"
StdError_G080$TempPrefMean <- "NA"

StdError_G105 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G105, ES = "LRRi")
StdError_G105$Ratio <- log(Ratios_NAT$G105[c(19:27)]) 
StdError_G105$TempPrefMax <- "NA"
StdError_G105$TempPrefMean <- "NA"

StdError_G107 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G107, ES = "LRRi")
StdError_G107$Ratio <- log(Ratios_NAT$G107[c(19:27)]) 
StdError_G107$TempPrefMax <- "NA"
StdError_G107$TempPrefMean <- "NA"

StdError_G112 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G112, ES = "LRRi")
StdError_G112$Ratio <- log(Ratios_NAT$G112[c(19:27)]) 
StdError_G112$TempPrefMax <- "NA"
StdError_G112$TempPrefMean <- "NA"

StdError_G134 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G134, ES = "LRRi")
StdError_G134$Ratio <- log(Ratios_NAT$G134[c(19:27)]) 
StdError_G134$TempPrefMax <- "NA"
StdError_G134$TempPrefMean <- "NA"

StdError_G142 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G142, ES = "LRRi")
StdError_G142$Ratio <- log(Ratios_NAT$G142[c(19:27)]) 
StdError_G142$TempPrefMax <- "NA"
StdError_G142$TempPrefMean <- "NA"

StdError_G147 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G147, ES = "LRRi")
StdError_G147$Ratio <- log(Ratios_NAT$G147[c(19:27)]) 
StdError_G147$TempPrefMax <- "NA"
StdError_G147$TempPrefMean <- "NA"

StdError_G192 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G192, ES = "LRRi")
StdError_G192$Ratio <- log(Ratios_NAT$G192[c(19:27)]) 
StdError_G192$TempPrefMax <- "NA"
StdError_G192$TempPrefMean <- "NA"

StdError_G211 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G211, ES = "LRRi")
StdError_G211$Ratio <- log(Ratios_NAT$G211[c(19:27)]) 
StdError_G211$TempPrefMax <- "NA"
StdError_G211$TempPrefMean <- "NA"

StdError_G275 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G275, ES = "LRRi")
StdError_G275$Ratio <- log(Ratios_NAT$G275[c(19:27)])
StdError_G275$TempPrefMax <- "NA"
StdError_G275$TempPrefMean <- "NA"

StdError_G299 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G299, ES = "LRRi")
StdError_G299$Ratio <- log(Ratios_NAT$G299[c(19:27)]) 
StdError_G299$TempPrefMax <- "23.25"
StdError_G299$TempPrefMean <- "16.54"

StdError_G338 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(19:27)]) 
StdError_G338$TempPrefMax <- "NA"
StdError_G338$TempPrefMean <- "NA"

StdError_G341 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G341, ES = "LRRi")
StdError_G341$Ratio <- log(Ratios_NAT$G341[c(19:27)]) 
StdError_G341$TempPrefMax <- "18.55"
StdError_G341$TempPrefMean <- "18.19"

StdError_G349 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G349, ES = "LRRi")
StdError_G349$Ratio <- log(Ratios_NAT$G349[c(19:27)]) 
StdError_G349$TempPrefMax <- "NA"
StdError_G349$TempPrefMean <- "NA"

StdError_G401 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G401, ES = "LRRi")
StdError_G401$Ratio <- log(Ratios_NAT$G401[c(19:27)])
StdError_G401$TempPrefMax <- "NA"
StdError_G401$TempPrefMean <- "NA"

StdError_G514 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G514, ES = "LRRi")
StdError_G514$Ratio <- log(Ratios_NAT$G514[c(19:27)]) 
StdError_G514$TempPrefMax <- "NA"
StdError_G514$TempPrefMean <- "NA"

StdError_G673 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G673, ES = "LRRi")
StdError_G673$Ratio <- log(Ratios_NAT$G673[c(19:27)]) 
StdError_G673$TempPrefMax <- "NA"
StdError_G673$TempPrefMean <- "NA"

StdError_G699 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(19:27)]) 
StdError_G699$TempPrefMax <- "NA"
StdError_G699$TempPrefMean <- "NA"

StdError_G711 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G711, ES = "LRRi")
StdError_G711$Ratio <- log(Ratios_NAT$G711[c(19:27)]) 
StdError_G711$TempPrefMax <- "NA"
StdError_G711$TempPrefMean <- "NA"

StdError_G714 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G714, ES = "LRRi")
StdError_G714$Ratio <- log(Ratios_NAT$G714[c(19:27)]) 
StdError_G714$TempPrefMax <- "NA"
StdError_G714$TempPrefMean <- "NA"

StdError_G724 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G724, ES = "LRRi")
StdError_G724$Ratio <- log(Ratios_NAT$G724[c(19:27)]) 
StdError_G724$TempPrefMax <- "NA"
StdError_G724$TempPrefMean <- "NA"

StdError_G849 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G849, ES = "LRRi")
StdError_G849$Ratio <- log(Ratios_NAT$G849[c(19:27)]) 
StdError_G849$TempPrefMax <- "NA"
StdError_G849$TempPrefMean <- "NA"

StdError_G913 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G913, ES = "LRRi")
StdError_G913$Ratio <- log(Ratios_NAT$G913[c(19:27)]) 
StdError_G913$TempPrefMax <- "NA"
StdError_G913$TempPrefMean <- "NA"

StdError_G916 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G916, ES = "LRRi")
StdError_G916$Ratio <- log(Ratios_NAT$G916[c(19:27)]) 
StdError_G916$TempPrefMax <- "NA"
StdError_G916$TempPrefMean <- "NA"

StdError_S009 <- batch_calc_ES(dat = SoreqNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = S009, ES = "LRRi")
StdError_S009$Ratio <- log(Ratios_NAT$S009[c(19:27)]) 
StdError_S009$TempPrefMax <- "18.78"
StdError_S009$TempPrefMean <- "17.92"

StdError_NAT<- rbind(StdError_B004,StdError_B005,StdError_B006,StdError_B013,StdError_B014,StdError_B016,StdError_B020,
                     StdError_B022,StdError_B023,StdError_B032,StdError_B033,StdError_B039,StdError_B044,StdError_B045,
                     StdError_B046,StdError_B052,StdError_B064,StdError_B084,StdError_B088,StdError_B118,StdError_B121,
                     StdError_B136,StdError_B141,StdError_B158,StdError_B184,StdError_B186,StdError_B187,StdError_B252,
                     StdError_B284,StdError_B318,StdError_B321,StdError_B343,StdError_B453,StdError_B503,StdError_G010,
                     StdError_G013,StdError_G027,StdError_G045,StdError_G053,StdError_G056,StdError_G066,StdError_G075,
                     StdError_G080,StdError_G105,StdError_G107,StdError_G112,StdError_G134,StdError_G142,StdError_G147,
                     StdError_G192,StdError_G211,StdError_G275,StdError_G299,StdError_G338,StdError_G341,StdError_G349,
                     StdError_G401,StdError_G514,StdError_G673,StdError_G699,StdError_G711,StdError_G714,StdError_G724,
                     StdError_G849,StdError_G913,StdError_G916,StdError_S009)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
Soreq_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9,10)],
                                 StdError_NAT[c(1,4,7,8,9,10)])

Soreq_Species_Abundance <- Soreq_Species_Abundance[Soreq_Species_Abundance$TempPrefMax != c('NA'),]
