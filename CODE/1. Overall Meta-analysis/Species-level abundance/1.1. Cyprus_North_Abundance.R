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
#%######################################################%##
#                                                          #
####             Load Mollusc dataset                   ####
#                                                          #
##%######################################################%##
load("~/GitHub/Seasonal-dynamics/DATA/CyprusNorth.Rdata")
CyprusN <- LA

### Split data up
CyprusCategories <- as.data.frame(colnames(CyprusN))
CyprusCategories <- as.data.frame(CyprusCategories[c(7:63),])
colnames(CyprusCategories)[1] <- c("Sites_Replicate")

CyprusCategories$Season <- c(rep("Spring",3), rep("Autumn",3),rep("Spring",3), rep("Autumn",3),rep("Spring",3), rep("Autumn",3),
                             rep("Spring",3), rep("Autumn",3),rep("Spring",3), rep("Autumn",3),rep("Spring",3), rep("Autumn",3),
                             rep("Spring",3), rep("Autumn",3),rep("Spring",3), rep("Autumn",3),rep("Spring",3), rep("Autumn",3),
                             rep("Spring",3))

CyprusCategories$Sites <- c(rep("KPos05",6), rep("KPos10",6), rep("KPos15",6), rep("KPos20",6), rep("KPos30",6),
                            rep("KR05",6), rep("KR10",6), rep("KR15",6), rep("KR20",6), rep("KR30",3))

CyprusSpecies <- CyprusN[c(1, 6:63)]

### Create categories to merch replicates
CyprusCategories$Merched_Category <- factor(paste(CyprusCategories$Sites, 
                                                  CyprusCategories$Season,
                                                  sep = "_"))

### Create Community level abundance columns
CyprusNIS  <- CyprusSpecies[which(CyprusSpecies$Alien  =='Yes'),]
CyprusNAT  <- CyprusSpecies[which(CyprusSpecies$Alien  =='No'),]

### Mean values per Year*Season 
CyprusNIS <- t(CyprusNIS)
CyprusNIS <- as.data.frame(CyprusNIS)
colnames(CyprusNIS) <- CyprusNIS[1,]
CyprusNIS <- CyprusNIS[-c(1,2),]
CyprusNIS <- CyprusNIS %>% 
  mutate_if(is.character, as.numeric)
CyprusNIS$Merched_Category <- CyprusCategories$Merched_Category
Mean_NIS <- aggregate(CyprusNIS[c(1:27)], 
                      list(CyprusNIS$Merched_Category), FUN=mean) 

CyprusNAT <- t(CyprusNAT)
CyprusNAT <- as.data.frame(CyprusNAT)
colnames(CyprusNAT) <- CyprusNAT[1,]
CyprusNAT <- CyprusNAT[-c(1,2),]
CyprusNAT <- CyprusNAT %>% 
  mutate_if(is.character, as.numeric)
CyprusNAT$Merched_Category <- CyprusCategories$Merched_Category
Mean_NAT <- aggregate(CyprusNAT[c(1:112)], 
                      list(CyprusNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(9,10,19)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$RatioKPos05 <-Ratios_NIS$KPos05_Autumn/Ratios_NIS$KPos05_Spring
Ratios_NIS$RatioKPos10 <-Ratios_NIS$KPos10_Autumn/Ratios_NIS$KPos10_Spring
Ratios_NIS$RatioKPos15 <-Ratios_NIS$KPos15_Autumn/Ratios_NIS$KPos15_Spring
Ratios_NIS$RatioKPos20 <-Ratios_NIS$KPos20_Autumn/Ratios_NIS$KPos20_Spring
Ratios_NIS$RatioKR05 <-Ratios_NIS$KR05_Autumn/Ratios_NIS$KR05_Spring
Ratios_NIS$RatioKR10 <-Ratios_NIS$KR10_Autumn/Ratios_NIS$KR10_Spring
Ratios_NIS$RatioKR15 <-Ratios_NIS$KR15_Autumn/Ratios_NIS$KR15_Spring
Ratios_NIS$RatioKR20 <-Ratios_NIS$KR20_Autumn/Ratios_NIS$KR20_Spring
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),-c(9,10,19)]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$RatioKPos05 <-Ratios_NAT$KPos05_Autumn/Ratios_NAT$KPos05_Spring
Ratios_NAT$RatioKPos10 <-Ratios_NAT$KPos10_Autumn/Ratios_NAT$KPos10_Spring
Ratios_NAT$RatioKPos15 <-Ratios_NAT$KPos15_Autumn/Ratios_NAT$KPos15_Spring
Ratios_NAT$RatioKPos20 <-Ratios_NAT$KPos20_Autumn/Ratios_NAT$KPos20_Spring
Ratios_NAT$RatioKR05 <-Ratios_NAT$KR05_Autumn/Ratios_NAT$KR05_Spring
Ratios_NAT$RatioKR10 <-Ratios_NAT$KR10_Autumn/Ratios_NAT$KR10_Spring
Ratios_NAT$RatioKR15 <-Ratios_NAT$KR15_Autumn/Ratios_NAT$KR15_Spring
Ratios_NAT$RatioKR20 <-Ratios_NAT$KR20_Autumn/Ratios_NAT$KR20_Spring
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
CyprusNIS$Sites <- CyprusCategories$Sites
CyprusNIS$Season <-  CyprusCategories$Season
StdError_Data <- CyprusNIS[CyprusNIS$Sites != c('KR30'),]
StdError_Data <- StdError_Data[StdError_Data$Sites != c('KPos30'),]
StdError_Data[StdError_Data == 0] <- 0.1

StdError_B078 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(17:24)])
StdError_B078$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B078$Species <- "B078"

StdError_B124 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(17:24)])
StdError_B124$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B124$Species <- "B124"

StdError_B132 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B132, ES = "LRRi")
StdError_B132$Ratio <- log(Ratios_NIS$B132[c(17:24)])
StdError_B132$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B132$Species <- "B132"

StdError_B163 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B163, ES = "LRRi")
StdError_B163$Ratio <- log(Ratios_NIS$B163[c(17:24)])
StdError_B163$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B163$Species <- "B163"

StdError_B216 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B216, ES = "LRRi")
StdError_B216$Ratio <- log(Ratios_NIS$B216[c(17:24)])
StdError_B216$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B216$Species <- "B216"

StdError_B227 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(17:24)])
StdError_B227$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B227$Species <- "B227"

StdError_B464 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B464, ES = "LRRi")
StdError_B464$Ratio <- log(Ratios_NIS$B464[c(17:24)])
StdError_B464$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B464$Species <- "B464"

StdError_B481 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B481, ES = "LRRi")
StdError_B481$Ratio <- log(Ratios_NIS$B481[c(17:24)])
StdError_B481$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B481$Species <- "B481"

StdError_B624 <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = B624, ES = "LRRi")
StdError_B624$Ratio <- log(Ratios_NIS$B624[c(17:24)])
StdError_B624$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B624$Species <- "B624"

StdError_G002<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                               outcome = G002, ES = "LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(17:24)])
StdError_G002$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G002$Species <- "G002"

StdError_G007<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(17:24)])
StdError_G007$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G007$Species <- "G007"

StdError_G089<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G089, ES = "LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(17:24)])
StdError_G089$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G089$Species <- "G089"

StdError_G102<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G102, ES = "LRRi")
StdError_G102$Ratio <- log(Ratios_NIS$G102[c(17:24)])
StdError_G102$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G102$Species <- "G102"

StdError_G128<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G128, ES = "LRRi")
StdError_G128$Ratio <- log(Ratios_NIS$G128[c(17:24)])
StdError_G128$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G128$Species <- "G128"

StdError_G246<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G246, ES = "LRRi")
StdError_G246$Ratio <- log(Ratios_NIS$G246[c(17:24)])
StdError_G246$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G246$Species <- "G246"

StdError_G247<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G247, ES = "LRRi")
StdError_G247$Ratio <- log(Ratios_NIS$G247[c(17:24)])
StdError_G247$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G247$Species <- "G247"

StdError_G254<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G254, ES = "LRRi")
StdError_G254$Ratio <- log(Ratios_NIS$G254[c(17:24)])
StdError_G254$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G254$Species <- "G254"

StdError_G352 <-batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G352, ES = "LRRi")
StdError_G352$Ratio <- log(Ratios_NIS$G352[c(17:24)])
StdError_G352$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G352$Species <- "G352"

StdError_G385<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G385, ES = "LRRi")
StdError_G385$Ratio <- log(Ratios_NIS$G385[c(17:24)])
StdError_G385$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G385$Species <- "G385"

StdError_G390<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G390, ES = "LRRi")
StdError_G390$Ratio <- log(Ratios_NIS$G390[c(17:24)])
StdError_G390$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G390$Species <- "G390"

StdError_G395<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G395, ES = "LRRi")
StdError_G395$Ratio <- log(Ratios_NIS$G395[c(17:24)])
StdError_G395$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G395$Species <- "G395"

StdError_G543<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G543, ES = "LRRi")
StdError_G543$Ratio <- log(Ratios_NIS$G543[c(17:24)])
StdError_G543$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G543$Species <- "G543"

StdError_G550<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G550, ES = "LRRi")
StdError_G550$Ratio <- log(Ratios_NIS$G550[c(17:24)])
StdError_G550$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G550$Species <- "G550"

StdError_G814<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G814, ES = "LRRi")
StdError_G814$Ratio <- log(Ratios_NIS$G814[c(17:24)])
StdError_G814$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G814$Species <- "G814"

StdError_G815<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G815, ES = "LRRi")
StdError_G815$Ratio <- log(Ratios_NIS$G815[c(17:24)])
StdError_G815$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G815$Species <- "G815"

StdError_G816<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G816, ES = "LRRi")
StdError_G816$Ratio <- log(Ratios_NIS$G816[c(17:24)])
StdError_G816$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G816$Species <- "G816"

StdError_G862<- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = G862, ES = "LRRi")
StdError_G862$Ratio <- log(Ratios_NIS$G862[c(17:24)])
StdError_G862$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G862$Species <- "G862"

StdError_NIS<- rbind(StdError_B078,StdError_B124,StdError_B132,StdError_B163,StdError_B216,StdError_B227,StdError_B464,StdError_B481,StdError_B624,
                     StdError_G002,StdError_G007,StdError_G089,StdError_G102,StdError_G128,StdError_G246,StdError_G247,StdError_G254,StdError_G352,
                     StdError_G385,StdError_G390,StdError_G395,StdError_G543,StdError_G550,StdError_G814,StdError_G815,StdError_G816,StdError_G862)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NAT species ####
CyprusNAT$Sites <- CyprusCategories$Sites
CyprusNAT$Season <-  CyprusCategories$Season
StdError_DataB <- CyprusNAT[CyprusNAT$Sites != c('KR30'),]
StdError_DataB <- StdError_DataB[StdError_DataB$Sites != c('KPos30'),]
StdError_DataB[StdError_DataB == 0] <- 0.1

StdError_B005 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(17:24)])
StdError_B005$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B005$Species <- "B005"

StdError_B019 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(17:24)])
StdError_B019$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B019$Species <- "B019"

StdError_B023 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(17:24)])
StdError_B023$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B023$Species <- "B023"

StdError_B033 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(17:24)])
StdError_B033$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B033$Species <- "B033"

StdError_B045 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B045, ES = "LRRi")
StdError_B045$Ratio <- log(Ratios_NAT$B045[c(17:24)])
StdError_B045$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B045$Species <- "B045"

StdError_B046 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(17:24)])
StdError_B046$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B046$Species <- "B046"

StdError_B049 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B049, ES = "LRRi")
StdError_B049$Ratio <- log(Ratios_NAT$B049[c(17:24)])
StdError_B049$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B049$Species <- "B049"

StdError_B055 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(17:24)])
StdError_B055$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B055$Species <- "B055"

StdError_B060 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B060, ES = "LRRi")
StdError_B060$Ratio <- log(Ratios_NAT$B060[c(17:24)])
StdError_B060$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B060$Species <- "B060"

StdError_B065 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B065, ES = "LRRi")
StdError_B065$Ratio <- log(Ratios_NAT$B065[c(17:24)])
StdError_B065$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B065$Species <- "B065"

StdError_B066 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B066, ES = "LRRi")
StdError_B066$Ratio <- log(Ratios_NAT$B066[c(17:24)])
StdError_B066$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B066$Species <- "B066"

StdError_B068 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B068, ES = "LRRi")
StdError_B068$Ratio <- log(Ratios_NAT$B068[c(17:24)])
StdError_B068$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B068$Species <- "B068"

StdError_B088 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B088, ES = "LRRi")
StdError_B088$Ratio <- log(Ratios_NAT$B088[c(17:24)])
StdError_B088$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B088$Species <- "B088"

StdError_B125 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B125, ES = "LRRi")
StdError_B125$Ratio <- log(Ratios_NAT$B125[c(17:24)])
StdError_B125$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B125$Species <- "B125"

StdError_B138 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B138, ES = "LRRi")
StdError_B138$Ratio <- log(Ratios_NAT$B138[c(17:24)])
StdError_B138$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B138$Species <- "B138"

StdError_B139 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B139, ES = "LRRi")
StdError_B139$Ratio <- log(Ratios_NAT$B139[c(17:24)])
StdError_B139$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B139$Species <- "B139"

StdError_B142 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B142, ES = "LRRi")
StdError_B142$Ratio <- log(Ratios_NAT$B142[c(17:24)])
StdError_B142$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B142$Species <- "B142"

StdError_B143 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B143, ES = "LRRi")
StdError_B143$Ratio <- log(Ratios_NAT$B143[c(17:24)])
StdError_B143$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B143$Species <- "B143"

StdError_B147<- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B147, ES = "LRRi")
StdError_B147$Ratio <- log(Ratios_NAT$B147[c(17:24)])
StdError_B147$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B147$Species <- "B147"

StdError_B148 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B148, ES = "LRRi")
StdError_B148$Ratio <- log(Ratios_NAT$B148[c(17:24)])
StdError_B148$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B148$Species <- "B148"

StdError_B150 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B150, ES = "LRRi")
StdError_B150$Ratio <- log(Ratios_NAT$B150[c(17:24)])
StdError_B150$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B150$Species <- "B150"

StdError_B154 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B154, ES = "LRRi")
StdError_B154$Ratio <- log(Ratios_NAT$B154[c(17:24)])
StdError_B154$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B154$Species <- "B154"

StdError_B160 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B160, ES = "LRRi")
StdError_B160$Ratio <- log(Ratios_NAT$B160[c(17:24)])
StdError_B160$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B160$Species <- "B160"

StdError_B161 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B161, ES = "LRRi")
StdError_B161$Ratio <- log(Ratios_NAT$B161[c(17:24)])
StdError_B161$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B161$Species <- "B161"

StdError_B176 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B176, ES = "LRRi")
StdError_B176$Ratio <- log(Ratios_NAT$B176[c(17:24)])
StdError_B176$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B176$Species <- "B176"

StdError_B188 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B188, ES = "LRRi")
StdError_B188$Ratio <- log(Ratios_NAT$B188[c(17:24)])
StdError_B188$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B188$Species <- "B188"

StdError_B247 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B247, ES = "LRRi")
StdError_B247$Ratio <- log(Ratios_NAT$B247[c(17:24)])
StdError_B247$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B247$Species <- "B247"

StdError_B260 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B260, ES = "LRRi")
StdError_B260$Ratio <- log(Ratios_NAT$B260[c(17:24)])
StdError_B260$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B260$Species <- "B260"

StdError_B270 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B270, ES = "LRRi")
StdError_B270$Ratio <- log(Ratios_NAT$B270[c(17:24)])
StdError_B270$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B270$Species <- "B270"

StdError_B317 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B317, ES = "LRRi")
StdError_B317$Ratio <- log(Ratios_NAT$B317[c(17:24)])
StdError_B317$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B317$Species <- "B317"

StdError_B325 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B325, ES = "LRRi")
StdError_B325$Ratio <- log(Ratios_NAT$B325[c(17:24)])
StdError_B325$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B325$Species <- "B325"

StdError_B332 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B332, ES = "LRRi")
StdError_B332$Ratio <- log(Ratios_NAT$B332[c(17:24)])
StdError_B332$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B332$Species <- "B332"

StdError_B504 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = B504, ES = "LRRi")
StdError_B504$Ratio <- log(Ratios_NAT$B504[c(17:24)])
StdError_B504$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_B504$Species <- "B504"

StdError_G010 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(17:24)])
StdError_G010$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G010$Species <- "G010"

StdError_G017 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(17:24)])
StdError_G017$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G017$Species <- "G017"

StdError_G026 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G026, ES = "LRRi")
StdError_G026$Ratio <- log(Ratios_NAT$G026[c(17:24)])
StdError_G026$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G026$Species <- "G026"

StdError_G027 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(17:24)])
StdError_G027$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G027$Species <- "G027"

StdError_G029 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G029, ES = "LRRi")
StdError_G029$Ratio <- log(Ratios_NAT$G029[c(17:24)])
StdError_G029$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G029$Species <- "G029"

StdError_G032 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G032, ES = "LRRi")
StdError_G032$Ratio <- log(Ratios_NAT$G032[c(17:24)])
StdError_G032$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G032$Species <- "G032"

StdError_G037 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G037, ES = "LRRi")
StdError_G037$Ratio <- log(Ratios_NAT$G037[c(17:24)])
StdError_G037$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G037$Species <- "G037"

StdError_G044 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G044, ES = "LRRi")
StdError_G044$Ratio <- log(Ratios_NAT$G044[c(17:24)])
StdError_G044$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G044$Species <- "G044"

StdError_G046 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G046, ES = "LRRi")
StdError_G046$Ratio <- log(Ratios_NAT$G046[c(17:24)])
StdError_G046$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G046$Species <- "G046"

StdError_G047 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G047, ES = "LRRi")
StdError_G047$Ratio <- log(Ratios_NAT$G047[c(17:24)])
StdError_G047$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G047$Species <- "G047"

StdError_G050 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G050, ES = "LRRi")
StdError_G050$Ratio <- log(Ratios_NAT$G050[c(17:24)])
StdError_G050$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G050$Species <- "G050"

StdError_G051 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G051, ES = "LRRi")
StdError_G051$Ratio <- log(Ratios_NAT$G051[c(17:24)])
StdError_G051$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G051$Species <- "G051"

StdError_G064 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G064, ES = "LRRi")
StdError_G064$Ratio <- log(Ratios_NAT$G064[c(17:24)])
StdError_G064$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G064$Species <- "G064"

StdError_G073 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G073, ES = "LRRi")
StdError_G073$Ratio <- log(Ratios_NAT$G073[c(17:24)])
StdError_G073$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G073$Species <- "G073"

StdError_G087 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G087, ES = "LRRi")
StdError_G087$Ratio <- log(Ratios_NAT$G087[c(17:24)])
StdError_G087$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G087$Species <- "G087"

StdError_G152 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G152, ES = "LRRi")
StdError_G152$Ratio <- log(Ratios_NAT$G152[c(17:24)])
StdError_G152$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G152$Species <- "G152"

StdError_G153 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G153, ES = "LRRi")
StdError_G153$Ratio <- log(Ratios_NAT$G153[c(17:24)])
StdError_G153$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G153$Species <- "G153"

StdError_G155 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G155, ES = "LRRi")
StdError_G155$Ratio <- log(Ratios_NAT$G155[c(17:24)])
StdError_G155$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G155$Species <- "G155"

StdError_G167 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G167, ES = "LRRi")
StdError_G167$Ratio <- log(Ratios_NAT$G167[c(17:24)])
StdError_G167$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G167$Species <- "G167"

StdError_G168 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G168, ES = "LRRi")
StdError_G168$Ratio <- log(Ratios_NAT$G168[c(17:24)])
StdError_G168$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G168$Species <- "G168"

StdError_G169 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G169, ES = "LRRi")
StdError_G169$Ratio <- log(Ratios_NAT$G169[c(17:24)])
StdError_G169$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G169$Species <- "G169"

StdError_G170 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G170, ES = "LRRi")
StdError_G170$Ratio <- log(Ratios_NAT$G170[c(17:24)])
StdError_G170$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G170$Species <- "G170"

StdError_G175 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G175, ES = "LRRi")
StdError_G175$Ratio <- log(Ratios_NAT$G175[c(17:24)])
StdError_G175$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G175$Species <- "G175"

StdError_G176 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G176, ES = "LRRi")
StdError_G176$Ratio <- log(Ratios_NAT$G176[c(17:24)])
StdError_G176$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G176$Species <- "G176"

StdError_G177 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G177, ES = "LRRi")
StdError_G177$Ratio <- log(Ratios_NAT$G177[c(17:24)])
StdError_G177$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G177$Species <- "G177"

StdError_G180 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                              outcome = G180, ES = "LRRi")
StdError_G180$Ratio <- log(Ratios_NAT$G180[c(17:24)])
StdError_G180$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G180$Species <- "G180"

StdError_G181 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G181, ES = "LRRi")
StdError_G181$Ratio <- log(Ratios_NAT$G181[c(17:24)])
StdError_G181$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G181$Species <- "G181"

StdError_G185 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G185, ES = "LRRi")
StdError_G185$Ratio <- log(Ratios_NAT$G185[c(17:24)])
StdError_G185$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G185$Species <- "G185"

StdError_G187 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G187, ES = "LRRi")
StdError_G187$Ratio <- log(Ratios_NAT$G187[c(17:24)])
StdError_G187$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G187$Species <- "G187"

StdError_G188 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G188, ES = "LRRi")
StdError_G188$Ratio <- log(Ratios_NAT$G188[c(17:24)])
StdError_G188$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G188$Species <- "G188"

StdError_G192 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G192, ES = "LRRi")
StdError_G192$Ratio <- log(Ratios_NAT$G192[c(17:24)])
StdError_G192$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G192$Species <- "G192"

StdError_G194 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G194, ES = "LRRi")
StdError_G194$Ratio <- log(Ratios_NAT$G194[c(17:24)])
StdError_G194$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G194$Species <- "G194"

StdError_G201 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G201, ES = "LRRi")
StdError_G201$Ratio <- log(Ratios_NAT$G201[c(17:24)])
StdError_G201$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G201$Species <- "G201"

StdError_G2135 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G2135, ES = "LRRi")
StdError_G2135$Ratio <- log(Ratios_NAT$G2135[c(17:24)])
StdError_G2135$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G2135$Species <- "G2135"

StdError_G214 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G214, ES = "LRRi")
StdError_G214$Ratio <- log(Ratios_NAT$G214[c(17:24)])
StdError_G214$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G214$Species <- "G214"

StdError_G215 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G215, ES = "LRRi")
StdError_G215$Ratio <- log(Ratios_NAT$G215[c(17:24)])
StdError_G215$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G215$Species <- "G215"

StdError_G248 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G248, ES = "LRRi")
StdError_G248$Ratio <- log(Ratios_NAT$G248[c(17:24)])
StdError_G248$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G248$Species <- "G248"

StdError_G250 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G250, ES = "LRRi")
StdError_G250$Ratio <- log(Ratios_NAT$G250[c(17:24)])
StdError_G250$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G250$Species <- "G250"

StdError_G255 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G255, ES = "LRRi")
StdError_G255$Ratio <- log(Ratios_NAT$G255[c(17:24)])
StdError_G255$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G255$Species <- "G255"

StdError_G266 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G266, ES = "LRRi")
StdError_G266$Ratio <- log(Ratios_NAT$G266[c(17:24)])
StdError_G266$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G266$Species <- "G266"

StdError_G289 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G289, ES = "LRRi")
StdError_G289$Ratio <- log(Ratios_NAT$G289[c(17:24)])
StdError_G289$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G289$Species <- "G289"

StdError_G292 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G292, ES = "LRRi")
StdError_G292$Ratio <- log(Ratios_NAT$G292[c(17:24)])
StdError_G292$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G292$Species <- "G292"

StdError_G295 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G295, ES = "LRRi")
StdError_G295$Ratio <- log(Ratios_NAT$G295[c(17:24)])
StdError_G295$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G295$Species <- "G295"

StdError_G337 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G337, ES = "LRRi")
StdError_G337$Ratio <- log(Ratios_NAT$G337[c(17:24)])
StdError_G337$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G337$Species <- "G337"

StdError_G338 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(17:24)])
StdError_G338$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G338$Species <- "G338"

StdError_G339 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G339, ES = "LRRi")
StdError_G339$Ratio <- log(Ratios_NAT$G339[c(17:24)])
StdError_G339$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G339$Species <- "G339"

StdError_G345 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G345, ES = "LRRi")
StdError_G345$Ratio <- log(Ratios_NAT$G345[c(17:24)])
StdError_G345$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G345$Species <- "G345"

StdError_G347 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G347, ES = "LRRi")
StdError_G347$Ratio <- log(Ratios_NAT$G347[c(17:24)])
StdError_G347$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G347$Species <- "G347"

StdError_G362 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G362, ES = "LRRi")
StdError_G362$Ratio <- log(Ratios_NAT$G362[c(17:24)])
StdError_G362$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G362$Species <- "G362"

StdError_G428 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G428, ES = "LRRi")
StdError_G428$Ratio <- log(Ratios_NAT$G428[c(17:24)])
StdError_G428$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G428$Species <- "G428"

StdError_G433 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G433, ES = "LRRi")
StdError_G433$Ratio <- log(Ratios_NAT$G433[c(17:24)])
StdError_G433$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G433$Species <- "G433"

StdError_G448 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G448, ES = "LRRi")
StdError_G448$Ratio <- log(Ratios_NAT$G448[c(17:24)])
StdError_G448$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G448$Species <- "G448"

StdError_G459 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G459, ES = "LRRi")
StdError_G459$Ratio <- log(Ratios_NAT$G459[c(17:24)])
StdError_G459$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G459$Species <- "G459"

StdError_G464 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G464, ES = "LRRi")
StdError_G464$Ratio <- log(Ratios_NAT$G464[c(17:24)])
StdError_G464$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G464$Species <- "G464"

StdError_G498 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G498, ES = "LRRi")
StdError_G498$Ratio <- log(Ratios_NAT$G498[c(17:24)])
StdError_G498$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G498$Species <- "G498"

StdError_G560 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G560, ES = "LRRi")
StdError_G560$Ratio <- log(Ratios_NAT$G560[c(17:24)])
StdError_G560$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G560$Species <- "G560"

StdError_G598 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G598, ES = "LRRi")
StdError_G598$Ratio <- log(Ratios_NAT$G598[c(17:24)])
StdError_G598$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G598$Species <- "G598"

StdError_G602 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G602, ES = "LRRi")
StdError_G602$Ratio <- log(Ratios_NAT$G602[c(17:24)])
StdError_G602$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G602$Species <- "G602"

StdError_G628 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G628, ES = "LRRi")
StdError_G628$Ratio <- log(Ratios_NAT$G628[c(17:24)])
StdError_G628$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G628$Species <- "G628"

StdError_G699 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(17:24)])
StdError_G699$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G699$Species <- "G699"

StdError_G702 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G702, ES = "LRRi")
StdError_G702$Ratio <- log(Ratios_NAT$G702[c(17:24)])
StdError_G702$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G702$Species <- "G702"

StdError_G703 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G703, ES = "LRRi")
StdError_G703$Ratio <- log(Ratios_NAT$G703[c(17:24)])
StdError_G703$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G703$Species <- "G703"

StdError_G705 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G705, ES = "LRRi")
StdError_G705$Ratio <- log(Ratios_NAT$G705[c(17:24)])
StdError_G705$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G705$Species <- "G705"

StdError_G710 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G710, ES = "LRRi")
StdError_G710$Ratio <- log(Ratios_NAT$G710[c(17:24)])
StdError_G710$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G710$Species <- "G710"

StdError_G746 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G746, ES = "LRRi")
StdError_G746$Ratio <- log(Ratios_NAT$G746[c(17:24)])
StdError_G746$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G746$Species <- "G746"

StdError_G787 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G787, ES = "LRRi")
StdError_G787$Ratio <- log(Ratios_NAT$G787[c(17:24)])
StdError_G787$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G787$Species <- "G787"

StdError_G809 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G809, ES = "LRRi")
StdError_G809$Ratio <- log(Ratios_NAT$G809[c(17:24)])
StdError_G809$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G809$Species <- "G809"

StdError_G825 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G825, ES = "LRRi")
StdError_G825$Ratio <- log(Ratios_NAT$G825[c(17:24)])
StdError_G825$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G825$Species <- "G825"

StdError_G840 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G840, ES = "LRRi")
StdError_G840$Ratio <- log(Ratios_NAT$G840[c(17:24)])
StdError_G840$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G840$Species <- "G840"

StdError_G849 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G849, ES = "LRRi")
StdError_G849$Ratio <- log(Ratios_NAT$G849[c(17:24)])
StdError_G849$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G849$Species <- "G849"

StdError_G857 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G857, ES = "LRRi")
StdError_G857$Ratio <- log(Ratios_NAT$G857[c(17:24)])
StdError_G857$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G857$Species <- "G857"

StdError_G908 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G908, ES = "LRRi")
StdError_G908$Ratio <- log(Ratios_NAT$G908[c(17:24)])
StdError_G908$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G908$Species <- "G908"

StdError_G914 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G914, ES = "LRRi")
StdError_G914$Ratio <- log(Ratios_NAT$G914[c(17:24)])
StdError_G914$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G914$Species <- "G914"

StdError_G916 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G916, ES = "LRRi")
StdError_G916$Ratio <- log(Ratios_NAT$G916[c(17:24)])
StdError_G916$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G916$Species <- "G916"

StdError_G958 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = G958, ES = "LRRi")
StdError_G958$Ratio <- log(Ratios_NAT$G958[c(17:24)])
StdError_G958$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_G958$Species <- "G958"

StdError_P001 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = P001, ES = "LRRi")
StdError_P001$Ratio <- log(Ratios_NAT$P001[c(17:24)])
StdError_P001$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_P001$Species <- "P001"

StdError_P002 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = P002, ES = "LRRi")
StdError_P002$Ratio <- log(Ratios_NAT$P002[c(17:24)])
StdError_P002$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_P002$Species <- "P002"

StdError_P009 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = P009, ES = "LRRi")
StdError_P009$Ratio <- log(Ratios_NAT$P009[c(17:24)])
StdError_P009$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_P009$Species <- "P009"

StdError_S003 <- batch_calc_ES(dat = StdError_DataB, grouping = Sites, condition = Season, 
                               outcome = S003, ES = "LRRi")
StdError_S003$Ratio <- log(Ratios_NAT$S003[c(17:24)])
StdError_S003$Habitat <- c(rep("Posidonia meadow",4),rep("Hard-bottom",4))
StdError_S003$Species <- "S003"


StdError_NAT<- rbind(StdError_B005,StdError_B019,StdError_B023,StdError_B033,StdError_B045,StdError_B046,StdError_B049,StdError_B055,StdError_B060,
                     StdError_B065,StdError_B066,StdError_B068,StdError_B088,StdError_B125,StdError_B138,StdError_B139,StdError_B142,StdError_B143,
                     StdError_B147,StdError_B148,StdError_B150,StdError_B154,StdError_B160,StdError_B161,StdError_B176,StdError_B188,StdError_B247,
                     StdError_B260,StdError_B270,StdError_B317,StdError_B325,StdError_B332,StdError_B504,StdError_G010,StdError_G017,StdError_G026,
                     StdError_G064,StdError_G073,StdError_G087,StdError_G152,StdError_G153,StdError_G155,StdError_G167,StdError_G168,StdError_G169,
                     StdError_G170,StdError_G175,StdError_G176,StdError_G177,StdError_G180,StdError_G181,StdError_G185,StdError_G187,StdError_G188,
                     StdError_G192,StdError_G194,StdError_G201,StdError_G2135,StdError_G214,StdError_G215,StdError_G248,StdError_G250,StdError_G255,
                     StdError_G266,StdError_G289,StdError_G292,StdError_G295,StdError_G337,StdError_G338,StdError_G339,StdError_G345,StdError_G347,
                     StdError_G362,StdError_G428,StdError_G433,StdError_G448,StdError_G459,StdError_G464,StdError_G498,StdError_G560,StdError_G598,
                     StdError_G602,StdError_G628,StdError_G699,StdError_G702,StdError_G703,StdError_G705,StdError_G710,StdError_G746,StdError_G787,
                     StdError_G809,StdError_G825,StdError_G840,StdError_G849,StdError_G857,StdError_G908,StdError_G914,StdError_G916,StdError_G958,
                     StdError_P001,StdError_P002,StdError_P009,StdError_S003)

StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
colnames(StdError_NAT)[1] <- "Sites"
CyprusN_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9,10)],
                                   StdError_NAT[c(1,4,7,8,9,10)])
