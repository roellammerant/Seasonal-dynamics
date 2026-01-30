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
####        Load Lessepsian dataset and summarise       ####
#                                                          #
##%######################################################%##

LessepsianHard <- read.csv("DATA/LessepsianHard.csv", 
                           na = "NA")

SpLessepsianHard <- read_excel("output_data/LessepsianHard_InvasiveStatus.xlsx", 
                               na = "NA")

### Split data up
LessepsianHardCategories <- LessepsianHard[c(2:4)]
LessepsianHardSpecies <- LessepsianHard[-c(1:4)]

### Create categories to merch replicates
LessepsianHardCategories$Merched_Category <- factor(paste(LessepsianHardCategories$StationCode, 
                                                          LessepsianHardCategories$eventSeason,
                                                          sep = "_"))

ReplicateNumber <- as.data.frame(table(LessepsianHardCategories$Merched_Category))

LessepsianHardSpecies$Merched_Category <- LessepsianHardCategories$Merched_Category

### Create Community level abundance columns
LessepsianHTranspose <- t(LessepsianHardSpecies)
LessepsianHTranspose <- as.data.frame(LessepsianHTranspose)
LessepsianHTranspose <- LessepsianHTranspose[-c(138),]
LessepsianHTranspose$Invasive <- SpLessepsianHard$...2
LessepsianHNIS  <- LessepsianHTranspose[which(LessepsianHTranspose$Invasive  =='1'),]
LessepsianHNAT  <- LessepsianHTranspose[which(LessepsianHTranspose$Invasive  =='0'),]
LessepsianHNIS <- LessepsianHNIS %>% 
  mutate_if(is.character, as.numeric)
LessepsianHNAT <- LessepsianHNAT %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
LessepsianHNIS <- t(LessepsianHNIS)
LessepsianHNIS <- as.data.frame(LessepsianHNIS)
LessepsianHNIS <- LessepsianHNIS[-c(24),]
LessepsianHNIS$Merched_Category <- LessepsianHardCategories$Merched_Category
Mean_NIS <- aggregate(LessepsianHNIS[c(1:67)], 
                      list(LessepsianHNIS$Merched_Category), FUN=mean) 

LessepsianHNAT <- t(LessepsianHNAT)
LessepsianHNAT <- as.data.frame(LessepsianHNAT)
LessepsianHNAT <- LessepsianHNAT[-c(24),]
LessepsianHNAT$Merched_Category <- LessepsianHardCategories$Merched_Category
Mean_NAT <- aggregate(LessepsianHNAT[c(1:66)], 
                      list(LessepsianHNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$Ratio_Ahk12 <-Ratios_NIS$Ahk12_Autumn/Ratios_NIS$Ahk12_Spring
Ratios_NIS$Ratio_Ahk20 <-Ratios_NIS$Ahk20_Autumn/Ratios_NIS$Ahk20_Spring
Ratios_NIS$Ratio_Ash12 <-Ratios_NIS$Ash12_Autumn/Ratios_NIS$Ash12_Spring
Ratios_NIS$Ratio_Ash25 <-Ratios_NIS$Ash25_Autumn/Ratios_NIS$Ash25_Spring
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio_Ahk12 <-Ratios_NAT$Ahk12_Autumn/Ratios_NAT$Ahk12_Spring
Ratios_NAT$Ratio_Ahk20 <-Ratios_NAT$Ahk20_Autumn/Ratios_NAT$Ahk20_Spring
Ratios_NAT$Ratio_Ash12 <-Ratios_NAT$Ash12_Autumn/Ratios_NAT$Ash12_Spring
Ratios_NAT$Ratio_Ash25 <-Ratios_NAT$Ash25_Autumn/Ratios_NAT$Ash25_Spring
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
LessepsianHNIS$StationCode <- LessepsianHardCategories$StationCode
LessepsianHNIS$Season <-  LessepsianHardCategories$eventSeason
LessepsianHNIS[LessepsianHNIS == 0] <- 0.1

StdError_B007 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B007, ES = "LRRi")
StdError_B007$Ratio <- log(Ratios_NIS$B007[c(9:12)])
StdError_B007$Species <- "B007"

StdError_B009 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B009, ES = "LRRi")
StdError_B009$Ratio <- log(Ratios_NIS$B009[c(9:12)])
StdError_B009$Species <- "B009"

StdError_B029 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B029, ES = "LRRi")
StdError_B029$Ratio <- log(Ratios_NIS$B029[c(9:12)])
StdError_B029$Species <- "B029"

StdError_B030 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B030, ES = "LRRi")
StdError_B030$Ratio <- log(Ratios_NIS$B030[c(9:12)])
StdError_B030$Species <- "B030"

StdError_B031 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B031, ES = "LRRi")
StdError_B031$Ratio <- log(Ratios_NIS$B031[c(9:12)])
StdError_B031$Species <- "B031"

StdError_B078 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(9:12)])
StdError_B078$Species <- "B078"

StdError_B094 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B094, ES = "LRRi")
StdError_B094$Ratio <- log(Ratios_NIS$B094[c(9:12)])
StdError_B094$Species <- "B094"

StdError_B124 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(9:12)])
StdError_B124$Species <- "B124"

StdError_B132 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B132, ES = "LRRi")
StdError_B132$Ratio <- log(Ratios_NIS$B132[c(9:12)])
StdError_B132$Species <- "B132"

StdError_B157 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B157, ES = "LRRi")
StdError_B157$Ratio <- log(Ratios_NIS$B157[c(9:12)])
StdError_B157$Species <- "B157"

StdError_B163 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B163, ES = "LRRi")
StdError_B163$Ratio <- log(Ratios_NIS$B163[c(9:12)])
StdError_B163$Species <- "B163"

StdError_B191 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B191, ES = "LRRi")
StdError_B191$Ratio <- log(Ratios_NIS$B191[c(9:12)])
StdError_B191$Species <- "B191"

StdError_B192 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B192, ES = "LRRi")
StdError_B192$Ratio <- log(Ratios_NIS$B192[c(9:12)])
StdError_B192$Species <- "B192"

StdError_B203 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B203, ES = "LRRi")
StdError_B203$Ratio <- log(Ratios_NIS$B203[c(9:12)])
StdError_B203$Species <- "B203"

StdError_B208 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B208, ES = "LRRi")
StdError_B208$Ratio <- log(Ratios_NIS$B208[c(9:12)])
StdError_B208$Species <- "B208"

StdError_B212 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B212, ES = "LRRi")
StdError_B212$Ratio <- log(Ratios_NIS$B212[c(9:12)])
StdError_B212$Species <- "B212"

StdError_B213 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B213, ES = "LRRi")
StdError_B213$Ratio <- log(Ratios_NIS$B213[c(9:12)])
StdError_B213$Species <- "B213"

StdError_B215 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B215, ES = "LRRi")
StdError_B215$Ratio <- log(Ratios_NIS$B215[c(9:12)])
StdError_B215$Species <- "B215"

StdError_B216 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B216, ES = "LRRi")
StdError_B216$Ratio <- log(Ratios_NIS$B216[c(9:12)])
StdError_B216$Species <- "B216"

StdError_B225 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B225, ES = "LRRi")
StdError_B225$Ratio <- log(Ratios_NIS$B225[c(9:12)])
StdError_B225$Species <- "B225"

StdError_B227 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(9:12)])
StdError_B227$Species <- "B227"

StdError_B232 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B232, ES = "LRRi")
StdError_B232$Ratio <- log(Ratios_NIS$B232[c(9:12)])
StdError_B232$Species <- "B232"

StdError_B481 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B481, ES = "LRRi")
StdError_B481$Ratio <- log(Ratios_NIS$B481[c(9:12)])
StdError_B481$Species <- "B481"

StdError_B482 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = B482, ES = "LRRi")
StdError_B482$Ratio <- log(Ratios_NIS$B482[c(9:12)])
StdError_B482$Species <- "B482"

StdError_G002 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G002, ES = "LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(9:12)])
StdError_G002$Species <- "G002"

StdError_G004 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G004, ES = "LRRi")
StdError_G004$Ratio <- log(Ratios_NIS$G004[c(9:12)])
StdError_G004$Species <- "G004"

StdError_G007 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(9:12)])
StdError_G007$Species <- "G007"

StdError_G008 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G008, ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(9:12)])
StdError_G008$Species <- "G008"

StdError_G009 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G009, ES = "LRRi")
StdError_G009$Ratio <- log(Ratios_NIS$G009[c(9:12)])
StdError_G009$Species <- "G009"

StdError_G018 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G018, ES = "LRRi")
StdError_G018$Ratio <- log(Ratios_NIS$G018[c(9:12)])
StdError_G018$Species <- "G018"

StdError_G085 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G085, ES = "LRRi")
StdError_G085$Ratio <- log(Ratios_NIS$G085[c(9:12)])
StdError_G085$Species <- "G085"

StdError_G089 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G089, ES = "LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(9:12)])
StdError_G089$Species <- "G089"

StdError_G102 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G102, ES = "LRRi")
StdError_G102$Ratio <- log(Ratios_NIS$G102[c(9:12)])
StdError_G102$Species <- "G102"

StdError_G104 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G104, ES = "LRRi")
StdError_G104$Ratio <- log(Ratios_NIS$G104[c(9:12)])
StdError_G104$Species <- "G104"

StdError_G128 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G128, ES = "LRRi")
StdError_G128$Ratio <- log(Ratios_NIS$G128[c(9:12)])
StdError_G128$Species <- "G128"

StdError_G133 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G133, ES = "LRRi")
StdError_G133$Ratio <- log(Ratios_NIS$G133[c(9:12)])
StdError_G133$Species <- "G133"

StdError_G149 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G149, ES = "LRRi")
StdError_G149$Ratio <- log(Ratios_NIS$G149[c(9:12)])
StdError_G149$Species <- "G149"

StdError_G206 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G206, ES = "LRRi")
StdError_G206$Ratio <- log(Ratios_NIS$G206[c(9:12)])
StdError_G206$Species <- "G206"

StdError_G220 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G220, ES = "LRRi")
StdError_G220$Ratio <- log(Ratios_NIS$G220[c(9:12)])
StdError_G220$Species <- "G220"

StdError_G246 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G246, ES = "LRRi")
StdError_G246$Ratio <- log(Ratios_NIS$G246[c(9:12)])
StdError_G246$Species <- "G246"

StdError_G247 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G247, ES = "LRRi")
StdError_G247$Ratio <- log(Ratios_NIS$G247[c(9:12)])
StdError_G247$Species <- "G247"

StdError_G254 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G254, ES = "LRRi")
StdError_G254$Ratio <- log(Ratios_NIS$G254[c(9:12)])
StdError_G254$Species <- "G254"

StdError_G256 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G256, ES = "LRRi")
StdError_G256$Ratio <- log(Ratios_NIS$G256[c(9:12)])
StdError_G256$Species <- "G256"

StdError_G352 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G352, ES = "LRRi")
StdError_G352$Ratio <- log(Ratios_NIS$G352[c(9:12)])
StdError_G352$Species <- "G352"

StdError_G385 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G385, ES = "LRRi")
StdError_G385$Ratio <- log(Ratios_NIS$G385[c(9:12)])
StdError_G385$Species <- "G385"

StdError_G387 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G387, ES = "LRRi")
StdError_G387$Ratio <- log(Ratios_NIS$G387[c(9:12)])
StdError_G387$Species <- "G387"

StdError_G390 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G390, ES = "LRRi")
StdError_G390$Ratio <- log(Ratios_NIS$G390[c(9:12)])
StdError_G390$Species <- "G390"

StdError_G395 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G395, ES = "LRRi")
StdError_G395$Ratio <- log(Ratios_NIS$G395[c(9:12)])
StdError_G395$Species <- "G395"

StdError_G400 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G400, ES = "LRRi")
StdError_G400$Ratio <- log(Ratios_NIS$G400[c(9:12)])
StdError_G400$Species <- "G400"

StdError_G520 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G520, ES = "LRRi")
StdError_G520$Ratio <- log(Ratios_NIS$G520[c(9:12)])
StdError_G520$Species <- "G520"

StdError_G543 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G543, ES = "LRRi")
StdError_G543$Ratio <- log(Ratios_NIS$G543[c(9:12)])
StdError_G543$Species <- "G543"

StdError_G550 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G550, ES = "LRRi")
StdError_G550$Ratio <- log(Ratios_NIS$G550[c(9:12)])
StdError_G550$Species <- "G550"

StdError_G795 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G795, ES = "LRRi")
StdError_G795$Ratio <- log(Ratios_NIS$G795[c(9:12)])
StdError_G795$Species <- "G795"

StdError_G814 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G814, ES = "LRRi")
StdError_G814$Ratio <- log(Ratios_NIS$G814[c(9:12)])
StdError_G814$Species <- "G814"

StdError_G815 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G815, ES = "LRRi")
StdError_G815$Ratio <- log(Ratios_NIS$G815[c(9:12)])
StdError_G815$Species <- "G815"

StdError_G816 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G816, ES = "LRRi")
StdError_G816$Ratio <- log(Ratios_NIS$G816[c(9:12)])
StdError_G816$Species <- "G816"

StdError_G820 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G820, ES = "LRRi")
StdError_G820$Ratio <- log(Ratios_NIS$G820[c(9:12)])
StdError_G820$Species <- "G820"

StdError_G822 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G822, ES = "LRRi")
StdError_G822$Ratio <- log(Ratios_NIS$G822[c(9:12)])
StdError_G822$Species <- "G822"

StdError_G823 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G823, ES = "LRRi")
StdError_G823$Ratio <- log(Ratios_NIS$G823[c(9:12)])
StdError_G823$Species <- "G823"

StdError_G858 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G858, ES = "LRRi")
StdError_G858$Ratio <- log(Ratios_NIS$G858[c(9:12)])
StdError_G858$Species <- "G858"

StdError_G860 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G860, ES = "LRRi")
StdError_G860$Ratio <- log(Ratios_NIS$G860[c(9:12)])
StdError_G860$Species <- "G860"

StdError_G861 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G861, ES = "LRRi")
StdError_G861$Ratio <- log(Ratios_NIS$G861[c(9:12)])
StdError_G861$Species <- "G861"

StdError_G862 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G862, ES = "LRRi")
StdError_G862$Ratio <- log(Ratios_NIS$G862[c(9:12)])
StdError_G862$Species <- "G862"

StdError_G883 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G883, ES = "LRRi")
StdError_G883$Ratio <- log(Ratios_NIS$G883[c(9:12)])
StdError_G883$Species <- "G883"

StdError_G884 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G884, ES = "LRRi")
StdError_G884$Ratio <- log(Ratios_NIS$G884[c(9:12)])
StdError_G884$Species <- "G884"

StdError_G887 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = G887, ES = "LRRi")
StdError_G887$Ratio <- log(Ratios_NIS$G887[c(9:12)])
StdError_G887$Species <- "G887"

StdError_P002 <- batch_calc_ES(dat = LessepsianHNIS, grouping = StationCode, condition = Season, 
                               outcome = P002, ES = "LRRi")
StdError_P002$Ratio <- log(Ratios_NIS$P002[c(9:12)])
StdError_P002$Species <- "P002"


StdError_NIS<- rbind(StdError_B007,StdError_B009,StdError_B029,StdError_B030,StdError_B031,StdError_B078,StdError_B094,StdError_B124,
                     StdError_B132,StdError_B157,StdError_B163,StdError_B191,StdError_B192,StdError_B203,StdError_B208,StdError_B212,
                     StdError_B213,StdError_B215,StdError_B216,StdError_B225,StdError_B227,StdError_B232,StdError_B482,StdError_B482,
                     StdError_G002,StdError_G004,StdError_G007,StdError_G008,StdError_G009,StdError_G018,StdError_G085,StdError_G089,
                     StdError_G102,StdError_G104,StdError_G128,StdError_G133,StdError_G149,StdError_G206,StdError_G220,StdError_G246,
                     StdError_G247,StdError_G254,StdError_G256,StdError_G352,StdError_G385,StdError_G387,StdError_G390,StdError_G395,
                     StdError_G400,StdError_G520,StdError_G543,StdError_G550,StdError_G795,StdError_G814,StdError_G815,StdError_G816,
                     StdError_G820,StdError_G822,StdError_G823,StdError_G858,StdError_G860,StdError_G861,StdError_G862,StdError_G883,
                     StdError_G884,StdError_G887,StdError_P002)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NAT species ####
LessepsianHNAT$StationCode <- LessepsianHardCategories$StationCode
LessepsianHNAT$Season <-  LessepsianHardCategories$eventSeason
LessepsianHNAT[LessepsianHNAT == 0] <- 0.1

StdError_B002 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B002, ES = "LRRi")
StdError_B002$Ratio <- log(Ratios_NAT$B002[c(9:12)])
StdError_B002$Species <- "B002"

StdError_B004 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B004, ES = "LRRi")
StdError_B004$Ratio <- log(Ratios_NAT$B004[c(9:12)])
StdError_B004$Species <- "B004"

StdError_B005 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(9:12)])
StdError_B005$Species <- "B005"

StdError_B016 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B016, ES = "LRRi")
StdError_B016$Ratio <- log(Ratios_NAT$B016[c(9:12)])
StdError_B016$Species <- "B016"

StdError_B019 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(9:12)])
StdError_B019$Species <- "B019"

StdError_B028 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B028, ES = "LRRi")
StdError_B028$Ratio <- log(Ratios_NAT$B028[c(9:12)])
StdError_B028$Species <- "B028"

StdError_B033 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(9:12)])
StdError_B033$Species <- "B033"

StdError_B049 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B049, ES = "LRRi")
StdError_B049$Ratio <- log(Ratios_NAT$B049[c(9:12)])
StdError_B049$Species <- "B049"

StdError_B055 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(9:12)])
StdError_B055$Species <- "B055"

StdError_B060 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B060, ES = "LRRi")
StdError_B060$Ratio <- log(Ratios_NAT$B060[c(9:12)])
StdError_B060$Species <- "B060"

StdError_B061 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B061, ES = "LRRi")
StdError_B061$Ratio <- log(Ratios_NAT$B061[c(9:12)])
StdError_B061$Species <- "B061"

StdError_B064 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B064, ES = "LRRi")
StdError_B064$Ratio <- log(Ratios_NAT$B064[c(9:12)])
StdError_B064$Species <- "B064"

StdError_B065 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B065, ES = "LRRi")
StdError_B065$Ratio <- log(Ratios_NAT$B065[c(9:12)])
StdError_B065$Species <- "B065"

StdError_B066 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B066, ES = "LRRi")
StdError_B066$Ratio <- log(Ratios_NAT$B066[c(9:12)])
StdError_B066$Species <- "B066"

StdError_B074 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B074, ES = "LRRi")
StdError_B074$Ratio <- log(Ratios_NAT$B074[c(9:12)])
StdError_B074$Species <- "B074"

StdError_B088 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B088, ES = "LRRi")
StdError_B088$Ratio <- log(Ratios_NAT$B088[c(9:12)])
StdError_B088$Species <- "B088"

StdError_B125 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B125, ES = "LRRi")
StdError_B125$Ratio <- log(Ratios_NAT$B125[c(9:12)])
StdError_B125$Species <- "B125"

StdError_B128 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B128, ES = "LRRi")
StdError_B128$Ratio <- log(Ratios_NAT$B128[c(9:12)])
StdError_B128$Species <- "B128"

StdError_B139 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B139, ES = "LRRi")
StdError_B139$Ratio <- log(Ratios_NAT$B139[c(9:12)])
StdError_B139$Species <- "B139"

StdError_B143 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B143, ES = "LRRi")
StdError_B143$Ratio <- log(Ratios_NAT$B143[c(9:12)])
StdError_B143$Species <- "B143"

StdError_B148 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B148, ES = "LRRi")
StdError_B148$Ratio <- log(Ratios_NAT$B148[c(9:12)])
StdError_B148$Species <- "B148"

StdError_B150 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B150, ES = "LRRi")
StdError_B150$Ratio <- log(Ratios_NAT$B150[c(9:12)])
StdError_B150$Species <- "B150"

StdError_B154 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B154, ES = "LRRi")
StdError_B154$Ratio <- log(Ratios_NAT$B154[c(9:12)])
StdError_B154$Species <- "B154"

StdError_B172 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B172, ES = "LRRi")
StdError_B172$Ratio <- log(Ratios_NAT$B172[c(9:12)])
StdError_B172$Species <- "B172"

StdError_B266 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B266, ES = "LRRi")
StdError_B266$Ratio <- log(Ratios_NAT$B266[c(9:12)])
StdError_B266$Species <- "B266"

StdError_B321 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B321, ES = "LRRi")
StdError_B321$Ratio <- log(Ratios_NAT$B321[c(9:12)])
StdError_B321$Species <- "B321"

StdError_B345 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = B345, ES = "LRRi")
StdError_B345$Ratio <- log(Ratios_NAT$B345[c(9:12)])
StdError_B345$Species <- "B345"

StdError_BR001 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = BR001, ES = "LRRi")
StdError_BR001$Ratio <- log(Ratios_NAT$BR001[c(9:12)])
StdError_BR001$Species <- "BR001"

StdError_BR002 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = BR002, ES = "LRRi")
StdError_BR002$Ratio <- log(Ratios_NAT$BR002[c(9:12)])
StdError_BR002$Species <- "BR002"

StdError_G017 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(9:12)])
StdError_G017$Species <- "G017"

StdError_G027 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(9:12)])
StdError_G027$Species <- "G027"

StdError_G029 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G029, ES = "LRRi")
StdError_G029$Ratio <- log(Ratios_NAT$G029[c(9:12)])
StdError_G029$Species <- "G029"

StdError_G050 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G050, ES = "LRRi")
StdError_G050$Ratio <- log(Ratios_NAT$G050[c(9:12)])
StdError_G050$Species <- "G050"

StdError_G062 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G062, ES = "LRRi")
StdError_G062$Ratio <- log(Ratios_NAT$G062[c(9:12)])
StdError_G062$Species <- "G062"

StdError_G066 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G066, ES = "LRRi")
StdError_G066$Ratio <- log(Ratios_NAT$G066[c(9:12)])
StdError_G066$Species <- "G066"

StdError_G097 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G097, ES = "LRRi")
StdError_G097$Ratio <- log(Ratios_NAT$G097[c(9:12)])
StdError_G097$Species <- "G097"

StdError_G113 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G113, ES = "LRRi")
StdError_G113$Ratio <- log(Ratios_NAT$G113[c(9:12)])
StdError_G113$Species <- "G113"

StdError_G134 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G134, ES = "LRRi")
StdError_G134$Ratio <- log(Ratios_NAT$G134[c(9:12)])
StdError_G134$Species <- "G134"

StdError_G139 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G139, ES = "LRRi")
StdError_G139$Ratio <- log(Ratios_NAT$G139[c(9:12)])
StdError_G139$Species <- "G139"

StdError_G145 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G145, ES = "LRRi")
StdError_G145$Ratio <- log(Ratios_NAT$G145[c(9:12)])
StdError_G145$Species <- "G145"

StdError_G146 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G146, ES = "LRRi")
StdError_G146$Ratio <- log(Ratios_NAT$G146[c(9:12)])
StdError_G146$Species <- "G146"

StdError_G147 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G147, ES = "LRRi")
StdError_G147$Ratio <- log(Ratios_NAT$G147[c(9:12)])
StdError_G147$Species <- "G147"

StdError_G151 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G151, ES = "LRRi")
StdError_G151$Ratio <- log(Ratios_NAT$G151[c(9:12)])
StdError_G151$Species <- "G151"

StdError_G153 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G153, ES = "LRRi")
StdError_G153$Ratio <- log(Ratios_NAT$G153[c(9:12)])
StdError_G153$Species <- "G153"

StdError_G175 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G175, ES = "LRRi")
StdError_G175$Ratio <- log(Ratios_NAT$G175[c(9:12)])
StdError_G175$Species <- "G175"

StdError_G201 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G201, ES = "LRRi")
StdError_G201$Ratio <- log(Ratios_NAT$G201[c(9:12)])
StdError_G201$Species <- "G201"

StdError_G205 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G205, ES = "LRRi")
StdError_G205$Ratio <- log(Ratios_NAT$G205[c(9:12)])
StdError_G205$Species <- "G205"

StdError_G214 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G214, ES = "LRRi")
StdError_G214$Ratio <- log(Ratios_NAT$G214[c(9:12)])
StdError_G214$Species <- "G214"

StdError_G248 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G248, ES = "LRRi")
StdError_G248$Ratio <- log(Ratios_NAT$G248[c(9:12)])
StdError_G248$Species <- "G248"

StdError_G292 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G292, ES = "LRRi")
StdError_G292$Ratio <- log(Ratios_NAT$G292[c(9:12)])
StdError_G292$Species <- "G292"

StdError_G295 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G295, ES = "LRRi")
StdError_G295$Ratio <- log(Ratios_NAT$G295[c(9:12)])
StdError_G295$Species <- "G295"

StdError_G338 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(9:12)])
StdError_G338$Species <- "G338"

StdError_G354 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G354, ES = "LRRi")
StdError_G354$Ratio <- log(Ratios_NAT$G354[c(9:12)])
StdError_G354$Species <- "G354"

StdError_G362 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G362, ES = "LRRi")
StdError_G362$Ratio <- log(Ratios_NAT$G362[c(9:12)])
StdError_G362$Species <- "G362"

StdError_G401 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G401, ES = "LRRi")
StdError_G401$Ratio <- log(Ratios_NAT$G401[c(9:12)])
StdError_G401$Species <- "G401"

StdError_G466 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G466, ES = "LRRi")
StdError_G466$Ratio <- log(Ratios_NAT$G466[c(9:12)])
StdError_G466$Species <- "G466"

StdError_G501 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G501, ES = "LRRi")
StdError_G501$Ratio <- log(Ratios_NAT$G501[c(9:12)])
StdError_G501$Species <- "G501"

StdError_G600 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G600, ES = "LRRi")
StdError_G600$Ratio <- log(Ratios_NAT$G600[c(9:12)])
StdError_G600$Species <- "G600"

StdError_G615 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G615, ES = "LRRi")
StdError_G615$Ratio <- log(Ratios_NAT$G615[c(9:12)])
StdError_G615$Species <- "G615"

StdError_G617 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G617, ES = "LRRi")
StdError_G617$Ratio <- log(Ratios_NAT$G617[c(9:12)])
StdError_G617$Species <- "G617"

StdError_G702 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G702, ES = "LRRi")
StdError_G702$Ratio <- log(Ratios_NAT$G702[c(9:12)])
StdError_G702$Species <- "G702"

StdError_G703 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G703, ES = "LRRi")
StdError_G703 $Ratio <- log(Ratios_NAT$G703[c(9:12)])
StdError_G703$Species <- "G703"

StdError_G809 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G809, ES = "LRRi")
StdError_G809$Ratio <- log(Ratios_NAT$G809[c(9:12)])
StdError_G809$Species <- "G809"

StdError_G818 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G818, ES = "LRRi")
StdError_G818$Ratio <- log(Ratios_NAT$G818[c(9:12)])
StdError_G818$Species <- "G818"

StdError_G824 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G824, ES = "LRRi")
StdError_G824$Ratio <- log(Ratios_NAT$G824[c(9:12)])
StdError_G824$Species <- "G824"

StdError_G831 <- batch_calc_ES(dat = LessepsianHNAT, grouping = StationCode, condition = Season, 
                               outcome = G831, ES = "LRRi")
StdError_G831$Ratio <- log(Ratios_NAT$G831[c(9:12)])
StdError_G831$Species <- "G831"


StdError_NAT<- rbind(StdError_B002,StdError_B004,StdError_B005,StdError_B016,StdError_B019,StdError_B028,StdError_B033,StdError_B049,
                     StdError_B055,StdError_B060,StdError_B061,StdError_B064,StdError_B065,StdError_B066,StdError_B074,StdError_B088,
                     StdError_B125,StdError_B128,StdError_B139,StdError_B143,StdError_B148,StdError_B150,StdError_B154,StdError_B172,
                     StdError_B266,StdError_B321,StdError_B345,StdError_BR001,StdError_BR002,StdError_G017,StdError_G027,StdError_G029,
                     StdError_G050,StdError_G062,StdError_G097,StdError_G113,StdError_G134,StdError_G139,StdError_G145,StdError_G146,
                     StdError_G147,StdError_G151,StdError_G153,StdError_G175,StdError_G201,StdError_G205,StdError_G214,StdError_G248,
                     StdError_G292,StdError_G295,StdError_G338,StdError_G354,StdError_G362,StdError_G401,StdError_G466,StdError_G501,
                     StdError_G600,StdError_G615,StdError_G617,StdError_G702,StdError_G703,StdError_G809,StdError_G818,StdError_G824,
                     StdError_G831)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
LessepsianH_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9)],
                                       StdError_NAT[c(1,4,7,8,9)])
