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
####          Load Cyprus data and summarise            ####
#                                                          #
##%######################################################%##

CyprusAutumn <- read.csv("DATA/Cyprus_Autumn.csv", 
                         na = "NA")
CyprusSpring <- read.csv("DATA/Cyprus_Spring.csv", 
                         na = "NA")

Cyprus <- merge(CyprusSpring, CyprusAutumn,  by.x = "Code", by.y = "Code", all.x = TRUE)

### Split data up
CyprusCategories <- as.data.frame(colnames(Cyprus))
CyprusCategories <- as.data.frame(CyprusCategories[c(8:67, 74:133),])
CyprusCategories$Season <- c(rep("Spring",60), rep("Autumn",60))
colnames(CyprusCategories)[1] <- c("Sites_Replicate")
CyprusCategories$Sites <- c(rep("APos05",3), rep("APos10",3), rep("APos15",3), rep("APos20",3), rep("APos30",3),
                            rep("GPos05",3), rep("GPos10",3),rep("GPos15",3), rep("GPos20",3), rep("GPos30",3), 
                            rep("AR05",3), rep("AR10",3), rep("AR15",3), rep("AR20",3), rep("AR30",3),
                            rep("GR05",3), rep("GR10",3), rep("GR15",3),rep("GR20",3), rep("GR30",3),
                            rep("APos05",3), rep("APos10",3), rep("APos15",3), rep("APos20",3), rep("APos30",3),
                            rep("GPos05",3), rep("GPos10",3),rep("GPos15",3), rep("GPos20",3), rep("GPos30",3), 
                            rep("AR05",3), rep("AR10",3), rep("AR15",3), rep("AR20",3), rep("AR30",3),
                            rep("GR05",3), rep("GR10",3), rep("GR15",3),rep("GR20",3), rep("GR30",3))

CyprusSpecies <- Cyprus[-c(2:6, 68:73)]

### Create categories to merch replicates
CyprusCategories$Merched_Category <- factor(paste(CyprusCategories$Sites, 
                                                  CyprusCategories$Season,
                                                  sep = "_"))

### Create Community level abundance columns
CyprusNIS  <- CyprusSpecies[which(CyprusSpecies$Alien.x  =='Yes'),]
CyprusNAT  <- CyprusSpecies[which(CyprusSpecies$Alien.x  =='No'),]

### Mean values per Year*Season 
CyprusNIS <- t(CyprusNIS)
CyprusNIS <- as.data.frame(CyprusNIS)
colnames(CyprusNIS) <- CyprusNIS[1,]
CyprusNIS <- CyprusNIS[-c(1,2),]
CyprusNIS <- CyprusNIS %>% 
  mutate_if(is.character, as.numeric)
CyprusNIS$Merched_Category <- CyprusCategories$Merched_Category
Mean_NIS <- aggregate(CyprusNIS[c(1:32)], 
                      list(CyprusNIS$Merched_Category), FUN=mean) 

CyprusNAT <- t(CyprusNAT)
CyprusNAT <- as.data.frame(CyprusNAT)
colnames(CyprusNAT) <- CyprusNAT[1,]
CyprusNAT <- CyprusNAT[-c(1,2),]
CyprusNAT <- CyprusNAT %>% 
  mutate_if(is.character, as.numeric)
CyprusNAT$Merched_Category <- CyprusCategories$Merched_Category
Mean_NAT <- aggregate(CyprusNAT[c(1:324)], 
                      list(CyprusNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$RatioAPos05 <-Ratios_NIS$APos05_Autumn/Ratios_NIS$APos05_Spring
Ratios_NIS$RatioAPos10 <-Ratios_NIS$APos10_Autumn/Ratios_NIS$APos10_Spring
Ratios_NIS$RatioAPos15 <-Ratios_NIS$APos15_Autumn/Ratios_NIS$APos15_Spring
Ratios_NIS$RatioAPos20 <-Ratios_NIS$APos20_Autumn/Ratios_NIS$APos20_Spring
Ratios_NIS$RatioAPos30 <-Ratios_NIS$APos30_Autumn/Ratios_NIS$APos30_Spring
Ratios_NIS$RatioAR05 <-Ratios_NIS$AR05_Autumn/Ratios_NIS$AR05_Spring
Ratios_NIS$RatioAR10 <-Ratios_NIS$AR10_Autumn/Ratios_NIS$AR10_Spring
Ratios_NIS$RatioAR15 <-Ratios_NIS$AR15_Autumn/Ratios_NIS$AR15_Spring
Ratios_NIS$RatioAR20 <-Ratios_NIS$AR20_Autumn/Ratios_NIS$AR20_Spring
Ratios_NIS$RatioAR30 <-Ratios_NIS$AR30_Autumn/Ratios_NIS$AR30_Spring
Ratios_NIS$RatioGPos05 <-Ratios_NIS$GPos05_Autumn/Ratios_NIS$GPos05_Spring
Ratios_NIS$RatioGPos10 <-Ratios_NIS$GPos10_Autumn/Ratios_NIS$GPos10_Spring
Ratios_NIS$RatioGPos15 <-Ratios_NIS$GPos15_Autumn/Ratios_NIS$GPos15_Spring
Ratios_NIS$RatioGPos20 <-Ratios_NIS$GPos20_Autumn/Ratios_NIS$GPos20_Spring
Ratios_NIS$RatioGPos30 <-Ratios_NIS$GPos30_Spring/Ratios_NIS$GPos30_Spring
Ratios_NIS$RatioGR05 <-Ratios_NIS$GR05_Autumn/Ratios_NIS$GR05_Spring
Ratios_NIS$RatioGR10 <-Ratios_NIS$GR10_Autumn/Ratios_NIS$GR10_Spring
Ratios_NIS$RatioGR15 <-Ratios_NIS$GR15_Autumn/Ratios_NIS$GR15_Spring
Ratios_NIS$RatioGR20 <-Ratios_NIS$GR20_Autumn/Ratios_NIS$GR20_Spring
Ratios_NIS$RatioGR30 <-Ratios_NIS$GR30_Autumn/Ratios_NIS$GR30_Spring
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$RatioAPos05 <-Ratios_NAT$APos05_Autumn/Ratios_NAT$APos05_Spring
Ratios_NAT$RatioAPos10 <-Ratios_NAT$APos10_Autumn/Ratios_NAT$APos10_Spring
Ratios_NAT$RatioAPos15 <-Ratios_NAT$APos15_Autumn/Ratios_NAT$APos15_Spring
Ratios_NAT$RatioAPos20 <-Ratios_NAT$APos20_Autumn/Ratios_NAT$APos20_Spring
Ratios_NAT$RatioAPos30 <-Ratios_NAT$APos30_Autumn/Ratios_NAT$APos30_Spring
Ratios_NAT$RatioAR05 <-Ratios_NAT$AR05_Autumn/Ratios_NAT$AR05_Spring
Ratios_NAT$RatioAR10 <-Ratios_NAT$AR10_Autumn/Ratios_NAT$AR10_Spring
Ratios_NAT$RatioAR15 <-Ratios_NAT$AR15_Autumn/Ratios_NAT$AR15_Spring
Ratios_NAT$RatioAR20 <-Ratios_NAT$AR20_Autumn/Ratios_NAT$AR20_Spring
Ratios_NAT$RatioAR30 <-Ratios_NAT$AR30_Autumn/Ratios_NAT$AR30_Spring
Ratios_NAT$RatioGPos05 <-Ratios_NAT$GPos05_Autumn/Ratios_NAT$GPos05_Spring
Ratios_NAT$RatioGPos10 <-Ratios_NAT$GPos10_Autumn/Ratios_NAT$GPos10_Spring
Ratios_NAT$RatioGPos15 <-Ratios_NAT$GPos15_Autumn/Ratios_NAT$GPos15_Spring
Ratios_NAT$RatioGPos20 <-Ratios_NAT$GPos20_Autumn/Ratios_NAT$GPos20_Spring
Ratios_NAT$RatioGPos30 <-Ratios_NAT$GPos30_Autumn/Ratios_NAT$GPos30_Spring
Ratios_NAT$RatioGR05 <-Ratios_NAT$GR05_Autumn/Ratios_NAT$GR05_Spring
Ratios_NAT$RatioGR10 <-Ratios_NAT$GR10_Autumn/Ratios_NAT$GR10_Spring
Ratios_NAT$RatioGR15 <-Ratios_NAT$GR15_Autumn/Ratios_NAT$GR15_Spring
Ratios_NAT$RatioGR20 <-Ratios_NAT$GR20_Autumn/Ratios_NAT$GR20_Spring
Ratios_NAT$RatioGR30 <-Ratios_NAT$GR30_Autumn/Ratios_NAT$GR30_Spring
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
CyprusNIS$Sites <- CyprusCategories$Sites
CyprusNIS$Season <-  CyprusCategories$Season
CyprusNIS[CyprusNIS == 0] <- 0.1

StdError_B078 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(41:60)])
StdError_B078$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B078$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B078$Species <- "B078"

StdError_B124 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(41:60)])
StdError_B124$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B124$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B124$Species <- "B124"

StdError_B132 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B132, ES = "LRRi")
StdError_B132$Ratio <- log(Ratios_NIS$B132[c(41:60)])
StdError_B132$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B132$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B132$Species <- "B132"

StdError_B191 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B191, ES = "LRRi")
StdError_B191$Ratio <- log(Ratios_NIS$B191[c(41:60)])
StdError_B191$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B191$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B191$Species <- "B191"

StdError_B464 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B464, ES = "LRRi")
StdError_B464$Ratio <- log(Ratios_NIS$B464[c(41:60)])
StdError_B464$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B464$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B464$Species <- "B464"

StdError_B481 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B481, ES = "LRRi")
StdError_B481$Ratio <- log(Ratios_NIS$B481[c(41:60)])
StdError_B481$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B481$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B481$Species <- "B481"

StdError_B482 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B482, ES = "LRRi")
StdError_B482$Ratio <- log(Ratios_NIS$B482[c(41:60)])
StdError_B482$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B482$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B482$Species <- "B482"

StdError_B489 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = B489, ES = "LRRi")
StdError_B489$Ratio <- log(Ratios_NIS$B489[c(41:60)])
StdError_B489$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B489$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B489$Species <- "B489"

StdError_G001 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G001, ES = "LRRi")
StdError_G001$Ratio <- log(Ratios_NIS$G001[c(41:60)])
StdError_G001$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G001$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G001$Species <- "G001"

StdError_G007 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(41:60)])
StdError_G007$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G007$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G007$Species <- "G007"

StdError_G008 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G008, ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(41:60)])
StdError_G008$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G008$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G008$Species <- "G008"

StdError_G089 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G089, ES = "LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(41:60)])
StdError_G089$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G089$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G089$Species <- "G089"

StdError_G102 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G102, ES = "LRRi")
StdError_G102$Ratio <- log(Ratios_NIS$G102[c(41:60)])
StdError_G102$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G102$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G102$Species <- "G102"

StdError_G133 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G133, ES = "LRRi")
StdError_G133$Ratio <- log(Ratios_NIS$G133[c(41:60)])
StdError_G133$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G133$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G133$Species <- "G133"

StdError_G246 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G246, ES = "LRRi")
StdError_G246$Ratio <- log(Ratios_NIS$G246[c(41:60)])
StdError_G246$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G246$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G246$Species <- "G246"

StdError_G247 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G247, ES = "LRRi")
StdError_G247$Ratio <- log(Ratios_NIS$G247[c(41:60)])
StdError_G247$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G247$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G247$Species <- "G247"

StdError_G254 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G254, ES = "LRRi")
StdError_G254$Ratio <- log(Ratios_NIS$G254[c(41:60)])
StdError_G254$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G254$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G254$Species <- "G254"

StdError_G310 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G310, ES = "LRRi")
StdError_G310$Ratio <- log(Ratios_NIS$G310[c(41:60)])
StdError_G310$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G310$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G310$Species <- "G310"

StdError_G352 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G352, ES = "LRRi")
StdError_G352$Ratio <- log(Ratios_NIS$G352[c(41:60)])
StdError_G352$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G352$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G352$Species <- "G352"

StdError_G354 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G354, ES = "LRRi")
StdError_G354$Ratio <- log(Ratios_NIS$G354[c(41:60)])
StdError_G354$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G354$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G354$Species <- "G354"

StdError_G385 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G385, ES = "LRRi")
StdError_G385$Ratio <- log(Ratios_NIS$G385[c(41:60)])
StdError_G385$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G385$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G385$Species <- "G385"

StdError_G387 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G387, ES = "LRRi")
StdError_G387$Ratio <- log(Ratios_NIS$G387[c(41:60)])
StdError_G387$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G387$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G387$Species <- "G387"

StdError_G390 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G390, ES = "LRRi")
StdError_G390$Ratio <- log(Ratios_NIS$G390[c(41:60)])
StdError_G390$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G390$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G390$Species <- "G390"

StdError_G395 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G395, ES = "LRRi")
StdError_G395$Ratio <- log(Ratios_NIS$G395[c(41:60)])
StdError_G395$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G395$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G395$Species <- "G395"

StdError_G543 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G543, ES = "LRRi")
StdError_G543$Ratio <- log(Ratios_NIS$G543[c(41:60)])
StdError_G543$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G543$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G543$Species <- "G543"

StdError_G550 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G550, ES = "LRRi")
StdError_G550$Ratio <- log(Ratios_NIS$G550[c(41:60)])
StdError_G550$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G550$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G550$Species <- "G550"

StdError_G795 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G795, ES = "LRRi")
StdError_G795$Ratio <- log(Ratios_NIS$G795[c(41:60)])
StdError_G795$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G795$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G795$Species <- "G795"

StdError_G814 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G814, ES = "LRRi")
StdError_G814$Ratio <- log(Ratios_NIS$G814[c(41:60)])
StdError_G814$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G814$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G814$Species <- "G814"

StdError_G815 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G815, ES = "LRRi")
StdError_G815$Ratio <- log(Ratios_NIS$G815[c(41:60)])
StdError_G815$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G815$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G815$Species <- "G815"

StdError_G816 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G816, ES = "LRRi")
StdError_G816$Ratio <- log(Ratios_NIS$G816[c(41:60)])
StdError_G816$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G816$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G816$Species <- "G816"

StdError_G860 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G860, ES = "LRRi")
StdError_G860$Ratio <- log(Ratios_NIS$G860[c(41:60)])
StdError_G860$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G860$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G860$Species <- "G860"

StdError_G943 <- batch_calc_ES(dat = CyprusNIS, grouping = Sites, condition = Season, 
                               outcome = G943, ES = "LRRi")
StdError_G943$Ratio <- log(Ratios_NIS$G943[c(41:60)])
StdError_G943$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G943$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                          rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G943$Species <- "G943"

StdError_NIS<- rbind(StdError_B078,StdError_B124,StdError_B132,StdError_B191,StdError_B464,StdError_B481,StdError_B482,
                     StdError_B489,StdError_G001,StdError_G007,StdError_G008,StdError_G089,StdError_G102,StdError_G133,
                     StdError_G246,StdError_G247,StdError_G254,StdError_G310,StdError_G352,StdError_G354,StdError_G385,
                     StdError_G387,StdError_G390,StdError_G395,StdError_G543,StdError_G550,StdError_G795,StdError_G814,
                     StdError_G815,StdError_G816,StdError_G860,StdError_G943)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NAT species ####
CyprusNAT$StationCode <- CyprusCategories$Sites
CyprusNAT$Season <-  CyprusCategories$Season
CyprusNAT[CyprusNAT == 0] <- 0.1

StdError_B004 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B004, ES = "LRRi")
StdError_B004$Ratio <- log(Ratios_NAT$B004[c(41:60)])
StdError_B004$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B004$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                           rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B004$Species <- "B004"

StdError_B005 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(41:60)])
StdError_B005$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B005$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B005$Species <- "B005"

StdError_B006 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B006, ES = "LRRi")
StdError_B006$Ratio <- log(Ratios_NAT$B006[c(41:60)])
StdError_B006$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B006$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B006$Species <- "B006"

StdError_B013 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B013, ES = "LRRi")
StdError_B013$Ratio <- log(Ratios_NAT$B013[c(41:60)])
StdError_B013$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B013$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B013$Species <- "B013"

StdError_B014 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B014, ES = "LRRi")
StdError_B014$Ratio <- log(Ratios_NAT$B014[c(41:60)])
StdError_B014$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B014$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B014$Species <- "B014"

StdError_B016 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B016, ES = "LRRi")
StdError_B016$Ratio <- log(Ratios_NAT$B016[c(41:60)])
StdError_B016$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B016$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B016$Species <- "B016"

StdError_B019 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(41:60)])
StdError_B019$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B019$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B019$Species <- "B019"

StdError_B020 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B020, ES = "LRRi")
StdError_B020$Ratio <- log(Ratios_NAT$B020[c(41:60)])
StdError_B020$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B020$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B020$Species <- "B020"

StdError_B022 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B022, ES = "LRRi")
StdError_B022$Ratio <- log(Ratios_NAT$B022[c(41:60)])
StdError_B022$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B022$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B022$Species <- "B022"

StdError_B023 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(41:60)])
StdError_B023$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B023$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B023$Species <- "B023"

StdError_B028 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B028, ES = "LRRi")
StdError_B028$Ratio <- log(Ratios_NAT$B028[c(41:60)])
StdError_B028$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B028$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B028$Species <- "B028"

StdError_B033 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(41:60)])
StdError_B033$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B033$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B033$Species <- "B033"

StdError_B034 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B034, ES = "LRRi")
StdError_B034$Ratio <- log(Ratios_NAT$B034[c(41:60)])
StdError_B034$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B034$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B034$Species <- "B034"

StdError_B044 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B044, ES = "LRRi")
StdError_B044$Ratio <- log(Ratios_NAT$B044[c(41:60)])
StdError_B044$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B044$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B044$Species <- "B044"

StdError_B045 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B045, ES = "LRRi")
StdError_B045$Ratio <- log(Ratios_NAT$B045[c(41:60)])
StdError_B045$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B045$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B045$Species <- "B045"

StdError_B046 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(41:60)])
StdError_B046$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B046$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B046$Species <- "B046"

StdError_B049 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B049, ES = "LRRi")
StdError_B049$Ratio <- log(Ratios_NAT$B049[c(41:60)])
StdError_B049$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B049$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B049$Species <- "B049"

StdError_B051 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B051, ES = "LRRi")
StdError_B051$Ratio <- log(Ratios_NAT$B051[c(41:60)])
StdError_B051$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B051$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B051$Species <- "B051"

StdError_B052 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B052, ES = "LRRi")
StdError_B052$Ratio <- log(Ratios_NAT$B052[c(41:60)])
StdError_B052$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B052$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B052$Species <- "B052"

StdError_B055 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(41:60)])
StdError_B055$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B055$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B055$Species <- "B055"

StdError_B060 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B060, ES = "LRRi")
StdError_B060$Ratio <- log(Ratios_NAT$B060[c(41:60)])
StdError_B060$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B060$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B060$Species <- "B060"

StdError_B065 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B065, ES = "LRRi")
StdError_B065$Ratio <- log(Ratios_NAT$B065[c(41:60)])
StdError_B065$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B065$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B065$Species <- "B065"

StdError_B066 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B066, ES = "LRRi")
StdError_B066$Ratio <- log(Ratios_NAT$B066[c(41:60)])
StdError_B066$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B066$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B066$Species <- "B066"

StdError_B068 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B068, ES = "LRRi")
StdError_B068$Ratio <- log(Ratios_NAT$B068[c(41:60)])
StdError_B068$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B068$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B068$Species <- "B068"

StdError_B074 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B074, ES = "LRRi")
StdError_B074$Ratio <- log(Ratios_NAT$B074[c(41:60)])
StdError_B074$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B074$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B074$Species <- "B074"

StdError_B088 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B088, ES = "LRRi")
StdError_B088$Ratio <- log(Ratios_NAT$B088[c(41:60)])
StdError_B088$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B088$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B088$Species <- "B088"

StdError_B089 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B089, ES = "LRRi")
StdError_B089$Ratio <- log(Ratios_NAT$B089[c(41:60)])
StdError_B089$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B089$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B089$Species <- "B089"

StdError_B111 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B111, ES = "LRRi")
StdError_B111$Ratio <- log(Ratios_NAT$B111[c(41:60)])
StdError_B111$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B111$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B111$Species <- "B111"

StdError_B121 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B121, ES = "LRRi")
StdError_B121$Ratio <- log(Ratios_NAT$B121[c(41:60)])
StdError_B121$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B121$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B121$Species <- "B121"

StdError_B123 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B123, ES = "LRRi")
StdError_B123$Ratio <- log(Ratios_NAT$B123[c(41:60)])
StdError_B123$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B123$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B123$Species <- "B123"

StdError_B125 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B125, ES = "LRRi")
StdError_B125$Ratio <- log(Ratios_NAT$B125[c(41:60)])
StdError_B125$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B125$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B125$Species <- "B125"

StdError_B128 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B128, ES = "LRRi")
StdError_B128$Ratio <- log(Ratios_NAT$B128[c(41:60)])
StdError_B128$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B128$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B128$Species <- "B128"

StdError_B138 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B138, ES = "LRRi")
StdError_B138$Ratio <- log(Ratios_NAT$B138[c(41:60)])
StdError_B138$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B138$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B138$Species <- "B138"

StdError_B139 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B139, ES = "LRRi")
StdError_B139$Ratio <- log(Ratios_NAT$B139[c(41:60)])
StdError_B139$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B139$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B139$Species <- "B139"

StdError_B140 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B140, ES = "LRRi")
StdError_B140$Ratio <- log(Ratios_NAT$B140[c(41:60)])
StdError_B140$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B140$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B140$Species <- "B140"

StdError_B141 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B141, ES = "LRRi")
StdError_B141$Ratio <- log(Ratios_NAT$B141[c(41:60)])
StdError_B141$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B141$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B141$Species <- "B141"

StdError_B142 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B142, ES = "LRRi")
StdError_B142$Ratio <- log(Ratios_NAT$B142[c(41:60)])
StdError_B142$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B142$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B142$Species <- "B142"

StdError_B143 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B143, ES = "LRRi")
StdError_B143$Ratio <- log(Ratios_NAT$B143[c(41:60)])
StdError_B143$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B143$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B143$Species <- "B143"

StdError_B144 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B144, ES = "LRRi")
StdError_B144$Ratio <- log(Ratios_NAT$B144[c(41:60)])
StdError_B144$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B144$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B144$Species <- "B144"

StdError_B147 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B147, ES = "LRRi")
StdError_B147$Ratio <- log(Ratios_NAT$B147[c(41:60)])
StdError_B147$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B147$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B147$Species <- "B147"

StdError_B148 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B148, ES = "LRRi")
StdError_B148$Ratio <- log(Ratios_NAT$B148[c(41:60)])
StdError_B148$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B148$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B148$Species <- "B148"

StdError_B150 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B150, ES = "LRRi")
StdError_B150$Ratio <- log(Ratios_NAT$B150[c(41:60)])
StdError_B150$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B150$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B150$Species <- "B150"

StdError_B152 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B152, ES = "LRRi")
StdError_B152$Ratio <- log(Ratios_NAT$B152[c(41:60)])
StdError_B152$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B152$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B152$Species <- "B152"

StdError_B154 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B154, ES = "LRRi")
StdError_B154$Ratio <- log(Ratios_NAT$B154[c(41:60)])
StdError_B154$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B154$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B154$Species <- "B154"

StdError_B160 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B160, ES = "LRRi")
StdError_B160$Ratio <- log(Ratios_NAT$B160[c(41:60)])
StdError_B160$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B160$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B160$Species <- "B160"

StdError_B161 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B161, ES = "LRRi")
StdError_B161$Ratio <- log(Ratios_NAT$B161[c(41:60)])
StdError_B161$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B161$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B161$Species <- "B161"

StdError_B167 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B167, ES = "LRRi")
StdError_B167$Ratio <- log(Ratios_NAT$B167[c(41:60)])
StdError_B167$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B167$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B167$Species <- "B167"

StdError_B176 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B176, ES = "LRRi")
StdError_B176$Ratio <- log(Ratios_NAT$B176[c(41:60)])
StdError_B176$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B176$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B176$Species <- "B176"

StdError_B184 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B184, ES = "LRRi")
StdError_B184$Ratio <- log(Ratios_NAT$B184[c(41:60)])
StdError_B184$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B184$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B184$Species <- "B184"

StdError_B188 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B188, ES = "LRRi")
StdError_B188$Ratio <- log(Ratios_NAT$B188[c(41:60)])
StdError_B188$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B188$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B188$Species <- "B188"

StdError_B189 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B189, ES = "LRRi")
StdError_B189$Ratio <- log(Ratios_NAT$B189[c(41:60)])
StdError_B189$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B189$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B189$Species <- "B189"

StdError_B247 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B247, ES = "LRRi")
StdError_B247$Ratio <- log(Ratios_NAT$B247[c(41:60)])
StdError_B247$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B247$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B247$Species <- "B247"

StdError_B260 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B260, ES = "LRRi")
StdError_B260$Ratio <- log(Ratios_NAT$B260[c(41:60)])
StdError_B260$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B260$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B260$Species <- "B260"

StdError_B263 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B263, ES = "LRRi")
StdError_B263$Ratio <- log(Ratios_NAT$B263[c(41:60)])
StdError_B263$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B263$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B263$Species <- "B263"

StdError_B269 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B269, ES = "LRRi")
StdError_B269$Ratio <- log(Ratios_NAT$B269[c(41:60)])
StdError_B269$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B269$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B269$Species <- "B269"

StdError_B276 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B276, ES = "LRRi")
StdError_B276$Ratio <- log(Ratios_NAT$B276[c(41:60)])
StdError_B276$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B276$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B276$Species <- "B276"

StdError_B284 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B284, ES = "LRRi")
StdError_B284$Ratio <- log(Ratios_NAT$B284[c(41:60)])
StdError_B284$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B284$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B284$Species <- "B284"

StdError_B298 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B298, ES = "LRRi")
StdError_B298$Ratio <- log(Ratios_NAT$B298[c(41:60)])
StdError_B298$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B298$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B298$Species <- "B298"

StdError_B302 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B302, ES = "LRRi")
StdError_B302$Ratio <- log(Ratios_NAT$B302[c(41:60)])
StdError_B302$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B302$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B302$Species <- "B302"

StdError_B312 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B312, ES = "LRRi")
StdError_B312$Ratio <- log(Ratios_NAT$B312[c(41:60)])
StdError_B312$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B312$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B312$Species <- "B312"

StdError_B313 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B313, ES = "LRRi")
StdError_B313$Ratio <- log(Ratios_NAT$B313[c(41:60)])
StdError_B313$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B313$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B313$Species <- "B313"

StdError_B317 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B317, ES = "LRRi")
StdError_B317$Ratio <- log(Ratios_NAT$B317[c(41:60)])
StdError_B317$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B317$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B317$Species <- "B317"

StdError_B328 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B328, ES = "LRRi")
StdError_B328$Ratio <- log(Ratios_NAT$B328[c(41:60)])
StdError_B328$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B328$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B328$Species <- "B328"

StdError_B332 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B332, ES = "LRRi")
StdError_B332$Ratio <- log(Ratios_NAT$B332[c(41:60)])
StdError_B332$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B332$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B332$Species <- "B332"

StdError_B341 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B341, ES = "LRRi")
StdError_B341$Ratio <- log(Ratios_NAT$B341[c(41:60)])
StdError_B341$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B341$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B341$Species <- "B341"

StdError_B450 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B450, ES = "LRRi")
StdError_B450$Ratio <- log(Ratios_NAT$B450[c(41:60)])
StdError_B450$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B450$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B450$Species <- "B450"

StdError_B454 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B454, ES = "LRRi")
StdError_B454$Ratio <- log(Ratios_NAT$B454[c(41:60)])
StdError_B454$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B454$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B454$Species <- "B454"

StdError_B475 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B475, ES = "LRRi")
StdError_B475$Ratio <- log(Ratios_NAT$B475[c(41:60)])
StdError_B475$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B475$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B475$Species <- "B475"

StdError_B504 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B504, ES = "LRRi")
StdError_B504$Ratio <- log(Ratios_NAT$B504[c(41:60)])
StdError_B504$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B504$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B504$Species <- "B504"

StdError_B510 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B510, ES = "LRRi")
StdError_B510$Ratio <- log(Ratios_NAT$B510[c(41:60)])
StdError_B510$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B510$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B510$Species <- "B510"

StdError_B517 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = B517, ES = "LRRi")
StdError_B517$Ratio <- log(Ratios_NAT$B517[c(41:60)])
StdError_B517$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_B517$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_B517$Species <- "B517"

StdError_G010 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(41:60)])
StdError_G010$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G010$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G010$Species <- "G010"

StdError_G013 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G013, ES = "LRRi")
StdError_G013$Ratio <- log(Ratios_NAT$G013[c(41:60)])
StdError_G013$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G013$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G013$Species <- "G013"

StdError_G017 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(41:60)])
StdError_G017$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G017$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G017$Species <- "G017"

StdError_G020 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G020, ES = "LRRi")
StdError_G020$Ratio <- log(Ratios_NAT$G020[c(41:60)])
StdError_G020$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G020$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G020$Species <- "G020"

StdError_G021 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G021, ES = "LRRi")
StdError_G021$Ratio <- log(Ratios_NAT$G021[c(41:60)])
StdError_G021$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G021$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G021$Species <- "G021"

StdError_G026 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G026, ES = "LRRi")
StdError_G026$Ratio <- log(Ratios_NAT$G026[c(41:60)])
StdError_G026$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G026$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G026$Species <- "G026"

StdError_G027 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(41:60)])
StdError_G027$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G027$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G027$Species <- "G027"

StdError_G029 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G029, ES = "LRRi")
StdError_G029$Ratio <- log(Ratios_NAT$G029[c(41:60)])
StdError_G029$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G029$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G029$Species <- "G029"

StdError_G030 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G030, ES = "LRRi")
StdError_G030$Ratio <- log(Ratios_NAT$G030[c(41:60)])
StdError_G030$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G030$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G030$Species <- "G030"

StdError_G032 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G032, ES = "LRRi")
StdError_G032$Ratio <- log(Ratios_NAT$G032[c(41:60)])
StdError_G032$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G032$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G032$Species <- "G032"

StdError_G036 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G036, ES = "LRRi")
StdError_G036$Ratio <- log(Ratios_NAT$G036[c(41:60)])
StdError_G036$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G036$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G036$Species <- "G036"

StdError_G037 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G037, ES = "LRRi")
StdError_G037$Ratio <- log(Ratios_NAT$G037[c(41:60)])
StdError_G037$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G037$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G037$Species <- "G037"

StdError_G041 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G041, ES = "LRRi")
StdError_G041$Ratio <- log(Ratios_NAT$G041[c(41:60)])
StdError_G041$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G041$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G041$Species <- "G041"

StdError_G044 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G044, ES = "LRRi")
StdError_G044$Ratio <- log(Ratios_NAT$G044[c(41:60)])
StdError_G044$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G044$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G044$Species <- "G044"

StdError_G046 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G046, ES = "LRRi")
StdError_G046$Ratio <- log(Ratios_NAT$G046[c(41:60)])
StdError_G046$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G046$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G046$Species <- "G046"

StdError_G047 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G047, ES = "LRRi")
StdError_G047$Ratio <- log(Ratios_NAT$G047[c(41:60)])
StdError_G047$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G047$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G047$Species <- "G047"

StdError_G048 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G048, ES = "LRRi")
StdError_G048$Ratio <- log(Ratios_NAT$G048[c(41:60)])
StdError_G048$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G048$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G048$Species <- "G048"

StdError_G050 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G050, ES = "LRRi")
StdError_G050$Ratio <- log(Ratios_NAT$G050[c(41:60)])
StdError_G050$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G050$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G050$Species <- "G050"

StdError_G051 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G051, ES = "LRRi")
StdError_G051$Ratio <- log(Ratios_NAT$G051[c(41:60)])
StdError_G051$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G051$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G051$Species <- "G051"

StdError_G053 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G053, ES = "LRRi")
StdError_G053$Ratio <- log(Ratios_NAT$G053[c(41:60)])
StdError_G053$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G053$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G053$Species <- "G053"

StdError_G057 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G057, ES = "LRRi")
StdError_G057$Ratio <- log(Ratios_NAT$G057[c(41:60)])
StdError_G057$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G057$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G057$Species <- "G057"

StdError_G064 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G064, ES = "LRRi")
StdError_G064$Ratio <- log(Ratios_NAT$G064[c(41:60)])
StdError_G064$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G064$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G064$Species <- "G064"

StdError_G073 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G073, ES = "LRRi")
StdError_G073$Ratio <- log(Ratios_NAT$G073[c(41:60)])
StdError_G073$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G073$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G073$Species <- "G073"

StdError_G074 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G074, ES = "LRRi")
StdError_G074$Ratio <- log(Ratios_NAT$G074[c(41:60)])
StdError_G074$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G074$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G074$Species <- "G074"

StdError_G075 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G075, ES = "LRRi")
StdError_G075$Ratio <- log(Ratios_NAT$G075[c(41:60)])
StdError_G075$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G075$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G075$Species <- "G075"

StdError_G077 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G077, ES = "LRRi")
StdError_G077$Ratio <- log(Ratios_NAT$G077[c(41:60)])
StdError_G077$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G077$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G077$Species <- "G077"

StdError_G083 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G083, ES = "LRRi")
StdError_G083$Ratio <- log(Ratios_NAT$G083[c(41:60)])
StdError_G083$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G083$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G083$Species <- "G083"

StdError_G094 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G094, ES = "LRRi")
StdError_G094$Ratio <- log(Ratios_NAT$G094[c(41:60)])
StdError_G094$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G094$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G094$Species <- "G094"

StdError_G097 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G097, ES = "LRRi")
StdError_G097$Ratio <- log(Ratios_NAT$G097[c(41:60)])
StdError_G097$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G097$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G097$Species <- "G097"

StdError_G100 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G100, ES = "LRRi")
StdError_G100$Ratio <- log(Ratios_NAT$G100[c(41:60)])
StdError_G100$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G100$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G100$Species <- "G100"

StdError_G101 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G101, ES = "LRRi")
StdError_G101$Ratio <- log(Ratios_NAT$G101[c(41:60)])
StdError_G101$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G101$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G101$Species <- "G101"

StdError_G105 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G105, ES = "LRRi")
StdError_G105$Ratio <- log(Ratios_NAT$G105[c(41:60)])
StdError_G105$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G105$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G105$Species <- "G105"

StdError_G109 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G109, ES = "LRRi")
StdError_G109$Ratio <- log(Ratios_NAT$G109[c(41:60)])
StdError_G109$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G109$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G109$Species <- "G109"

StdError_G119 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G119, ES = "LRRi")
StdError_G119$Ratio <- log(Ratios_NAT$G119[c(41:60)])
StdError_G119$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G119$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G119$Species <- "G119"

StdError_G139 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G139, ES = "LRRi")
StdError_G139$Ratio <- log(Ratios_NAT$G139[c(41:60)])
StdError_G139$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G139$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G139$Species <- "G139"

StdError_G140 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G140, ES = "LRRi")
StdError_G140$Ratio <- log(Ratios_NAT$G140[c(41:60)])
StdError_G140$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G140$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G140$Species <- "G140"

StdError_G142 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G142, ES = "LRRi")
StdError_G142$Ratio <- log(Ratios_NAT$G142[c(41:60)])
StdError_G142$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G142$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G142$Species <- "G142"

StdError_G144 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G144, ES = "LRRi")
StdError_G144$Ratio <- log(Ratios_NAT$G144[c(41:60)])
StdError_G144$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G144$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G144$Species <- "G144"

StdError_G150 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G150, ES = "LRRi")
StdError_G150$Ratio <- log(Ratios_NAT$G150[c(41:60)])
StdError_G150$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G150$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G150$Species <- "G150"

StdError_G151 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G151, ES = "LRRi")
StdError_G151$Ratio <- log(Ratios_NAT$G151[c(41:60)])
StdError_G151$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G151$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G151$Species <- "G151"

StdError_G152 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G152, ES = "LRRi")
StdError_G152$Ratio <- log(Ratios_NAT$G152[c(41:60)])
StdError_G152$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G152$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G152$Species <- "G152"

StdError_G153 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G153, ES = "LRRi")
StdError_G153$Ratio <- log(Ratios_NAT$G153[c(41:60)])
StdError_G153$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G153$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G153$Species <- "G153"

StdError_G154 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G154, ES = "LRRi")
StdError_G154$Ratio <- log(Ratios_NAT$G154[c(41:60)])
StdError_G154$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G154$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G154$Species <- "G154"

StdError_G155 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G155, ES = "LRRi")
StdError_G155$Ratio <- log(Ratios_NAT$G155[c(41:60)])
StdError_G155$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G155$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G155$Species <- "G155"

StdError_G163 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G163, ES = "LRRi")
StdError_G163$Ratio <- log(Ratios_NAT$G163[c(41:60)])
StdError_G163$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G163$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G163$Species <- "G163"

StdError_G165 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G165, ES = "LRRi")
StdError_G165$Ratio <- log(Ratios_NAT$G165[c(41:60)])
StdError_G165$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G165$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G165$Species <- "G165"

StdError_G167 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G167, ES = "LRRi")
StdError_G167$Ratio <- log(Ratios_NAT$G167[c(41:60)])
StdError_G167$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G167$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G167$Species <- "G167"

StdError_G168 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G168, ES = "LRRi")
StdError_G168$Ratio <- log(Ratios_NAT$G168[c(41:60)])
StdError_G168$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G168$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G168$Species <- "G168"

StdError_G169 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G169, ES = "LRRi")
StdError_G169$Ratio <- log(Ratios_NAT$G169[c(41:60)])
StdError_G169$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G169$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G169$Species <- "G169"

StdError_G170 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G170, ES = "LRRi")
StdError_G170$Ratio <- log(Ratios_NAT$G170[c(41:60)])
StdError_G170$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G170$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G170$Species <- "G170"

StdError_G172 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G172, ES = "LRRi")
StdError_G172$Ratio <- log(Ratios_NAT$G172[c(41:60)])
StdError_G172$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G172$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G172$Species <- "G172"

StdError_G175 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G175, ES = "LRRi")
StdError_G175$Ratio <- log(Ratios_NAT$G175[c(41:60)])
StdError_G175$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G175$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G175$Species <- "G175"

StdError_G176 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G176, ES = "LRRi")
StdError_G176$Ratio <- log(Ratios_NAT$G176[c(41:60)])
StdError_G176$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G176$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G176$Species <- "G176"

StdError_G177 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G177, ES = "LRRi")
StdError_G177$Ratio <- log(Ratios_NAT$G177[c(41:60)])
StdError_G177$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G177$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G177$Species <- "G177"

StdError_G178 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G178, ES = "LRRi")
StdError_G178$Ratio <- log(Ratios_NAT$G178[c(41:60)])
StdError_G178$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G178$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G178$Species <- "G178"

StdError_G179 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G179, ES = "LRRi")
StdError_G179$Ratio <- log(Ratios_NAT$G179[c(41:60)])
StdError_G179$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G179$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G179$Species <- "G179"

StdError_G180 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G180, ES = "LRRi")
StdError_G180$Ratio <- log(Ratios_NAT$G180[c(41:60)])
StdError_G180$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G180$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G180$Species <- "G180"

StdError_G181 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G181, ES = "LRRi")
StdError_G181$Ratio <- log(Ratios_NAT$G181[c(41:60)])
StdError_G181$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G181$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G181$Species <- "G181"

StdError_G183 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G183, ES = "LRRi")
StdError_G183$Ratio <- log(Ratios_NAT$G183[c(41:60)])
StdError_G183$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G183$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G183$Species <- "G183"

StdError_G184 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G184, ES = "LRRi")
StdError_G184$Ratio <- log(Ratios_NAT$G184[c(41:60)])
StdError_G184$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G184$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G184$Species <- "G184"

StdError_G185 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G185, ES = "LRRi")
StdError_G185$Ratio <- log(Ratios_NAT$G185[c(41:60)])
StdError_G185$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G185$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G185$Species <- "G185"

StdError_G186 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G186, ES = "LRRi")
StdError_G186$Ratio <- log(Ratios_NAT$G186[c(41:60)])
StdError_G186$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G186$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G186$Species <- "G186"

StdError_G187 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G187, ES = "LRRi")
StdError_G187$Ratio <- log(Ratios_NAT$G187[c(41:60)])
StdError_G187$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G187$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G187$Species <- "G187"

StdError_G188 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G188, ES = "LRRi")
StdError_G188$Ratio <- log(Ratios_NAT$G188[c(41:60)])
StdError_G188$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G188$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G188$Species <- "G188"

StdError_G189 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G189, ES = "LRRi")
StdError_G189$Ratio <- log(Ratios_NAT$G189[c(41:60)])
StdError_G189$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G189$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G189$Species <- "G189"

StdError_G191 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G191, ES = "LRRi")
StdError_G191$Ratio <- log(Ratios_NAT$G191[c(41:60)])
StdError_G191$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G191$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G191$Species <- "G191"

StdError_G192 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G192, ES = "LRRi")
StdError_G192$Ratio <- log(Ratios_NAT$G192[c(41:60)])
StdError_G192$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G192$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G192$Species <- "G192"

StdError_G194 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G194, ES = "LRRi")
StdError_G194$Ratio <- log(Ratios_NAT$G194[c(41:60)])
StdError_G194$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G194$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G194$Species <- "G194"

StdError_G197 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G197, ES = "LRRi")
StdError_G197$Ratio <- log(Ratios_NAT$G197[c(41:60)])
StdError_G197$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G197$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G197$Species <- "G197"

StdError_G198 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G198, ES = "LRRi")
StdError_G198$Ratio <- log(Ratios_NAT$G198[c(41:60)])
StdError_G198$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G198$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G198$Species <- "G198"

StdError_G199 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G199, ES = "LRRi")
StdError_G199$Ratio <- log(Ratios_NAT$G199[c(41:60)])
StdError_G199$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G199$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G199$Species <- "G199"

StdError_G200 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G200, ES = "LRRi")
StdError_G200$Ratio <- log(Ratios_NAT$G200[c(41:60)])
StdError_G200$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G200$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G200$Species <- "G200"

StdError_G202 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G202, ES = "LRRi")
StdError_G202$Ratio <- log(Ratios_NAT$G202[c(41:60)])
StdError_G202$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G202$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G202$Species <- "G202"

StdError_G203 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G203, ES = "LRRi")
StdError_G203$Ratio <- log(Ratios_NAT$G203[c(41:60)])
StdError_G203$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G203$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G203$Species <- "G203"

StdError_G205 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G205, ES = "LRRi")
StdError_G205$Ratio <- log(Ratios_NAT$G205[c(41:60)])
StdError_G205$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G205$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G205$Species <- "G205"

StdError_G207 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G207, ES = "LRRi")
StdError_G207$Ratio <- log(Ratios_NAT$G207[c(41:60)])
StdError_G207$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G207$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G207$Species <- "G207"

StdError_G208 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G208, ES = "LRRi")
StdError_G208$Ratio <- log(Ratios_NAT$G208[c(41:60)])
StdError_G208$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G208$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G208$Species <- "G208"

StdError_G209 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G209, ES = "LRRi")
StdError_G209$Ratio <- log(Ratios_NAT$G209[c(41:60)])
StdError_G209$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G209$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G209$Species <- "G209"

StdError_G212 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G212, ES = "LRRi")
StdError_G212$Ratio <- log(Ratios_NAT$G212[c(41:60)])
StdError_G212$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G212$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G212$Species <- "G212"

StdError_G214 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G214, ES = "LRRi")
StdError_G214$Ratio <- log(Ratios_NAT$G214[c(41:60)])
StdError_G214$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G214$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G214$Species <- "G214"

StdError_G215 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G215, ES = "LRRi")
StdError_G215$Ratio <- log(Ratios_NAT$G215[c(41:60)])
StdError_G215$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G215$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G215$Species <- "G215"

StdError_G216 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G216, ES = "LRRi")
StdError_G216$Ratio <- log(Ratios_NAT$G216[c(41:60)])
StdError_G216$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G216$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G216$Species <- "G216"

StdError_G219 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G219, ES = "LRRi")
StdError_G219$Ratio <- log(Ratios_NAT$G219[c(41:60)])
StdError_G219$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G219$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G219$Species <- "G219"

StdError_G221 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G221, ES = "LRRi")
StdError_G221$Ratio <- log(Ratios_NAT$G221[c(41:60)])
StdError_G221$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G221$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G221$Species <- "G221"

StdError_G245 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G245, ES = "LRRi")
StdError_G245$Ratio <- log(Ratios_NAT$G245[c(41:60)])
StdError_G245$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G245$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G245$Species <- "G245"

StdError_G248 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G248, ES = "LRRi")
StdError_G248$Ratio <- log(Ratios_NAT$G248[c(41:60)])
StdError_G248$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G248$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G248$Species <- "G248"

StdError_G250 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G250, ES = "LRRi")
StdError_G250$Ratio <- log(Ratios_NAT$G250[c(41:60)])
StdError_G250$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G250$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G250$Species <- "G250"

StdError_G255 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G255, ES = "LRRi")
StdError_G255$Ratio <- log(Ratios_NAT$G255[c(41:60)])
StdError_G255$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G255$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G255$Species <- "G255"

StdError_G266 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G266, ES = "LRRi")
StdError_G266$Ratio <- log(Ratios_NAT$G266[c(41:60)])
StdError_G266$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G266$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G266$Species <- "G266"

StdError_G271 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G271, ES = "LRRi")
StdError_G271$Ratio <- log(Ratios_NAT$G271[c(41:60)])
StdError_G271$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G271$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G271$Species <- "G271"

StdError_G275 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G275, ES = "LRRi")
StdError_G275$Ratio <- log(Ratios_NAT$G275[c(41:60)])
StdError_G275$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G275$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G275$Species <- "G275"

StdError_G284 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G284, ES = "LRRi")
StdError_G284$Ratio <- log(Ratios_NAT$G284[c(41:60)])
StdError_G284$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G284$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G284$Species <- "G284"

StdError_G287 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G287, ES = "LRRi")
StdError_G287$Ratio <- log(Ratios_NAT$G287[c(41:60)])
StdError_G287$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G287$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G287$Species <- "G287"

StdError_G289 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G289, ES = "LRRi")
StdError_G289$Ratio <- log(Ratios_NAT$G289[c(41:60)])
StdError_G289$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G289$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G289$Species <- "G289"

StdError_G292 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G292, ES = "LRRi")
StdError_G292$Ratio <- log(Ratios_NAT$G292[c(41:60)])
StdError_G292$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G292$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G292$Species <- "G292"

StdError_G294 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G294, ES = "LRRi")
StdError_G294$Ratio <- log(Ratios_NAT$G294[c(41:60)])
StdError_G294$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G294$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G294$Species <- "G294"

StdError_G295 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G295, ES = "LRRi")
StdError_G295$Ratio <- log(Ratios_NAT$G295[c(41:60)])
StdError_G295$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G295$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G295$Species <- "G295"

StdError_G299 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G299, ES = "LRRi")
StdError_G299$Ratio <- log(Ratios_NAT$G299[c(41:60)])
StdError_G299$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G299$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G299$Species <- "G299"

StdError_G304 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G304, ES = "LRRi")
StdError_G304$Ratio <- log(Ratios_NAT$G304[c(41:60)])
StdError_G304$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G304$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G304$Species <- "G304"

StdError_G305 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G305, ES = "LRRi")
StdError_G305$Ratio <- log(Ratios_NAT$G305[c(41:60)])
StdError_G305$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G305$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G305$Species <- "G305"

StdError_G330 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G330, ES = "LRRi")
StdError_G330$Ratio <- log(Ratios_NAT$G330[c(41:60)])
StdError_G330$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G330$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G330$Species <- "G330"

StdError_G332 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G332, ES = "LRRi")
StdError_G332$Ratio <- log(Ratios_NAT$G332[c(41:60)])
StdError_G332$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G332$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G332$Species <- "G332"

StdError_G336 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G336, ES = "LRRi")
StdError_G336$Ratio <- log(Ratios_NAT$G336[c(41:60)])
StdError_G336$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G336$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G336$Species <- "G336"

StdError_G337 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G337, ES = "LRRi")
StdError_G337$Ratio <- log(Ratios_NAT$G337[c(41:60)])
StdError_G337$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G337$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G337$Species <- "G337"

StdError_G338 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(41:60)])
StdError_G338$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G338$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G338$Species <- "G338"

StdError_G339 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G339, ES = "LRRi")
StdError_G339$Ratio <- log(Ratios_NAT$G339[c(41:60)])
StdError_G339$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G339$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G339$Species <- "G339"

StdError_G340 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G340, ES = "LRRi")
StdError_G340$Ratio <- log(Ratios_NAT$G340[c(41:60)])
StdError_G340$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G340$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G340$Species <- "G340"

StdError_G341 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G341, ES = "LRRi")
StdError_G341$Ratio <- log(Ratios_NAT$G341[c(41:60)])
StdError_G341$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G341$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G341$Species <- "G341"

StdError_G342 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G342, ES = "LRRi")
StdError_G342$Ratio <- log(Ratios_NAT$G342[c(41:60)])
StdError_G342$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G342$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G342$Species <- "G342"

StdError_G345 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G345, ES = "LRRi")
StdError_G345$Ratio <- log(Ratios_NAT$G345[c(41:60)])
StdError_G345$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G345$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G345$Species <- "G345"

StdError_G347 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G347, ES = "LRRi")
StdError_G347$Ratio <- log(Ratios_NAT$G347[c(41:60)])
StdError_G347$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G347$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G347$Species <- "G347"

StdError_G350 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G350, ES = "LRRi")
StdError_G350$Ratio <- log(Ratios_NAT$G350[c(41:60)])
StdError_G350$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G350$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G350$Species <- "G350"

StdError_G351 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G351, ES = "LRRi")
StdError_G351$Ratio <- log(Ratios_NAT$G351[c(41:60)])
StdError_G351$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G351$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G351$Species <- "G351"

StdError_G362<- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G362, ES = "LRRi")
StdError_G362$Ratio <- log(Ratios_NAT$G362[c(41:60)])
StdError_G362$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G362$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G362$Species <- "G362"

StdError_G363 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G363, ES = "LRRi")
StdError_G363$Ratio <- log(Ratios_NAT$G363[c(41:60)])
StdError_G363$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G363$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G363$Species <- "G363"

StdError_G364 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G364, ES = "LRRi")
StdError_G364$Ratio <- log(Ratios_NAT$G364[c(41:60)])
StdError_G364$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G364$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G364$Species <- "G364"

StdError_G367 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G367, ES = "LRRi")
StdError_G367$Ratio <- log(Ratios_NAT$G367[c(41:60)])
StdError_G367$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G367$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G367$Species <- "G367"

StdError_G422 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G422, ES = "LRRi")
StdError_G422$Ratio <- log(Ratios_NAT$G422[c(41:60)])
StdError_G422$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G422$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G422$Species <- "G422"

StdError_G423 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G423, ES = "LRRi")
StdError_G423$Ratio <- log(Ratios_NAT$G423[c(41:60)])
StdError_G423$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G423$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G423$Species <- "G423"

StdError_G424 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G424, ES = "LRRi")
StdError_G424$Ratio <- log(Ratios_NAT$G424[c(41:60)])
StdError_G424$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G424$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G424$Species <- "G424"

StdError_G428 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G428, ES = "LRRi")
StdError_G428$Ratio <- log(Ratios_NAT$G428[c(41:60)])
StdError_G428$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G428$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G428$Species <- "G428"

StdError_G433 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G433, ES = "LRRi")
StdError_G433$Ratio <- log(Ratios_NAT$G433[c(41:60)])
StdError_G433$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G433$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G433$Species <- "G433"

StdError_G447 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G447, ES = "LRRi")
StdError_G447$Ratio <- log(Ratios_NAT$G447[c(41:60)])
StdError_G447$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G447$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G447$Species <- "G447"

StdError_G448 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G448, ES = "LRRi")
StdError_G448$Ratio <- log(Ratios_NAT$G448[c(41:60)])
StdError_G448$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G448$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G448$Species <- "G448"

StdError_G449 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G449, ES = "LRRi")
StdError_G449$Ratio <- log(Ratios_NAT$G449[c(41:60)])
StdError_G449$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G449$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G449$Species <- "G449"

StdError_G450 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G450, ES = "LRRi")
StdError_G450$Ratio <- log(Ratios_NAT$G450[c(41:60)])
StdError_G450$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G450$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G450$Species <- "G450"

StdError_G451 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G451, ES = "LRRi")
StdError_G451$Ratio <- log(Ratios_NAT$G451[c(41:60)])
StdError_G451$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G451$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G451$Species <- "G451"

StdError_G452 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G452, ES = "LRRi")
StdError_G452$Ratio <- log(Ratios_NAT$G452[c(41:60)])
StdError_G452$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G452$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G452$Species <- "G452"

StdError_G457 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G457, ES = "LRRi")
StdError_G457$Ratio <- log(Ratios_NAT$G457[c(41:60)])
StdError_G457$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G457$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G457$Species <- "G457"

StdError_G459 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G459, ES = "LRRi")
StdError_G459$Ratio <- log(Ratios_NAT$G459[c(41:60)])
StdError_G459$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G459$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G459$Species <- "G459"

StdError_G464 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G464, ES = "LRRi")
StdError_G464$Ratio <- log(Ratios_NAT$G464[c(41:60)])
StdError_G464$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G464$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G464$Species <- "G464"

StdError_G466 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G466, ES = "LRRi")
StdError_G466$Ratio <- log(Ratios_NAT$G466[c(41:60)])
StdError_G466$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G466$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G466$Species <- "G466"

StdError_G475 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G475, ES = "LRRi")
StdError_G475$Ratio <- log(Ratios_NAT$G475[c(41:60)])
StdError_G475$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G475$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G475$Species <- "G475"

StdError_G476 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G476, ES = "LRRi")
StdError_G476$Ratio <- log(Ratios_NAT$G476[c(41:60)])
StdError_G476$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G476$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G476$Species <- "G476"

StdError_G481 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G481, ES = "LRRi")
StdError_G481$Ratio <- log(Ratios_NAT$G481[c(41:60)])
StdError_G481$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G481$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G481$Species <- "G481"

StdError_G482 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G482, ES = "LRRi")
StdError_G482$Ratio <- log(Ratios_NAT$G482[c(41:60)])
StdError_G482$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G482$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G482$Species <- "G482"

StdError_G483 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G483, ES = "LRRi")
StdError_G483$Ratio <- log(Ratios_NAT$G483[c(41:60)])
StdError_G483$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G483$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G483$Species <- "G483"

StdError_G485 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G485, ES = "LRRi")
StdError_G485$Ratio <- log(Ratios_NAT$G485[c(41:60)])
StdError_G485$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G485$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G485$Species <- "G485"

StdError_G487 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G487, ES = "LRRi")
StdError_G487$Ratio <- log(Ratios_NAT$G487[c(41:60)])
StdError_G487$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G487$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G487$Species <- "G487"

StdError_G488 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G488, ES = "LRRi")
StdError_G488$Ratio <- log(Ratios_NAT$G488[c(41:60)])
StdError_G488$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G488$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G488$Species <- "G488"

StdError_G491 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G491, ES = "LRRi")
StdError_G491$Ratio <- log(Ratios_NAT$G491[c(41:60)])
StdError_G491$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G491$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G491$Species <- "G491"

StdError_G492 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G492, ES = "LRRi")
StdError_G492$Ratio <- log(Ratios_NAT$G492[c(41:60)])
StdError_G492$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G492$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G492$Species <- "G492"

StdError_G498 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G498, ES = "LRRi")
StdError_G498$Ratio <- log(Ratios_NAT$G498[c(41:60)])
StdError_G498$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G498$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G498$Species <- "G498"

StdError_G499 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G499, ES = "LRRi")
StdError_G499$Ratio <- log(Ratios_NAT$G499[c(41:60)])
StdError_G499$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G499$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G499$Species <- "G499"

StdError_G501 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G501, ES = "LRRi")
StdError_G501$Ratio <- log(Ratios_NAT$G501[c(41:60)])
StdError_G501$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G501$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G501$Species <- "G501"

StdError_G518 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G518, ES = "LRRi")
StdError_G518$Ratio <- log(Ratios_NAT$G518[c(41:60)])
StdError_G518$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G518$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G518$Species <- "G518"

StdError_G519 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G519, ES = "LRRi")
StdError_G519$Ratio <- log(Ratios_NAT$G519[c(41:60)])
StdError_G519$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G519$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G519$Species <- "G519"

StdError_G551 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G551, ES = "LRRi")
StdError_G551$Ratio <- log(Ratios_NAT$G551[c(41:60)])
StdError_G551$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G551$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G551$Species <- "G551"

StdError_G563 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G563, ES = "LRRi")
StdError_G563$Ratio <- log(Ratios_NAT$G563[c(41:60)])
StdError_G563$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G563$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G563$Species <- "G563"

StdError_G566 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G566, ES = "LRRi")
StdError_G566$Ratio <- log(Ratios_NAT$G566[c(41:60)])
StdError_G566$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G566$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G566$Species <- "G566"

StdError_G572 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G572, ES = "LRRi")
StdError_G572$Ratio <- log(Ratios_NAT$G572[c(41:60)])
StdError_G572$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G572$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G572$Species <- "G572"

StdError_G574 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G574, ES = "LRRi")
StdError_G574$Ratio <- log(Ratios_NAT$G574[c(41:60)])
StdError_G574$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G574$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G574$Species <- "G574"

StdError_G578 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G578, ES = "LRRi")
StdError_G578$Ratio <- log(Ratios_NAT$G578[c(41:60)])
StdError_G578$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G578$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G578$Species <- "G578"

StdError_G588<- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G588, ES = "LRRi")
StdError_G588$Ratio <- log(Ratios_NAT$G588[c(41:60)])
StdError_G588$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G588$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G588$Species <- "G588"

StdError_G589 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G589, ES = "LRRi")
StdError_G589$Ratio <- log(Ratios_NAT$G589[c(41:60)])
StdError_G589$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G589$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G589$Species <- "G589"

StdError_G592<- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G592, ES = "LRRi")
StdError_G592$Ratio <- log(Ratios_NAT$G592[c(41:60)])
StdError_G592$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G592$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G592$Species <- "G592"

StdError_G598 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G598, ES = "LRRi")
StdError_G598$Ratio <- log(Ratios_NAT$G598[c(41:60)])
StdError_G598$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G598$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G598$Species <- "G598"

StdError_G602 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G602, ES = "LRRi")
StdError_G602$Ratio <- log(Ratios_NAT$G602[c(41:60)])
StdError_G602$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G602$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G602$Species <- "G602"

StdError_G603 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G603, ES = "LRRi")
StdError_G603$Ratio <- log(Ratios_NAT$G603[c(41:60)])
StdError_G603$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G603$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G603$Species <- "G603"

StdError_G613 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G613, ES = "LRRi")
StdError_G613$Ratio <- log(Ratios_NAT$G613[c(41:60)])
StdError_G613$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G613$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G613$Species <- "G613"

StdError_G615 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G615, ES = "LRRi")
StdError_G615$Ratio <- log(Ratios_NAT$G615[c(41:60)])
StdError_G615$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G615$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G615$Species <- "G615"

StdError_G617 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G617, ES = "LRRi")
StdError_G617$Ratio <- log(Ratios_NAT$G617[c(41:60)])
StdError_G617$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G617$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G617$Species <- "G617"

StdError_G621 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G621, ES = "LRRi")
StdError_G621$Ratio <- log(Ratios_NAT$G621[c(41:60)])
StdError_G621$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G621$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G621$Species <- "G621"

StdError_G627 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G627, ES = "LRRi")
StdError_G627$Ratio <- log(Ratios_NAT$G627[c(41:60)])
StdError_G627$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G627$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G627$Species <- "G627"

StdError_G639 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G639, ES = "LRRi")
StdError_G639$Ratio <- log(Ratios_NAT$G639[c(41:60)])
StdError_G639$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G639$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G639$Species <- "G639"

StdError_G643 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G643, ES = "LRRi")
StdError_G643$Ratio <- log(Ratios_NAT$G643[c(41:60)])
StdError_G643$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G643$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G643$Species <- "G643"

StdError_G649 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G649, ES = "LRRi")
StdError_G649$Ratio <- log(Ratios_NAT$G649[c(41:60)])
StdError_G649$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G649$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G649$Species <- "G649"

StdError_G662 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G662, ES = "LRRi")
StdError_G662$Ratio <- log(Ratios_NAT$G662[c(41:60)])
StdError_G662$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G662$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G662$Species <- "G662"

StdError_G671 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G671, ES = "LRRi")
StdError_G671$Ratio <- log(Ratios_NAT$G671[c(41:60)])
StdError_G671$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G671$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G671$Species <- "G671"

StdError_G674 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G674, ES = "LRRi")
StdError_G674$Ratio <- log(Ratios_NAT$G674[c(41:60)])
StdError_G674$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G674$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G674$Species <- "G674"

StdError_G678 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G678, ES = "LRRi")
StdError_G678$Ratio <- log(Ratios_NAT$G678[c(41:60)])
StdError_G678$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G678$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G678$Species <- "G678"

StdError_G685 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G685, ES = "LRRi")
StdError_G685$Ratio <- log(Ratios_NAT$G685[c(41:60)])
StdError_G685$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G685$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G685$Species <- "G685"

StdError_G689 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G689, ES = "LRRi")
StdError_G689$Ratio <- log(Ratios_NAT$G689[c(41:60)])
StdError_G689$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G689$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G689$Species <- "G689"

StdError_G699 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(41:60)])
StdError_G699$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G699$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G699$Species <- "G699"

StdError_G702 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G702, ES = "LRRi")
StdError_G702$Ratio <- log(Ratios_NAT$G702[c(41:60)])
StdError_G702$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G702$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G702$Species <- "G702"

StdError_G703 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G703, ES = "LRRi")
StdError_G703$Ratio <- log(Ratios_NAT$G703[c(41:60)])
StdError_G703$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G703$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G703$Species <- "G703"

StdError_G707 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G707, ES = "LRRi")
StdError_G707$Ratio <- log(Ratios_NAT$G707[c(41:60)])
StdError_G707$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G707$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G707$Species <- "G707"

StdError_G709 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G709, ES = "LRRi")
StdError_G709$Ratio <- log(Ratios_NAT$G709[c(41:60)])
StdError_G709$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G709$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G709$Species <- "G709"

StdError_G710 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G710, ES = "LRRi")
StdError_G710$Ratio <- log(Ratios_NAT$G710[c(41:60)])
StdError_G710$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G710$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G710$Species <- "G710"

StdError_G711 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G711, ES = "LRRi")
StdError_G711$Ratio <- log(Ratios_NAT$G711[c(41:60)])
StdError_G711$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G711$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G711$Species <- "G711"

StdError_G720 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G720, ES = "LRRi")
StdError_G720$Ratio <- log(Ratios_NAT$G720[c(41:60)])
StdError_G720$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G720$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G720$Species <- "G720"

StdError_G723 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G723, ES = "LRRi")
StdError_G723$Ratio <- log(Ratios_NAT$G723[c(41:60)])
StdError_G723$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G723$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G723$Species <- "G723"

StdError_G734 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G734, ES = "LRRi")
StdError_G734$Ratio <- log(Ratios_NAT$G734[c(41:60)])
StdError_G734$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G734$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G734$Species <- "G734"

StdError_G777 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G777, ES = "LRRi")
StdError_G777$Ratio <- log(Ratios_NAT$G777[c(41:60)])
StdError_G777$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G777$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G777$Species <- "G777"

StdError_G781<- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G781, ES = "LRRi")
StdError_G781$Ratio <- log(Ratios_NAT$G781[c(41:60)])
StdError_G781$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G781$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G781$Species <- "G781"

StdError_G782 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G782, ES = "LRRi")
StdError_G782$Ratio <- log(Ratios_NAT$G782[c(41:60)])
StdError_G782$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G782$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G782$Species <- "G782"

StdError_G783 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G783, ES = "LRRi")
StdError_G783$Ratio <- log(Ratios_NAT$G783[c(41:60)])
StdError_G783$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G783$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G783$Species <- "G783"

StdError_G785<- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G785, ES = "LRRi")
StdError_G785$Ratio <- log(Ratios_NAT$G785[c(41:60)])
StdError_G785$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G785$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G785$Species <- "G785"

StdError_G787 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G787, ES = "LRRi")
StdError_G787$Ratio <- log(Ratios_NAT$G787[c(41:60)])
StdError_G787$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G787$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G787$Species <- "G787"

StdError_G802 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G802, ES = "LRRi")
StdError_G802$Ratio <- log(Ratios_NAT$G802[c(41:60)])
StdError_G802$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G802$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G802$Species <- "G802"

StdError_G807 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G807, ES = "LRRi")
StdError_G807$Ratio <- log(Ratios_NAT$G807[c(41:60)])
StdError_G807$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G807$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G807$Species <- "G807"

StdError_G809 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G809, ES = "LRRi")
StdError_G809$Ratio <- log(Ratios_NAT$G809[c(41:60)])
StdError_G809$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G809$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G809$Species <- "G809"

StdError_G812 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G812, ES = "LRRi")
StdError_G812$Ratio <- log(Ratios_NAT$G812[c(41:60)])
StdError_G812$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G812$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G812$Species <- "G812"

StdError_G825 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G825, ES = "LRRi")
StdError_G825$Ratio <- log(Ratios_NAT$G825[c(41:60)])
StdError_G825$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G825$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G825$Species <- "G825"

StdError_G826 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G826, ES = "LRRi")
StdError_G826$Ratio <- log(Ratios_NAT$G826[c(41:60)])
StdError_G826$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G826$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G826$Species <- "G826"

StdError_G831 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G831, ES = "LRRi")
StdError_G831$Ratio <- log(Ratios_NAT$G831[c(41:60)])
StdError_G831$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G831$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G831$Species <- "G831"

StdError_G835 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G835, ES = "LRRi")
StdError_G835$Ratio <- log(Ratios_NAT$G835[c(41:60)])
StdError_G835$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G835$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G835$Species <- "G835"

StdError_G836 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G836, ES = "LRRi")
StdError_G836$Ratio <- log(Ratios_NAT$G836[c(41:60)])
StdError_G836$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G836$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G836$Species <- "G836"

StdError_G837 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G837, ES = "LRRi")
StdError_G837$Ratio <- log(Ratios_NAT$G837[c(41:60)])
StdError_G837$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G837$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G837$Species <- "G837"

StdError_G840 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G840, ES = "LRRi")
StdError_G840$Ratio <- log(Ratios_NAT$G840[c(41:60)])
StdError_G840$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G840$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G840$Species <- "G840"

StdError_G845 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G845, ES = "LRRi")
StdError_G845$Ratio <- log(Ratios_NAT$G845[c(41:60)])
StdError_G845$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G845$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G845$Species <- "G845"

StdError_G849 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G849, ES = "LRRi")
StdError_G849$Ratio <- log(Ratios_NAT$G849[c(41:60)])
StdError_G849$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G849$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G849$Species <- "G849"

StdError_G851 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G851, ES = "LRRi")
StdError_G851$Ratio <- log(Ratios_NAT$G851[c(41:60)])
StdError_G851$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G851$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G851$Species <- "G851"

StdError_G857 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G857, ES = "LRRi")
StdError_G857$Ratio <- log(Ratios_NAT$G857[c(41:60)])
StdError_G857$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G857$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G857$Species <- "G857"

StdError_G863 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G863, ES = "LRRi")
StdError_G863$Ratio <- log(Ratios_NAT$G863[c(41:60)])
StdError_G863$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G863$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G863$Species <- "G863"

StdError_G869 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G869, ES = "LRRi")
StdError_G869$Ratio <- log(Ratios_NAT$G869[c(41:60)])
StdError_G869$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G869$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G869$Species <- "G869"

StdError_G899 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G899, ES = "LRRi")
StdError_G899$Ratio <- log(Ratios_NAT$G899[c(41:60)])
StdError_G899$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G899$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G899$Species <- "G899"

StdError_G908 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G908, ES = "LRRi")
StdError_G908$Ratio <- log(Ratios_NAT$G908[c(41:60)])
StdError_G908$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G908$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G908$Species <- "G908"

StdError_G910 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G910, ES = "LRRi")
StdError_G910$Ratio <- log(Ratios_NAT$G910[c(41:60)])
StdError_G910$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G910$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G910$Species <- "G910"

StdError_G911 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G911, ES = "LRRi")
StdError_G911$Ratio <- log(Ratios_NAT$G911[c(41:60)])
StdError_G911$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G911$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G911$Species <- "G911"

StdError_G914 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G914, ES = "LRRi")
StdError_G914$Ratio <- log(Ratios_NAT$G914[c(41:60)])
StdError_G914$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G914$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G914$Species <- "G914"

StdError_G915 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G915, ES = "LRRi")
StdError_G915$Ratio <- log(Ratios_NAT$G915[c(41:60)])
StdError_G915$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G915$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G915$Species <- "G915"

StdError_G923 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G923, ES = "LRRi")
StdError_G923$Ratio <- log(Ratios_NAT$G923[c(41:60)])
StdError_G923$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G923$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G923$Species <- "G923"

StdError_G929 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G929, ES = "LRRi")
StdError_G929$Ratio <- log(Ratios_NAT$G929[c(41:60)])
StdError_G929$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G929$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G929$Species <- "G929"

StdError_G936 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G936, ES = "LRRi")
StdError_G936$Ratio <- log(Ratios_NAT$G936[c(41:60)])
StdError_G936$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G936$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G936$Species <- "G936"

StdError_G937 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G937, ES = "LRRi")
StdError_G937$Ratio <- log(Ratios_NAT$G937[c(41:60)])
StdError_G937$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G937$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G937$Species <- "G937"

StdError_G938 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G938, ES = "LRRi")
StdError_G938$Ratio <- log(Ratios_NAT$G938[c(41:60)])
StdError_G938$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G938$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G938$Species <- "G938"

StdError_G939 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G939, ES = "LRRi")
StdError_G939$Ratio <- log(Ratios_NAT$G939[c(41:60)])
StdError_G939$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G939$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G939$Species <- "G939"

StdError_G940 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G940, ES = "LRRi")
StdError_G940$Ratio <- log(Ratios_NAT$G940[c(41:60)])
StdError_G940$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G940$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G940$Species <- "G940"

StdError_G941 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G941, ES = "LRRi")
StdError_G941$Ratio <- log(Ratios_NAT$G941[c(41:60)])
StdError_G941$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G941$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G941$Species <- "G941"

StdError_G942 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G942, ES = "LRRi")
StdError_G942$Ratio <- log(Ratios_NAT$G942[c(41:60)])
StdError_G942$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G942$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G942$Species <- "G942"

StdError_G945 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G945, ES = "LRRi")
StdError_G945$Ratio <- log(Ratios_NAT$G945[c(41:60)])
StdError_G945$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G945$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G945$Species <- "G945"

StdError_G947 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G947, ES = "LRRi")
StdError_G947$Ratio <- log(Ratios_NAT$G947[c(41:60)])
StdError_G947$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G947$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G947$Species <- "G947"

StdError_G948 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G948, ES = "LRRi")
StdError_G948$Ratio <- log(Ratios_NAT$G948[c(41:60)])
StdError_G948$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G948$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G948$Species <- "G948"

StdError_G951 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G951, ES = "LRRi")
StdError_G951$Ratio <- log(Ratios_NAT$G951[c(41:60)])
StdError_G951$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G951$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G951$Species <- "G951"

StdError_G952 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G952, ES = "LRRi")
StdError_G952$Ratio <- log(Ratios_NAT$G952[c(41:60)])
StdError_G952$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G952$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G952$Species <- "G952"

StdError_G953 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G953, ES = "LRRi")
StdError_G953$Ratio <- log(Ratios_NAT$G953[c(41:60)])
StdError_G953$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G953$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G953$Species <- "G953"

StdError_G954 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G954, ES = "LRRi")
StdError_G954$Ratio <- log(Ratios_NAT$G954[c(41:60)])
StdError_G954$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G954$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G954$Species <- "G954"

StdError_G955 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G955, ES = "LRRi")
StdError_G955$Ratio <- log(Ratios_NAT$G955[c(41:60)])
StdError_G955$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G955$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G955$Species <- "G955"

StdError_G956 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G956, ES = "LRRi")
StdError_G956$Ratio <- log(Ratios_NAT$G956[c(41:60)])
StdError_G956$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G956$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G956$Species <- "G956"

StdError_G957 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G957, ES = "LRRi")
StdError_G957$Ratio <- log(Ratios_NAT$G957[c(41:60)])
StdError_G957$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G957$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G957$Species <- "G957"

StdError_G958 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G958, ES = "LRRi")
StdError_G958$Ratio <- log(Ratios_NAT$G958[c(41:60)])
StdError_G958$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G958$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G958$Species <- "G958"

StdError_G959 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G959, ES = "LRRi")
StdError_G959$Ratio <- log(Ratios_NAT$G959[c(41:60)])
StdError_G959$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G959$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G959$Species <- "G959"

StdError_G982 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G982, ES = "LRRi")
StdError_G982$Ratio <- log(Ratios_NAT$G982[c(41:60)])
StdError_G982$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G982$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G982$Species <- "G982"

StdError_G983 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G983, ES = "LRRi")
StdError_G983$Ratio <- log(Ratios_NAT$G983[c(41:60)])
StdError_G983$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G983$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G983$Species <- "G983"

StdError_G986 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G986, ES = "LRRi")
StdError_G986$Ratio <- log(Ratios_NAT$G986[c(41:60)])
StdError_G986$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G986$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G986$Species <- "G986"

StdError_G987 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = G987, ES = "LRRi")
StdError_G987$Ratio <- log(Ratios_NAT$G987[c(41:60)])
StdError_G987$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_G987$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_G987$Species <- "G987"

StdError_P001 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P001, ES = "LRRi")
StdError_P001$Ratio <- log(Ratios_NAT$P001[c(41:60)])
StdError_P001$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P001$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P001$Species <- "P001"

StdError_P002 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P002, ES = "LRRi")
StdError_P002$Ratio <- log(Ratios_NAT$P002[c(41:60)])
StdError_P002$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P002$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P002$Species <- "P002"

StdError_P003 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P003, ES = "LRRi")
StdError_P003$Ratio <- log(Ratios_NAT$P003[c(41:60)])
StdError_P003$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P003$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P003$Species <- "P003"

StdError_P004 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P004, ES = "LRRi")
StdError_P004$Ratio <- log(Ratios_NAT$P004[c(41:60)])
StdError_P004$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P004$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P004$Species <- "P004"

StdError_P007 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P007, ES = "LRRi")
StdError_P007$Ratio <- log(Ratios_NAT$P007[c(41:60)])
StdError_P007$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P007$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P007$Species <- "P007"

StdError_P008 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P008, ES = "LRRi")
StdError_P008$Ratio <- log(Ratios_NAT$P008[c(41:60)])
StdError_P008$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P008$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P008$Species <- "P008"

StdError_P009 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P009, ES = "LRRi")
StdError_P009$Ratio <- log(Ratios_NAT$P009[c(41:60)])
StdError_P009$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P009$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P009$Species <- "P009"

StdError_P011 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P011, ES = "LRRi")
StdError_P011$Ratio <- log(Ratios_NAT$P011[c(41:60)])
StdError_P011$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P011$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P011$Species <- "P011"

StdError_P013 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P013, ES = "LRRi")
StdError_P013$Ratio <- log(Ratios_NAT$P013[c(41:60)])
StdError_P013$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P013$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P013$Species <- "P013"

StdError_P016 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P016, ES = "LRRi")
StdError_P016$Ratio <- log(Ratios_NAT$P016[c(41:60)])
StdError_P016$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P016$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P016$Species <- "P016"

StdError_P017 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = P017, ES = "LRRi")
StdError_P017$Ratio <- log(Ratios_NAT$P017[c(41:60)])
StdError_P017$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_P017$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_P017$Species <- "P017"

StdError_S003 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = S003, ES = "LRRi")
StdError_S003$Ratio <- log(Ratios_NAT$S003[c(41:60)])
StdError_S003$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_S003$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_S003$Species <- "S003"

StdError_S006 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = S006, ES = "LRRi")
StdError_S006$Ratio <- log(Ratios_NAT$S006[c(41:60)])
StdError_S006$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_S006$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_S006$Species <- "S006"

StdError_S008 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = S008, ES = "LRRi")
StdError_S008$Ratio <- log(Ratios_NAT$S008[c(41:60)])
StdError_S008$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_S008$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_S008$Species <- "S008"

StdError_S009 <- batch_calc_ES(dat = CyprusNAT, grouping = StationCode, condition = Season, 
                               outcome = S009, ES = "LRRi")
StdError_S009$Ratio <- log(Ratios_NAT$S009[c(41:60)])
StdError_S009$Location <- c(rep("Akrotori",10),rep("Cape Greco",10))
StdError_S009$Habitat <- c(rep("Posidonia meadow",5),rep("Hard-bottom",5),
                      rep("Posidonia meadow",5),rep("Hard-bottom",5))
StdError_S009$Species <- "S009"

StdError_NAT<- rbind(StdError_B004,StdError_B005,StdError_B006,StdError_B013,StdError_B014,StdError_B016,StdError_B019,
                     StdError_B020,StdError_B022,StdError_B023,StdError_B028,StdError_B033,StdError_B034,StdError_B044,
                     StdError_B045,StdError_B046,StdError_B049,StdError_B051,StdError_B052,StdError_B055,StdError_B060,
                     StdError_B065,StdError_B066,StdError_B068,StdError_B074,StdError_B088,StdError_B089,StdError_B111,
                     StdError_B121,StdError_B123,StdError_B125,StdError_B128,StdError_B138,StdError_B139,StdError_B140,
                     StdError_B141,StdError_B142,StdError_B143,StdError_B144,StdError_B147,StdError_B148,StdError_B150,
                     StdError_B152,StdError_B154,StdError_B160,StdError_B161,StdError_B167,StdError_B176,StdError_B184,
                     StdError_B188,StdError_B189,StdError_B247,StdError_B260,StdError_B263,StdError_B269,StdError_B276,
                     StdError_B284,StdError_B298,StdError_B302,StdError_B312,StdError_B313,StdError_B317,StdError_B328,
                     StdError_B332,StdError_B341,StdError_B450,StdError_B454,StdError_B475,StdError_B504,StdError_B510,
                     StdError_B517,StdError_G010,StdError_G013,StdError_G017,StdError_G020,StdError_G021,StdError_G026,
                     StdError_G027,StdError_G029,StdError_G030,StdError_G032,StdError_G036,StdError_G037,StdError_G041,
                     StdError_G044,StdError_G046,StdError_G047,StdError_G048,StdError_G050,StdError_G051,StdError_G053,
                     StdError_G057,StdError_G064,StdError_G073,StdError_G074,StdError_G075,StdError_G077,StdError_G083,
                     StdError_G094,StdError_G097,StdError_G100,StdError_G101,StdError_G105,StdError_G109,StdError_G119,
                     StdError_G139,StdError_G140,StdError_G142,StdError_G144,StdError_G150,StdError_G151,StdError_G152,
                     StdError_G153,StdError_G154,StdError_G155,StdError_G163,StdError_G165,StdError_G167,StdError_G168,
                     StdError_G169,StdError_G170,StdError_G172,StdError_G175,StdError_G176,StdError_G177,StdError_G178,
                     StdError_G179,StdError_G180,StdError_G181,StdError_G183,StdError_G184,StdError_G185,StdError_G186,
                     StdError_G187,StdError_G188,StdError_G189,StdError_G191,StdError_G192, StdError_G194,StdError_G197,
                     StdError_G198,StdError_G199,StdError_G200,StdError_G202,StdError_G203,StdError_G205,StdError_G207,
                     StdError_G208,StdError_G209,StdError_G212,StdError_G214,StdError_G215,StdError_G216,StdError_G219,
                     StdError_G221,StdError_G245,StdError_G248,StdError_G250,StdError_G255,StdError_G266,StdError_G271,
                     StdError_G275,StdError_G284,StdError_G287,StdError_G289,StdError_G292,StdError_G294,StdError_G295,
                     StdError_G299,StdError_G304,StdError_G305,StdError_G330,StdError_G332,StdError_G336,StdError_G337,
                     StdError_G338,StdError_G339,StdError_G340,StdError_G341,StdError_G342,StdError_G345,StdError_G347,
                     StdError_G350,StdError_G351,StdError_G362,StdError_G363,StdError_G364,StdError_G367,StdError_G422,
                     StdError_G423,StdError_G424,StdError_G428,StdError_G433,StdError_G447,StdError_G448,StdError_G449,
                     StdError_G450,StdError_G451,StdError_G452,StdError_G457,StdError_G459,StdError_G464,StdError_G466,
                     StdError_G475,StdError_G476,StdError_G481,StdError_G482,StdError_G483,StdError_G485,StdError_G487,
                     StdError_G488,StdError_G491,StdError_G482,StdError_G498,StdError_G499,StdError_G501,StdError_G518,
                     StdError_G519,StdError_G551,StdError_G563,StdError_G566,StdError_G572,StdError_G574,StdError_G578,
                     StdError_G588,StdError_G589,StdError_G592,StdError_G598,StdError_G602,StdError_G603,StdError_G613,
                     StdError_G615,StdError_G617,StdError_G621,StdError_G627,StdError_G639,StdError_G643,StdError_G649,
                     StdError_G662,StdError_G671,StdError_G674,StdError_G678,StdError_G685,StdError_G689,StdError_G699,
                     StdError_G702,StdError_G703,StdError_G707,StdError_G709,StdError_G710,StdError_G711,StdError_G720,
                     StdError_G723,StdError_G734,StdError_G777,StdError_G781,StdError_G782,StdError_G783,StdError_G785,
                     StdError_G787,StdError_G802,StdError_G807,StdError_G809,StdError_G812,StdError_G825,StdError_G826,
                     StdError_G831,StdError_G835,StdError_G836,StdError_G837,StdError_G840,StdError_G845,StdError_G849,
                     StdError_G851,StdError_G857,StdError_G863,StdError_G869,StdError_G899,StdError_G908,StdError_G910,
                     StdError_G911,StdError_G914,StdError_G915,StdError_G923,StdError_G929,StdError_G936,StdError_G937,
                     StdError_G938,StdError_G939,StdError_G940,StdError_G941,StdError_G942,StdError_G945,StdError_G947,
                     StdError_G956,StdError_G957,StdError_G958,StdError_G959,StdError_G982,StdError_G983,StdError_G986,
                     StdError_G987,StdError_P001,StdError_P002,StdError_P003,StdError_P004,StdError_P007,StdError_P008,
                     StdError_P009,StdError_P011,StdError_P013,StdError_P016,StdError_P017,StdError_S003,StdError_S006,
                     StdError_S008,StdError_S009)

StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
colnames(StdError_NAT)[1] <- "Sites"
Cyprus_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9,10,11)],
                                  StdError_NAT[c(1,4,7,8,9,10,11)])
