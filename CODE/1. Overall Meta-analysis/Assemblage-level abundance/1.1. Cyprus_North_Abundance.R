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

#%######################################################%##
#                                                          #
####                 Community level                    ####
#                                                          #
##%######################################################%##

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
row.names(CyprusSpecies) <- CyprusSpecies[,1]
CyprusSpeciesB <- CyprusSpecies[-c(1,2)]
CyprusSpeciesB$Invasive <-  CyprusSpecies$Alien

CyprusNIS  <- CyprusSpeciesB[which(CyprusSpeciesB$Invasive  =='Yes'),]
CyprusNAT  <- CyprusSpeciesB[which(CyprusSpeciesB$Invasive  =='No'),]
CyprusNIS <- CyprusNIS %>% 
  mutate_if(is.character, as.numeric)
CyprusNAT <- CyprusNAT%>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(CyprusNIS))
AbundanceNAT <- as.data.frame(colSums(CyprusNAT))

CyprusCategories$AbundanceNIS <- AbundanceNIS$`colSums(CyprusNIS)`[-c(58)]
CyprusCategories$AbundanceNAT <- AbundanceNAT$`colSums(CyprusNAT)`[-c(58)]

### Mean values per Site*Season 
Mean_NIS <- aggregate(CyprusCategories$AbundanceNIS, 
                      list(CyprusCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(CyprusCategories$AbundanceNAT, 
                      list(CyprusCategories$Merched_Category), FUN=mean)

### Calculate Ratios
### Take 30m samples out
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(9,10,19)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
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
StdError_Data <- CyprusCategories[CyprusCategories$Sites != c('KR30'),]
StdError_Data <- StdError_Data[StdError_Data$Sites != c('KPos30'),]


StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(17:24)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = Sites, condition = Season, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(17:24)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
CyprusN_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
CyprusN_Comm_Abundance$Ratio <- log(CyprusN_Comm_Abundance$Ratio)
