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
####      Load LessepsianS dataset and summarise        ####
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

### Create Community level abundance columns
LessepsianHTranspose <- t(LessepsianHardSpecies)
LessepsianHTranspose <- as.data.frame(LessepsianHTranspose)
LessepsianHTranspose$Invasive <- SpLessepsianHard$...2

LessepsianHNIS  <- LessepsianHTranspose[which(LessepsianHTranspose$Invasive  =='1'),]
LessepsianHNAT  <- LessepsianHTranspose[which(LessepsianHTranspose$Invasive  =='0'),]
LessepsianHNIS <- LessepsianHNIS %>% 
  mutate_if(is.character, as.numeric)
LessepsianHNAT <- LessepsianHNAT %>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(LessepsianHNIS))
AbundanceNAT <- as.data.frame(colSums(LessepsianHNAT))

LessepsianHardCategories$AbundanceNIS <- AbundanceNIS$`colSums(LessepsianHNIS)`[-c(24)]
LessepsianHardCategories$AbundanceNAT <- AbundanceNAT$`colSums(LessepsianHNAT)`[-c(24)]

### Mean values per Year*Season 
Mean_NIS <- aggregate(LessepsianHardCategories$AbundanceNIS, 
                      list(LessepsianHardCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(LessepsianHardCategories$AbundanceNAT, 
                      list(LessepsianHardCategories$Merched_Category), FUN=mean)
### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$RatioAhk12 <-Ratios_NIS$Ahk12_Autumn/Ratios_NIS$Ahk12_Spring
Ratios_NIS$RatioAhk20 <-Ratios_NIS$Ahk20_Autumn/Ratios_NIS$Ahk20_Spring
Ratios_NIS$RatioAsh12 <-Ratios_NIS$Ash12_Autumn/Ratios_NIS$Ash12_Spring
Ratios_NIS$RatioAhs25 <-Ratios_NIS$Ash25_Autumn/Ratios_NIS$Ash25_Spring
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$RatioAhk12 <-Ratios_NAT$Ahk12_Autumn/Ratios_NAT$Ahk12_Spring
Ratios_NAT$RatioAhk20 <-Ratios_NAT$Ahk20_Autumn/Ratios_NAT$Ahk20_Spring
Ratios_NAT$RatioAsh12 <-Ratios_NAT$Ash12_Autumn/Ratios_NAT$Ash12_Spring
Ratios_NAT$RatioAhs25 <-Ratios_NAT$Ash25_Autumn/Ratios_NAT$Ash25_Spring
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios

StdError_NIS <- batch_calc_ES(dat = LessepsianHardCategories, grouping = StationCode, condition = eventSeason, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(9:12)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = LessepsianHardCategories, grouping = StationCode, condition = eventSeason, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(9:12)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
LessepsianH_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
LessepsianH_Comm_Abundance$Ratio <- log(LessepsianH_Comm_Abundance$Ratio)
