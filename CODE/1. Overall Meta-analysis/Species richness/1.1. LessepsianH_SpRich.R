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
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'dplyr','SingleCaseES')
ipak(packages)


#%######################################################%##
#                                                          #
####             Load Mollusc dataset                   ####
#                                                          #
##%######################################################%##

LessepsianHard <- read.csv("DATA/LessepsianHard.csv", 
                           na = "NA")

SpLessepsianHard <- read_excel("output_data/LessepsianHard_InvasiveStatus.xlsx", 
                               na = "NA")
#Equal Amount of replicates
LessepsianHard <- LessepsianHard[-c(3,6,9,14,17,20,23),]

### Split data up
LessepsianHardCategories <- LessepsianHard[c(2:4)]
LessepsianHardSpecies <- LessepsianHard[-c(1:4)]

### Create categories to merch replicates
LessepsianHardCategories$Merched_Replicates <- factor(paste(LessepsianHardCategories$StationCode, 
                                                            LessepsianHardCategories$eventSeason,
                                                            sep = "_"))

### Create categories to merch replicate
LessepsianHardSpecies$Merched_Replicates <- LessepsianHardCategories$Merched_Replicates
LessepsianHardSpeciesMerched <- aggregate(LessepsianHardSpecies[-c(138)], by=list(Category=LessepsianHardSpecies$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
LessepsianHardTranspose <- t(LessepsianHardSpeciesMerched[-c(1)])
LessepsianHardTranspose <- as.data.frame(LessepsianHardTranspose)
LessepsianHardTranspose$Invasive <- SpLessepsianHard$...2

# delete sites with no species present
LessepsianInvasive  <- LessepsianHardTranspose[which(LessepsianHardTranspose$Invasive  =='1'),]
LessepsianInvasive <- LessepsianInvasive[-c(9)]
LessepsianInv_Cov <- iNEXT::DataInfo(LessepsianInvasive, datatype = "abundance") 
LessepsianInv_Cov$Category <- LessepsianHardSpeciesMerched$Category
summary(LessepsianInv_Cov)
LessepsianInvSp <- estimateD(LessepsianInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.7480)
colnames(LessepsianInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

LessepsianIndigenous <- LessepsianHardTranspose[which(LessepsianHardTranspose$Invasive =='0'),]
LessepsianIndigenous <- LessepsianIndigenous[-c(9)]
LessepsianInd_Cov <- iNEXT::DataInfo(LessepsianIndigenous, datatype = "abundance") 
LessepsianInd_Cov$Category <- LessepsianHardSpeciesMerched$Category
summary(LessepsianInd_Cov)
LessepsianIndSp <- estimateD(LessepsianIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.7480)
colnames(LessepsianIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")


# Link back to the other categories
LessepsianHardSpeciesMerched$Match <- LessepsianIndSp$Assemblage
LessepsianHardSpeciesMerched <- LessepsianHardSpeciesMerched[c(1,139)]
LessepsianHardSpeciesMerched <- merge(LessepsianHardSpeciesMerched, LessepsianIndSp,  
                                      by.x = "Match", by.y = "Assemblage", all.x = TRUE)
LessepsianHardSpeciesMerched <- merge(LessepsianHardSpeciesMerched, LessepsianInvSp, 
                                      by.x = "Match", by.y = "Assemblage", all.x = TRUE)

LessepsianHardSpeciesMerched <-  LessepsianHardSpeciesMerched[c(2,7:9, 14:16) ]
colnames(LessepsianHardSpeciesMerched)[1] <- c("Category")

LessepsianHardCategoriessSubset <- LessepsianHardCategories[which(LessepsianHardCategories$Replicate  =='1'),]
LessepsianHardSpeciesMerched <- merge(LessepsianHardSpeciesMerched, LessepsianHardCategoriessSubset[c(1,3, 4)],  
                                      by.x = "Category", by.y = "Merched_Replicates", all.x = TRUE)

### Create categories to merch replicates
LessepsianHardSpeciesMerched$eventYear <- "2017"
LessepsianHardSpeciesMerched$Merched_Category <- factor(paste(LessepsianHardSpeciesMerched$eventYear, 
                                                              LessepsianHardSpeciesMerched$eventSeason,
                                                              sep = "_"))

### Mean values per Year*Season 
Mean_NIS <- aggregate(LessepsianHardSpeciesMerched$SpRichNIS, 
                      list(LessepsianHardSpeciesMerched$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(LessepsianHardSpeciesMerched$SpRichNAT, 
                      list(LessepsianHardSpeciesMerched$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio <-Ratios_NIS$`2017_Autumn`/Ratios_NIS$`2017_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio <-Ratios_NAT$`2017_Autumn`/Ratios_NAT$`2017_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_NIS <- batch_calc_ES(dat = LessepsianHardSpeciesMerched, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(3)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = LessepsianHardSpeciesMerched, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(3)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
LessepsianH_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
LessepsianH_Comm_SpRich$Ratio <- log(LessepsianH_Comm_SpRich$Ratio)
