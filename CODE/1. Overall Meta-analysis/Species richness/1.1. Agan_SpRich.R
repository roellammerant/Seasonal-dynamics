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
####        Load Agan dataset and summarise             ####
#                                                          #
##%######################################################%##
Agan <- read.csv("DATA/AGAN.csv", 
                 na = "NA")
SpAganSoft <- read_excel("output_data/AGAN_InvasiveStatus.xlsx", 
                         na = "NA")

### turn data into presence absence
AganCategories <- Agan[c(2:5)]
AganSpecies <- Agan[-c(1:5)]

### Create categories to merch replicates
AganCategories$Merched_Replicates <- factor(paste(AganCategories$eventYear, 
                                                  AganCategories$eventSeason,
                                                  AganCategories$StationCode,
                                                  sep = "_"))
### Create categories to merch replicate
AganSpecies$Merched_Replicates <- AganCategories$Merched_Replicates
AganSpeciesMerched <- aggregate(AganSpecies[-c(106)], by=list(Category=AganSpecies$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
AganTranspose <- t(AganSpeciesMerched[,-c(1)])
AganTranspose <- as.data.frame(AganTranspose)
AganTranspose$Invasive <- SpAganSoft$`Indigenous/Invasive (0/1)`

# Standardized species richness
# delete sites with no species present
AganInvasive  <- AganTranspose[which(AganTranspose$Invasive  =='1'),]
AganInvasive <- AganInvasive %>% mutate_if(is.character, as.numeric)
AganInvasive <- AganInvasive[-c(35,39,41,106,109,110, # zero species
                                12,14,16,19,20,21,22,23,24,36,38,66,88, #one species
                                10,90,95,82,107,120,133)] # equal N and S
AganInv_Cov <- iNEXT::DataInfo(AganInvasive, datatype = "abundance") 
AganInv_Cov$Category <- AganSpeciesMerched$Category[-c(35,39,41,106,109,110, # zero species
                                                       12,14,16,19,20,21,22,23,24,36,38,66,88, #one species
                                                       10,90,95,82,107,120)] # equal N and S
summary(AganInv_Cov)
AganInvSp <- estimateD(AganInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.5435)
colnames(AganInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

AganIndigenous <- AganTranspose[which(AganTranspose$Invasive =='0'),]
AganIndigenous <- AganIndigenous %>% mutate_if(is.character, as.numeric)
AganIndigenous <- AganIndigenous[-c(6,10,53,55,  #one species
                                    59, # really low coverage
                                    54,64,133)]# equal N and S
AganInd_Cov <- iNEXT::DataInfo(AganIndigenous, datatype = "abundance") 
AganInd_Cov$Category <- AganSpeciesMerched$Category[-c(6,10,53,55,  #one species
                                                       59, # really low coverage
                                                       54,64)]# equal N and S
summary(AganInd_Cov)
AganIndSp <- estimateD(AganIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.5435)
colnames(AganIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
AganSpeciesMerched$Match <- colnames(AganTranspose[-c(133)])
AganSpeciesMerched <- AganSpeciesMerched[c(1, 107)]
AganSpeciesMerched <- merge(AganSpeciesMerched, AganIndSp,  by.x = "Match", by.y = "Assemblage", all.x = TRUE)
AganSpeciesMerched <- merge(AganSpeciesMerched, AganInvSp,  by.x = "Match", by.y = "Assemblage", all.x = TRUE)

AganSpeciesMerched <-  AganSpeciesMerched[c(1,2,7,14) ]
colnames(AganSpeciesMerched)[2] <- c("Category")
AganSpeciesMerched[is.na(AganSpeciesMerched)] <- 0
AganCategoriesSubset <- AganCategories[which(AganCategories$Replicate  =='1'),]
AganSpeciesMerched <- merge(AganSpeciesMerched, AganCategoriesSubset[c(1:3, 5)],  by.x = "Category", by.y = "Merched_Replicates", all.x = TRUE)

# Only leave replicates with a zero when actually no individuals were present
# Clean out data that had only one species or the same species as number of individuals
# Loss of 26 replicates after cleaning data 

AganSpeciesMerched <- AganSpeciesMerched[-c( 12,14,16,19,20,21,22,23,24,36,38,66,88, 
                                             10,90,95,82,107,120,
                                             6,10,53,55,59,54,64),] 

### Create categories to merch replicates
AganSpeciesMerched$Merched_Category <- factor(paste(AganSpeciesMerched$eventYear, 
                                                    AganSpeciesMerched$eventSeason,
                                                sep = "_"))
### Mean values per Year*Season 
Mean_NIS <- aggregate(AganSpeciesMerched$SpRichNIS, 
                      list(AganSpeciesMerched$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(AganSpeciesMerched$SpRichNAT, 
                      list(AganSpeciesMerched$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio2007 <-Ratios_NIS$`2007_Autumn`/Ratios_NIS$`2007_Spring`
Ratios_NIS$Ratio2008 <-Ratios_NIS$`2008_Autumn`/Ratios_NIS$`2008_Spring`
Ratios_NIS$Ratio2009 <-Ratios_NIS$`2009_Autumn`/Ratios_NIS$`2009_Spring`
Ratios_NIS$Ratio2010 <-Ratios_NIS$`2010_Autumn`/Ratios_NIS$`2010_Spring`
Ratios_NIS$Ratio2011<-Ratios_NIS$`2011_Autumn`/Ratios_NIS$`2011_Spring`
Ratios_NIS$Ratio2012 <-Ratios_NIS$`2012_Autumn`/Ratios_NIS$`2012_Spring`
Ratios_NIS$Ratio2013 <-Ratios_NIS$`2013_Autumn`/Ratios_NIS$`2013_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),-c(1)]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio2007 <-Ratios_NAT$`2007_Autumn`/Ratios_NAT$`2007_Spring`
Ratios_NAT$Ratio2008 <-Ratios_NAT$`2008_Autumn`/Ratios_NAT$`2008_Spring`
Ratios_NAT$Ratio2009 <-Ratios_NAT$`2009_Autumn`/Ratios_NAT$`2009_Spring`
Ratios_NAT$Ratio2010 <-Ratios_NAT$`2010_Autumn`/Ratios_NAT$`2010_Spring`
Ratios_NAT$Ratio2011<-Ratios_NAT$`2011_Autumn`/Ratios_NAT$`2011_Spring`
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2013 <-Ratios_NAT$`2013_Autumn`/Ratios_NAT$`2013_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_Data <- AganSpeciesMerched[AganSpeciesMerched$eventYear != c('2006'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(15:21)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(15:21)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Agan_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Agan_Comm_SpRich$Ratio <- log(Agan_Comm_SpRich$Ratio)

