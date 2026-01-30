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
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'data.table', 'dplyr','SingleCaseES')
ipak(packages)


#%######################################################%##
#                                                          #
####       Load Cyprus dataset and summarise            ####
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

### Create categories to merch replicate
CyprusTranspose <- t(CyprusSpecies)
CyprusTranspose <- as.data.frame(CyprusTranspose)
colnames(CyprusTranspose) <- CyprusTranspose[1,]
CyprusTranspose <- CyprusTranspose[-c(1,2),]
CyprusTranspose <- CyprusTranspose %>% mutate_if(is.character, as.numeric)
CyprusTranspose$Merched_Replicates <- CyprusCategories$Merched_Category
CyprusSpeciesMerched <- aggregate(CyprusTranspose[-c(148)], by=list(Category=CyprusTranspose$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
CyprusMerchedTranspose <- t(CyprusSpeciesMerched[-c(1)])
CyprusMerchedTranspose <- as.data.frame(CyprusMerchedTranspose)
CyprusMerchedTranspose$Invasive <- CyprusSpecies$Alien

# delete sites with no species present
CyprusInvasive  <- CyprusMerchedTranspose[which(CyprusMerchedTranspose$Invasive  =='Yes'),]
CyprusInvasive <- CyprusInvasive[-c(9,10,19,20)]
CyprusInv_Cov <- iNEXT::DataInfo(CyprusInvasive, datatype = "abundance") 
CyprusInv_Cov$Category <- CyprusSpeciesMerched$Category[-c(9,10,19,20)]
summary(CyprusInv_Cov)
CyprusInvSp <- estimateD(CyprusInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.9458)
colnames(CyprusInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

CyprusIndigenous  <- CyprusMerchedTranspose[which(CyprusMerchedTranspose$Invasive  =='No'),]
CyprusIndigenous <- CyprusIndigenous[-c(9,10,19,20)]
CyprusInd_Cov <- iNEXT::DataInfo(CyprusIndigenous, datatype = "abundance") 
CyprusInd_Cov$Category <- CyprusSpeciesMerched$Category[-c(9,10,19,20)]
summary(CyprusInd_Cov)
CyprusIndSp <- estimateD(CyprusIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.9458)
colnames(CyprusIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
CyprusSpeciesMerched$Match <- colnames(CyprusMerchedTranspose[-c(20)])

CyprusSpeciesMerched <- CyprusSpeciesMerched[c(1,149)]
CyprusSpeciesMerched <- merge(CyprusSpeciesMerched, CyprusIndSp,  
                              by.x = "Match", by.y = "Assemblage", all.x = TRUE)
CyprusSpeciesMerched <- merge(CyprusSpeciesMerched, CyprusInvSp, 
                              by.x = "Match", by.y = "Assemblage", all.x = TRUE)

CyprusSpeciesMerched <-  CyprusSpeciesMerched[c(2,7:9, 14:16)]

CyprusCategories$Replicate <- rep(c("1","2","3"),19)
CyprusCategoriesSubset <- CyprusCategories[which(CyprusCategories$Replicate  =='1'),]
CyprusSpeciesMerched <- merge(CyprusSpeciesMerched, CyprusCategoriesSubset[c(2,3,4)],  
                              by.x = "Category", by.y = "Merched_Category", all.x = TRUE)

# Take out 30m samples
CyprusSpeciesMerched <- CyprusSpeciesMerched[-c(9,10,19),]


### Create categories to merch replicates
CyprusSpeciesMerched$Location <- c("KPos","KPos","KPos","KPos","KPos","KPos","KPos","KPos",
                                   "KR","KR","KR","KR","KR","KR","KR","KR")
CyprusSpeciesMerched$Merched_Category <- factor(paste(CyprusSpeciesMerched$Location, 
                                                      CyprusSpeciesMerched$Season,
                                                      sep = "_"))

### Mean values per Year*Season 
Mean_NIS <- aggregate(CyprusSpeciesMerched$SpRichNIS, 
                      list(CyprusSpeciesMerched$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(CyprusSpeciesMerched$SpRichNAT, 
                      list(CyprusSpeciesMerched$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$RatioKPos <-Ratios_NIS$KPos_Autumn/Ratios_NIS$KPos_Spring
Ratios_NIS$RatioKR <-Ratios_NIS$KR_Autumn/Ratios_NIS$KR_Spring
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$RatioKPos <-Ratios_NAT$KPos_Autumn/Ratios_NAT$KPos_Spring
Ratios_NAT$RatioKR <-Ratios_NAT$KR_Autumn/Ratios_NAT$KR_Spring
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_NIS <- batch_calc_ES(dat = CyprusSpeciesMerched, grouping = Location, condition = Season, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(5:6)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = CyprusSpeciesMerched, grouping = Location, condition = Season, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(5:6)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
CyprusN_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
CyprusN_Comm_SpRich$Ratio <- log(CyprusN_Comm_SpRich$Ratio)
