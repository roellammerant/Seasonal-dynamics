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
CyprusCategories$Merched_Replicates <- factor(paste(CyprusCategories$Sites, 
                                                    CyprusCategories$Season,
                                                    sep = "_"))
### Create categories to merch replicate
CyprusTranspose <- t(CyprusSpecies)
CyprusTranspose <- as.data.frame(CyprusTranspose)
colnames(CyprusTranspose) <- CyprusTranspose[1,]
CyprusTranspose <- CyprusTranspose[-c(1,2),]
CyprusTranspose$Merched_Replicates <- CyprusCategories$Merched_Replicates
CyprusTranspose <- CyprusTranspose %>% mutate_if(is.character, as.numeric)
CyprusSpeciesMerched <- aggregate(CyprusTranspose[-c(359)], by=list(Category=CyprusTranspose$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
CyprusMerchedTranspose <- t(CyprusSpeciesMerched[-c(1)])
CyprusMerchedTranspose <- as.data.frame(CyprusMerchedTranspose)
CyprusMerchedTranspose$Invasive <- CyprusSpecies$Alien.x

# delete sites with no species present
CyprusInvasive  <- CyprusMerchedTranspose[which(CyprusMerchedTranspose$Invasive  =='Yes'),]
CyprusInvasive <- CyprusInvasive[-c(9,10,20,41)]
CyprusInv_Cov <- iNEXT::DataInfo(CyprusInvasive, datatype = "abundance") 
CyprusInv_Cov$Category <- CyprusSpeciesMerched$Category[-c(9,10,20)]
summary(CyprusInv_Cov)
CyprusInvSp <- estimateD(CyprusInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.7857)
colnames(CyprusInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

CyprusIndigenous  <- CyprusMerchedTranspose[which(CyprusMerchedTranspose$Invasive  =='No'),]
CyprusIndigenous <- CyprusIndigenous[-c(20,41)]
CyprusInd_Cov <- iNEXT::DataInfo(CyprusIndigenous, datatype = "abundance") 
CyprusInd_Cov$Category <- CyprusSpeciesMerched$Category[-c(20)]
summary(CyprusInd_Cov)
CyprusIndSp <- estimateD(CyprusIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.7857)
colnames(CyprusIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
CyprusSpeciesMerched$Match <- colnames(CyprusMerchedTranspose[-c(41)])

CyprusSpeciesMerched <- CyprusSpeciesMerched[c(1,360)]
CyprusSpeciesMerched <- merge(CyprusSpeciesMerched, CyprusIndSp,  
                              by.x = "Match", by.y = "Assemblage", all.x = TRUE)
CyprusSpeciesMerched <- merge(CyprusSpeciesMerched, CyprusInvSp, 
                              by.x = "Match", by.y = "Assemblage", all.x = TRUE)

CyprusSpeciesMerched <-  CyprusSpeciesMerched[c(2,7:9, 14:16) ]
colnames(CyprusSpeciesMerched)[1] <- c("Category")

CyprusCategories$Replicate <- rep(c("1","2","3"),40)
CyprusCategoriesSubset <- CyprusCategories[which(CyprusCategories$Replicate  =='1'),]
CyprusSpeciesMerched <- merge(CyprusSpeciesMerched, CyprusCategoriesSubset[c(2,3,4)],  
                              by.x = "Category", by.y = "Merched_Replicates", all.x = TRUE)

# Take out APos30_Autumn due to only one species for NIS
# Take out AR30_Spring due to zero species for NAT and NIS

CyprusSpeciesMerched <- CyprusSpeciesMerched[-c(9,20),]
CyprusSpeciesMerched[is.na(CyprusSpeciesMerched)] <- 0


### Create categories to merch replicates
CyprusSpeciesMerched$Location <- c("APos","APos","APos","APos","APos","APos","APos","APos","APos",
                                           "AR","AR","AR","AR","AR","AR","AR","AR","AR",
                                           "GPos","GPos","GPos","GPos","GPos","GPos","GPos","GPos","GPos","GPos",
                                           "GR","GR","GR","GR","GR","GR","GR","GR","GR","GR")
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
Ratios_NIS$RatioAPos <-Ratios_NIS$APos_Autumn/Ratios_NIS$APos_Spring
Ratios_NIS$RatioAR <-Ratios_NIS$AR_Autumn/Ratios_NIS$AR_Spring
Ratios_NIS$RatioGPos <-Ratios_NIS$GPos_Autumn/Ratios_NIS$GPos_Spring
Ratios_NIS$RatioGR <-Ratios_NIS$GR_Autumn/Ratios_NIS$GR_Spring
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$RatioAPos <-Ratios_NAT$APos_Autumn/Ratios_NAT$APos_Spring
Ratios_NAT$RatioAR <-Ratios_NAT$AR_Autumn/Ratios_NAT$AR_Spring
Ratios_NAT$RatioGPos <-Ratios_NAT$GPos_Autumn/Ratios_NAT$GPos_Spring
Ratios_NAT$RatioGR <-Ratios_NAT$GR_Autumn/Ratios_NAT$GR_Spring
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_NIS <- batch_calc_ES(dat = CyprusSpeciesMerched, grouping = Location, condition = Season, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(9:12)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = CyprusSpeciesMerched, grouping = Location, condition = Season, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(9:12)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Cyprus_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Cyprus_Comm_SpRich$Ratio <- log(Cyprus_Comm_SpRich$Ratio)
