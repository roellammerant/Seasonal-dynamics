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

CyprusAutumn <- read.csv("DATA/Cyprus_Autumn.csv", 
                         na = "NA")
CyprusSpring <- read.csv("DATA/Cyprus_Spring.csv", 
                         na = "NA")

Cyprus <- merge(CyprusSpring, CyprusAutumn,  by.x = "Code", by.y = "Code", all.x = TRUE)

#%######################################################%##
#                                                          #
####                 Community level                    ####
#                                                          #
##%######################################################%##

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
row.names(CyprusSpecies) <- CyprusSpecies[,1]
CyprusSpeciesB <- CyprusSpecies[-c(1,2)]
CyprusSpeciesB$Invasive <-  CyprusSpecies$Alien.x

CyprusNIS  <- CyprusSpeciesB[which(CyprusSpeciesB$Invasive  =='Yes'),]
CyprusNAT  <- CyprusSpeciesB[which(CyprusSpeciesB$Invasive  =='No'),]
CyprusNIS <- CyprusNIS %>% 
  mutate_if(is.character, as.numeric)
CyprusNAT <- CyprusNAT%>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(CyprusNIS))
AbundanceNAT <- as.data.frame(colSums(CyprusNAT))

CyprusCategories$AbundanceNIS <- AbundanceNIS$`colSums(CyprusNIS)`[-c(121)]
CyprusCategories$AbundanceNAT <- AbundanceNAT$`colSums(CyprusNAT)`[-c(121)]

### Mean values per Year*Season 
Mean_NIS <- aggregate(CyprusCategories$AbundanceNIS, 
                      list(CyprusCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(CyprusCategories$AbundanceNAT, 
                      list(CyprusCategories$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
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
Ratios_NIS$RatioGPos30 <-Ratios_NIS$GPos30_Autumn/Ratios_NIS$GPos30_Spring
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

StdError_NIS <- batch_calc_ES(dat = CyprusCategories, grouping = Sites, condition = Season, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(41:60)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = CyprusCategories, grouping = Sites, condition = Season, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(41:60)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Cyprus_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Cyprus_Comm_Abundance <- na.omit(Cyprus_Comm_Abundance)
Cyprus_Comm_Abundance$Ratio <- log(Cyprus_Comm_Abundance$Ratio)
