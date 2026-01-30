##%######################################################%##
#                                                          #
####        Load R packages and set-up workspace        ####
#                                                          #
##%######################################################%##
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'reshape2', 'dplyr','SingleCaseES')
ipak(packages)

##%######################################################%##
#                                                          #
####        Load Fish dataset and summarise             ####
#                                                          #
##%######################################################%##

MetaTrawling <- read.csv("DATA/Trawling_Meta.csv", 
                         na = "NA")
MetaTrawling <- MetaTrawling [-c(695:696),]
MetaTrawling$Period[MetaTrawling$Period == "1990-4"] <- "1990"
MetaTrawling$Period[MetaTrawling$Period == "2008-12"] <- "2008"
MetaTrawling$id_period_haul <- paste(MetaTrawling$Period, MetaTrawling$Haul.., sep="_")
MetaTrawlingSubset <-  MetaTrawling [c(1,5:7, 18,19,22,35,63)]

Trawling <- read_excel("DATA/Trawling_data.xlsx", 
                       na = "NA")
TrawlingSubset <-reshape2::dcast(Trawling, id_period_haul~species, value.var="amount")

SpTrawling <- read_excel("output_data/Trawling_InvasiveStatus.xlsx", 
                         na = "NA")
SpTrawling <- SpTrawling [c(1,3)]

##%######################################################%##
#                                                          #
####            Match raw and meta data                 ####
#                                                          #
##%######################################################%##


TrawlingAll <- merge(TrawlingSubset, MetaTrawlingSubset,  by.x = "id_period_haul", by.y = "id_period_haul", all.x = TRUE)
colnames(TrawlingAll)[176:177]<- c("Mean_Depth", "Depth_Group")
colnames(TrawlingAll)[179]<- c("LessepsianRelativeAbundance")

### Season -> Spring (month 4-5) and Autumn (month 10-11)
TrawlingAllApril <- TrawlingAll[TrawlingAll$Month == c('4'),]
TrawlingAllMay <- TrawlingAll[TrawlingAll$Month == c('5'),]
TrawlingAllOctober <- TrawlingAll[TrawlingAll$Month == c('10'),]
TrawlingAllNovember <- TrawlingAll[TrawlingAll$Month == c('11'),]
TrawlingAll_Season <- rbind(TrawlingAllApril,TrawlingAllMay,TrawlingAllOctober,TrawlingAllNovember)

#### Subset by depth

TrawlingAll_Season <- TrawlingAll_Season[TrawlingAll_Season$Mean_Depth < 40.3,]

TrawlingAll_Season$Merched_Category <- factor(paste(TrawlingAll_Season$Period, 
                                                    TrawlingAll_Season$Season,
                                                     sep = "_"))
ReplicateNumber <- as.data.frame(table(TrawlingAll_Season$Merched_Category))


#%######################################################%##
#                                                          #
####            Community abundance Trawling            ####
#                                                          #
##%######################################################%##
### subset species and categories
TrawlingCategories <- TrawlingAll_Season[c(1, 172:180)]
TrawlingSpecies <- TrawlingAll_Season[-c(1, 172:180)]

# Change structure around: Species become rows and sites become columns
TrawlingTranspose <- t(TrawlingSpecies)
TrawlingTranspose <- as.data.frame(TrawlingTranspose)
TrawlingTranspose$Invasive <- SpTrawling$`Indegenous/Invasive(0/1)`

# Community level abundance
TrawlingNIS  <- TrawlingTranspose[which(TrawlingTranspose$Invasive  =='1'),]
TrawlingNAT  <- TrawlingTranspose[which(TrawlingTranspose$Invasive  =='0'),]
TrawlingNIS <- TrawlingNIS %>% 
  mutate_if(is.character, as.numeric)
TrawlingNAT <- TrawlingNAT%>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(TrawlingNIS[-c(54)]))
AbundanceNAT <- as.data.frame(colSums(TrawlingNAT[-c(54)]))

TrawlingCategories$AbundanceNIS <- AbundanceNIS$`colSums(TrawlingNIS[-c(54)])`
TrawlingCategories$AbundanceNAT <- AbundanceNAT$`colSums(TrawlingNAT[-c(54)])`

### Mean values per Year*Season 
Mean_NIS <- aggregate(TrawlingCategories$AbundanceNIS, 
                      list(TrawlingCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(TrawlingCategories$AbundanceNAT, 
                      list(TrawlingCategories$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio1990 <-Ratios_NIS$`1990_Autumn`/Ratios_NIS$`1990_Spring`
Ratios_NIS$Ratio2000 <-Ratios_NIS$`2000_Autumn`/Ratios_NIS$`2000_Spring`
Ratios_NIS$Ratio2008 <-Ratios_NIS$`2008_Autumn`/Ratios_NIS$`2008_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio1990 <-Ratios_NAT$`1990_Autumn`/Ratios_NAT$`1990_Spring`
Ratios_NAT$Ratio2000 <-Ratios_NAT$`2000_Autumn`/Ratios_NAT$`2000_Spring`
Ratios_NAT$Ratio2008 <-Ratios_NAT$`2008_Autumn`/Ratios_NAT$`2008_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_NIS <- batch_calc_ES(dat = TrawlingCategories, grouping = Period, condition = Season, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(7:9)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = TrawlingCategories, grouping = Period, condition = Season, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(7:9)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Trawling40_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Trawling40_Comm_Abundance$Ratio <- log(Trawling40_Comm_Abundance$Ratio)
