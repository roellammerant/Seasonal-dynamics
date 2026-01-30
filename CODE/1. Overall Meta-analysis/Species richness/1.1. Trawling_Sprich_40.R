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
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'reshape2', 'dplyr','data.table','SingleCaseES')
ipak(packages)


##%######################################################%##
#                                                          #
####        Load Trawling dataset and summarise         ####
#                                                          #
##%######################################################%##
MetaTrawling <- read.csv("DATA/Trawling_meta.csv", 
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

#%######################################################%##
#                                                          #
####                  Sprich Trawling                   ####
#                                                          #
##%######################################################%##
### subset species and categories
TrawlingCategories <- TrawlingAll_Season[c(1, 172:179)]
TrawlingSpecies <- TrawlingAll_Season[-c(1, 172:179)]

# Change structure around: Species become rows and sites become columns
TrawlingTranspose <- t(TrawlingSpecies)
TrawlingTranspose <- as.data.frame(TrawlingTranspose)
TrawlingTranspose$Invasive <- SpTrawling$`Indegenous/Invasive(0/1)`

# standardized species richness
TrawlingInvasive  <- TrawlingTranspose[which(TrawlingTranspose$Invasive  =='1'),]
TrawlingInvStandardized<- TrawlingInvasive[-c(54)]
TrawlingInvStandardized <- as.matrix(TrawlingInvStandardized)
TrawlingInvSp_Cov<- iNEXT::DataInfo(TrawlingInvStandardized, datatype = "abundance")
summary(TrawlingInvSp_Cov)
InvSprich <- estimateD(TrawlingInvStandardized, q = c(0), datatype = "abundance", 
                       base="coverage",level = 0.54)
colnames(InvSprich)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

TrawlingIndigenous <- TrawlingTranspose[which(TrawlingTranspose$Invasive =='0'),]
TrawlingIndStandardized <-TrawlingIndigenous[-c(54)]
TrawlingIndStandardized <- as.matrix(TrawlingIndStandardized)
TrawlingIndSp_Cov<- iNEXT::DataInfo(TrawlingIndStandardized, datatype = "abundance")
summary(TrawlingIndSp_Cov) 
IndSprich <- estimateD(TrawlingIndStandardized, q = c(0), datatype = "abundance", base="coverage",
                       level = 0.54)
colnames(IndSprich)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")


# Link back to the other categories
TrawlingCategories$match <- colnames(TrawlingInvasive[-c(54)])
TrawlingCategories <- merge(TrawlingCategories, IndSprich,  by.x = "match", by.y = "Assemblage", all.x = TRUE)
TrawlingCategories <- merge(TrawlingCategories, InvSprich,  by.x = "match", by.y = "Assemblage", all.x = TRUE)
TrawlingCategories <-  TrawlingCategories[c(2:8, 15:17, 22:24) ]

# Remove hauls with low coverage 10,13,48 (2000_79, 2008_174, 2008_7)
TrawlingCategories <- TrawlingCategories[-c(10,13,48),]

### Create categories to merch replicates
TrawlingCategories$Merched_Category <- factor(paste(TrawlingCategories$Period, 
                                                    TrawlingCategories$Season,
                                                sep = "_"))

### Mean values per Year*Season 
Mean_NIS <- aggregate(TrawlingCategories$SpRichNIS, 
                      list(TrawlingCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(TrawlingCategories$SpRichNAT, 
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
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(7:9)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = TrawlingCategories, grouping = Period, condition = Season, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(7:9)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Trawling40_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Trawling40_Comm_SpRich$Ratio <- log(Trawling40_Comm_SpRich$Ratio)
