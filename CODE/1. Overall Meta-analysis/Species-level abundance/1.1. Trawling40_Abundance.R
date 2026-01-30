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

TrawlingAll_Season$Merched_Category <- factor(paste(TrawlingAll_Season$Period, 
                                                    TrawlingAll_Season$Season,
                                                    sep = "_"))
ReplicateNumber <- as.data.frame(table(TrawlingAll_Season$Merched_Category))


#%######################################################%##
#                                                          #
####                  Sprich Trawling                   ####
#                                                          #
##%######################################################%##
### subset species and categories
TrawlingCategories <- TrawlingAll_Season[c(1, 172:180)]
TrawlingSpecies <- TrawlingAll_Season[-c(1, 172:180)]

# Change structure around: Species become rows and sites become columns
TrawlingTranspose <- t(TrawlingSpecies)
TrawlingTranspose <- as.data.frame(TrawlingTranspose)
TrawlingTranspose$Invasive <- SpTrawling$`Indegenous/Invasive(0/1)`

### Invasive Status
TrawlingNIS  <- TrawlingTranspose[which(TrawlingTranspose$Invasive =='1'),]
TrawlingNAT  <- TrawlingTranspose[which(TrawlingTranspose$Invasive  =='0'),]
TrawlingNIS <- TrawlingNIS %>% 
  mutate_if(is.character, as.numeric)
TrawlingNAT <- TrawlingNAT %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
TrawlingNIS <- t(TrawlingNIS)
TrawlingNIS <- as.data.frame(TrawlingNIS)
TrawlingNIS <- TrawlingNIS[-c(54),]
TrawlingNIS$Merched_Category <- TrawlingCategories$Merched_Category
TrawlingNIS <- TrawlingNIS %>% 
  mutate_if(is.character, as.numeric)
Mean_NIS <- aggregate(TrawlingNIS[c(1:39)], 
                      list(TrawlingNIS$Merched_Category), FUN=mean) 

TrawlingNAT <- t(TrawlingNAT)
TrawlingNAT <- as.data.frame(TrawlingNAT)
TrawlingNAT <- TrawlingNAT[-c(54),]
TrawlingNAT$Merched_Category <- TrawlingCategories$Merched_Category
TrawlingNAT <- TrawlingNAT %>% 
  mutate_if(is.character, as.numeric)
Mean_NAT <- aggregate(TrawlingNAT[c(1:131)], 
                      list(TrawlingNAT$Merched_Category), FUN=mean) 


### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$Ratio2008 <-Ratios_NIS$`2008_Autumn`/Ratios_NIS$`2008_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))
colnames(Ratios_NIS) <- c(1:39)

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio2008 <-Ratios_NAT$`2008_Autumn`/Ratios_NAT$`2008_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))
colnames(Ratios_NAT) <- c(1:131)

### StdError values for NIS ratios
TrawlingNIS$Season <- TrawlingCategories$Season
TrawlingNIS$Year <- TrawlingCategories$Period
StdError_DataNIS <- TrawlingNIS[TrawlingNIS$Year != c('1990'),]
StdError_DataNIS <- StdError_DataNIS[StdError_DataNIS$Year != c('2000'),]
StdError_DataNIS[StdError_DataNIS == 0] <- 0.1
colnames(StdError_DataNIS) <- c(1:39,"Category","Season","Year")

StdError_1 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `1`, ES = "LRRi")
StdError_1$Ratio <- log(Ratios_NIS$`1`[c(7)])

StdError_2 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `2`, ES = "LRRi")
StdError_2$Ratio <- log(Ratios_NIS$`2`[c(7)])

StdError_3 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `3`, ES = "LRRi")
StdError_3$Ratio <- log(Ratios_NIS$`3`[c(7)])

StdError_4 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `4`, ES = "LRRi")
StdError_4$Ratio <- log(Ratios_NIS$`4`[c(7)])

StdError_5 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `5`, ES = "LRRi")
StdError_5$Ratio <- log(Ratios_NIS$`5`[c(7)])

StdError_6 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `6`, ES = "LRRi")
StdError_6$Ratio <- log(Ratios_NIS$`6`[c(7)])

StdError_7 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `7`, ES = "LRRi")
StdError_7$Ratio <- log(Ratios_NIS$`7`[c(7)])

StdError_8 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `8`, ES = "LRRi")
StdError_8$Ratio <- log(Ratios_NIS$`8`[c(7)])

StdError_9 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `9`, ES = "LRRi")
StdError_9$Ratio <- log(Ratios_NIS$`9`[c(7)])

StdError_10 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `10`, ES = "LRRi")
StdError_10$Ratio <- log(Ratios_NIS$`10`[c(7)])

StdError_11 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `11`, ES = "LRRi")
StdError_11$Ratio <- log(Ratios_NIS$`11`[c(7)])

StdError_12 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `12`, ES = "LRRi")
StdError_12$Ratio <- log(Ratios_NIS$`12`[c(7)])

StdError_13 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `13`, ES = "LRRi")
StdError_13$Ratio <- log(Ratios_NIS$`13`[c(7)])

StdError_14 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `14`, ES = "LRRi")
StdError_14$Ratio <- log(Ratios_NIS$`14`[c(7)])

StdError_15 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `15`, ES = "LRRi")
StdError_15$Ratio <- log(Ratios_NIS$`15`[c(7)])

StdError_16 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `16`, ES = "LRRi")
StdError_16$Ratio <- log(Ratios_NIS$`16`[c(7)])

StdError_17 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `17`, ES = "LRRi")
StdError_17$Ratio <- log(Ratios_NIS$`17`[c(7)])

StdError_18 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `18`, ES = "LRRi")
StdError_18$Ratio <- log(Ratios_NIS$`18`[c(7)])

StdError_19 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `19`, ES = "LRRi")
StdError_19$Ratio <- log(Ratios_NIS$`19`[c(7)])

StdError_20 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `20`, ES = "LRRi")
StdError_20$Ratio <- log(Ratios_NIS$`20`[c(7)])

StdError_21 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `21`, ES = "LRRi")
StdError_21$Ratio <- log(Ratios_NIS$`21`[c(7)])

StdError_22 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `22`, ES = "LRRi")
StdError_22$Ratio <- log(Ratios_NIS$`22`[c(7)])

StdError_23 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `23`, ES = "LRRi")
StdError_23$Ratio <- log(Ratios_NIS$`23`[c(7)])

StdError_24 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `24`, ES = "LRRi")
StdError_24$Ratio <- log(Ratios_NIS$`24`[c(7)])

StdError_25 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `25`, ES = "LRRi")
StdError_25$Ratio <- log(Ratios_NIS$`25`[c(7)])

StdError_26 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `26`, ES = "LRRi")
StdError_26$Ratio <- log(Ratios_NIS$`26`[c(7)])

StdError_27 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `27`, ES = "LRRi")
StdError_27$Ratio <- log(Ratios_NIS$`27`[c(7)])

StdError_28 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `28`, ES = "LRRi")
StdError_28$Ratio <- log(Ratios_NIS$`28`[c(7)])

StdError_29 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `29`, ES = "LRRi")
StdError_29$Ratio <- log(Ratios_NIS$`29`[c(7)])

StdError_30 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `30`, ES = "LRRi")
StdError_30$Ratio <- log(Ratios_NIS$`30`[c(7)])

StdError_31 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `31`, ES = "LRRi")
StdError_31$Ratio <- log(Ratios_NIS$`31`[c(7)])

StdError_32 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `32`, ES = "LRRi")
StdError_32$Ratio <- log(Ratios_NIS$`32`[c(7)])

StdError_33 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `33`, ES = "LRRi")
StdError_33$Ratio <- log(Ratios_NIS$`33`[c(7)])

StdError_34 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `34`, ES = "LRRi")
StdError_34$Ratio <- log(Ratios_NIS$`34`[c(7)])

StdError_35 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `35`, ES = "LRRi")
StdError_35$Ratio <- log(Ratios_NIS$`35`[c(7)])

StdError_36 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `36`, ES = "LRRi")
StdError_36$Ratio <- log(Ratios_NIS$`36`[c(7)])

StdError_37 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `37`, ES = "LRRi")
StdError_37$Ratio <- log(Ratios_NIS$`37`[c(7)])

StdError_38 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `38`, ES = "LRRi")
StdError_38$Ratio <- log(Ratios_NIS$`38`[c(7)])

StdError_39 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `39`, ES = "LRRi")
StdError_39$Ratio <- log(Ratios_NIS$`39`[c(7)])

StdError_NIS <- rbind(StdError_1,StdError_2,StdError_3,StdError_4,StdError_5,StdError_6,StdError_7,StdError_8,StdError_9,StdError_10,
                      StdError_11,StdError_12,StdError_13,StdError_14,StdError_15,StdError_16,StdError_17,StdError_18,StdError_19,StdError_20,
                      StdError_21,StdError_22,StdError_23,StdError_24,StdError_25,StdError_26,StdError_27,StdError_28,StdError_29,StdError_30,
                      StdError_31,StdError_32,StdError_33,StdError_34,StdError_35,StdError_36,StdError_37,StdError_38,StdError_39)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

### StdError values for NAT ratios
TrawlingNAT$Season <- TrawlingCategories$Season
TrawlingNAT$Year <- TrawlingCategories$Period
StdError_DataNAT <- TrawlingNAT[TrawlingNAT$Year != c('1990'),]
StdError_DataNAT <- StdError_DataNAT[StdError_DataNAT$Year != c('2000'),]
StdError_DataNAT[StdError_DataNAT == 0] <- 0.1
colnames(StdError_DataNAT) <- c(1:131,"Category","Season","Year")

StdErrorB_1 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `1`, ES = "LRRi")
StdErrorB_1$Ratio <- log(Ratios_NAT$`1`[c(7)])

StdErrorB_2 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `2`, ES = "LRRi")
StdErrorB_2$Ratio <- log(Ratios_NAT$`2`[c(7)])

StdErrorB_3 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `3`, ES = "LRRi")
StdErrorB_3$Ratio <- log(Ratios_NAT$`3`[c(7)])

StdErrorB_4 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `4`, ES = "LRRi")
StdErrorB_4$Ratio <- log(Ratios_NAT$`4`[c(7)])

StdErrorB_5 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `5`, ES = "LRRi")
StdErrorB_5$Ratio <- log(Ratios_NAT$`5`[c(7)])

StdErrorB_6 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `6`, ES = "LRRi")
StdErrorB_6$Ratio <- log(Ratios_NAT$`6`[c(7)])

StdErrorB_7 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `7`, ES = "LRRi")
StdErrorB_7$Ratio <- log(Ratios_NAT$`7`[c(7)])

StdErrorB_8 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `8`, ES = "LRRi")
StdErrorB_8$Ratio <- log(Ratios_NAT$`8`[c(7)])

StdErrorB_9 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `9`, ES = "LRRi")
StdErrorB_9$Ratio <- log(Ratios_NAT$`9`[c(7)])

StdErrorB_10 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `10`, ES = "LRRi")
StdErrorB_10$Ratio <- log(Ratios_NAT$`10`[c(7)])

StdErrorB_11 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `11`, ES = "LRRi")
StdErrorB_11$Ratio <- log(Ratios_NAT$`11`[c(7)])

StdErrorB_12 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `12`, ES = "LRRi")
StdErrorB_12$Ratio <- log(Ratios_NAT$`12`[c(7)])

StdErrorB_13 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `13`, ES = "LRRi")
StdErrorB_13$Ratio <- log(Ratios_NAT$`13`[c(7)])

StdErrorB_14 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `14`, ES = "LRRi")
StdErrorB_14$Ratio <- log(Ratios_NAT$`14`[c(7)])

StdErrorB_15 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `15`, ES = "LRRi")
StdErrorB_15$Ratio <- log(Ratios_NAT$`15`[c(7)])

StdErrorB_16 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `16`, ES = "LRRi")
StdErrorB_16$Ratio <- log(Ratios_NAT$`16`[c(7)])

StdErrorB_17 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `17`, ES = "LRRi")
StdErrorB_17$Ratio <- log(Ratios_NAT$`17`[c(7)])

StdErrorB_18 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `18`, ES = "LRRi")
StdErrorB_18$Ratio <- log(Ratios_NAT$`18`[c(7)])

StdErrorB_19 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `19`, ES = "LRRi")
StdErrorB_19$Ratio <- log(Ratios_NAT$`19`[c(7)])

StdErrorB_20 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `20`, ES = "LRRi")
StdErrorB_20$Ratio <- log(Ratios_NAT$`20`[c(7)])

StdErrorB_21 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `21`, ES = "LRRi")
StdErrorB_21$Ratio <- log(Ratios_NAT$`21`[c(7)])

StdErrorB_22 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `22`, ES = "LRRi")
StdErrorB_22$Ratio <- log(Ratios_NAT$`22`[c(7)])

StdErrorB_23 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `23`, ES = "LRRi")
StdErrorB_23$Ratio <- log(Ratios_NAT$`23`[c(7)])

StdErrorB_24 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `24`, ES = "LRRi")
StdErrorB_24$Ratio <- log(Ratios_NAT$`24`[c(7)])

StdErrorB_25 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `25`, ES = "LRRi")
StdErrorB_25$Ratio <- log(Ratios_NAT$`25`[c(7)])

StdErrorB_26 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `26`, ES = "LRRi")
StdErrorB_26$Ratio <- log(Ratios_NAT$`26`[c(7)])

StdErrorB_27 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `27`, ES = "LRRi")
StdErrorB_27$Ratio <- log(Ratios_NAT$`27`[c(7)])

StdErrorB_28 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `28`, ES = "LRRi")
StdErrorB_28$Ratio <- log(Ratios_NAT$`28`[c(7)])

StdErrorB_29 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `29`, ES = "LRRi")
StdErrorB_29$Ratio <- log(Ratios_NAT$`29`[c(7)])

StdErrorB_30 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `30`, ES = "LRRi")
StdErrorB_30$Ratio <- log(Ratios_NAT$`30`[c(7)])

StdErrorB_31 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `31`, ES = "LRRi")
StdErrorB_31$Ratio <- log(Ratios_NAT$`31`[c(7)])

StdErrorB_32 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `32`, ES = "LRRi")
StdErrorB_32$Ratio <- log(Ratios_NAT$`32`[c(7)])

StdErrorB_33 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `33`, ES = "LRRi")
StdErrorB_33$Ratio <- log(Ratios_NAT$`33`[c(7)])

StdErrorB_34 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `34`, ES = "LRRi")
StdErrorB_34$Ratio <- log(Ratios_NAT$`34`[c(7)])

StdErrorB_35 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `35`, ES = "LRRi")
StdErrorB_35$Ratio <- log(Ratios_NAT$`35`[c(7)])

StdErrorB_36 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `36`, ES = "LRRi")
StdErrorB_36$Ratio <- log(Ratios_NAT$`36`[c(7)])

StdErrorB_37 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `37`, ES = "LRRi")
StdErrorB_37$Ratio <- log(Ratios_NAT$`37`[c(7)])

StdErrorB_38 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `38`, ES = "LRRi")
StdErrorB_38$Ratio <- log(Ratios_NAT$`38`[c(7)])

StdErrorB_39 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `39`, ES = "LRRi")
StdErrorB_39$Ratio <- log(Ratios_NAT$`39`[c(7)])

StdErrorB_40 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `40`, ES = "LRRi")
StdErrorB_40$Ratio <- log(Ratios_NAT$`40`[c(7)])

StdErrorB_41 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `41`, ES = "LRRi")
StdErrorB_41$Ratio <- log(Ratios_NAT$`41`[c(7)])

StdErrorB_42 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `42`, ES = "LRRi")
StdErrorB_42$Ratio <- log(Ratios_NAT$`42`[c(7)])

StdErrorB_43 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `43`, ES = "LRRi")
StdErrorB_43$Ratio <- log(Ratios_NAT$`43`[c(7)])

StdErrorB_44 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `44`, ES = "LRRi")
StdErrorB_44$Ratio <- log(Ratios_NAT$`44`[c(7)])

StdErrorB_45 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `45`, ES = "LRRi")
StdErrorB_45$Ratio <- log(Ratios_NAT$`45`[c(7)])

StdErrorB_46 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `46`, ES = "LRRi")
StdErrorB_46$Ratio <- log(Ratios_NAT$`46`[c(7)])

StdErrorB_47 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `47`, ES = "LRRi")
StdErrorB_47$Ratio <- log(Ratios_NAT$`47`[c(7)])

StdErrorB_48 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `48`, ES = "LRRi")
StdErrorB_48$Ratio <- log(Ratios_NAT$`48`[c(7)])

StdErrorB_49 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `49`, ES = "LRRi")
StdErrorB_49$Ratio <- log(Ratios_NAT$`49`[c(7)])

StdErrorB_50 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `50`, ES = "LRRi")
StdErrorB_50$Ratio <- log(Ratios_NAT$`50`[c(7)])

StdErrorB_51 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `51`, ES = "LRRi")
StdErrorB_51$Ratio <- log(Ratios_NAT$`51`[c(7)])

StdErrorB_52 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `52`, ES = "LRRi")
StdErrorB_52$Ratio <- log(Ratios_NAT$`52`[c(7)])

StdErrorB_53 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `53`, ES = "LRRi")
StdErrorB_53$Ratio <- log(Ratios_NAT$`53`[c(7)])

StdErrorB_54 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `54`, ES = "LRRi")
StdErrorB_54$Ratio <- log(Ratios_NAT$`54`[c(7)])

StdErrorB_55 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `55`, ES = "LRRi")
StdErrorB_55$Ratio <- log(Ratios_NAT$`55`[c(7)])

StdErrorB_56 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `56`, ES = "LRRi")
StdErrorB_56$Ratio <- log(Ratios_NAT$`56`[c(7)])

StdErrorB_57 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `57`, ES = "LRRi")
StdErrorB_57$Ratio <- log(Ratios_NAT$`57`[c(7)])

StdErrorB_58 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `58`, ES = "LRRi")
StdErrorB_58$Ratio <- log(Ratios_NAT$`58`[c(7)])

StdErrorB_59 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `59`, ES = "LRRi")
StdErrorB_59$Ratio <- log(Ratios_NAT$`59`[c(7)])

StdErrorB_60 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `60`, ES = "LRRi")
StdErrorB_60$Ratio <- log(Ratios_NAT$`60`[c(7)])

StdErrorB_61 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `61`, ES = "LRRi")
StdErrorB_61$Ratio <- log(Ratios_NAT$`61`[c(7)])

StdErrorB_62 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `62`, ES = "LRRi")
StdErrorB_62$Ratio <- log(Ratios_NAT$`62`[c(7)])

StdErrorB_63 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `63`, ES = "LRRi")
StdErrorB_63$Ratio <- log(Ratios_NAT$`63`[c(7)])

StdErrorB_64 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `64`, ES = "LRRi")
StdErrorB_64$Ratio <- log(Ratios_NAT$`64`[c(7)])

StdErrorB_65 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `65`, ES = "LRRi")
StdErrorB_65$Ratio <- log(Ratios_NAT$`65`[c(7)])

StdErrorB_66 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `66`, ES = "LRRi")
StdErrorB_66$Ratio <- log(Ratios_NAT$`66`[c(7)])

StdErrorB_67 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `67`, ES = "LRRi")
StdErrorB_67$Ratio <- log(Ratios_NAT$`67`[c(7)])

StdErrorB_68 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `68`, ES = "LRRi")
StdErrorB_68$Ratio <- log(Ratios_NAT$`68`[c(7)])

StdErrorB_69 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `69`, ES = "LRRi")
StdErrorB_69$Ratio <- log(Ratios_NAT$`69`[c(7)])

StdErrorB_70 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `70`, ES = "LRRi")
StdErrorB_70$Ratio <- log(Ratios_NAT$`70`[c(7)])

StdErrorB_71 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `71`, ES = "LRRi")
StdErrorB_71$Ratio <- log(Ratios_NAT$`71`[c(7)])

StdErrorB_72 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `72`, ES = "LRRi")
StdErrorB_72$Ratio <- log(Ratios_NAT$`72`[c(7)])

StdErrorB_73 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `73`, ES = "LRRi")
StdErrorB_73$Ratio <- log(Ratios_NAT$`73`[c(7)])

StdErrorB_73 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `73`, ES = "LRRi")
StdErrorB_73$Ratio <- log(Ratios_NAT$`73`[c(7)])

StdErrorB_74 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `74`, ES = "LRRi")
StdErrorB_74$Ratio <- log(Ratios_NAT$`74`[c(7)])

StdErrorB_75 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `75`, ES = "LRRi")
StdErrorB_75$Ratio <- log(Ratios_NAT$`75`[c(7)])

StdErrorB_76 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `76`, ES = "LRRi")
StdErrorB_76$Ratio <- log(Ratios_NAT$`76`[c(7)])

StdErrorB_77 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `77`, ES = "LRRi")
StdErrorB_77$Ratio <- log(Ratios_NAT$`77`[c(7)])

StdErrorB_78 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `78`, ES = "LRRi")
StdErrorB_78$Ratio <- log(Ratios_NAT$`78`[c(7)])

StdErrorB_79 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `79`, ES = "LRRi")
StdErrorB_79$Ratio <- log(Ratios_NAT$`79`[c(7)])

StdErrorB_80 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `80`, ES = "LRRi")
StdErrorB_80$Ratio <- log(Ratios_NAT$`80`[c(7)])

StdErrorB_81 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `81`, ES = "LRRi")
StdErrorB_81$Ratio <- log(Ratios_NAT$`81`[c(7)])

StdErrorB_82 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `82`, ES = "LRRi")
StdErrorB_82$Ratio <- log(Ratios_NAT$`82`[c(7)])

StdErrorB_83 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `83`, ES = "LRRi")
StdErrorB_83$Ratio <- log(Ratios_NAT$`83`[c(7)])

StdErrorB_84 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `84`, ES = "LRRi")
StdErrorB_84$Ratio <- log(Ratios_NAT$`84`[c(7)])

StdErrorB_85 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `85`, ES = "LRRi")
StdErrorB_85$Ratio <- log(Ratios_NAT$`85`[c(7)])

StdErrorB_86 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `86`, ES = "LRRi")
StdErrorB_86$Ratio <- log(Ratios_NAT$`86`[c(7)])

StdErrorB_87 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `87`, ES = "LRRi")
StdErrorB_87$Ratio <- log(Ratios_NAT$`87`[c(7)])

StdErrorB_88 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `88`, ES = "LRRi")
StdErrorB_88$Ratio <- log(Ratios_NAT$`88`[c(7)])

StdErrorB_89 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `89`, ES = "LRRi")
StdErrorB_89$Ratio <- log(Ratios_NAT$`89`[c(7)])

StdErrorB_90 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `90`, ES = "LRRi")
StdErrorB_90$Ratio <- log(Ratios_NAT$`90`[c(7)])

StdErrorB_91 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `91`, ES = "LRRi")
StdErrorB_91$Ratio <- log(Ratios_NAT$`91`[c(7)])

StdErrorB_92 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `92`, ES = "LRRi")
StdErrorB_92$Ratio <- log(Ratios_NAT$`92`[c(7)])

StdErrorB_93 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `93`, ES = "LRRi")
StdErrorB_93$Ratio <- log(Ratios_NAT$`93`[c(7)])

StdErrorB_94 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `94`, ES = "LRRi")
StdErrorB_94$Ratio <- log(Ratios_NAT$`94`[c(7)])

StdErrorB_95 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `95`, ES = "LRRi")
StdErrorB_95$Ratio <- log(Ratios_NAT$`95`[c(7)])

StdErrorB_96 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `96`, ES = "LRRi")
StdErrorB_96$Ratio <- log(Ratios_NAT$`96`[c(7)])

StdErrorB_97 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `97`, ES = "LRRi")
StdErrorB_97$Ratio <- log(Ratios_NAT$`97`[c(7)])

StdErrorB_98 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `98`, ES = "LRRi")
StdErrorB_98$Ratio <- log(Ratios_NAT$`98`[c(7)])

StdErrorB_99 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `99`, ES = "LRRi")
StdErrorB_99$Ratio <- log(Ratios_NAT$`99`[c(7)])

StdErrorB_100 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `100`, ES = "LRRi")
StdErrorB_100$Ratio <- log(Ratios_NAT$`100`[c(7)])

StdErrorB_101 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `101`, ES = "LRRi")
StdErrorB_101$Ratio <- log(Ratios_NAT$`101`[c(7)])

StdErrorB_102 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `102`, ES = "LRRi")
StdErrorB_102$Ratio <- log(Ratios_NAT$`102`[c(7)])

StdErrorB_103 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `103`, ES = "LRRi")
StdErrorB_103$Ratio <- log(Ratios_NAT$`103`[c(7)])

StdErrorB_104 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `104`, ES = "LRRi")
StdErrorB_104$Ratio <- log(Ratios_NAT$`104`[c(7)])

StdErrorB_105 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `105`, ES = "LRRi")
StdErrorB_105$Ratio <- log(Ratios_NAT$`105`[c(7)])

StdErrorB_106 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `106`, ES = "LRRi")
StdErrorB_106$Ratio <- log(Ratios_NAT$`106`[c(7)])

StdErrorB_107 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `107`, ES = "LRRi")
StdErrorB_107$Ratio <- log(Ratios_NAT$`107`[c(7)])

StdErrorB_108 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `108`, ES = "LRRi")
StdErrorB_108$Ratio <- log(Ratios_NAT$`108`[c(7)])

StdErrorB_109 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `109`, ES = "LRRi")
StdErrorB_109$Ratio <- log(Ratios_NAT$`109`[c(7)])

StdErrorB_110 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `110`, ES = "LRRi")
StdErrorB_110$Ratio <- log(Ratios_NAT$`110`[c(7)])

StdErrorB_111 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `111`, ES = "LRRi")
StdErrorB_111$Ratio <- log(Ratios_NAT$`111`[c(7)])

StdErrorB_112 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `112`, ES = "LRRi")
StdErrorB_112$Ratio <- log(Ratios_NAT$`112`[c(7)])

StdErrorB_113 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `113`, ES = "LRRi")
StdErrorB_113$Ratio <- log(Ratios_NAT$`113`[c(7)])

StdErrorB_114 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `114`, ES = "LRRi")
StdErrorB_114$Ratio <- log(Ratios_NAT$`114`[c(7)])

StdErrorB_115 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `115`, ES = "LRRi")
StdErrorB_115$Ratio <- log(Ratios_NAT$`115`[c(7)])

StdErrorB_116 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `116`, ES = "LRRi")
StdErrorB_116$Ratio <- log(Ratios_NAT$`116`[c(7)])

StdErrorB_117 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `117`, ES = "LRRi")
StdErrorB_117$Ratio <- log(Ratios_NAT$`117`[c(7)])

StdErrorB_118 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `118`, ES = "LRRi")
StdErrorB_118$Ratio <- log(Ratios_NAT$`118`[c(7)])

StdErrorB_119 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `119`, ES = "LRRi")
StdErrorB_119$Ratio <- log(Ratios_NAT$`119`[c(7)])

StdErrorB_120 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `120`, ES = "LRRi")
StdErrorB_120$Ratio <- log(Ratios_NAT$`120`[c(7)])

StdErrorB_121 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `121`, ES = "LRRi")
StdErrorB_121$Ratio <- log(Ratios_NAT$`121`[c(7)])

StdErrorB_122 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `122`, ES = "LRRi")
StdErrorB_122$Ratio <- log(Ratios_NAT$`122`[c(7)])

StdErrorB_123 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `123`, ES = "LRRi")
StdErrorB_123$Ratio <- log(Ratios_NAT$`123`[c(7)])

StdErrorB_124 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `124`, ES = "LRRi")
StdErrorB_124$Ratio <- log(Ratios_NAT$`124`[c(7)])

StdErrorB_125 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `125`, ES = "LRRi")
StdErrorB_125$Ratio <- log(Ratios_NAT$`125`[c(7)])

StdErrorB_126 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `126`, ES = "LRRi")
StdErrorB_126$Ratio <- log(Ratios_NAT$`126`[c(7)])

StdErrorB_127 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `127`, ES = "LRRi")
StdErrorB_127$Ratio <- log(Ratios_NAT$`127`[c(7)])

StdErrorB_128 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `128`, ES = "LRRi")
StdErrorB_128$Ratio <- log(Ratios_NAT$`128`[c(7)])

StdErrorB_129 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `129`, ES = "LRRi")
StdErrorB_129$Ratio <- log(Ratios_NAT$`129`[c(7)])

StdErrorB_130 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `130`, ES = "LRRi")
StdErrorB_130$Ratio <- log(Ratios_NAT$`130`[c(7)])

StdErrorB_131 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `131`, ES = "LRRi")
StdErrorB_131$Ratio <- log(Ratios_NAT$`131`[c(7)])


StdError_NAT <- rbind(StdErrorB_1,StdErrorB_2,StdErrorB_3,StdErrorB_4,StdErrorB_5,StdErrorB_6,StdErrorB_7,StdErrorB_8,StdErrorB_9,StdErrorB_10,
                      StdErrorB_11,StdErrorB_12,StdErrorB_13,StdErrorB_14,StdErrorB_15,StdErrorB_16,StdErrorB_17,StdErrorB_18,StdErrorB_19,StdErrorB_20,
                      StdErrorB_21,StdErrorB_22,StdErrorB_23,StdErrorB_24,StdErrorB_25,StdErrorB_26,StdErrorB_27,StdErrorB_28,StdErrorB_29,StdErrorB_30,
                      StdErrorB_31,StdErrorB_32,StdErrorB_33,StdErrorB_34,StdErrorB_35,StdErrorB_36,StdErrorB_37,StdErrorB_38,StdErrorB_39,StdErrorB_40,
                      StdErrorB_41,StdErrorB_42,StdErrorB_43,StdErrorB_44,StdErrorB_45,StdErrorB_46,StdErrorB_47,StdErrorB_48,StdErrorB_49,StdErrorB_50,
                      StdErrorB_51,StdErrorB_52,StdErrorB_53,StdErrorB_54,StdErrorB_55,StdErrorB_56,StdErrorB_57,StdErrorB_58,StdErrorB_59,StdErrorB_60,
                      StdErrorB_61,StdErrorB_62,StdErrorB_63,StdErrorB_64,StdErrorB_65,StdErrorB_66,StdErrorB_67,StdErrorB_68,StdErrorB_69,StdErrorB_70,
                      StdErrorB_71,StdErrorB_72,StdErrorB_73,StdErrorB_74,StdErrorB_75,StdErrorB_76,StdErrorB_77,StdErrorB_78,StdErrorB_79,StdErrorB_80,
                      StdErrorB_81,StdErrorB_82,StdErrorB_83,StdErrorB_84,StdErrorB_85,StdErrorB_86,StdErrorB_87,StdErrorB_88,StdErrorB_89,StdErrorB_90,
                      StdErrorB_91,StdErrorB_92,StdErrorB_93,StdErrorB_94,StdErrorB_95,StdErrorB_96,StdErrorB_97,StdErrorB_98,StdErrorB_99,StdErrorB_100,
                      StdErrorB_101,StdErrorB_102,StdErrorB_103,StdErrorB_104,StdErrorB_105,StdErrorB_106,StdErrorB_107,StdErrorB_108,StdErrorB_109,StdErrorB_110,
                      StdErrorB_111,StdErrorB_112,StdErrorB_113,StdErrorB_114,StdErrorB_115,StdErrorB_116,StdErrorB_117,StdErrorB_118,StdErrorB_119,StdErrorB_120,
                      StdErrorB_121,StdErrorB_122,StdErrorB_123,StdErrorB_124,StdErrorB_125,StdErrorB_126,StdErrorB_127,StdErrorB_128,StdErrorB_129,StdErrorB_130,
                      StdErrorB_131)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]


### Create dataframe
Trawl40_Sp_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
