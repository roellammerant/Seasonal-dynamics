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

TrawlingAll_Season <- TrawlingAll_Season[TrawlingAll_Season$Mean_Depth > 40.3 & TrawlingAll_Season$Mean_Depth < 100,]


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
TrawlingNIS <- TrawlingNIS[-c(99),]
TrawlingNIS$Merched_Category <- TrawlingCategories$Merched_Category
TrawlingNIS <- TrawlingNIS %>% 
  mutate_if(is.character, as.numeric)
Mean_NIS <- aggregate(TrawlingNIS[c(1:39)], 
                      list(TrawlingNIS$Merched_Category), FUN=mean) 

TrawlingNAT <- t(TrawlingNAT)
TrawlingNAT <- as.data.frame(TrawlingNAT)
TrawlingNAT <- TrawlingNAT[-c(99),]
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
StdError_1$TempPrefMean <- "27.17"
StdError_1$TempPrefMax <- "30.46"

StdError_2 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `2`, ES = "LRRi")
StdError_2$Ratio <- log(Ratios_NIS$`2`[c(7)])
StdError_2$TempPrefMean <- "27.69"
StdError_2$TempPrefMax <- "29.34"

StdError_3 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `3`, ES = "LRRi")
StdError_3$Ratio <- log(Ratios_NIS$`3`[c(7)])
StdError_3$TempPrefMean <- "25.84"
StdError_3$TempPrefMax <- "27.79"

StdError_4 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `4`, ES = "LRRi")
StdError_4$Ratio <- log(Ratios_NIS$`4`[c(7)])
StdError_4$TempPrefMean <- "27.12"
StdError_4$TempPrefMax <- "29.82"

StdError_5 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `5`, ES = "LRRi")
StdError_5$Ratio <- log(Ratios_NIS$`5`[c(7)])
StdError_5$TempPrefMean <- "25.56"
StdError_5$TempPrefMax <- "29.68"

StdError_6 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `6`, ES = "LRRi")
StdError_6$Ratio <- log(Ratios_NIS$`6`[c(7)])
StdError_6$TempPrefMean <- "27.23"
StdError_6$TempPrefMax <- "29.47"

StdError_7 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `7`, ES = "LRRi")
StdError_7$Ratio <- log(Ratios_NIS$`7`[c(7)])
StdError_7$TempPrefMean <- "24.05"
StdError_7$TempPrefMax <- "24.14"

StdError_8 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `8`, ES = "LRRi")
StdError_8$Ratio <- log(Ratios_NIS$`8`[c(7)])
StdError_8$TempPrefMean <- "27.37"
StdError_8$TempPrefMax <- "30.35"

StdError_9 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                            outcome = `9`, ES = "LRRi")
StdError_9$Ratio <- log(Ratios_NIS$`9`[c(7)])
StdError_9$TempPrefMean <- "27.85"
StdError_9$TempPrefMax <- "29.72"

StdError_10 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `10`, ES = "LRRi")
StdError_10$Ratio <- log(Ratios_NIS$`10`[c(7)])
StdError_10$TempPrefMean <- "27.26"
StdError_10$TempPrefMax <- "29.45"

StdError_11 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `11`, ES = "LRRi")
StdError_11$Ratio <- log(Ratios_NIS$`11`[c(7)])
StdError_11$TempPrefMean <- "25.45"
StdError_11$TempPrefMax <- "25.45"

StdError_12 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `12`, ES = "LRRi")
StdError_12$Ratio <- log(Ratios_NIS$`12`[c(7)])
StdError_12$TempPrefMean <- "26.58"
StdError_12$TempPrefMax <- "30.37"

StdError_13 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `13`, ES = "LRRi")
StdError_13$Ratio <- log(Ratios_NIS$`13`[c(7)])
StdError_13$TempPrefMean <- "26.40"
StdError_13$TempPrefMax <- "30.30"

StdError_14 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `14`, ES = "LRRi")
StdError_14$Ratio <- log(Ratios_NIS$`14`[c(7)])
StdError_14$TempPrefMean <- "27.82"
StdError_14$TempPrefMax <- "30.14"

StdError_15 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `15`, ES = "LRRi")
StdError_15$Ratio <- log(Ratios_NIS$`15`[c(7)])
StdError_15$TempPrefMean <- "27.38"
StdError_15$TempPrefMax <- "29.43"

StdError_16 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `16`, ES = "LRRi")
StdError_16$Ratio <- log(Ratios_NIS$`16`[c(7)])
StdError_16$TempPrefMean <- "26.86"
StdError_16$TempPrefMax <- "29.43"

StdError_17 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `17`, ES = "LRRi")
StdError_17$Ratio <- log(Ratios_NIS$`17`[c(7)])
StdError_17$TempPrefMean <- "27.36"
StdError_17$TempPrefMax <- "29.74"

StdError_18 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `18`, ES = "LRRi")
StdError_18$Ratio <- log(Ratios_NIS$`18`[c(7)])
StdError_18$TempPrefMean <- "26.93"
StdError_18$TempPrefMax <- "30.02"

StdError_19 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `19`, ES = "LRRi")
StdError_19$Ratio <- log(Ratios_NIS$`19`[c(7)])
StdError_19$TempPrefMean <- "27.09"
StdError_19$TempPrefMax <- "29.81"

StdError_20 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `20`, ES = "LRRi")
StdError_20$Ratio <- log(Ratios_NIS$`20`[c(7)])
StdError_20$TempPrefMean <- "27.34"
StdError_20$TempPrefMax <- "30.25"

StdError_21 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `21`, ES = "LRRi")
StdError_21$Ratio <- log(Ratios_NIS$`21`[c(7)])
StdError_21$TempPrefMean <- "28.25"
StdError_21$TempPrefMax <- "29.80"

StdError_22 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `22`, ES = "LRRi")
StdError_22$Ratio <- log(Ratios_NIS$`22`[c(7)])
StdError_22$TempPrefMean <- "26.67"
StdError_22$TempPrefMax <- "29.88"

StdError_23 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `23`, ES = "LRRi")
StdError_23$Ratio <- log(Ratios_NIS$`23`[c(7)])
StdError_23$TempPrefMean <- "28.27"
StdError_23$TempPrefMax <- "29.86"

StdError_24 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `24`, ES = "LRRi")
StdError_24$Ratio <- log(Ratios_NIS$`24`[c(7)])
StdError_24$TempPrefMean <- "27.02"
StdError_24$TempPrefMax <- "30.30"

StdError_25 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `25`, ES = "LRRi")
StdError_25$Ratio <- log(Ratios_NIS$`25`[c(7)])
StdError_25$TempPrefMean <- "26.44"
StdError_25$TempPrefMax <- "28.46"

StdError_26 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `26`, ES = "LRRi")
StdError_26$Ratio <- log(Ratios_NIS$`26`[c(7)])
StdError_26$TempPrefMean <- "25.52"
StdError_26$TempPrefMax <- "27.73"

StdError_27 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `27`, ES = "LRRi")
StdError_27$Ratio <- log(Ratios_NIS$`27`[c(7)])
StdError_27$TempPrefMean <- "27.70"
StdError_27$TempPrefMax <- "30.39"

StdError_28 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `28`, ES = "LRRi")
StdError_28$Ratio <- log(Ratios_NIS$`28`[c(7)])
StdError_28$TempPrefMean <- "NA"
StdError_28$TempPrefMax <- "NA"

StdError_29 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `29`, ES = "LRRi")
StdError_29$Ratio <- log(Ratios_NIS$`29`[c(7)])
StdError_29$TempPrefMean <- "27.24"
StdError_29$TempPrefMax <- "30.30"

StdError_30 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `30`, ES = "LRRi")
StdError_30$Ratio <- log(Ratios_NIS$`30`[c(7)])
StdError_30$TempPrefMean <- "26.50"
StdError_30$TempPrefMax <- "30.33"

StdError_31 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `31`, ES = "LRRi")
StdError_31$Ratio <- log(Ratios_NIS$`31`[c(7)])
StdError_31$TempPrefMean <- "24.84"
StdError_31$TempPrefMax <- "30.33"

StdError_32 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `32`, ES = "LRRi")
StdError_32$Ratio <- log(Ratios_NIS$`32`[c(7)])
StdError_32$TempPrefMean <- "NA"
StdError_32$TempPrefMax <- "NA"

StdError_33 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `33`, ES = "LRRi")
StdError_33$Ratio <- log(Ratios_NIS$`33`[c(7)])
StdError_33$TempPrefMean <- "26.25"
StdError_33$TempPrefMax <- "27.74"

StdError_34 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `34`, ES = "LRRi")
StdError_34$Ratio <- log(Ratios_NIS$`34`[c(7)])
StdError_34$TempPrefMean <- "27.55"
StdError_34$TempPrefMax <- "28.30"

StdError_35 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `35`, ES = "LRRi")
StdError_35$Ratio <- log(Ratios_NIS$`35`[c(7)])
StdError_35$TempPrefMean <- "27.42"
StdError_35$TempPrefMax <- "29.74"

StdError_36 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `36`, ES = "LRRi")
StdError_36$Ratio <- log(Ratios_NIS$`36`[c(7)])
StdError_36$TempPrefMean <- "24.54"
StdError_36$TempPrefMax <- "28.33"

StdError_37 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `37`, ES = "LRRi")
StdError_37$Ratio <- log(Ratios_NIS$`37`[c(7)])
StdError_37$TempPrefMean <- "26.34"
StdError_37$TempPrefMax <- "28.26"

StdError_38 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `38`, ES = "LRRi")
StdError_38$Ratio <- log(Ratios_NIS$`38`[c(7)])
StdError_38$TempPrefMean <- "27.39"
StdError_38$TempPrefMax <- "30.25"

StdError_39 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `39`, ES = "LRRi")
StdError_39$Ratio <- log(Ratios_NIS$`39`[c(7)])
StdError_39$TempPrefMean <- "24.75"
StdError_39$TempPrefMax <- "27.13"

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
StdErrorB_1$TempPrefMean <- "26.90"
StdErrorB_1$TempPrefMax <- "28.73"

StdErrorB_2 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `2`, ES = "LRRi")
StdErrorB_2$Ratio <- log(Ratios_NAT$`2`[c(7)])
StdErrorB_2$TempPrefMean <- "12.32"
StdErrorB_2$TempPrefMax <- "28.04"

StdErrorB_3 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `3`, ES = "LRRi")
StdErrorB_3$Ratio <- log(Ratios_NAT$`3`[c(7)])
StdErrorB_3$TempPrefMean <- "23.68"
StdErrorB_3$TempPrefMax <- "29.39"

StdErrorB_4 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `4`, ES = "LRRi")
StdErrorB_4$Ratio <- log(Ratios_NAT$`4`[c(7)])
StdErrorB_4$TempPrefMean <- "25.19"
StdErrorB_4$TempPrefMax <- "28.73"

StdErrorB_5 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `5`, ES = "LRRi")
StdErrorB_5$Ratio <- log(Ratios_NAT$`5`[c(7)])
StdErrorB_5$TempPrefMean <- "12.29"
StdErrorB_5$TempPrefMax <- "23.98"

StdErrorB_6 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `6`, ES = "LRRi")
StdErrorB_6$Ratio <- log(Ratios_NAT$`6`[c(7)])
StdErrorB_6$TempPrefMean <- "17.81"
StdErrorB_6$TempPrefMax <- "28.57"

StdErrorB_7 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `7`, ES = "LRRi")
StdErrorB_7$Ratio <- log(Ratios_NAT$`7`[c(7)])
StdErrorB_7$TempPrefMean <- "25.03"
StdErrorB_7$TempPrefMax <- "29.70"

StdErrorB_8 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `8`, ES = "LRRi")
StdErrorB_8$Ratio <- log(Ratios_NAT$`8`[c(7)])
StdErrorB_8$TempPrefMean <- "NA"
StdErrorB_8$TempPrefMax <- "NA"

StdErrorB_9 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `9`, ES = "LRRi")
StdErrorB_9$Ratio <- log(Ratios_NAT$`9`[c(7)])
StdErrorB_9$TempPrefMean <- "17.03"
StdErrorB_9$TempPrefMax <- "19.62"

StdErrorB_10 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `10`, ES = "LRRi")
StdErrorB_10$Ratio <- log(Ratios_NAT$`10`[c(7)])
StdErrorB_10$TempPrefMean <- "14.12"
StdErrorB_10$TempPrefMax <- "22.61"
StdErrorB_11 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `11`, ES = "LRRi")
StdErrorB_11$Ratio <- log(Ratios_NAT$`11`[c(7)])
StdErrorB_11$TempPrefMean <- "22.46"
StdErrorB_11$TempPrefMax <- "29.30"

StdErrorB_12 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `12`, ES = "LRRi")
StdErrorB_12$Ratio <- log(Ratios_NAT$`12`[c(7)])
StdErrorB_12$TempPrefMean <- "13.11"
StdErrorB_12$TempPrefMax <- "28.41"

StdErrorB_13 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `13`, ES = "LRRi")
StdErrorB_13$Ratio <- log(Ratios_NAT$`13`[c(7)])
StdErrorB_13$TempPrefMean <- "19.79"
StdErrorB_13$TempPrefMax <- "28.73"

StdErrorB_14 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `14`, ES = "LRRi")
StdErrorB_14$Ratio <- log(Ratios_NAT$`14`[c(7)])
StdErrorB_14$TempPrefMean <- "24.32"
StdErrorB_14$TempPrefMax <- "29.48"

StdErrorB_15 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `15`, ES = "LRRi")
StdErrorB_15$Ratio <- log(Ratios_NAT$`15`[c(7)])
StdErrorB_15$TempPrefMean <- "13.15"
StdErrorB_15$TempPrefMax <- "28.49"

StdErrorB_16 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `16`, ES = "LRRi")
StdErrorB_16$Ratio <- log(Ratios_NAT$`16`[c(7)])
StdErrorB_16$TempPrefMean <- "24.28"
StdErrorB_16$TempPrefMax <- "29.53"

StdErrorB_17 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `17`, ES = "LRRi")
StdErrorB_17$Ratio <- log(Ratios_NAT$`17`[c(7)])
StdErrorB_17$TempPrefMean <- "24.82"
StdErrorB_17$TempPrefMax <- "28.73"

StdErrorB_18 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `18`, ES = "LRRi")
StdErrorB_18$Ratio <- log(Ratios_NAT$`18`[c(7)])
StdErrorB_18$TempPrefMean <- "23.11"
StdErrorB_18$TempPrefMax <- "29.62"

StdErrorB_19 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `19`, ES = "LRRi")
StdErrorB_19$Ratio <- log(Ratios_NAT$`19`[c(7)])
StdErrorB_19$TempPrefMean <- "12.68"
StdErrorB_19$TempPrefMax <- "28.73"

StdErrorB_20 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `20`, ES = "LRRi")
StdErrorB_20$Ratio <- log(Ratios_NAT$`20`[c(7)])
StdErrorB_20$TempPrefMean <- "21.34"
StdErrorB_20$TempPrefMax <- "28.81"

StdErrorB_21 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `21`, ES = "LRRi")
StdErrorB_21$Ratio <- log(Ratios_NAT$`21`[c(7)])
StdErrorB_21$TempPrefMean <- "25.57"
StdErrorB_21$TempPrefMax <- "28.73"

StdErrorB_22 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `22`, ES = "LRRi")
StdErrorB_22$Ratio <- log(Ratios_NAT$`22`[c(7)])
StdErrorB_22$TempPrefMean <- "17.69"
StdErrorB_22$TempPrefMax <- "28.73"

StdErrorB_23 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `23`, ES = "LRRi")
StdErrorB_23$Ratio <- log(Ratios_NAT$`23`[c(7)])
StdErrorB_23$TempPrefMean <- "13.47"
StdErrorB_23$TempPrefMax <- "28.00"

StdErrorB_24 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `24`, ES = "LRRi")
StdErrorB_24$Ratio <- log(Ratios_NAT$`24`[c(7)])
StdErrorB_24$TempPrefMean <- "11.85"
StdErrorB_24$TempPrefMax <- "17.56"

StdErrorB_25 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `25`, ES = "LRRi")
StdErrorB_25$Ratio <- log(Ratios_NAT$`25`[c(7)])
StdErrorB_25$TempPrefMean <- "24.99"
StdErrorB_25$TempPrefMax <- "29.43"

StdErrorB_26 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `26`, ES = "LRRi")
StdErrorB_26$Ratio <- log(Ratios_NAT$`26`[c(7)])
StdErrorB_26$TempPrefMean <- "17.78"
StdErrorB_26$TempPrefMax <- "29.12"
StdErrorB_27 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `27`, ES = "LRRi")
StdErrorB_27$Ratio <- log(Ratios_NAT$`27`[c(7)])
StdErrorB_27$TempPrefMean <- "17.66"
StdErrorB_27$TempPrefMax <- "21.80"

StdErrorB_28 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `28`, ES = "LRRi")
StdErrorB_28$Ratio <- log(Ratios_NAT$`28`[c(7)])
StdErrorB_28$TempPrefMean <- "15.85"
StdErrorB_28$TempPrefMax <- "20.24"

StdErrorB_29 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `29`, ES = "LRRi")
StdErrorB_29$Ratio <- log(Ratios_NAT$`29`[c(7)])
StdErrorB_29$TempPrefMean <- "19.72"
StdErrorB_29$TempPrefMax <- "24.94"

StdErrorB_30 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `30`, ES = "LRRi")
StdErrorB_30$Ratio <- log(Ratios_NAT$`30`[c(7)])
StdErrorB_30$TempPrefMean <- "25.47"
StdErrorB_30$TempPrefMax <- "28.73"

StdErrorB_31 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `31`, ES = "LRRi")
StdErrorB_31$Ratio <- log(Ratios_NAT$`31`[c(7)])
StdErrorB_31$TempPrefMean <- "24.65"
StdErrorB_31$TempPrefMax <- "28.73"

StdErrorB_32 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `32`, ES = "LRRi")
StdErrorB_32$Ratio <- log(Ratios_NAT$`32`[c(7)])
StdErrorB_32$TempPrefMean <- "22.28"
StdErrorB_32$TempPrefMax <- "28.37"

StdErrorB_33 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `33`, ES = "LRRi")
StdErrorB_33$Ratio <- log(Ratios_NAT$`33`[c(7)])
StdErrorB_33$TempPrefMean <- "13.31"
StdErrorB_33$TempPrefMax <- "21.80"

StdErrorB_34 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `34`, ES = "LRRi")
StdErrorB_34$Ratio <- log(Ratios_NAT$`34`[c(7)])
StdErrorB_34$TempPrefMean <- "17.95"
StdErrorB_34$TempPrefMax <- "21.80"

StdErrorB_35 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `35`, ES = "LRRi")
StdErrorB_35$Ratio <- log(Ratios_NAT$`35`[c(7)])
StdErrorB_35$TempPrefMean <- "19.77"
StdErrorB_35$TempPrefMax <- "21.54"

StdErrorB_36 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `36`, ES = "LRRi")
StdErrorB_36$Ratio <- log(Ratios_NAT$`36`[c(7)])
StdErrorB_36$TempPrefMean <- "19.25"
StdErrorB_36$TempPrefMax <- "24.38"

StdErrorB_37 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `37`, ES = "LRRi")
StdErrorB_37$Ratio <- log(Ratios_NAT$`37`[c(7)])
StdErrorB_37$TempPrefMean <- "18.88"
StdErrorB_37$TempPrefMax <- "23.51"

StdErrorB_38 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `38`, ES = "LRRi")
StdErrorB_38$Ratio <- log(Ratios_NAT$`38`[c(7)])
StdErrorB_38$TempPrefMean <- "18.82"
StdErrorB_38$TempPrefMax <- "26.02"

StdErrorB_39 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `39`, ES = "LRRi")
StdErrorB_39$Ratio <- log(Ratios_NAT$`39`[c(7)])
StdErrorB_39$TempPrefMean <- "11.99"
StdErrorB_39$TempPrefMax <- "26.34"

StdErrorB_40 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `40`, ES = "LRRi")
StdErrorB_40$Ratio <- log(Ratios_NAT$`40`[c(7)])
StdErrorB_40$TempPrefMean <- "23.60"
StdErrorB_40$TempPrefMax <- "27.57"

StdErrorB_41 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `41`, ES = "LRRi")
StdErrorB_41$Ratio <- log(Ratios_NAT$`41`[c(7)])
StdErrorB_41$TempPrefMean <- "26.57"
StdErrorB_41$TempPrefMax <- "30.39"

StdErrorB_42 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `42`, ES = "LRRi")
StdErrorB_42$Ratio <- log(Ratios_NAT$`42`[c(7)])
StdErrorB_42$TempPrefMean <- "13.03"
StdErrorB_42$TempPrefMax <- "28.73"

StdErrorB_43 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `43`, ES = "LRRi")
StdErrorB_43$Ratio <- log(Ratios_NAT$`43`[c(7)])
StdErrorB_43$TempPrefMean <- "26.07"
StdErrorB_43$TempPrefMax <- "28.73"

StdErrorB_44 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `44`, ES = "LRRi")
StdErrorB_44$Ratio <- log(Ratios_NAT$`44`[c(7)])
StdErrorB_44$TempPrefMean <- "23.08"
StdErrorB_44$TempPrefMax <- "27.15"

StdErrorB_45 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `45`, ES = "LRRi")
StdErrorB_45$Ratio <- log(Ratios_NAT$`45`[c(7)])
StdErrorB_45$TempPrefMean <- "25.69"
StdErrorB_45$TempPrefMax <- "29.12"

StdErrorB_46 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `46`, ES = "LRRi")
StdErrorB_46$Ratio <- log(Ratios_NAT$`46`[c(7)])
StdErrorB_46$TempPrefMean <- "11.68"
StdErrorB_46$TempPrefMax <- "28.71"

StdErrorB_47 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `47`, ES = "LRRi")
StdErrorB_47$Ratio <- log(Ratios_NAT$`47`[c(7)])
StdErrorB_47$TempPrefMean <- "22.17"
StdErrorB_47$TempPrefMax <- "26.57"

StdErrorB_48 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `48`, ES = "LRRi")
StdErrorB_48$Ratio <- log(Ratios_NAT$`48`[c(7)])
StdErrorB_48$TempPrefMean <- "21.94"
StdErrorB_48$TempPrefMax <- "28.01"

StdErrorB_49 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `49`, ES = "LRRi")
StdErrorB_49$Ratio <- log(Ratios_NAT$`49`[c(7)])
StdErrorB_49$TempPrefMean <- "11.0"
StdErrorB_49$TempPrefMax <- "23.78"

StdErrorB_50 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `50`, ES = "LRRi")
StdErrorB_50$Ratio <- log(Ratios_NAT$`50`[c(7)])
StdErrorB_50$TempPrefMean <- "19.98"
StdErrorB_50$TempPrefMax <- "29.28"

StdErrorB_51 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `51`, ES = "LRRi")
StdErrorB_51$Ratio <- log(Ratios_NAT$`51`[c(7)])
StdErrorB_51$TempPrefMean <- "19.08"
StdErrorB_51$TempPrefMax <- "24.60"

StdErrorB_52 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `52`, ES = "LRRi")
StdErrorB_52$Ratio <- log(Ratios_NAT$`52`[c(7)])
StdErrorB_52$TempPrefMean <- "15.53"
StdErrorB_52$TempPrefMax <- "28.69"

StdErrorB_53 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `53`, ES = "LRRi")
StdErrorB_53$Ratio <- log(Ratios_NAT$`53`[c(7)])
StdErrorB_53$TempPrefMean <- "14.43"
StdErrorB_53$TempPrefMax <- "29.69"

StdErrorB_54 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `54`, ES = "LRRi")
StdErrorB_54$Ratio <- log(Ratios_NAT$`54`[c(7)])
StdErrorB_54$TempPrefMean <- "19.06"
StdErrorB_54$TempPrefMax <- "27.01"

StdErrorB_55 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `55`, ES = "LRRi")
StdErrorB_55$Ratio <- log(Ratios_NAT$`55`[c(7)])
StdErrorB_55$TempPrefMean <- "24.91"
StdErrorB_55$TempPrefMax <- "26.65"

StdErrorB_56 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `56`, ES = "LRRi")
StdErrorB_56$Ratio <- log(Ratios_NAT$`56`[c(7)])
StdErrorB_56$TempPrefMean <- "17.03"
StdErrorB_56$TempPrefMax <- "28.71"

StdErrorB_57 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `57`, ES = "LRRi")
StdErrorB_57$Ratio <- log(Ratios_NAT$`57`[c(7)])
StdErrorB_57$TempPrefMean <- "12.26"
StdErrorB_57$TempPrefMax <- "20.98"

StdErrorB_58 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `58`, ES = "LRRi")
StdErrorB_58$Ratio <- log(Ratios_NAT$`58`[c(7)])
StdErrorB_58$TempPrefMean <- "19.11"
StdErrorB_58$TempPrefMax <- "25.89"

StdErrorB_59 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `59`, ES = "LRRi")
StdErrorB_59$Ratio <- log(Ratios_NAT$`59`[c(7)])
StdErrorB_59$TempPrefMean <- "17.40"
StdErrorB_59$TempPrefMax <- "21.06"

StdErrorB_60 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `60`, ES = "LRRi")
StdErrorB_60$Ratio <- log(Ratios_NAT$`60`[c(7)])
StdErrorB_60$TempPrefMean <- "13.93"
StdErrorB_60$TempPrefMax <- "13.93"

StdErrorB_61 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `61`, ES = "LRRi")
StdErrorB_61$Ratio <- log(Ratios_NAT$`61`[c(7)])
StdErrorB_61$TempPrefMean <- "21.10"
StdErrorB_61$TempPrefMax <- "27.96"

StdErrorB_62 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `62`, ES = "LRRi")
StdErrorB_62$Ratio <- log(Ratios_NAT$`62`[c(7)])
StdErrorB_62$TempPrefMean <- "NA"
StdErrorB_62$TempPrefMax <- "NA"

StdErrorB_63 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `63`, ES = "LRRi")
StdErrorB_63$Ratio <- log(Ratios_NAT$`63`[c(7)])
StdErrorB_63$TempPrefMean <- "13.50"
StdErrorB_63$TempPrefMax <- "28.57"

StdErrorB_64 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `64`, ES = "LRRi")
StdErrorB_64$Ratio <- log(Ratios_NAT$`64`[c(7)])
StdErrorB_64$TempPrefMean <- "12.86"
StdErrorB_64$TempPrefMax <- "27.09"

StdErrorB_65 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `65`, ES = "LRRi")
StdErrorB_65$Ratio <- log(Ratios_NAT$`65`[c(7)])
StdErrorB_65$TempPrefMean <- "19.13"
StdErrorB_65$TempPrefMax <- "28.19"

StdErrorB_66 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `66`, ES = "LRRi")
StdErrorB_66$Ratio <- log(Ratios_NAT$`66`[c(7)])
StdErrorB_66$TempPrefMean <- "12.46"
StdErrorB_66$TempPrefMax <- "23.26"

StdErrorB_67 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `67`, ES = "LRRi")
StdErrorB_67$Ratio <- log(Ratios_NAT$`67`[c(7)])
StdErrorB_67$TempPrefMean <- "26.37"
StdErrorB_67$TempPrefMax <- "28.73"

StdErrorB_68 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `68`, ES = "LRRi")
StdErrorB_68$Ratio <- log(Ratios_NAT$`68`[c(7)])
StdErrorB_68$TempPrefMean <- "12.63"
StdErrorB_68$TempPrefMax <- "27.17"

StdErrorB_69 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `69`, ES = "LRRi")
StdErrorB_69$Ratio <- log(Ratios_NAT$`69`[c(7)])
StdErrorB_69$TempPrefMean <- "26.91"
StdErrorB_69$TempPrefMax <- "28.73"

StdErrorB_70 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `70`, ES = "LRRi")
StdErrorB_70$Ratio <- log(Ratios_NAT$`70`[c(7)])
StdErrorB_70$TempPrefMean <- "10.84"
StdErrorB_70$TempPrefMax <- "13.18"

StdErrorB_71 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `71`, ES = "LRRi")
StdErrorB_71$Ratio <- log(Ratios_NAT$`71`[c(7)])
StdErrorB_71$TempPrefMean <- "12.85"
StdErrorB_71$TempPrefMax <- "27.42"

StdErrorB_72 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `72`, ES = "LRRi")
StdErrorB_72$Ratio <- log(Ratios_NAT$`72`[c(7)])
StdErrorB_72$TempPrefMean <- "19.32"
StdErrorB_72$TempPrefMax <- "28.40"

StdErrorB_73 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `73`, ES = "LRRi")
StdErrorB_73$Ratio <- log(Ratios_NAT$`73`[c(7)])
StdErrorB_73$TempPrefMean <- "15.86"
StdErrorB_73$TempPrefMax <- "28.73"


StdErrorB_74 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `74`, ES = "LRRi")
StdErrorB_74$Ratio <- log(Ratios_NAT$`74`[c(7)])
StdErrorB_74$TempPrefMean <- "23.08"
StdErrorB_74$TempPrefMax <- "29.38"

StdErrorB_75 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `75`, ES = "LRRi")
StdErrorB_75$Ratio <- log(Ratios_NAT$`75`[c(7)])
StdErrorB_75$TempPrefMean <- "19.52"
StdErrorB_75$TempPrefMax <- "27.15"

StdErrorB_76 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `76`, ES = "LRRi")
StdErrorB_76$Ratio <- log(Ratios_NAT$`76`[c(7)])
StdErrorB_76$TempPrefMean <- "25.87"
StdErrorB_76$TempPrefMax <- "28.73"

StdErrorB_77 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `77`, ES = "LRRi")
StdErrorB_77$Ratio <- log(Ratios_NAT$`77`[c(7)])
StdErrorB_77$TempPrefMean <- "18.30"
StdErrorB_77$TempPrefMax <- "24.71"

StdErrorB_78 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `78`, ES = "LRRi")
StdErrorB_78$Ratio <- log(Ratios_NAT$`78`[c(7)])
StdErrorB_78$TempPrefMean <- "17.58"
StdErrorB_78$TempPrefMax <- "24.61"

StdErrorB_79 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `79`, ES = "LRRi")
StdErrorB_79$Ratio <- log(Ratios_NAT$`79`[c(7)])
StdErrorB_79$TempPrefMean <- "17.68"
StdErrorB_79$TempPrefMax <- "28.29"

StdErrorB_80 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `80`, ES = "LRRi")
StdErrorB_80$Ratio <- log(Ratios_NAT$`80`[c(7)])
StdErrorB_80$TempPrefMean <- "24.56"
StdErrorB_80$TempPrefMax <- "28.73"

StdErrorB_81 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `81`, ES = "LRRi")
StdErrorB_81$Ratio <- log(Ratios_NAT$`81`[c(7)])
StdErrorB_81$TempPrefMean <- "25.25"
StdErrorB_81$TempPrefMax <- "28.73"

StdErrorB_82 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `82`, ES = "LRRi")
StdErrorB_82$Ratio <- log(Ratios_NAT$`82`[c(7)])
StdErrorB_82$TempPrefMean <- "22.04"
StdErrorB_82$TempPrefMax <- "28.73"

StdErrorB_83 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `83`, ES = "LRRi")
StdErrorB_83$Ratio <- log(Ratios_NAT$`83`[c(7)])
StdErrorB_83$TempPrefMean <- "NA"
StdErrorB_83$TempPrefMax <- "NA"

StdErrorB_84 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `84`, ES = "LRRi")
StdErrorB_84$Ratio <- log(Ratios_NAT$`84`[c(7)])
StdErrorB_84$TempPrefMean <- "13.08"
StdErrorB_84$TempPrefMax <- "27.14"

StdErrorB_85 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `85`, ES = "LRRi")
StdErrorB_85$Ratio <- log(Ratios_NAT$`85`[c(7)])
StdErrorB_85$TempPrefMean <- "19.75"
StdErrorB_85$TempPrefMax <- "28.35"

StdErrorB_86 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `86`, ES = "LRRi")
StdErrorB_86$Ratio <- log(Ratios_NAT$`86`[c(7)])
StdErrorB_86$TempPrefMean <- "24.24"
StdErrorB_86$TempPrefMax <- "28.73"

StdErrorB_87 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `87`, ES = "LRRi")
StdErrorB_87$Ratio <- log(Ratios_NAT$`87`[c(7)])
StdErrorB_87$TempPrefMean <- "17.52"
StdErrorB_87$TempPrefMax <- "28.73"

StdErrorB_88 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `88`, ES = "LRRi")
StdErrorB_88$Ratio <- log(Ratios_NAT$`88`[c(7)])
StdErrorB_88$TempPrefMean <- "23.41"
StdErrorB_88$TempPrefMax <- "26.80"
StdErrorB_89 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `89`, ES = "LRRi")
StdErrorB_89$Ratio <- log(Ratios_NAT$`89`[c(7)])
StdErrorB_89$TempPrefMean <- "12.56"
StdErrorB_89$TempPrefMax <- "25.35"

StdErrorB_90 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `90`, ES = "LRRi")
StdErrorB_90$Ratio <- log(Ratios_NAT$`90`[c(7)])
StdErrorB_90$TempPrefMean <- "24.23"
StdErrorB_90$TempPrefMax <- "28.73"

StdErrorB_91 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `91`, ES = "LRRi")
StdErrorB_91$Ratio <- log(Ratios_NAT$`91`[c(7)])
StdErrorB_91$TempPrefMean <- "26.45"
StdErrorB_91$TempPrefMax <- "28.73"

StdErrorB_92 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `92`, ES = "LRRi")
StdErrorB_92$Ratio <- log(Ratios_NAT$`92`[c(7)])
StdErrorB_92$TempPrefMean <- "13.35"
StdErrorB_92$TempPrefMax <- "24.89"

StdErrorB_93 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `93`, ES = "LRRi")
StdErrorB_93$Ratio <- log(Ratios_NAT$`93`[c(7)])
StdErrorB_93$TempPrefMean <- "22.75"
StdErrorB_93$TempPrefMax <- "29.18"

StdErrorB_94 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `94`, ES = "LRRi")
StdErrorB_94$Ratio <- log(Ratios_NAT$`94`[c(7)])
StdErrorB_94$TempPrefMean <- "25.20"
StdErrorB_94$TempPrefMax <- ",28.73"

StdErrorB_95 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `95`, ES = "LRRi")
StdErrorB_95$Ratio <- log(Ratios_NAT$`95`[c(7)])
StdErrorB_95$TempPrefMean <- "24.53"
StdErrorB_95$TempPrefMax <- "28.73"

StdErrorB_96 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `96`, ES = "LRRi")
StdErrorB_96$Ratio <- log(Ratios_NAT$`96`[c(7)])
StdErrorB_96$TempPrefMean <- "19.06"
StdErrorB_96$TempPrefMax <- "29.43"

StdErrorB_97 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `97`, ES = "LRRi")
StdErrorB_97$Ratio <- log(Ratios_NAT$`97`[c(7)])
StdErrorB_97$TempPrefMean <- "25.69"
StdErrorB_97$TempPrefMax <- "28.73"

StdErrorB_98 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `98`, ES = "LRRi")
StdErrorB_98$Ratio <- log(Ratios_NAT$`98`[c(7)])
StdErrorB_98$TempPrefMean <- "20.36"
StdErrorB_98$TempPrefMax <- "25.40"

StdErrorB_99 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                              outcome = `99`, ES = "LRRi")
StdErrorB_99$Ratio <- log(Ratios_NAT$`99`[c(7)])
StdErrorB_99$TempPrefMean <- "18.55"
StdErrorB_99$TempPrefMax <- "29.39"
StdErrorB_100 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `100`, ES = "LRRi")
StdErrorB_100$Ratio <- log(Ratios_NAT$`100`[c(7)])
StdErrorB_100$TempPrefMean <- "17.34"
StdErrorB_100$TempPrefMax <- "26.54"

StdErrorB_101 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `101`, ES = "LRRi")
StdErrorB_101$Ratio <- log(Ratios_NAT$`101`[c(7)])
StdErrorB_101$TempPrefMean <- "22.10"
StdErrorB_101$TempPrefMax <- "29.64"

StdErrorB_102 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `102`, ES = "LRRi")
StdErrorB_102$Ratio <- log(Ratios_NAT$`102`[c(7)])
StdErrorB_102$TempPrefMean <- "24.24"
StdErrorB_102$TempPrefMax <- "29.60"

StdErrorB_103 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `103`, ES = "LRRi")
StdErrorB_103$Ratio <- log(Ratios_NAT$`103`[c(7)])
StdErrorB_103$TempPrefMean <- "20.86"
StdErrorB_103$TempPrefMax <- "28.73"

StdErrorB_104 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `104`, ES = "LRRi")
StdErrorB_104$Ratio <- log(Ratios_NAT$`104`[c(7)])
StdErrorB_104$TempPrefMean <- "17.43"
StdErrorB_104$TempPrefMax <- "21.34"

StdErrorB_105 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `105`, ES = "LRRi")
StdErrorB_105$Ratio <- log(Ratios_NAT$`105`[c(7)])
StdErrorB_105$TempPrefMean <- "11.99"
StdErrorB_105$TempPrefMax <- "27.93"

StdErrorB_106 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `106`, ES = "LRRi")
StdErrorB_106$Ratio <- log(Ratios_NAT$`106`[c(7)])
StdErrorB_106$TempPrefMean <- "22.96"
StdErrorB_106$TempPrefMax <- "29.39"

StdErrorB_107 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `107`, ES = "LRRi")
StdErrorB_107$Ratio <- log(Ratios_NAT$`107`[c(7)])
StdErrorB_107$TempPrefMean <- "15.89"
StdErrorB_107$TempPrefMax <- "25.11"

StdErrorB_108 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `108`, ES = "LRRi")
StdErrorB_108$Ratio <- log(Ratios_NAT$`108`[c(7)])
StdErrorB_108$TempPrefMean <- "20.77"
StdErrorB_108$TempPrefMax <- "28.99"

StdErrorB_109 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `109`, ES = "LRRi")
StdErrorB_109$Ratio <- log(Ratios_NAT$`109`[c(7)])
StdErrorB_109$TempPrefMean <- "23.90"
StdErrorB_109$TempPrefMax <- "28.57"

StdErrorB_110 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `110`, ES = "LRRi")
StdErrorB_110$Ratio <- log(Ratios_NAT$`110`[c(7)])
StdErrorB_110$TempPrefMean <- "21.05"
StdErrorB_110$TempPrefMax <- "29.42"

StdErrorB_111 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `111`, ES = "LRRi")
StdErrorB_111$Ratio <- log(Ratios_NAT$`111`[c(7)])
StdErrorB_111$TempPrefMean <- "18.39"
StdErrorB_111$TempPrefMax <- "24.74"

StdErrorB_112 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `112`, ES = "LRRi")
StdErrorB_112$Ratio <- log(Ratios_NAT$`112`[c(7)])
StdErrorB_112$TempPrefMean <- "NA"
StdErrorB_112$TempPrefMax <- "NA"

StdErrorB_113 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `113`, ES = "LRRi")
StdErrorB_113$Ratio <- log(Ratios_NAT$`113`[c(7)])
StdErrorB_113$TempPrefMean <- "15.03"
StdErrorB_113$TempPrefMax <- "24.86"

StdErrorB_114 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `114`, ES = "LRRi")
StdErrorB_114$Ratio <- log(Ratios_NAT$`114`[c(7)])
StdErrorB_114$TempPrefMean <- "22.29"
StdErrorB_114$TempPrefMax <- "28.65"

StdErrorB_115 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `115`, ES = "LRRi")
StdErrorB_115$Ratio <- log(Ratios_NAT$`115`[c(7)])
StdErrorB_115$TempPrefMean <- "22.98"
StdErrorB_115$TempPrefMax <- "28.47"

StdErrorB_116 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `116`, ES = "LRRi")
StdErrorB_116$Ratio <- log(Ratios_NAT$`116`[c(7)])
StdErrorB_116$TempPrefMean <- "24.37"
StdErrorB_116$TempPrefMax <- "29.22"

StdErrorB_117 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `117`, ES = "LRRi")
StdErrorB_117$Ratio <- log(Ratios_NAT$`117`[c(7)])
StdErrorB_117$TempPrefMean <- "18.13"
StdErrorB_117$TempPrefMax <- "28.73"

StdErrorB_118 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `118`, ES = "LRRi")
StdErrorB_118$Ratio <- log(Ratios_NAT$`118`[c(7)])
StdErrorB_118$TempPrefMean <- "20.48"
StdErrorB_118$TempPrefMax <- "29.43"

StdErrorB_119 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `119`, ES = "LRRi")
StdErrorB_119$Ratio <- log(Ratios_NAT$`119`[c(7)])
StdErrorB_119$TempPrefMean <- "25.90"
StdErrorB_119$TempPrefMax <- "28.73"

StdErrorB_120 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `120`, ES = "LRRi")
StdErrorB_120$Ratio <- log(Ratios_NAT$`120`[c(7)])
StdErrorB_120$TempPrefMean <- "21.73"
StdErrorB_120$TempPrefMax <- "24.74"

StdErrorB_121 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `121`, ES = "LRRi")
StdErrorB_121$Ratio <- log(Ratios_NAT$`121`[c(7)])
StdErrorB_121$TempPrefMean <- "12.14"
StdErrorB_121$TempPrefMax <- "27.73"

StdErrorB_122 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `122`, ES = "LRRi")
StdErrorB_122$Ratio <- log(Ratios_NAT$`122`[c(7)])
StdErrorB_122$TempPrefMean <- "25.67"
StdErrorB_122$TempPrefMax <- "28.73"

StdErrorB_123 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `123`, ES = "LRRi")
StdErrorB_123$Ratio <- log(Ratios_NAT$`123`[c(7)])
StdErrorB_123$TempPrefMean <- "15.88"
StdErrorB_123$TempPrefMax <- "21.16"

StdErrorB_124 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `124`, ES = "LRRi")
StdErrorB_124$Ratio <- log(Ratios_NAT$`124`[c(7)])
StdErrorB_124$TempPrefMean <- "19.09"
StdErrorB_124$TempPrefMax <- "22.73"

StdErrorB_125 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `125`, ES = "LRRi")
StdErrorB_125$Ratio <- log(Ratios_NAT$`125`[c(7)])
StdErrorB_125$TempPrefMean <- "13.83"
StdErrorB_125$TempPrefMax <- "28.04"

StdErrorB_126 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `126`, ES = "LRRi")
StdErrorB_126$Ratio <- log(Ratios_NAT$`126`[c(7)])
StdErrorB_126$TempPrefMean <- "24.90"
StdErrorB_126$TempPrefMax <- "30.40"

StdErrorB_127 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `127`, ES = "LRRi")
StdErrorB_127$Ratio <- log(Ratios_NAT$`127`[c(7)])
StdErrorB_127$TempPrefMean <- "18.23"
StdErrorB_127$TempPrefMax <- "28.73"

StdErrorB_128 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `128`, ES = "LRRi")
StdErrorB_128$Ratio <- log(Ratios_NAT$`128`[c(7)])
StdErrorB_128$TempPrefMean <- "NA"
StdErrorB_128$TempPrefMax <- "NA"

StdErrorB_129 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `129`, ES = "LRRi")
StdErrorB_129$Ratio <- log(Ratios_NAT$`129`[c(7)])
StdErrorB_129$TempPrefMean <- "20.08"
StdErrorB_129$TempPrefMax <- "28.92"

StdErrorB_130 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `130`, ES = "LRRi")
StdErrorB_130$Ratio <- log(Ratios_NAT$`130`[c(7)])
StdErrorB_130$TempPrefMean <- "25.16"
StdErrorB_130$TempPrefMax <- "29.07"

StdErrorB_131 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                               outcome = `131`, ES = "LRRi")
StdErrorB_131$Ratio <- log(Ratios_NAT$`131`[c(7)])
StdErrorB_131$TempPrefMean <- "15.79"
StdErrorB_131$TempPrefMax <- "29.47"

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
Trawl100_Sp_Abundance <- rbind(StdError_NIS[c(1,4,7,8:10)],StdError_NAT[c(1,4,7,8:10)])
Trawl100_Sp_Abundance <- Trawl100_Sp_Abundance[-c(78,18,27,85),]
