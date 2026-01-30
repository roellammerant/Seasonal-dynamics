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
BRUV_data <- read.csv("DATA/BRUV.csv", 
                      na = "NA")
BRUV_MED  <- BRUV_data[which(BRUV_data$sea  =='med'),]

### take out dates outside our scope
BRUV_MED <- BRUV_MED[BRUV_MED$date != c('2021-02-03'),]
BRUV_MED <- BRUV_MED[BRUV_MED$date != c('2021-02-04'),]
BRUV_MED <- BRUV_MED[BRUV_MED$date != c('2022-02-28'),]
BRUV_MED <- BRUV_MED[BRUV_MED$date != c('2022-03-01'),]
BRUV_MED <- BRUV_MED[BRUV_MED$date != c('2022-07-27'),]
BRUV_MED <- BRUV_MED[BRUV_MED$date != c('2022-07-28'),]

### subset by depth
BRUV_MED$depth <- as.numeric(BRUV_MED$depth)
BRUV_MED <- BRUV_MED[BRUV_MED$depth < 40,]

### reshape dataset
BRUVSubset <-reshape2::dcast(BRUV_MED, OpCode~species, value.var="MaxN")
BRUVSubset[is.na(BRUVSubset)] <- 0
#%######################################################%##
#                                                          #
####                  Sprich Trawling                   ####
#                                                          #
##%######################################################%##

# Change structure around: Species become rows and sites become columns
BRUVTranspose <- t(BRUVSubset[-c(1)])
BRUVTranspose <- as.data.frame(BRUVTranspose)
colnames(BRUVTranspose) <-BRUVSubset$OpCode

# Create invasive status list
Invasive <- as.data.frame(table(BRUV_MED$species,BRUV_MED$is_les))
Invasive <- Invasive[Invasive$Freq > 0,]
colnames(Invasive)[1:2]<- c("Species", "Invasive")
Invasive <- Invasive[c(1:2)]
BRUVTranspose$Species <- rownames(BRUVTranspose)
BRUVTranspose <- merge(BRUVTranspose, Invasive,  by.x = "Species", by.y = "Species")


BruvCategories <- as.data.frame(BRUVSubset$OpCode)
BruvCategories$Season <- c("Fall","Fall","Fall","Fall","Spring","Spring","Spring","Spring","Spring","Spring","Spring",
                           "Fall","Fall","Spring","Spring","Fall","Fall","Fall","Fall","Fall","Fall","Fall","Fall",
                           "Fall","Fall","Fall","Fall")
BruvCategories$Year <- c("2021","2021","2021","2019","2022","2022","2022","2021","2021","2021","2021","2019","2019",
                         "2022","2022","2020","2020","2020","2020","2020","2020","2019","2019","2019",
                         "2021","2021","2021")

### Create categories to merch replicates
BruvCategories$Merched_Category <- factor(paste(BruvCategories$Year, 
                                                BruvCategories$Season,
                                                sep = "_"))
### Invasive Status
BRUVNIS  <- BRUVTranspose[which(BRUVTranspose$Invasive  =='1'),]
BRUVNAT  <- BRUVTranspose[which(BRUVTranspose$Invasive  =='0'),]
BRUVNIS[c(2:28)] <- BRUVNIS[c(2:28)] %>% 
  mutate_if(is.character, as.numeric)
BRUVNAT[c(2:28)] <- BRUVNAT[c(2:28)] %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
BRUVNIS <- t(BRUVNIS)
BRUVNIS <- as.data.frame(BRUVNIS)
BRUVNIS <- BRUVNIS[-c(1,29),]
BRUVNIS$Merched_Category <- BruvCategories$Merched_Category
BRUVNIS <- BRUVNIS %>% 
  mutate_if(is.character, as.numeric)
Mean_NIS <- aggregate(BRUVNIS[c(1:14)], 
                      list(BRUVNIS$Merched_Category), FUN=mean) 

BRUVNAT <- t(BRUVNAT)
BRUVNAT <- as.data.frame(BRUVNAT)
BRUVNAT <- BRUVNAT[-c(1,29),]
BRUVNAT$Merched_Category <- BruvCategories$Merched_Category
BRUVNAT <- BRUVNAT %>% 
  mutate_if(is.character, as.numeric)
Mean_NAT <- aggregate(BRUVNAT[c(1:33)], 
                      list(BRUVNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$Ratio2021 <-Ratios_NIS$`2021_Fall`/Ratios_NIS$`2021_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio2021 <-Ratios_NAT$`2021_Fall`/Ratios_NAT$`2021_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for NIS ratios
BRUVNIS$Season <- BruvCategories$Season
BRUVNIS$Year <- BruvCategories$Year
StdError_DataNIS <- BRUVNIS[BRUVNIS$Year != c('2019'),]
StdError_DataNIS <- StdError_DataNIS[StdError_DataNIS$Year != c('2020'),]
StdError_DataNIS <- StdError_DataNIS[StdError_DataNIS$Year != c('2022'),]
StdError_DataNIS[StdError_DataNIS == 0] <- 0.1

StdError_14 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `14`, ES = "LRRi")
StdError_14$Ratio <- log(Ratios_NIS$`14`[c(6)])
StdError_14$TempPrefMean <- "26.58"
StdError_14$TempPrefMax <- "30.37"

StdError_18 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `18`, ES = "LRRi")
StdError_18$Ratio <- log(Ratios_NIS$`18`[c(6)])
StdError_18$TempPrefMean <- "27.82"
StdError_18$TempPrefMax <- "30.14"

StdError_19 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `19`, ES = "LRRi")
StdError_19$Ratio <- log(Ratios_NIS$`19`[c(6)])
StdError_19$TempPrefMean <- "23.92"
StdError_19$TempPrefMax <- "29.49"

StdError_20 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `20`, ES = "LRRi")
StdError_20$Ratio <- log(Ratios_NIS$`20`[c(6)])
StdError_20$TempPrefMean <- "26.86"
StdError_20$TempPrefMax <- "29.43"

StdError_21 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `21`, ES = "LRRi")
StdError_21$Ratio <- log(Ratios_NIS$`21`[c(6)])
StdError_21$TempPrefMean <- "26.93"
StdError_21$TempPrefMax <- "30.02"

StdError_29 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `29`, ES = "LRRi")
StdError_29$Ratio <- log(Ratios_NIS$`29`[c(6)])
StdError_29$TempPrefMean <- "26.90"
StdError_29$TempPrefMax <- "30.33"

StdError_30 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `30`, ES = "LRRi")
StdError_30$Ratio <- log(Ratios_NIS$`30`[c(6)])
StdError_30$TempPrefMean <- "25.95"
StdError_30$TempPrefMax <- "26.79"

StdError_31 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `31`, ES = "LRRi")
StdError_31$Ratio <- log(Ratios_NIS$`31`[c(6)])
StdError_31$TempPrefMean <- "27.14"
StdError_31$TempPrefMax <- "30.29"

StdError_33 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `33`, ES = "LRRi")
StdError_33$Ratio <- log(Ratios_NIS$`33`[c(6)])
StdError_33$TempPrefMean <- "27.70"
StdError_33$TempPrefMax <- "30.39"

StdError_34 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `34`, ES = "LRRi")
StdError_34$Ratio <- log(Ratios_NIS$`34`[c(6)])
StdError_34$TempPrefMean <- "26.89"
StdError_34$TempPrefMax <- "30.33"

StdError_35 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `35`, ES = "LRRi")
StdError_35$Ratio <- log(Ratios_NIS$`35`[c(6)])
StdError_35$TempPrefMean <- "27.24"
StdError_35$TempPrefMax <- "30.30"

StdError_39 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `39`, ES = "LRRi")
StdError_39$Ratio <- log(Ratios_NIS$`39`[c(6)])
StdError_39$TempPrefMean <- "26.50"
StdError_39$TempPrefMax <- "30.33"

StdError_40 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `40`, ES = "LRRi")
StdError_40$Ratio <- log(Ratios_NIS$`40`[c(6)])
StdError_40$TempPrefMean <- "24.84"
StdError_40$TempPrefMax <- "30.33"

StdError_46 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `46`, ES = "LRRi")
StdError_46$Ratio <- log(Ratios_NIS$`46`[c(6)])
StdError_46$TempPrefMean <- "24.54"
StdError_46$TempPrefMax <- "28.33"

StdError_NIS <- rbind(StdError_14,StdError_18,StdError_19,StdError_20,StdError_21,StdError_29,StdError_30,StdError_31,StdError_34,StdError_35,StdError_39,StdError_40,
                      StdError_46)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

### StdError values for ratios
BRUVNAT$Season <- BruvCategories$Season
BRUVNAT$Year <- BruvCategories$Year
StdError_DataNAT <- BRUVNAT[BRUVNAT$Year != c('2019'),]
StdError_DataNAT <- StdError_DataNAT[StdError_DataNAT$Year != c('2020'),]
StdError_DataNAT <- StdError_DataNAT[StdError_DataNAT$Year != c('2022'),]
StdError_DataNAT[StdError_DataNAT == 0] <- 0.1

StdError_1 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `1`, ES = "LRRi")
StdError_1$Ratio <- log(Ratios_NAT$`1`[c(6)])
StdError_1$TempPrefMean <- "22.46"
StdError_1$TempPrefMax <- "29.30"

StdError_2 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `2`, ES = "LRRi")
StdError_2$Ratio <- log(Ratios_NAT$`2`[c(6)])
StdError_2$TempPrefMean <- "19.79"
StdError_2$TempPrefMax <- "28.73"

StdError_3 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `3`, ES = "LRRi")
StdError_3$Ratio <- log(Ratios_NAT$`3`[c(6)])
StdError_3$TempPrefMean <- "24.32"
StdError_3$TempPrefMax <- "29.48"

StdError_4 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `4`, ES = "LRRi")
StdError_4$Ratio <- log(Ratios_NAT$`4`[c(6)])
StdError_4$TempPrefMean <- "24.28"
StdError_4$TempPrefMax <- "29.53"

StdError_5 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `5`, ES = "LRRi")
StdError_5$Ratio <- log(Ratios_NAT$`5`[c(6)])
StdError_5$TempPrefMean <- "24.88"
StdError_5$TempPrefMax <- "28.73"

StdError_6 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `6`, ES = "LRRi")
StdError_6$Ratio <- log(Ratios_NAT$`6`[c(6)])
StdError_6$TempPrefMean <- "22.51"
StdError_6$TempPrefMax <- "28.73"

StdError_7 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `7`, ES = "LRRi")
StdError_7$Ratio <- log(Ratios_NAT$`7`[c(6)])
StdError_7$TempPrefMean <- "17.78"
StdError_7$TempPrefMax <- "29.12"

StdError_8 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `8`, ES = "LRRi")
StdError_8$Ratio <- log(Ratios_NAT$`8`[c(6)])
StdError_8$TempPrefMean <- "19.72"
StdError_8$TempPrefMax <- "24.94"

StdError_9 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `9`, ES = "LRRi")
StdError_9$Ratio <- log(Ratios_NAT$`9`[c(6)])
StdError_9$TempPrefMean <- "18.88"
StdError_9$TempPrefMax <- "23.51"

StdError_10 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `10`, ES = "LRRi")
StdError_10$Ratio <- log(Ratios_NAT$`10`[c(6)])
StdError_10$TempPrefMean <- "18.82"
StdError_10$TempPrefMax <- "26.02"

StdError_11 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `11`, ES = "LRRi")
StdError_11$Ratio <- log(Ratios_NAT$`11`[c(6)])
StdError_11$TempPrefMean <- "26.57"
StdError_11$TempPrefMax <- "30.39"

StdError_12 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `12`, ES = "LRRi")
StdError_12$Ratio <- log(Ratios_NAT$`12`[c(6)])
StdError_12$TempPrefMean <- "23.08"
StdError_12$TempPrefMax <- "27.15"

StdError_13 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `13`, ES = "LRRi")
StdError_13$Ratio <- log(Ratios_NAT$`13`[c(6)])
StdError_13$TempPrefMean <- "22.94"
StdError_13$TempPrefMax <- "28.73"

StdError_15 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `15`, ES = "LRRi")
StdError_15$Ratio <- log(Ratios_NAT$`15`[c(6)])
StdError_15$TempPrefMean <- "26.41"
StdError_15$TempPrefMax <- "28.83"

StdError_16 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `16`, ES = "LRRi")
StdError_16$Ratio <- log(Ratios_NAT$`16`[c(6)])
StdError_16$TempPrefMean <- "17.54"
StdError_16$TempPrefMax <- "26.09"

StdError_17 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `17`, ES = "LRRi")
StdError_17$Ratio <- log(Ratios_NAT$`17`[c(6)])
StdError_17$TempPrefMean <- "NA"
StdError_17$TempPrefMax <- "NA"

StdError_22 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `22`, ES = "LRRi")
StdError_22$Ratio <- log(Ratios_NAT$`22`[c(6)])
StdError_22$TempPrefMean <- "19.32"
StdError_22$TempPrefMax <- "28.40"

StdError_23 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `23`, ES = "LRRi")
StdError_23$Ratio <- log(Ratios_NAT$`23`[c(6)])
StdError_23$TempPrefMean <- "15.86"
StdError_23$TempPrefMax <- "28.73"

StdError_24 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `24`, ES = "LRRi")
StdError_24$Ratio <- log(Ratios_NAT$`24`[c(6)])
StdError_24$TempPrefMean <- "18.30"
StdError_24$TempPrefMax <- "24.71"

StdError_25 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `25`, ES = "LRRi")
StdError_25$Ratio <- log(Ratios_NAT$`25`[c(6)])
StdError_25$TempPrefMean <- "17.68"
StdError_25$TempPrefMax <- "28.29"

StdError_26 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `26`, ES = "LRRi")
StdError_26$Ratio <- log(Ratios_NAT$`26`[c(6)])
StdError_26$TempPrefMean <- "24.56"
StdError_26$TempPrefMax <- "28.73"

StdError_27 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `27`, ES = "LRRi")
StdError_27$Ratio <- log(Ratios_NAT$`27`[c(6)])
StdError_27$TempPrefMean <- "25.25"
StdError_27$TempPrefMax <- "28.73"

StdError_28 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `28`, ES = "LRRi")
StdError_28$Ratio <- log(Ratios_NAT$`28`[c(6)])
StdError_28$TempPrefMean <- "16.29"
StdError_28$TempPrefMax <- "16.73"

StdError_32 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `32`, ES = "LRRi")
StdError_32$Ratio <- log(Ratios_NAT$`32`[c(6)])
StdError_32$TempPrefMean <- "26.45"
StdError_32$TempPrefMax <- "28.73"

StdError_36 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `36`, ES = "LRRi")
StdError_36$Ratio <- log(Ratios_NAT$`36`[c(6)])
StdError_36$TempPrefMean <- "24.24"
StdError_36$TempPrefMax <- "29.60"

StdError_37 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `37`, ES = "LRRi")
StdError_37$Ratio <- log(Ratios_NAT$`37`[c(6)])
StdError_37$TempPrefMean <- "20.86"
StdError_37$TempPrefMax <- "28.73"

StdError_38 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `38`, ES = "LRRi")
StdError_38$Ratio <- log(Ratios_NAT$`38`[c(6)])
StdError_38$TempPrefMean <- "20.27"
StdError_38$TempPrefMax <- "24.38"

StdError_41 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `41`, ES = "LRRi")
StdError_41$Ratio <- log(Ratios_NAT$`41`[c(6)])
StdError_41$TempPrefMean <- "22.96"
StdError_41$TempPrefMax <- "29.39"

StdError_42 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `42`, ES = "LRRi")
StdError_42$Ratio <- log(Ratios_NAT$`42`[c(6)])
StdError_42$TempPrefMean <- "NA"
StdError_42$TempPrefMax <- "NA"

StdError_43 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `43`, ES = "LRRi")
StdError_43$Ratio <- log(Ratios_NAT$`43`[c(6)])
StdError_43$TempPrefMean <- "18.34"
StdError_43$TempPrefMax <- "22.61"

StdError_44 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `44`, ES = "LRRi")
StdError_44$Ratio <- log(Ratios_NAT$`44`[c(6)])
StdError_44$TempPrefMean <- "24.95"
StdError_44$TempPrefMax <- "28.73"

StdError_45 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `45`, ES = "LRRi")
StdError_45$Ratio <- log(Ratios_NAT$`45`[c(6)])
StdError_45$TempPrefMean <- "20.60"
StdError_45$TempPrefMax <- "25.08"

StdError_47 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                             outcome = `47`, ES = "LRRi")
StdError_47$Ratio <- log(Ratios_NAT$`47`[c(6)])
StdError_47$TempPrefMean <- "25.16"
StdError_47$TempPrefMax <- "29.07"

StdError_NAT <- rbind(StdError_1,StdError_2,StdError_3,StdError_4,StdError_5,StdError_6,StdError_7,StdError_8,StdError_9,StdError_10,StdError_11,StdError_12,StdError_13,
                      StdError_15,StdError_16,StdError_17,StdError_22,StdError_23,StdError_24,StdError_25,StdError_26,StdError_27,StdError_28,StdError_32,StdError_36,StdError_37,
                      StdError_38,StdError_42,StdError_42,StdError_43,StdError_44,StdError_45,StdError_47)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
BRUV_Sp_Abundance <- rbind(StdError_NIS[c(1,4,7,8:10)],StdError_NAT[c(1,4,7,8:10)])
BRUV_Sp_Abundance <- BRUV_Sp_Abundance[-c(32,33),]
