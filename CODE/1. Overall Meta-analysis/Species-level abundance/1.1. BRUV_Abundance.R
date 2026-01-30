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
####        Load BRUV dataset and summarise             ####
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

StdError_18 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `18`, ES = "LRRi")
StdError_18$Ratio <- log(Ratios_NIS$`18`[c(6)])

StdError_19 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `19`, ES = "LRRi")
StdError_19$Ratio <- log(Ratios_NIS$`19`[c(6)])

StdError_20 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `20`, ES = "LRRi")
StdError_20$Ratio <- log(Ratios_NIS$`20`[c(6)])

StdError_21 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `21`, ES = "LRRi")
StdError_21$Ratio <- log(Ratios_NIS$`21`[c(6)])

StdError_29 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `29`, ES = "LRRi")
StdError_29$Ratio <- log(Ratios_NIS$`29`[c(6)])

StdError_30 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `30`, ES = "LRRi")
StdError_30$Ratio <- log(Ratios_NIS$`30`[c(6)])

StdError_31 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `31`, ES = "LRRi")
StdError_31$Ratio <- log(Ratios_NIS$`31`[c(6)])

StdError_33 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `33`, ES = "LRRi")
StdError_33$Ratio <- log(Ratios_NIS$`33`[c(6)])

StdError_34 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `34`, ES = "LRRi")
StdError_34$Ratio <- log(Ratios_NIS$`34`[c(6)])

StdError_35 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `35`, ES = "LRRi")
StdError_35$Ratio <- log(Ratios_NIS$`35`[c(6)])

StdError_39 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `39`, ES = "LRRi")
StdError_39$Ratio <- log(Ratios_NIS$`39`[c(6)])

StdError_40 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `40`, ES = "LRRi")
StdError_40$Ratio <- log(Ratios_NIS$`40`[c(6)])

StdError_46 <- batch_calc_ES(dat = StdError_DataNIS, grouping = Year, condition = Season, 
                             outcome = `46`, ES = "LRRi")
StdError_46$Ratio <- log(Ratios_NIS$`46`[c(6)])

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

StdError_2 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `2`, ES = "LRRi")
StdError_2$Ratio <- log(Ratios_NAT$`2`[c(6)])

StdError_3 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `3`, ES = "LRRi")
StdError_3$Ratio <- log(Ratios_NAT$`3`[c(6)])

StdError_4 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `4`, ES = "LRRi")
StdError_4$Ratio <- log(Ratios_NAT$`4`[c(6)])

StdError_5 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `5`, ES = "LRRi")
StdError_5$Ratio <- log(Ratios_NAT$`5`[c(6)])

StdError_6 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `6`, ES = "LRRi")
StdError_6$Ratio <- log(Ratios_NAT$`6`[c(6)])

StdError_7 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `7`, ES = "LRRi")
StdError_7$Ratio <- log(Ratios_NAT$`7`[c(6)])

StdError_8 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `8`, ES = "LRRi")
StdError_8$Ratio <- log(Ratios_NAT$`8`[c(6)])

StdError_9 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `9`, ES = "LRRi")
StdError_9$Ratio <- log(Ratios_NAT$`9`[c(6)])

StdError_10 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `10`, ES = "LRRi")
StdError_10$Ratio <- log(Ratios_NAT$`10`[c(6)])

StdError_11 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `11`, ES = "LRRi")
StdError_11$Ratio <- log(Ratios_NAT$`11`[c(6)])

StdError_12 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `12`, ES = "LRRi")
StdError_12$Ratio <- log(Ratios_NAT$`12`[c(6)])

StdError_13 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `13`, ES = "LRRi")
StdError_13$Ratio <- log(Ratios_NAT$`13`[c(6)])

StdError_15 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `15`, ES = "LRRi")
StdError_15$Ratio <- log(Ratios_NAT$`15`[c(6)])

StdError_16 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `16`, ES = "LRRi")
StdError_16$Ratio <- log(Ratios_NAT$`16`[c(6)])

StdError_17 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `17`, ES = "LRRi")
StdError_17$Ratio <- log(Ratios_NAT$`17`[c(6)])

StdError_22 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `22`, ES = "LRRi")
StdError_22$Ratio <- log(Ratios_NAT$`22`[c(6)])

StdError_23 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `23`, ES = "LRRi")
StdError_23$Ratio <- log(Ratios_NAT$`23`[c(6)])

StdError_24 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `24`, ES = "LRRi")
StdError_24$Ratio <- log(Ratios_NAT$`24`[c(6)])

StdError_25 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `25`, ES = "LRRi")
StdError_25$Ratio <- log(Ratios_NAT$`25`[c(6)])

StdError_26 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `26`, ES = "LRRi")
StdError_26$Ratio <- log(Ratios_NAT$`26`[c(6)])

StdError_27 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `27`, ES = "LRRi")
StdError_27$Ratio <- log(Ratios_NAT$`27`[c(6)])

StdError_28 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `28`, ES = "LRRi")
StdError_28$Ratio <- log(Ratios_NAT$`28`[c(6)])

StdError_32 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `32`, ES = "LRRi")
StdError_32$Ratio <- log(Ratios_NAT$`32`[c(6)])

StdError_36 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `36`, ES = "LRRi")
StdError_36$Ratio <- log(Ratios_NAT$`36`[c(6)])

StdError_37 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `37`, ES = "LRRi")
StdError_37$Ratio <- log(Ratios_NAT$`37`[c(6)])

StdError_38 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `38`, ES = "LRRi")
StdError_38$Ratio <- log(Ratios_NAT$`38`[c(6)])

StdError_41 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `41`, ES = "LRRi")
StdError_41$Ratio <- log(Ratios_NAT$`41`[c(6)])

StdError_42 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `42`, ES = "LRRi")
StdError_42$Ratio <- log(Ratios_NAT$`42`[c(6)])

StdError_43 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `43`, ES = "LRRi")
StdError_43$Ratio <- log(Ratios_NAT$`43`[c(6)])

StdError_44 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `44`, ES = "LRRi")
StdError_44$Ratio <- log(Ratios_NAT$`44`[c(6)])

StdError_45 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `45`, ES = "LRRi")
StdError_45$Ratio <- log(Ratios_NAT$`45`[c(6)])

StdError_47 <- batch_calc_ES(dat = StdError_DataNAT, grouping = Year, condition = Season, 
                            outcome = `47`, ES = "LRRi")
StdError_47$Ratio <- log(Ratios_NAT$`47`[c(6)])

StdError_NAT <- rbind(StdError_1,StdError_2,StdError_3,StdError_4,StdError_5,StdError_6,StdError_7,StdError_8,StdError_9,StdError_10,StdError_11,StdError_12,StdError_13,
                      StdError_15,StdError_16,StdError_17,StdError_22,StdError_23,StdError_24,StdError_25,StdError_26,StdError_27,StdError_28,StdError_32,StdError_36,StdError_37,
                      StdError_38,StdError_42,StdError_42,StdError_43,StdError_44,StdError_45,StdError_47)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
BRUV_Sp_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])

