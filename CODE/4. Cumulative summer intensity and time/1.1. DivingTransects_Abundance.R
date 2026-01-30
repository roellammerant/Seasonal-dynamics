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

TransectsAll <- read_excel("DATA/DivingTransects.xlsx", 
                           na = "NA")
### Only species that where seen in the transects
TransectsAll <- TransectsAll[TransectsAll$inside_150m_belt_transect == c('Inside'),]
AbunTransect <-as.data.frame(with(TransectsAll, tapply(abundance, list(trans_ID, species), sum)))
AbunTransect[is.na(AbunTransect)] = 0
AbunTransect$trans_ID <- rownames(AbunTransect)

### Create categories to merch replicates
TransectCategories <- TransectsAll[c(1:6,9,13)]
TransectCategories <- TransectCategories %>% 
  mutate_if(is.numeric, as.character)
TransectCategories$trans_ID <- as.character(TransectCategories$trans_ID)

Year <- as.data.frame(table(TransectCategories$trans_ID,TransectCategories$year))
Year <- Year[Year$Freq > 0,]
colnames(Year)[1:2]<- c("trans_ID", "Year")
Year <- Year[c(1:2)]

Season <- as.data.frame(table(TransectCategories$trans_ID,TransectCategories$season))
Season <- Season[Season$Freq > 0,]
colnames(Season)[1:2]<- c("trans_ID", "Season")
Season <- Season[c(1:2)]

Date <- as.data.frame(table(TransectCategories$trans_ID,TransectCategories$date))
Date <- Date[Date$Freq > 0,]
colnames(Date)[1:2]<- c("trans_ID", "Date")
Date <- Date[c(1:2)]

TransectCategoriesB <- merge(AbunTransect, Year,  by.x = "trans_ID", by.y = "trans_ID")
TransectCategoriesB <- merge(TransectCategoriesB, Season,  by.x = "trans_ID", by.y = "trans_ID")
TransectCategoriesB <- merge(TransectCategoriesB, Date,  by.x = "trans_ID", by.y = "trans_ID")

TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2017-06-04'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2017-06-05'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2017-06-06'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2017-06-07'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2021-06-28'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2021-09-29'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2023-06-26'),]
TransectCategoriesB <- TransectCategoriesB[TransectCategoriesB$Date != c('2023-06-28'),]

TransectCategoriesB$Merched_Category <- factor(paste(TransectCategoriesB$Year, 
                                                     TransectCategoriesB$Season,
                                                     sep = "_"))

### Create Community level abundance columns
TransectTranspose <- t(TransectCategoriesB)
TransectTranspose <- as.data.frame(TransectTranspose)
TransectTranspose <- TransectTranspose[-c(1,84,85,86,87),]

# Create invasive status list
Invasive <- as.data.frame(table(TransectsAll$species,TransectsAll$les))
Invasive <- Invasive[Invasive$Freq > 0,]
colnames(Invasive)[1:2]<- c("Species", "Invasive")
Invasive <- Invasive[c(1:2)]
TransectTranspose$Species <- rownames(TransectTranspose)
TransectTranspose <- merge(TransectTranspose, Invasive,  by.x = "Species", by.y = "Species")

TransectNIS  <- TransectTranspose[which(TransectTranspose$Invasive  =='1'),]
TransectNAT  <- TransectTranspose[which(TransectTranspose$Invasive  =='0'),]
TransectNIS[c(2:1315)] <- TransectNIS[c(2:1315)] %>% 
  mutate_if(is.character, as.numeric)
TransectNAT[c(2:1315)] <- TransectNAT[c(2:1315)] %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
TransectNIS <- t(TransectNIS)
TransectNIS <- as.data.frame(TransectNIS)
TransectNIS <- TransectNIS[-c(1,1316),]
TransectNIS$Merched_Category <- TransectCategoriesB$Merched_Category
TransectNIS <- TransectNIS %>% 
  mutate_if(is.character, as.numeric)
Mean_NIS <- aggregate(TransectNIS[c(1:20)], 
                      list(TransectNIS$Merched_Category), FUN=mean) 

TransectNAT <- t(TransectNAT)
TransectNAT <- as.data.frame(TransectNAT)
TransectNAT <- TransectNAT[-c(1,1316),]
TransectNAT$Merched_Category <- TransectCategoriesB$Merched_Category
TransectNAT <- TransectNAT %>% 
  mutate_if(is.character, as.numeric)
Mean_NAT <- aggregate(TransectNAT[c(1:61)], 
                      list(TransectNAT$Merched_Category), FUN=mean) 


### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
Ratios_NIS$Ratio2015 <-Ratios_NIS$`2015_Fall`/Ratios_NIS$`2015_Spring`
Ratios_NIS$Ratio2019 <-Ratios_NIS$`2019_Fall`/Ratios_NIS$`2019_Spring`
Ratios_NIS$Ratio2021 <-Ratios_NIS$`2021_Fall`/Ratios_NIS$`2021_Spring`
Ratios_NIS$Ratio2023 <-Ratios_NIS$`2023_Fall`/Ratios_NIS$`2023_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio2015 <-Ratios_NAT$`2015_Fall`/Ratios_NAT$`2015_Spring`
Ratios_NAT$Ratio2019 <-Ratios_NAT$`2019_Fall`/Ratios_NAT$`2019_Spring`
Ratios_NAT$Ratio2021 <-Ratios_NAT$`2021_Fall`/Ratios_NAT$`2021_Spring`
Ratios_NAT$Ratio2023 <-Ratios_NAT$`2023_Fall`/Ratios_NAT$`2023_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

##### StdError values for ratios NIS species #####
TransectNIS$eventYear <- TransectCategoriesB$Year
TransectNIS$eventSeason <-  TransectCategoriesB$Season
TransectNIS[TransectNIS == 0] <- 0.1
StdError_Data <- TransectNIS[TransectNIS$eventYear != c('2017'),]
StdError_Data <- StdError_Data[StdError_Data$eventYear != c('2018'),]

StdError_10 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                               outcome = `10`, ES ="LRRi")
StdError_10$Ratio <- log(Ratios_NIS$`10`[c(11:14)])
StdError_10$Species <- "10"

StdError_24 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome =`24`, ES ="LRRi")
StdError_24$Ratio <- log(Ratios_NIS$`24`[c(11:14)])
StdError_24$Species <- "24"

StdError_25 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `25`, ES ="LRRi")
StdError_25$Ratio <- log(Ratios_NIS$`25`[c(11:14)])
StdError_25$Species <- "25"

StdError_26 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `26`, ES ="LRRi")
StdError_26$Ratio <- log(Ratios_NIS$`26`[c(11:14)])
StdError_26$Species <- "26"

StdError_27 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `27`, ES ="LRRi")
StdError_27$Ratio <- log(Ratios_NIS$`27`[c(11:14)])
StdError_27$Species <- "27"

StdError_40 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `40`, ES ="LRRi")
StdError_40$Ratio <- log(Ratios_NIS$`40`[c(11:14)])
StdError_40$Species <- "40"

StdError_41 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `41`, ES ="LRRi")
StdError_41$Ratio <- log(Ratios_NIS$`41`[c(11:14)])
StdError_41$Species <- "41"

StdError_42 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `42`, ES ="LRRi")
StdError_42$Ratio <- log(Ratios_NIS$`42`[c(11:14)])
StdError_42$Species <- "42"

StdError_43 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `43`, ES ="LRRi")
StdError_43$Ratio <- log(Ratios_NIS$`43`[c(11:14)])
StdError_43$Species <- "43"

StdError_46 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `46`, ES ="LRRi")
StdError_46$Ratio <- log(Ratios_NIS$`46`[c(11:14)])
StdError_46$Species <- "46"

StdError_47 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `47`, ES ="LRRi")
StdError_47$Ratio <- log(Ratios_NIS$`47`[c(11:14)])
StdError_47$Species <- "47"

StdError_50 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `50`, ES ="LRRi")
StdError_50$Ratio <- log(Ratios_NIS$`50`[c(11:14)])
StdError_50$Species <- "50"

StdError_52 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `52`, ES ="LRRi")
StdError_52$Ratio <- log(Ratios_NIS$`52`[c(11:14)])
StdError_52$Species <- "52"

StdError_54 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `54`, ES ="LRRi")
StdError_54$Ratio <- log(Ratios_NIS$`54`[c(11:14)])
StdError_54$Species <- "54"

StdError_60 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `60`, ES ="LRRi")
StdError_60$Ratio <- log(Ratios_NIS$`60`[c(11:14)])
StdError_60$Species <- "60"

StdError_61 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `61`, ES ="LRRi")
StdError_61$Ratio <- log(Ratios_NIS$`61`[c(11:14)])
StdError_61$Species <- "61"

StdError_64 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `64`, ES ="LRRi")
StdError_64$Ratio <- log(Ratios_NIS$`64`[c(11:14)])
StdError_64$Species <- "64"

StdError_67 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `67`, ES ="LRRi")
StdError_67$Ratio <- log(Ratios_NIS$`67`[c(11:14)])
StdError_67$Species <- "67"

StdError_77 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `77`, ES ="LRRi")
StdError_77$Ratio <- log(Ratios_NIS$`77`[c(11:14)])
StdError_77$Species <- "77"

StdError_80 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `80`, ES ="LRRi")
StdError_80$Ratio <- log(Ratios_NIS$`80`[c(11:14)])
StdError_80$Species <- "80"

StdError_NIS<- rbind(StdError_10,StdError_24,StdError_25,StdError_26,StdError_27,StdError_40,
                     StdError_41,StdError_42,StdError_43,StdError_46,StdError_47,StdError_50,
                     StdError_52,StdError_54,StdError_60,StdError_61,StdError_64,StdError_67,
                     StdError_77,StdError_80)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NIS species ####
TransectNAT$eventYear <- TransectCategoriesB$Year
TransectNAT$eventSeason <-  TransectCategoriesB$Season
TransectNAT[TransectNAT == 0] <- 0.1
StdError_DataB <- TransectNAT[TransectNAT$eventYear != c('2017'),]
StdError_DataB <- StdError_DataB[StdError_DataB$eventYear != c('2018'),]

StdError_1 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `1`, ES ="LRRi")
StdError_1$Ratio <- log(Ratios_NAT$`1`[c(11:14)])
StdError_1$Species <- "1"

StdError_2 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `2`, ES ="LRRi")
StdError_2$Ratio <- log(Ratios_NAT$`2`[c(11:14)])
StdError_2$Species <- "2"

StdError_3 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `3`, ES ="LRRi")
StdError_3$Ratio <- log(Ratios_NAT$`3`[c(11:14)])
StdError_3$Species <- "3"

StdError_4 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `4`, ES ="LRRi")
StdError_4$Ratio <- log(Ratios_NAT$`4`[c(11:14)])
StdError_4$Species <- "4"

StdError_5 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `5`, ES ="LRRi")
StdError_5$Ratio <- log(Ratios_NAT$`5`[c(11:14)])
StdError_5$Species <- "5"

StdError_6 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `6`, ES ="LRRi")
StdError_6$Ratio <- log(Ratios_NAT$`6`[c(11:14)])
StdError_6$Species <- "6"

StdError_7 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `7`, ES ="LRRi")
StdError_7$Ratio <- log(Ratios_NAT$`7`[c(11:14)])
StdError_7$Species <- "7"

StdError_8 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `8`, ES ="LRRi")
StdError_8$Ratio <- log(Ratios_NAT$`8`[c(11:14)])
StdError_8$Species <- "8"

StdError_9 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `9`, ES ="LRRi")
StdError_9$Ratio <- log(Ratios_NAT$`9`[c(11:14)])
StdError_9$Species <- "9"

StdError_11 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `11`, ES ="LRRi")
StdError_11$Ratio <- log(Ratios_NAT$`11`[c(11:14)])
StdError_11$Species <- "11"

StdError_12 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `12`, ES ="LRRi")
StdError_12$Ratio <- log(Ratios_NAT$`12`[c(11:14)])
StdError_12$Species <- "12"

StdError_13 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `13`, ES ="LRRi")
StdError_13$Ratio <- log(Ratios_NAT$`13`[c(11:14)])
StdError_13$Species <- "13"

StdError_14 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `14`, ES ="LRRi")
StdError_14$Ratio <- log(Ratios_NAT$`14`[c(11:14)])
StdError_14$Species <- "14"

StdError_15 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `15`, ES ="LRRi")
StdError_15$Ratio <- log(Ratios_NAT$`15`[c(11:14)])
StdError_15$Species <- "15"

StdError_16 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `16`, ES ="LRRi")
StdError_16$Ratio <- log(Ratios_NAT$`16`[c(11:14)])
StdError_16$Species <- "16"

StdError_17 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `17`, ES ="LRRi")
StdError_17$Ratio <- log(Ratios_NAT$`17`[c(11:14)])
StdError_17$Species <- "17"

StdError_18 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `18`, ES ="LRRi")
StdError_18$Ratio <- log(Ratios_NAT$`18`[c(11:14)])
StdError_18$Species <- "18"

StdError_19 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `19`, ES ="LRRi")
StdError_19$Ratio <- log(Ratios_NAT$`19`[c(11:14)])
StdError_19$Species <- "19"

StdError_20 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `20`, ES ="LRRi")
StdError_20$Ratio <- log(Ratios_NAT$`20`[c(11:14)])
StdError_20$Species <- "20"

StdError_21 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `21`, ES ="LRRi")
StdError_21$Ratio <- log(Ratios_NAT$`21`[c(11:14)])
StdError_21$Species <- "21"

StdError_22 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `22`, ES ="LRRi")
StdError_22$Ratio <- log(Ratios_NAT$`22`[c(11:14)])
StdError_22$Species <- "22"

StdError_23 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `23`, ES ="LRRi")
StdError_23$Ratio <- log(Ratios_NAT$`23`[c(11:14)])
StdError_23$Species <- "23"

StdError_28 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `28`, ES ="LRRi")
StdError_28$Ratio <- log(Ratios_NAT$`28`[c(11:14)])
StdError_28$Species <- "28"

StdError_29 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `29`, ES ="LRRi")
StdError_29$Ratio <- log(Ratios_NAT$`29`[c(11:14)])
StdError_29$Species <- "29"

StdError_30 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `30`, ES ="LRRi")
StdError_30$Ratio <- log(Ratios_NAT$`30`[c(11:14)])
StdError_30$Species <- "30"

StdError_31 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `31`, ES ="LRRi")
StdError_31$Ratio <- log(Ratios_NAT$`31`[c(11:14)])
StdError_31$Species <- "31"

StdError_32 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `32`, ES ="LRRi")
StdError_32$Ratio <- log(Ratios_NAT$`32`[c(11:14)])
StdError_32$Species <- "32"

StdError_33 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `33`, ES ="LRRi")
StdError_33$Ratio <- log(Ratios_NAT$`33`[c(11:14)])
StdError_33$Species <- "33"

StdError_34 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `34`, ES ="LRRi")
StdError_34$Ratio <- log(Ratios_NAT$`34`[c(11:14)])
StdError_34$Species <- "34"

StdError_35 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `35`, ES ="LRRi")
StdError_35$Ratio <- log(Ratios_NAT$`35`[c(11:14)])
StdError_35$Species <- "35"

StdError_36 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `36`, ES ="LRRi")
StdError_36$Ratio <- log(Ratios_NAT$`36`[c(11:14)])
StdError_36$Species <- "36"

StdError_37 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `37`, ES ="LRRi")
StdError_37$Ratio <- log(Ratios_NAT$`37`[c(11:14)])
StdError_37$Species <- "37"

StdError_38 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `38`, ES ="LRRi")
StdError_38$Ratio <- log(Ratios_NAT$`38`[c(11:14)])
StdError_38$Species <- "38"

StdError_39 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `39`, ES ="LRRi")
StdError_39$Ratio <- log(Ratios_NAT$`39`[c(11:14)])
StdError_39$Species <- "39"

StdError_44 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `44`, ES ="LRRi")
StdError_44$Ratio <- log(Ratios_NAT$`44`[c(11:14)])
StdError_44$Species <- "44"

StdError_45 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `45`, ES ="LRRi")
StdError_45$Ratio <- log(Ratios_NAT$`45`[c(11:14)])
StdError_45$Species <- "45"

StdError_48 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `48`, ES ="LRRi")
StdError_48$Ratio <- log(Ratios_NAT$`48`[c(11:14)])
StdError_48$Species <- "48"

StdError_49 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `49`, ES ="LRRi")
StdError_49$Ratio <- log(Ratios_NAT$`49`[c(11:14)])
StdError_49$Species <- "49"

StdError_51 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `51`, ES ="LRRi")
StdError_51$Ratio <- log(Ratios_NAT$`51`[c(11:14)])
StdError_51$Species <- "51"

StdError_53 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `53`, ES ="LRRi")
StdError_53$Ratio <- log(Ratios_NAT$`53`[c(11:14)])
StdError_53$Species <- "53"

StdError_55 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `55`, ES ="LRRi")
StdError_55$Ratio <- log(Ratios_NAT$`55`[c(11:14)])
StdError_55$Species <- "55"

StdError_56 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `56`, ES ="LRRi")
StdError_56$Ratio <- log(Ratios_NAT$`56`[c(11:14)])
StdError_56$Species <- "56"

StdError_57 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `57`, ES ="LRRi")
StdError_57$Ratio <- log(Ratios_NAT$`57`[c(11:14)])
StdError_57$Species <- "57"

StdError_58 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `58`, ES ="LRRi")
StdError_58$Ratio <- log(Ratios_NAT$`58`[c(11:14)])
StdError_58$Species <- "58"

StdError_59 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `59`, ES ="LRRi")
StdError_59$Ratio <- log(Ratios_NAT$`59`[c(11:14)])
StdError_59$Species <- "59"

StdError_62 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `62`, ES ="LRRi")
StdError_62$Ratio <- log(Ratios_NAT$`62`[c(11:14)])
StdError_62$Species <- "62"

StdError_63 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `63`, ES ="LRRi")
StdError_63$Ratio <- log(Ratios_NAT$`63`[c(11:14)])
StdError_63$Species <- "63"

StdError_65 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `65`, ES ="LRRi")
StdError_65$Ratio <- log(Ratios_NAT$`65`[c(11:14)])
StdError_65$Species <- "65"

StdError_66 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `66`, ES ="LRRi")
StdError_66$Ratio <- log(Ratios_NAT$`66`[c(11:14)])
StdError_66$Species <- "66"

StdError_68 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `68`, ES ="LRRi")
StdError_68$Ratio <- log(Ratios_NAT$`68`[c(11:14)])
StdError_68$Species <- "68"

StdError_69 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `69`, ES ="LRRi")
StdError_69$Ratio <- log(Ratios_NAT$`69`[c(11:14)])
StdError_69$Species <- "69"

StdError_70 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `70`, ES ="LRRi")
StdError_70$Ratio <- log(Ratios_NAT$`70`[c(11:14)])
StdError_70$Species <- "70"

StdError_71 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `71`, ES ="LRRi")
StdError_71$Ratio <- log(Ratios_NAT$`71`[c(11:14)])
StdError_71$Species <- "71"

StdError_72 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `72`, ES ="LRRi")
StdError_72$Ratio <- log(Ratios_NAT$`72`[c(11:14)])
StdError_72$Species <- "72"

StdError_73 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `73`, ES ="LRRi")
StdError_73$Ratio <- log(Ratios_NAT$`73`[c(11:14)])
StdError_73$Species <- "73"

StdError_74 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `74`, ES ="LRRi")
StdError_74$Ratio <- log(Ratios_NAT$`74`[c(11:14)])
StdError_74$Species <- "74"

StdError_75 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `75`, ES ="LRRi")
StdError_75$Ratio <- log(Ratios_NAT$`75`[c(11:14)])
StdError_75$Species <- "75"

StdError_76 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `76`, ES ="LRRi")
StdError_76$Ratio <- log(Ratios_NAT$`76`[c(11:14)])
StdError_76$Species <- "76"

StdError_78 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `78`, ES ="LRRi")
StdError_78$Ratio <- log(Ratios_NAT$`78`[c(11:14)])
StdError_78$Species <- "78"

StdError_79 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `79`, ES ="LRRi")
StdError_79$Ratio <- log(Ratios_NAT$`79`[c(11:14)])
StdError_79$Species <- "79"

StdError_81 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `81`, ES ="LRRi")
StdError_81$Ratio <- log(Ratios_NAT$`81`[c(11:14)])
StdError_81$Species <- "81"

StdError_NAT<- rbind(StdError_1,StdError_2,StdError_3,StdError_4,StdError_5,StdError_6,
                     StdError_7,StdError_8,StdError_9,StdError_11,StdError_12,StdError_13,
                     StdError_14,StdError_15,StdError_16,StdError_17,StdError_18,StdError_19,
                     StdError_20,StdError_21,StdError_22,StdError_23,StdError_28,StdError_29,
                     StdError_30,StdError_31,StdError_32,StdError_33,StdError_34,StdError_35,
                     StdError_36,StdError_37,StdError_38,StdError_39,StdError_44,StdError_45,
                     StdError_48,StdError_49,StdError_51,StdError_53,StdError_55,StdError_56,
                     StdError_66,StdError_68,StdError_69,StdError_70,StdError_71,StdError_72,
                     StdError_73,StdError_74,StdError_75,StdError_76,StdError_78,StdError_79,
                     StdError_81)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
Transect_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9)],StdError_NAT[c(1,4,7,8,9)])
