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
StdError_10$TempPrefMean <- "27.82"
StdError_10$TempPrefMax <- "29.85"

StdError_24 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome =`24`, ES ="LRRi")
StdError_24$Ratio <- log(Ratios_NIS$`24`[c(11:14)])
StdError_24$TempPrefMean <- "26.58"
StdError_24$TempPrefMax <- "30.37"

StdError_25 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `25`, ES ="LRRi")
StdError_25$Ratio <- log(Ratios_NIS$`25`[c(11:14)])
StdError_25$TempPrefMean <- "28.35"
StdError_25$TempPrefMax <- "29.39"

StdError_26 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `26`, ES ="LRRi")
StdError_26$Ratio <- log(Ratios_NIS$`26`[c(11:14)])
StdError_26$TempPrefMean <- "27.82"
StdError_26$TempPrefMax <- "30.14"

StdError_27 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `27`, ES ="LRRi")
StdError_27$Ratio <- log(Ratios_NIS$`27`[c(11:14)])
StdError_27$TempPrefMean <- "26.86"
StdError_27$TempPrefMax <- "29.43"

StdError_40 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `40`, ES ="LRRi")
StdError_40$Ratio <- log(Ratios_NIS$`40`[c(11:14)])
StdError_40$TempPrefMean <- "26.90"
StdError_40$TempPrefMax <- "30.33"

StdError_41 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `41`, ES ="LRRi")
StdError_41$Ratio <- log(Ratios_NIS$`41`[c(11:14)])
StdError_41$TempPrefMean <- "27.49"
StdError_41$TempPrefMax <- "30.25"

StdError_42 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `42`, ES ="LRRi")
StdError_42$Ratio <- log(Ratios_NIS$`42`[c(11:14)])
StdError_42$TempPrefMean <- "28.27"
StdError_42$TempPrefMax <- "29.86"

StdError_43 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `43`, ES ="LRRi")
StdError_43$Ratio <- log(Ratios_NIS$`43`[c(11:14)])
StdError_43$TempPrefMean <- "27.02"
StdError_43$TempPrefMax <- "30.30"

StdError_46 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `46`, ES ="LRRi")
StdError_46$Ratio <- log(Ratios_NIS$`46`[c(11:14)])
StdError_46$TempPrefMean <- "25.95"
StdError_46$TempPrefMax <- "26.79"

StdError_47 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `47`, ES ="LRRi")
StdError_47$Ratio <- log(Ratios_NIS$`47`[c(11:14)])
StdError_47$TempPrefMean <- "27.14"
StdError_47$TempPrefMax <- "30.29"

StdError_50 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `50`, ES ="LRRi")
StdError_50$Ratio <- log(Ratios_NIS$`50`[c(11:14)])
StdError_50$TempPrefMean <- "27.70"
StdError_50$TempPrefMax <- "30.39"

StdError_52 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `52`, ES ="LRRi")
StdError_52$Ratio <- log(Ratios_NIS$`52`[c(11:14)])
StdError_52$TempPrefMean <- "26.89"
StdError_52$TempPrefMax <- "30.33"

StdError_54 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `54`, ES ="LRRi")
StdError_54$Ratio <- log(Ratios_NIS$`54`[c(11:14)])
StdError_54$TempPrefMean <- "27.24"
StdError_54$TempPrefMax <- "30.30"

StdError_60 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `60`, ES ="LRRi")
StdError_60$Ratio <- log(Ratios_NIS$`60`[c(11:14)])
StdError_60$TempPrefMean <- "26.50"
StdError_60$TempPrefMax <- "30.33"

StdError_61 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `61`, ES ="LRRi")
StdError_61$Ratio <- log(Ratios_NIS$`61`[c(11:14)])
StdError_61$TempPrefMean <- "24.84"
StdError_61$TempPrefMax <- "30.33"

StdError_64 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `64`, ES ="LRRi")
StdError_64$Ratio <- log(Ratios_NIS$`64`[c(11:14)])
StdError_64$TempPrefMean <- "26.25"
StdError_64$TempPrefMax <- "27.74"

StdError_67 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `67`, ES ="LRRi")
StdError_67$Ratio <- log(Ratios_NIS$`67`[c(11:14)])
StdError_67$TempPrefMean <- "27.55"
StdError_67$TempPrefMax <- "28.30"

StdError_77 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `77`, ES ="LRRi")
StdError_77$Ratio <- log(Ratios_NIS$`77`[c(11:14)])
StdError_77$TempPrefMean <- "24.54"
StdError_77$TempPrefMax <- "28.33"

StdError_80 <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                             outcome = `80`, ES ="LRRi")
StdError_80$Ratio <- log(Ratios_NIS$`80`[c(11:14)])
StdError_80$TempPrefMean <- "24.75"
StdError_80$TempPrefMax <- "27.13"

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
StdError_1$TempPrefMean <- "26.78"
StdError_1$TempPrefMax <- "29.88"

StdError_2 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `2`, ES ="LRRi")
StdError_2$Ratio <- log(Ratios_NAT$`2`[c(11:14)])
StdError_2$TempPrefMean <- "25.19"
StdError_2$TempPrefMax <- "28.73"

StdError_3 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `3`, ES ="LRRi")
StdError_3$Ratio <- log(Ratios_NAT$`3`[c(11:14)])
StdError_3$TempPrefMean <- "25.84"
StdError_3$TempPrefMax <- "27.79"

StdError_4 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `4`, ES ="LRRi")
StdError_4$Ratio <- log(Ratios_NAT$`4`[c(11:14)])
StdError_4$TempPrefMean <- "14.12"
StdError_4$TempPrefMax <- "22.61"

StdError_5 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `5`, ES ="LRRi")
StdError_5$Ratio <- log(Ratios_NAT$`5`[c(11:14)])
StdError_5$TempPrefMean <- "28.62"
StdError_5$TempPrefMax <- "29.58"

StdError_6 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `6`, ES ="LRRi")
StdError_6$Ratio <- log(Ratios_NAT$`6`[c(11:14)])
StdError_6$TempPrefMean <- "22.46"
StdError_6$TempPrefMax <- "29.30"

StdError_7 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `7`, ES ="LRRi")
StdError_7$Ratio <- log(Ratios_NAT$`7`[c(11:14)])
StdError_7$TempPrefMean <- "11.76"
StdError_7$TempPrefMax <- "27.92"

StdError_8 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `8`, ES ="LRRi")
StdError_8$Ratio <- log(Ratios_NAT$`8`[c(11:14)])
StdError_8$TempPrefMean <- "19.79"
StdError_8$TempPrefMax <- "28.73"

StdError_9 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                            outcome = `9`, ES ="LRRi")
StdError_9$Ratio <- log(Ratios_NAT$`9`[c(11:14)])
StdError_9$TempPrefMean <- "24.28"
StdError_9$TempPrefMax <- "29.53"

StdError_11 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `11`, ES ="LRRi")
StdError_11$Ratio <- log(Ratios_NAT$`11`[c(11:14)])
StdError_11$TempPrefMean <- "24.88"
StdError_11$TempPrefMax <- "28.73"

StdError_12 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `12`, ES ="LRRi")
StdError_12$Ratio <- log(Ratios_NAT$`12`[c(11:14)])
StdError_12$TempPrefMean <- "22.51"
StdError_12$TempPrefMax <- "28.73"

StdError_13 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `13`, ES ="LRRi")
StdError_13$Ratio <- log(Ratios_NAT$`13`[c(11:14)])
StdError_13$TempPrefMean <- "17.78"
StdError_13$TempPrefMax <- "29.12"

StdError_14 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `14`, ES ="LRRi")
StdError_14$Ratio <- log(Ratios_NAT$`14`[c(11:14)])
StdError_14$TempPrefMean <- "19.72"
StdError_14$TempPrefMax <- "24.94"

StdError_15 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `15`, ES ="LRRi")
StdError_15$Ratio <- log(Ratios_NAT$`15`[c(11:14)])
StdError_15$TempPrefMean <- "17.95"
StdError_15$TempPrefMax <- "21.80"

StdError_16 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `16`, ES ="LRRi")
StdError_16$Ratio <- log(Ratios_NAT$`16`[c(11:14)])
StdError_16$TempPrefMean <- "19.77"
StdError_16$TempPrefMax <- "21.54"

StdError_17 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `17`, ES ="LRRi")
StdError_17$Ratio <- log(Ratios_NAT$`17`[c(11:14)])
StdError_17$TempPrefMean <- "19.25"
StdError_17$TempPrefMax <- "24.38"

StdError_18 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `18`, ES ="LRRi")
StdError_18$Ratio <- log(Ratios_NAT$`18`[c(11:14)])
StdError_18$TempPrefMean <- "18.88"
StdError_18$TempPrefMax <- "23.51"

StdError_19 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `19`, ES ="LRRi")
StdError_19$Ratio <- log(Ratios_NAT$`19`[c(11:14)])
StdError_19$TempPrefMean <- "18.82"
StdError_19$TempPrefMax <- "26.02"

StdError_20 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `20`, ES ="LRRi")
StdError_20$Ratio <- log(Ratios_NAT$`20`[c(11:14)])
StdError_20$TempPrefMean <- "26.57"
StdError_20$TempPrefMax <- "30.39"

StdError_21 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `21`, ES ="LRRi")
StdError_21$Ratio <- log(Ratios_NAT$`21`[c(11:14)])
StdError_21$TempPrefMean <- "26.07"
StdError_21$TempPrefMax <- "28.73"

StdError_22 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `22`, ES ="LRRi")
StdError_22$Ratio <- log(Ratios_NAT$`22`[c(11:14)])
StdError_22$TempPrefMean <- "23.08"
StdError_22$TempPrefMax <- "27.15"

StdError_23 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `23`, ES ="LRRi")
StdError_23$Ratio <- log(Ratios_NAT$`23`[c(11:14)])
StdError_23$TempPrefMean <- "22.94"
StdError_23$TempPrefMax <- "28.73"

StdError_28 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `28`, ES ="LRRi")
StdError_28$Ratio <- log(Ratios_NAT$`28`[c(11:14)])
StdError_28$TempPrefMean <- "26.8"
StdError_28$TempPrefMax <- "29.43"

StdError_29 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `29`, ES ="LRRi")
StdError_29$Ratio <- log(Ratios_NAT$`29`[c(11:14)])
StdError_29$TempPrefMean <- "21.10"
StdError_29$TempPrefMax <- "27.96"

StdError_30 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `30`, ES ="LRRi")
StdError_30$Ratio <- log(Ratios_NAT$`30`[c(11:14)])
StdError_30$TempPrefMean <- "NA"
StdError_30$TempPrefMax <- "NA"

StdError_31 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `31`, ES ="LRRi")
StdError_31$Ratio <- log(Ratios_NAT$`31`[c(11:14)])
StdError_31$TempPrefMean <- "12.85"
StdError_31$TempPrefMax <- "27.42"

StdError_32 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `32`, ES ="LRRi")
StdError_32$Ratio <- log(Ratios_NAT$`32`[c(11:14)])
StdError_32$TempPrefMean <- "19.32"
StdError_32$TempPrefMax <- "28.40"

StdError_33 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `33`, ES ="LRRi")
StdError_33$Ratio <- log(Ratios_NAT$`33`[c(11:14)])
StdError_33$TempPrefMean <- "23.49"
StdError_33$TempPrefMax <- "28.88"

StdError_34 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `34`, ES ="LRRi")
StdError_34$Ratio <- log(Ratios_NAT$`34`[c(11:14)])
StdError_34$TempPrefMean <- "19.52"
StdError_34$TempPrefMax <- "27.15"

StdError_35 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `35`, ES ="LRRi")
StdError_35$Ratio <- log(Ratios_NAT$`35`[c(11:14)])
StdError_35$TempPrefMean <- "NA"
StdError_35$TempPrefMax <- "NA"

StdError_36 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `36`, ES ="LRRi")
StdError_36$Ratio <- log(Ratios_NAT$`36`[c(11:14)])
StdError_36$TempPrefMean <- "18.30"
StdError_36$TempPrefMax <- "24.71"

StdError_37 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `37`, ES ="LRRi")
StdError_37$Ratio <- log(Ratios_NAT$`37`[c(11:14)])
StdError_37$TempPrefMean <- "24.56"
StdError_37$TempPrefMax <- "28.73"

StdError_38 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `38`, ES ="LRRi")
StdError_38$Ratio <- log(Ratios_NAT$`38`[c(11:14)])
StdError_38$TempPrefMean <- "25.25"
StdError_38$TempPrefMax <- "28.73"

StdError_39 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `39`, ES ="LRRi")
StdError_39$Ratio <- log(Ratios_NAT$`39`[c(11:14)])
StdError_39$TempPrefMean <- "22.04"
StdError_39$TempPrefMax <- "28.73"

StdError_44 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `44`, ES ="LRRi")
StdError_44$Ratio <- log(Ratios_NAT$`44`[c(11:14)])
StdError_44$TempPrefMean <- "24.24"
StdError_44$TempPrefMax <- "28.73"

StdError_45 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `45`, ES ="LRRi")
StdError_45$Ratio <- log(Ratios_NAT$`45`[c(11:14)])
StdError_45$TempPrefMean <- "18.40"
StdError_45$TempPrefMax <- "29.53"

StdError_48 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `48`, ES ="LRRi")
StdError_48$Ratio <- log(Ratios_NAT$`48`[c(11:14)])
StdError_48$TempPrefMean <- "26.45"
StdError_48$TempPrefMax <- "28.73"

StdError_49 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `49`, ES ="LRRi")
StdError_49$Ratio <- log(Ratios_NAT$`49`[c(11:14)])
StdError_49$TempPrefMean <- "13.35"
StdError_49$TempPrefMax <- "24.89"

StdError_51 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `51`, ES ="LRRi")
StdError_51$Ratio <- log(Ratios_NAT$`51`[c(11:14)])
StdError_51$TempPrefMean <- "19.74"
StdError_51$TempPrefMax <- "27.37"

StdError_53 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `53`, ES ="LRRi")
StdError_53$Ratio <- log(Ratios_NAT$`53`[c(11:14)])
StdError_53$TempPrefMean <- "24.53"
StdError_53$TempPrefMax <- "28.73"

StdError_55 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `55`, ES ="LRRi")
StdError_55$Ratio <- log(Ratios_NAT$`55`[c(11:14)])
StdError_55$TempPrefMean <- "22.10"
StdError_55$TempPrefMax <- "29.64"

StdError_56 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `56`, ES ="LRRi")
StdError_56$Ratio <- log(Ratios_NAT$`56`[c(11:14)])
StdError_56$TempPrefMean <- "24.24"
StdError_56$TempPrefMax <- "29.60"

StdError_57 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `57`, ES ="LRRi")
StdError_57$Ratio <- log(Ratios_NAT$`57`[c(11:14)])
StdError_57$TempPrefMean <- "20.86"
StdError_57$TempPrefMax <- "28.73"

StdError_58 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `58`, ES ="LRRi")
StdError_58$Ratio <- log(Ratios_NAT$`58`[c(11:14)])
StdError_58$TempPrefMean <- "17.43"
StdError_58$TempPrefMax <- "21.34"

StdError_59 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `59`, ES ="LRRi")
StdError_59$Ratio <- log(Ratios_NAT$`59`[c(11:14)])
StdError_59$TempPrefMean <- "20.27"
StdError_59$TempPrefMax <- "24.38"

StdError_62 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `62`, ES ="LRRi")
StdError_62$Ratio <- log(Ratios_NAT$`62`[c(11:14)])
StdError_62$TempPrefMean <- "22.96"
StdError_62$TempPrefMax <- "29.39"

StdError_63 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `63`, ES ="LRRi")
StdError_63$Ratio <- log(Ratios_NAT$`63`[c(11:14)])
StdError_63$TempPrefMean <- "15.89"
StdError_63$TempPrefMax <- "25.11"

StdError_65 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `65`, ES ="LRRi")
StdError_65$Ratio <- log(Ratios_NAT$`65`[c(11:14)])
StdError_65$TempPrefMean <- "18.39"
StdError_65$TempPrefMax <- "24.74"

StdError_66 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `66`, ES ="LRRi")
StdError_66$Ratio <- log(Ratios_NAT$`66`[c(11:14)])
StdError_66$TempPrefMean <- "NA"
StdError_66$TempPrefMax <- "NA"

StdError_68 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `68`, ES ="LRRi")
StdError_68$Ratio <- log(Ratios_NAT$`68`[c(11:14)])
StdError_68$TempPrefMean <- "15.93"
StdError_68$TempPrefMax <- "22.61"

StdError_69 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `69`, ES ="LRRi")
StdError_69$Ratio <- log(Ratios_NAT$`69`[c(11:14)])
StdError_69$TempPrefMean <- "18.75"
StdError_69$TempPrefMax <- "20.41"

StdError_70 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `70`, ES ="LRRi")
StdError_70$Ratio <- log(Ratios_NAT$`70`[c(11:14)])
StdError_70$TempPrefMean <- "15.49"
StdError_70$TempPrefMax <- "17.70"

StdError_71 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `71`, ES ="LRRi")
StdError_71$Ratio <- log(Ratios_NAT$`71`[c(11:14)])
StdError_71$TempPrefMean <- "16.30"
StdError_71$TempPrefMax <- "20.45"

StdError_72 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `72`, ES ="LRRi")
StdError_72$Ratio <- log(Ratios_NAT$`72`[c(11:14)])
StdError_72$TempPrefMean <- "18.34"
StdError_72$TempPrefMax <- "22.61"

StdError_73 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `73`, ES ="LRRi")
StdError_73$Ratio <- log(Ratios_NAT$`73`[c(11:14)])
StdError_73$TempPrefMean <- "24.95"
StdError_73$TempPrefMax <- "28.73"

StdError_74 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `74`, ES ="LRRi")
StdError_74$Ratio <- log(Ratios_NAT$`74`[c(11:14)])
StdError_74$TempPrefMean <- "20.60"
StdError_74$TempPrefMax <- "25.08"

StdError_75 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `75`, ES ="LRRi")
StdError_75$Ratio <- log(Ratios_NAT$`75`[c(11:14)])
StdError_75$TempPrefMean <- "18.13"
StdError_75$TempPrefMax <- "28.73"

StdError_76 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `76`, ES ="LRRi")
StdError_76$Ratio <- log(Ratios_NAT$`76`[c(11:14)])
StdError_76$TempPrefMean <- "25.90"
StdError_76$TempPrefMax <- "28.73"

StdError_78 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `78`, ES ="LRRi")
StdError_78$Ratio <- log(Ratios_NAT$`78`[c(11:14)])
StdError_78$TempPrefMean <- "24.40"
StdError_78$TempPrefMax <- "30.38"

StdError_79 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `79`, ES ="LRRi")
StdError_79$Ratio <- log(Ratios_NAT$`79`[c(11:14)])
StdError_79$TempPrefMean <- "15.88"
StdError_79$TempPrefMax <- "21.16"

StdError_81 <- batch_calc_ES(dat = StdError_DataB, grouping = eventYear, condition = eventSeason, 
                             outcome = `81`, ES ="LRRi")
StdError_81$Ratio <- log(Ratios_NAT$`81`[c(11:14)])
StdError_81$TempPrefMean <- "25.16"
StdError_81$TempPrefMax <- "29.07"

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
Transect_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8:10)],StdError_NAT[c(1,4,7,8:10)])
Transect_Species_Abundance <- Transect_Species_Abundance[-c(123,124,125,126,139,140,160),]
