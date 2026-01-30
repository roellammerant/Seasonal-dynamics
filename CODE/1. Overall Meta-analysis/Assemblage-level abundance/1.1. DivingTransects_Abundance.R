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
ReplicateNumber <- as.data.frame(table(TransectCategoriesB$Merched_Category))

# Change structure around: Species become rows and sites become columns
TransectTranspose <- t(TransectCategoriesB)
TransectTranspose <- as.data.frame(TransectTranspose)
colnames(TransectTranspose) <- TransectCategoriesB$trans_ID
TransectTranspose <- TransectTranspose[-c(1,84:87),]

# Create invasive status list
Invasive <- as.data.frame(table(TransectsAll$species,TransectsAll$les))
Invasive <- Invasive[Invasive$Freq > 0,]
colnames(Invasive)[1:2]<- c("Species", "Invasive")
Invasive <- Invasive[c(1:2)]
TransectTranspose$Species <- rownames(TransectTranspose)
TransectTranspose <- merge(TransectTranspose, Invasive,  by.x = "Species", by.y = "Species")

# Community level abundance
TransectNIS  <- TransectTranspose[which(TransectTranspose$Invasive  =='1'),]
TransectNAT  <- TransectTranspose[which(TransectTranspose$Invasive  =='0'),]
TransectNIS <- TransectNIS %>% 
  mutate_if(is.character, as.numeric)
TransectNAT <- TransectNAT%>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(TransectNIS[-c(1,1316)]))
AbundanceNAT <- as.data.frame(colSums(TransectNAT[-c(1,1316)]))

TransectCategoriesB$AbundanceNIS <- AbundanceNIS$`colSums(TransectNIS[-c(1, 1316)])`
TransectCategoriesB$AbundanceNAT <- AbundanceNAT$`colSums(TransectNAT[-c(1, 1316)])`
  
### Mean values per Year*Season 
Mean_NIS <- aggregate(TransectCategoriesB$AbundanceNIS, 
                      list(TransectCategoriesB$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(TransectCategoriesB$AbundanceNAT, 
                      list(TransectCategoriesB$Merched_Category), FUN=mean)
### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
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
Ratios_NAT$Ratio2015 <-Ratios_NAT$`2015_Fall`/Ratios_NAT$`2015_Spring`
Ratios_NAT$Ratio2019 <-Ratios_NAT$`2019_Fall`/Ratios_NAT$`2019_Spring`
Ratios_NAT$Ratio2021 <-Ratios_NAT$`2021_Fall`/Ratios_NAT$`2021_Spring`
Ratios_NAT$Ratio2023 <-Ratios_NAT$`2023_Fall`/Ratios_NAT$`2023_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_Data <- TransectCategoriesB[TransectCategoriesB$Year != c('2017'),]
StdError_Data <- StdError_Data[StdError_Data$Year != c('2018'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(11:14)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(11:14)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Transect_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Transect_Comm_Abundance$Ratio <- log(Transect_Comm_Abundance$Ratio)

