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

### select two transects and only species that where seen in the transects
Transect_A <- TransectsAll[TransectsAll$trans_letter == c('A'),]
Transect_A <- Transect_A[Transect_A$inside_150m_belt_transect == c('Inside'),]
Transect_B <- TransectsAll[TransectsAll$trans_letter == c('B'),]
Transect_B <- Transect_B[Transect_B$inside_150m_belt_transect == c('Inside'),]

### Check if Sampling site ID´s match
SitesA <- as.data.frame(table(Transect_A$sampling_site_ID))
SitesB <- as.data.frame(table(Transect_B$sampling_site_ID))
SitesoverallA <- merge(SitesA, SitesB, 
                       by.x = "Var1", by.y = "Var1", all.x = TRUE)
SitesoverallA[is.na(SitesoverallA)] = 0
# Sampling site ID only present in transect A
# 21,24,58,72,155,208,213,215,216,339,367,369,396,404
# Take those out 
SitesoveralB <- merge(SitesB, SitesA, 
                      by.x = "Var1", by.y = "Var1", all.x = TRUE)
SitesoveralB[is.na(SitesoveralB)] = 0
# Sampling site ID only present in transect B
# 34,220,406,479
# Take those out 

Transect_AB <- rbind(Transect_A,Transect_B)
AbunTransect_AB <-as.data.frame(with(Transect_AB, tapply(abundance, list(sampling_site_ID, species), sum)))
AbunTransect_AB[is.na(AbunTransect_AB)] = 0
AbunTransect_AB$sampling_site_ID <- rownames(AbunTransect_AB)
AbunTransect_AB <- AbunTransect_AB[-c(21,24,58,72,155,208,213,214,215,216,
                                      339,367,369,396,404, #transects only present in A
                                      34,220,406,479),]#transects only present in B

TransectCategories <- TransectsAll[c(2:6,9,13)]
TransectCategories <- TransectCategories %>% 
  mutate_if(is.numeric, as.character)
TransectCategories$sampling_site_ID <- as.character(TransectCategories$sampling_site_ID)

Year <- as.data.frame(table(TransectCategories$sampling_site_ID,TransectCategories$year))
Year <- Year[Year$Freq > 0,]
colnames(Year)[1:2]<- c("sampling_site_ID", "Year")
Year <- Year[c(1:2)]

Season <- as.data.frame(table(TransectCategories$sampling_site_ID,TransectCategories$season))
Season <- Season[Season$Freq > 0,]
colnames(Season)[1:2]<- c("sampling_site_ID", "Season")
Season <- Season[c(1:2)]

Date <- as.data.frame(table(TransectCategories$sampling_site_ID,TransectCategories$date))
Date <- Date[Date$Freq > 0,]
colnames(Date)[1:2]<- c("sampling_site_ID", "Date")
Date <- Date[c(1:2)]

TransectMerged <- merge(AbunTransect_AB, Year,  by.x = "sampling_site_ID", by.y = "sampling_site_ID")
TransectMerged <- merge(TransectMerged, Season,  by.x = "sampling_site_ID", by.y = "sampling_site_ID")
TransectMerged <- merge(TransectMerged, Date,  by.x = "sampling_site_ID", by.y = "sampling_site_ID")

# Autumn samples are collected in September, October and November
# Spring samples are collected in April, May and June
# Take out samples in September and June - since we use April-May and Oct-Nov in the trawling data set

TransectMergedSubset <- TransectMerged[TransectMerged$Date != c('2017-06-04'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2017-06-05'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2017-06-06'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2017-06-07'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2021-06-28'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2021-09-29'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2023-06-26'),]
TransectMergedSubset <- TransectMergedSubset[TransectMergedSubset$Date != c('2023-06-28'),]


#%######################################################%##
#                                                          #
####                  Sprich Transect                   ####
#                                                          #
##%######################################################%##

### subset species and categories
TransectCategories <- TransectMergedSubset[c(1, 78:80)]
TransectSpecies <- TransectMergedSubset[-c(1, 78:80)]

# Change structure around: Species become rows and sites become columns
TransectTranspose <- t(TransectSpecies)
TransectTranspose <- as.data.frame(TransectTranspose)
colnames(TransectTranspose) <- TransectCategories$sampling_site_ID

# Create invasive status list
Invasive <- as.data.frame(table(TransectsAll$species,TransectsAll$les))
Invasive <- Invasive[Invasive$Freq > 0,]
colnames(Invasive)[1:2]<- c("Species", "Invasive")
Invasive <- Invasive[c(1:2)]
TransectTranspose$Species <- rownames(TransectTranspose)
TransectTranspose <- merge(TransectTranspose, Invasive,  by.x = "Species", by.y = "Species")

# standardized species richness
TransectInvasive  <- TransectTranspose[which(TransectTranspose$Invasive  =='1'),]
TransectInvStandardized<- TransectInvasive[-c(1,22,26,31,80,133,134,136,137,138,162,163,164,176,194,
                                              195,199,201,227,232,239,243,244,254,261,272,285,292,
                                              293,297,307,318,393, # 0 species present
                                              19,20,23,27,30,35,46,48,50,51,52,53,65,66,68,73,74,75,76,77,79,81,82,
                                              84,86,87,91,92,97,99,102,103,104,127,128,129,130,131,144,147,149,150,
                                              151,152,153,154,156,157,158,161,174,175,182,188,190,191,193,202,209,225,
                                              230,231,237,240,241,262,268,271,275,281,282,284,294,296,298,299,300,301,303,
                                              305,306,308,309,311,312,313,314,315,316,317,319,320,321,322,324,327,329,396,399, # 1 species present
                                              226,279,302, # same N as S
                                              330,# low coverage
                                              417)]
TransectInvStandardized <- as.matrix(TransectInvStandardized)
TransectInvSp_Cov<- iNEXT::DataInfo(TransectInvStandardized, datatype = "abundance")
colnames(TransectInvSp_Cov)[2] <- c("IndCount_NIS")
InvSprich <- estimateD(TransectInvStandardized, q = c(0), datatype = "abundance", 
                       base="coverage",level = 0.7619)
colnames(InvSprich)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

TransectIndigenous <- TransectTranspose[which(TransectTranspose$Invasive =='0'),]
TransectIndStandardized <-TransectIndigenous[-c(1,26,104,195,198,417)]
TransectIndStandardized <- as.matrix(TransectIndStandardized)
TransectIndSp_Cov<- iNEXT::DataInfo(TransectIndStandardized, datatype = "abundance")
colnames(TransectIndSp_Cov)[2] <- c("IndCount_NAT")
IndSprich <- estimateD(TransectIndStandardized, q = c(0), datatype = "abundance", base="coverage",
                       level = 0.7619)
colnames(IndSprich)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
TransectCategories <- merge(TransectCategories, InvSprich,  by.x = "sampling_site_ID", by.y = "Assemblage", all.x = TRUE)
TransectCategories <- merge(TransectCategories, IndSprich,  by.x = "sampling_site_ID", by.y = "Assemblage", all.x = TRUE)
TransectCategories <-  TransectCategories[c(1:4, 9:11,16:18) ]

#Remove transects that were left out in calculations
TransectCategories <- TransectCategories[-c(18,19,22,26,29,34,45,47,49,50,51,52,64,65,67,72,73,74,75,76,78,80,81,
                                            83,85,86,90,91,96,98,101,102,103,126,127,128,129,130,143,146,148,149,
                                            150,151,152,153,155,156,157,160,173,174,181,187,189,190,192,201,208,224,
                                            229,230,236,239,240,261,267,270,274,280,281,283,293,295,297,298,299,300,302,
                                            304,305,307,308,310,311,312,313,314,315,316,318,319,320,321,323,326,328,395,398, # 1 species present
                                            225,278,301, # same N as S
                                            25,194,197,416, # Native species same
                                            329),] # low coverage
TransectCategories[is.na(TransectCategories)] = 0

### Create categories to merch replicates
TransectCategories$Merched_Category <- factor(paste(TransectCategories$Year, 
                                                    TransectCategories$Season,
                                                     sep = "_"))

### Mean values per Year*Season 
Mean_NIS <- aggregate(TransectCategories$SpRichNIS, 
                      list(TransectCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(TransectCategories$SpRichNAT, 
                      list(TransectCategories$Merched_Category), FUN=mean)
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
StdError_Data <- TransectCategories[TransectCategories$Year != c('2017'),]
StdError_Data <- StdError_Data[StdError_Data$Year != c('2018'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(11:14)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(11:14)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Transect_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Transect_Comm_SpRich$Ratio <- log(Transect_Comm_SpRich$Ratio)
