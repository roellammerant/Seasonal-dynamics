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


# standardized species richness
BRUVInvasive  <- BRUVTranspose[which(BRUVTranspose$Invasive  =='1'),]
BRUVInvStandardized<- BRUVInvasive[-c(1,7,15,19,21,22,24,29)]
BRUVInvStandardized <- as.matrix(BRUVInvStandardized)
BRUVInvSp_Cov<- iNEXT::DataInfo(BRUVInvStandardized, datatype = "abundance")
summary(BRUVInvSp_Cov)
InvSprich <- estimateD(BRUVInvStandardized, q = c(0), datatype = "abundance", 
                       base="coverage",level = 0.6250)
colnames(InvSprich)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

BRUVIndigenous <- BRUVTranspose[which(BRUVTranspose$Invasive =='0'),]
BRUVIndStandardized <-BRUVIndigenous[-c(1,7,8,24,29)]
BRUVIndStandardized <- as.matrix(BRUVIndStandardized)
TrawlingIndSp_Cov<- iNEXT::DataInfo(BRUVIndStandardized, datatype = "abundance")
summary(TrawlingIndSp_Cov) 
IndSprich <- estimateD(BRUVIndStandardized, q = c(0), datatype = "abundance", base="coverage",
                       level = 0.6250)
colnames(IndSprich)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")


# Link back to the other categories
BRUVCategories <- as.data.frame(BRUVSubset$OpCode)
colnames(BRUVCategories) <- "OPCode"
BRUVCategories$Season <- c("Fall","Fall","Fall","Fall","Spring","Spring","Spring","Spring","Spring","Spring","Spring",
                           "Fall","Fall","Spring","Spring","Fall","Fall","Fall","Fall","Fall","Fall","Fall","Fall",
                           "Fall","Fall","Fall","Fall")
BRUVCategories$Year <- c("2021","2021","2021","2019","2022","2022","2022","2021","2021","2021","2021","2019","2019",
                         "2022","2022","2020","2020","2020","2020","2020","2020","2019","2019","2019",
                         "2021","2021","2021")

BRUVCategories <- merge(BRUVCategories, InvSprich,  by.x = "OPCode", by.y = "Assemblage", all.x = TRUE)
BRUVCategories <- merge(BRUVCategories, IndSprich,  by.x = "OPCode", by.y = "Assemblage", all.x = TRUE)
BRUVCategories <-  BRUVCategories[c(1:3, 8,15) ]

#Remove BRUV data points that were left out in calculations
BRUVCategories <- BRUVCategories[-c(6,14,18,21,20,23),] 
BRUVCategories[is.na(BRUVCategories)] = 0

### Create categories to merch replicates
BRUVCategories$Merched_Category <- factor(paste(BRUVCategories$Year, 
                                                BRUVCategories$Season,
                                                sep = "_"))

### Mean values per Year*Season 
Mean_NIS <- aggregate(BRUVCategories$SpRichNIS, 
                      list(BRUVCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(BRUVCategories$SpRichNAT, 
                      list(BRUVCategories$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio2021 <-Ratios_NIS$`2021_Fall`/Ratios_NIS$`2021_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),-c(1)]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio2021 <-Ratios_NAT$`2021_Fall`/Ratios_NAT$`2021_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_Data <- BRUVCategories[BRUVCategories$Year != c('2019'),]
StdError_Data <- StdError_Data[StdError_Data$Year != c('2020'),]
StdError_Data <- StdError_Data[StdError_Data$Year != c('2022'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(5)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(5)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
BRUV_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
BRUV_Comm_SpRich$Ratio <- log(BRUV_Comm_SpRich$Ratio)
