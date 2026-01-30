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

# Community level abundance
BRUVNIS  <- BRUVTranspose[which(BRUVTranspose$Invasive  =='1'),]
BRUVNAT  <- BRUVTranspose[which(BRUVTranspose$Invasive  =='0'),]
BRUVNIS <- BRUVNIS %>% 
  mutate_if(is.character, as.numeric)
BRUVNAT <- BRUVNAT%>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(BRUVNIS[-c(1,29)]))
colnames(AbundanceNIS) <- "AbundanceNIS"
AbundanceNAT <- as.data.frame(colSums(BRUVNAT[-c(1,29)]))
colnames(AbundanceNAT) <- "AbundanceNAT"


BruvCategories <- AbundanceNIS
BruvCategories$OPCode <- row.names(BruvCategories)
BruvCategories$AbundanceNAT <- AbundanceNAT$AbundanceNAT
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

### Mean values per Year*Season 
Mean_NIS <- aggregate(BruvCategories$AbundanceNIS, 
                      list(BruvCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(BruvCategories$AbundanceNAT, 
                      list(BruvCategories$Merched_Category), FUN=mean)

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
StdError_Data <- BruvCategories[BruvCategories$Year != c('2019'),]
StdError_Data <- StdError_Data[StdError_Data$Year != c('2020'),]
StdError_Data <- StdError_Data[StdError_Data$Year != c('2022'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(5)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = Year, condition = Season, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(5)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
BRUV_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
BRUV_Comm_Abundance$Ratio <- log(BRUV_Comm_Abundance$Ratio)

