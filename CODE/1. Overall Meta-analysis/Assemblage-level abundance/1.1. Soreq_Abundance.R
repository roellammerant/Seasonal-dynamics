#%######################################################%##
#                                                          #
####                  Load R packages                   ####
#                                                          #
##%######################################################%##
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'dplyr', 'data.table','SingleCaseES')
ipak(packages)

##%######################################################%##
#                                                          #
####        Load Agan dataset and summarise             ####
#                                                          #
##%######################################################%##

SOREQ <- read.csv("DATA/SOREQ.csv", 
                 na = "NA")
SpSoreq <- read_excel("output_data/SOREQ_InvasiveStatus.xlsx", 
                         na = "NA")


### turn data into presence absence
SoreqCategories <- SOREQ[c(2:5)]
SoreqSpecies <- SOREQ[-c(1:5)]

### Create categories to merch replicates
SoreqCategories$Merched_Category <- factor(paste(SoreqCategories$eventYear, 
                                                 SoreqCategories$eventSeason,
                                                sep = "_"))


### Create Community level abundance columns
SoreqTranspose <- t(SoreqSpecies)
SoreqTranspose <- as.data.frame(SoreqTranspose)
SoreqTranspose$Invasive <- SpSoreq$...2
SoreqNIS  <- SoreqTranspose[which(SoreqTranspose$Invasive  =='1'),]
SoreqNAT  <- SoreqTranspose[which(SoreqTranspose$Invasive  =='0'),]
SoreqNIS <- SoreqNIS %>% 
  mutate_if(is.character, as.numeric)
SoreqNAT <- SoreqNAT %>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(SoreqNIS))
AbundanceNAT <- as.data.frame(colSums(SoreqNAT))

SoreqCategories$AbundanceNIS <- AbundanceNIS$`colSums(SoreqNIS)`[-c(391)]
SoreqCategories$AbundanceNAT <- AbundanceNAT$`colSums(SoreqNAT)`[-c(391)]

### Mean values per Year*Season 
Mean_NIS <- aggregate(SoreqCategories$AbundanceNIS, 
                      list(SoreqCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(SoreqCategories$AbundanceNAT, 
                      list(SoreqCategories$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio2012 <-Ratios_NIS$`2012_Autumn`/Ratios_NIS$`2012_Spring`
Ratios_NIS$Ratio2015 <-Ratios_NIS$`2015_Autumn`/Ratios_NIS$`2015_Spring`
Ratios_NIS$Ratio2016 <-Ratios_NIS$`2016_Autumn`/Ratios_NIS$`2016_Spring`
Ratios_NIS$Ratio2017 <-Ratios_NIS$`2017_Autumn`/Ratios_NIS$`2017_Spring`
Ratios_NIS$Ratio2018 <-Ratios_NIS$`2018_Autumn`/Ratios_NIS$`2018_Spring`
Ratios_NIS$Ratio2019 <-Ratios_NIS$`2019_Autumn`/Ratios_NIS$`2019_Spring`
Ratios_NIS$Ratio2020 <-Ratios_NIS$`2020_Autumn`/Ratios_NIS$`2020_Spring`
Ratios_NIS$Ratio2021 <-Ratios_NIS$`2021_Autumn`/Ratios_NIS$`2021_Spring`
Ratios_NIS$Ratio2022 <-Ratios_NIS$`2022_Autumn`/Ratios_NIS$`2022_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2015 <-Ratios_NAT$`2015_Autumn`/Ratios_NAT$`2015_Spring`
Ratios_NAT$Ratio2016 <-Ratios_NAT$`2016_Autumn`/Ratios_NAT$`2016_Spring`
Ratios_NAT$Ratio2017 <-Ratios_NAT$`2017_Autumn`/Ratios_NAT$`2017_Spring`
Ratios_NAT$Ratio2018 <-Ratios_NAT$`2018_Autumn`/Ratios_NAT$`2018_Spring`
Ratios_NAT$Ratio2019 <-Ratios_NAT$`2019_Autumn`/Ratios_NAT$`2019_Spring`
Ratios_NAT$Ratio2020 <-Ratios_NAT$`2020_Autumn`/Ratios_NAT$`2020_Spring`
Ratios_NAT$Ratio2021 <-Ratios_NAT$`2021_Autumn`/Ratios_NAT$`2021_Spring`
Ratios_NAT$Ratio2022 <-Ratios_NAT$`2022_Autumn`/Ratios_NAT$`2022_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios

StdError_NIS <- batch_calc_ES(dat = SoreqCategories, grouping = eventYear, condition = eventSeason, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(19:27)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = SoreqCategories, grouping = eventYear, condition = eventSeason, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(19:27)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Soreq_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Soreq_Comm_Abundance$Ratio <- log(Soreq_Comm_Abundance$Ratio)

                   