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
####        Load Soreq dataset and summarise            ####
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
SoreqCategories$Merched_Replicates <- factor(paste(SoreqCategories$eventYear, 
                                                 SoreqCategories$eventSeason,
                                                 SoreqCategories$StationCode,
                                                 sep = "_"))

### Create categories to merch replicate
SoreqSpecies$Merched_Replicates <- SoreqCategories$Merched_Replicates
SoreqSpecies[c(1:107)] <- SoreqSpecies[c(1:107)] %>% 
  mutate_if(is.character, as.numeric)
SoreqSpeciesMerched <- aggregate(SoreqSpecies[-c(108)], by=list(Category=SoreqSpecies$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
SoreqTranspose <- t(SoreqSpeciesMerched[,-c(1)])
SoreqTranspose <- as.data.frame(SoreqTranspose)
SoreqTranspose$Invasive <- SpSoreq$...2

# Standardized species richness
# delete sites with no species present
SoreqInvasive  <- SoreqTranspose[which(SoreqTranspose$Invasive  =='1'),]
SoreqInvasive <- SoreqInvasive %>% mutate_if(is.character, as.numeric)
SoreqInvasive <- SoreqInvasive[-c(127, # zero species
                                22,54,70, #one species
                                85,94,130,133,132, # equal N and S
                                134,131,135)] #low coverage
SoreqInv_Cov <- iNEXT::DataInfo(SoreqInvasive, datatype = "abundance") 
SoreqInv_Cov$Category <- SoreqSpeciesMerched$Category[-c(127, # zero species
                                                         22,54,70, #one species
                                                         85,94,130,133,132, # equal N and S
                                                         134,131,135)] #low coverage

SoreqInvSp <- estimateD(SoreqInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.4857)
colnames(SoreqInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

SoreqIndigenous <- SoreqTranspose[which(SoreqTranspose$Invasive =='0'),]
SoreqIndigenous <- SoreqIndigenous %>% mutate_if(is.character, as.numeric)
SoreqIndigenous <- SoreqIndigenous[-c(39,55,57,58,70,85,94, #zero species
                                    26,40,44,45,59,74,77,78,89,91,126,62,56,79,117,99, # one species
                                    43,46,71,106,73,15,81,75, # equal N and S
                                    102,110,135)]# low coverage
SoreqInd_Cov <- iNEXT::DataInfo(SoreqIndigenous, datatype = "abundance") 
SoreqInd_Cov$Category <- SoreqSpeciesMerched$Category[-c(39,55,57,58,70,85,94, #zero species
                                                        26,40,44,45,59,74,77,78,89,91,126,62,56,79,117,99, # one species
                                                        43,46,71,106,73,15,81,75, # equal N and S
                                                        102,110,135)]# low coverage
SoreqIndSp <- estimateD(SoreqIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.4857)
colnames(SoreqIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
SoreqSpeciesMerched$Match <- colnames(SoreqTranspose[-c(135)])
SoreqSpeciesMerched <- SoreqSpeciesMerched[c(1, 109)]
SoreqSpeciesMerched <- merge(SoreqSpeciesMerched, SoreqIndSp,  by.x = "Match", by.y = "Assemblage", all.x = TRUE)
SoreqSpeciesMerched <- merge(SoreqSpeciesMerched, SoreqInvSp,  by.x = "Match", by.y = "Assemblage", all.x = TRUE)

SoreqSpeciesMerched <-  SoreqSpeciesMerched[c(1,2,7,14) ]
colnames(SoreqSpeciesMerched)[2] <- c("Category")
SoreqSpeciesMerched[is.na(SoreqSpeciesMerched)] <- 0
SoreqCategoriesSubset <- SoreqCategories[which(SoreqCategories$Replicate  =='1'),]
SoreqSpeciesMerched <- merge(SoreqSpeciesMerched, SoreqCategoriesSubset[c(1:3, 5)],  by.x = "Category", by.y = "Merched_Replicates", all.x = TRUE)

# Only leave replicates with a zero when actually no individuals were present
# Clean out data that had only one species or the same species as number of individuals
# Loss of 26 replicates after cleaning data 

SoreqSpeciesMerched <- SoreqSpeciesMerched[-c(26,40,44,45,59,74,77,78,89,91,126,62,56,79,117,99, 
                                            43,46,71,106,73,15,81,75, 
                                            102,110,
                                            22,54,70,
                                            85,94,130,133,132,
                                            134,131),]


### Create categories to merch replicates
SoreqSpeciesMerched$Merched_Category <- factor(paste(SoreqSpeciesMerched$eventYear, 
                                                     SoreqSpeciesMerched$eventSeason,
                                                    sep = "_"))
### Mean values per Year*Season 
Mean_NIS <- aggregate(SoreqSpeciesMerched$SpRichNIS, 
                      list(SoreqSpeciesMerched$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(SoreqSpeciesMerched$SpRichNAT, 
                      list(SoreqSpeciesMerched$Merched_Category), FUN=mean)

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
StdError_NIS <- batch_calc_ES(dat = SoreqSpeciesMerched, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(19:27)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = SoreqSpeciesMerched, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(19:27)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Soreq_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Soreq_Comm_SpRich$Ratio <- log(Soreq_Comm_SpRich$Ratio)
