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
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'data.table', 'dplyr','SingleCaseES')
ipak(packages)

#%######################################################%##
#                                                          #
####          Load Crete data and summarise             ####
#                                                          #
##%######################################################%##
CretePos <- read.csv("DATA/Crete.csv", 
                     na = "NA")

Crete <- t(CretePos)
Crete <- as.data.frame(Crete)
colnames(Crete) <- Crete[c(2),]
Crete <- Crete[-c(1:7),]
Crete$Depth <- c(rep("5",6), rep("10",6),rep("15",6), rep("20",6))
Crete$Season <- c(rep("Spring",3), rep("Autumn",3), rep("Spring",3), rep("Autumn",3),
                  rep("Spring",3), rep("Autumn",3), rep("Spring",3), rep("Autumn",3))

### Create categories to merge replicates
Crete$Merched_Replicates <- factor(paste(Crete$Depth, 
                                         Crete$Season,
                                         sep = "_"))

### Create Community level abundance columns
CreteTranspose <- t(Crete[-c(110:112)])
CreteTranspose <- as.data.frame(CreteTranspose)
CreteTranspose$Invasive <- CretePos$Alien
CreteNIS  <- CreteTranspose[which(CreteTranspose$Invasive  =='Yes'),]
CreteNAT  <- CreteTranspose[which(CreteTranspose$Invasive  =='No'),]
CreteNIS <- CreteNIS %>% 
  mutate_if(is.character, as.numeric)
CreteNAT <- CreteNAT %>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(CreteNIS))
AbundanceNAT <- as.data.frame(colSums(CreteNAT))

CreteCategories <- Crete[c(110:112)]
CreteCategories$AbundanceNIS <- AbundanceNIS$`colSums(CreteNIS)`[-c(25)]
CreteCategories$AbundanceNAT <- AbundanceNAT$`colSums(CreteNAT)`[-c(25)]

### Mean values per Year*Season 
Mean_NIS <- aggregate(CreteCategories$AbundanceNIS, 
                      list(CreteCategories$Merched_Replicates), FUN=mean) 
Mean_NAT <- aggregate(CreteCategories$AbundanceNAT, 
                      list(CreteCategories$Merched_Replicates), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio10 <-Ratios_NIS$`10_Autumn`/Ratios_NIS$`10_Spring`
Ratios_NIS$Ratio15 <-Ratios_NIS$`15_Autumn`/Ratios_NIS$`15_Spring`
Ratios_NIS$Ratio20 <-Ratios_NIS$`20_Autumn`/Ratios_NIS$`20_Spring`
Ratios_NIS$Ratio5 <-Ratios_NIS$`5_Autumn`/Ratios_NIS$`5_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio10 <-Ratios_NAT$`10_Autumn`/Ratios_NAT$`10_Spring`
Ratios_NAT$Ratio15 <-Ratios_NAT$`15_Autumn`/Ratios_NAT$`15_Spring`
Ratios_NAT$Ratio20 <-Ratios_NAT$`20_Autumn`/Ratios_NAT$`20_Spring`
Ratios_NAT$Ratio5 <- Ratios_NAT$`5_Autumn`/Ratios_NAT$`5_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios

StdError_NIS <- batch_calc_ES(dat = CreteCategories, grouping = Depth, condition = Season, 
                              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(9:12)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = CreteCategories, grouping = Depth, condition = Season, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(9:12)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Crete_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Crete_Comm_Abundance$Ratio <- log(Crete_Comm_Abundance$Ratio)
