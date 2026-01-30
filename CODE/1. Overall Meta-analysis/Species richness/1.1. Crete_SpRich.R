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
####           Load Crete data + summarise              ####
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
### Merge replicates
Crete <- Crete[-c(110,111)]
Crete[c(1:109)] <- Crete[c(1:109)]%>% mutate_if(is.character, as.numeric)
CreteMerched <- aggregate(Crete[c(1:109)], by=list(Category=Crete$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
CreteMerchedTranspose <- t(CreteMerched[-c(1)])
CreteMerchedTranspose <- as.data.frame(CreteMerchedTranspose)
colnames(CreteMerchedTranspose) <- CreteMerched$Category
CreteMerchedTranspose$Invasive <- CretePos$Alien

# delete sites with no species present
CreteInvasive  <- CreteMerchedTranspose[which(CreteMerchedTranspose$Invasive  =='Yes'),]
CreteInvasive <- CreteInvasive[-c(9)]
CreteInv_Cov <- iNEXT::DataInfo(CreteInvasive, datatype = "abundance")
CreteInv_Cov$Category <- CreteMerched$Category
summary(CreteInv_Cov)
CreteInvSp <- estimateD(CreteInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.8364)
colnames(CreteInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

CreteIndigenous  <- CreteMerchedTranspose[which(CreteMerchedTranspose$Invasive  =='No'),]
CreteIndigenous <- CreteIndigenous[-c(9)]
CreteInd_Cov <- iNEXT::DataInfo(CreteIndigenous, datatype = "abundance") 
CreteInd_Cov$Category <- CreteMerched$Category
summary(CreteInd_Cov)
CreteIndSp <- estimateD(CreteIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.8364)
colnames(CreteIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
CreteSpeciesMerched <- CreteMerched[c(1)]
CreteSpeciesMerched <- merge(CreteSpeciesMerched, CreteIndSp,  
                             by.x = "Category", by.y = "Assemblage", all.x = TRUE)
CreteSpeciesMerched <- merge(CreteSpeciesMerched, CreteInvSp, 
                             by.x = "Category", by.y = "Assemblage", all.x = TRUE)

CreteSpeciesMerched <-  CreteSpeciesMerched[c(1,6:8, 13:15) ]
CreteSpeciesMerched$Season <- c("Autumn", "Spring", "Autumn","Spring",
                                "Autumn", "Spring", "Autumn","Spring")

# Take out 10_Spring due to only one species for NIS
CreteSpeciesMerched <- CreteSpeciesMerched[-c(2),]

### Create categories to merch replicates
CreteSpeciesMerched$eventYear <- "202x"
CreteSpeciesMerched$Merched_Category <- factor(paste(CreteSpeciesMerched$eventYear, 
                                                     CreteSpeciesMerched$Season,
                                                              sep = "_"))

### Mean values per Year*Season 
Mean_NIS <- aggregate(CreteSpeciesMerched$SpRichNIS, 
                      list(CreteSpeciesMerched$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(CreteSpeciesMerched$SpRichNAT, 
                      list(CreteSpeciesMerched$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio <-Ratios_NIS$`202x_Autumn`/Ratios_NIS$`202x_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio <-Ratios_NAT$`202x_Autumn`/Ratios_NAT$`202x_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_NIS <- batch_calc_ES(dat = CreteSpeciesMerched, grouping = eventYear, condition = Season, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(3)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = CreteSpeciesMerched, grouping = eventYear, condition = Season, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(3)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Crete_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Crete_Comm_SpRich$Ratio <- log(Crete_Comm_SpRich$Ratio)
