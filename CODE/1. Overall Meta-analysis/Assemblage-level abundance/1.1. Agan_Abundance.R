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

Agan <- read.csv("DATA/AGAN.csv", 
                 na = "NA")
SpAganSoft <- read_excel("output_data/AGAN_InvasiveStatus.xlsx", 
                         na = "NA")


### turn data into presence absence
AganCategories <- Agan[c(2:5)]
AganSpecies <- Agan[-c(1:5)]

### Create categories to merch replicates
AganCategories$Merched_Category <- factor(paste(AganCategories$eventYear, 
                                                  AganCategories$eventSeason,
                                                  sep = "_"))

### Create Community level abundance columns
AganTranspose <- t(AganSpecies)
AganTranspose <- as.data.frame(AganTranspose)
AganTranspose$Invasive <- SpAganSoft$`Indigenous/Invasive (0/1)`
AganNIS  <- AganTranspose[which(AganTranspose$Invasive  =='1'),]
AganNAT  <- AganTranspose[which(AganTranspose$Invasive  =='0'),]
AganNIS <- AganNIS %>% 
  mutate_if(is.character, as.numeric)
AganNAT <- AganNAT %>% 
  mutate_if(is.character, as.numeric)

AbundanceNIS <- as.data.frame(colSums(AganNIS))
AbundanceNAT <- as.data.frame(colSums(AganNAT))

AganCategories$AbundanceNIS <- AbundanceNIS$`colSums(AganNIS)`[-c(391)]
AganCategories$AbundanceNAT <- AbundanceNAT$`colSums(AganNAT)`[-c(391)]

### Mean values per Year*Season 
Mean_NIS <- aggregate(AganCategories$AbundanceNIS, 
                      list(AganCategories$Merched_Category), FUN=mean) 
Mean_NAT <- aggregate(AganCategories$AbundanceNAT, 
                      list(AganCategories$Merched_Category), FUN=mean)

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio2007 <-Ratios_NIS$`2007_Autumn`/Ratios_NIS$`2007_Spring`
Ratios_NIS$Ratio2008 <-Ratios_NIS$`2008_Autumn`/Ratios_NIS$`2008_Spring`
Ratios_NIS$Ratio2009 <-Ratios_NIS$`2009_Autumn`/Ratios_NIS$`2009_Spring`
Ratios_NIS$Ratio2010 <-Ratios_NIS$`2010_Autumn`/Ratios_NIS$`2010_Spring`
Ratios_NIS$Ratio2011<-Ratios_NIS$`2011_Autumn`/Ratios_NIS$`2011_Spring`
Ratios_NIS$Ratio2012 <-Ratios_NIS$`2012_Autumn`/Ratios_NIS$`2012_Spring`
Ratios_NIS$Ratio2013 <-Ratios_NIS$`2013_Autumn`/Ratios_NIS$`2013_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),-c(1)]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio2007 <-Ratios_NAT$`2007_Autumn`/Ratios_NAT$`2007_Spring`
Ratios_NAT$Ratio2008 <-Ratios_NAT$`2008_Autumn`/Ratios_NAT$`2008_Spring`
Ratios_NAT$Ratio2009 <-Ratios_NAT$`2009_Autumn`/Ratios_NAT$`2009_Spring`
Ratios_NAT$Ratio2010 <-Ratios_NAT$`2010_Autumn`/Ratios_NAT$`2010_Spring`
Ratios_NAT$Ratio2011<-Ratios_NAT$`2011_Autumn`/Ratios_NAT$`2011_Spring`
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2013 <-Ratios_NAT$`2013_Autumn`/Ratios_NAT$`2013_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_Data <- AganCategories[AganCategories$eventYear != c('2006'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
              outcome = AbundanceNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(15:21)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = AbundanceNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(15:21)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
Agan_Comm_Abundance <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
Agan_Comm_Abundance$Ratio <- log(Agan_Comm_Abundance$Ratio)
