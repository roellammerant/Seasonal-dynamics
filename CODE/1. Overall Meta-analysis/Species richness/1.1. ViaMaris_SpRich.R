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
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'reshape2', 'dplyr', 'forcats','SingleCaseES')
ipak(packages)


##%######################################################%##
#                                                          #
####        Load Mollusc dataset and summarise           ####
#                                                          #
##%######################################################%##
ViaMaris <- read.csv("DATA/ViaMaris.csv", 
                     na = "NA")
SpViaMaris <- read_excel("output_data/ViaMaris_InvasiveStatus.xlsx", 
                         na = "NA")


### Create categories to merch replicates
ViaMaris$Merched_Replicates <- factor(paste(ViaMaris$eventYear, 
                                            ViaMaris$eventSeason,
                                            ViaMaris$StationCode,
                                            sep = "_"))
x <-as.data.frame(table(ViaMaris$Merched_Replicates))

#make sure that all replicates fall under the same category
# Rename levels
ViaMaris$Merched_Replicates <- recode(ViaMaris$Merched_Replicates, 
                                      '2016_Autumn_VM45N' = '2016_Autumn_VM45', 
                                      '2022_Autumn_VM43N' = '2022_Autumn_VM43',
                                      '2021_Spring_VM42N' = '2021_Spring_VM42',
                                      '2021_Spring_VM43N' = '2021_Spring_VM43',)

### Merge the first two replicates, since not all samples have three replicates
### Only one replicate for 2009_Spring_VM3, take sample out
ViaMaris <- ViaMaris[ViaMaris$Merched_Replicates != c('2009_Spring_VM3'),]
ViaMaris1 <- ViaMaris[ViaMaris$Replicate == c('1'),]
ViaMaris2 <- ViaMaris[ViaMaris$Replicate == c('2'),]
ViaMarisAll <- rbind(ViaMaris1,ViaMaris2)
x2 <-as.data.frame(table(ViaMarisAll$Merched_Replicates))

### Some replicates only have 2,3 instead of 1,2
### Add missing data
ViaMarisMissing <- ViaMaris[c(79,165,483,662,314,629,634,330),]
ViaMarisAllB <- rbind(ViaMarisAll,ViaMarisMissing)
x2 <-as.data.frame(table(ViaMarisAllB$Merched_Replicates))

### Create categories and species list
ViaMarisCategories <- ViaMarisAllB[c(2:5)]
ViaMarisSpecies <- ViaMarisAllB[-c(1:5)]

### Merge replicates
ViaMarisSpeciesMerched <- aggregate(ViaMarisSpecies[-c(138)], by=list(Category=ViaMarisSpecies$Merched_Replicates), FUN=sum)

# Change structure around: Species become rows and sites become columns
ViaMarisTranspose <- t(ViaMarisSpeciesMerched[,-c(1)])
ViaMarisTranspose <- as.data.frame(ViaMarisTranspose)
ViaMarisTranspose$Invasive <- SpViaMaris$Invasive

# Standardized species richness
# delete sites with no species present
ViaMarisInvasive  <- ViaMarisTranspose[which(ViaMarisTranspose$Invasive  =='1'),]
ViaMarisInvasive <- ViaMarisInvasive %>% mutate_if(is.character, as.numeric)
ViaMarisInvasive <- ViaMarisInvasive[-c(2,15,16,17,19,20,30,40,45,48,52,76,188,254, # zero species
                                        6,7,14,18,28,31,34,39,41,78,103,105,127,181,
                                        182,185,221,225,238,247,248,251,284, # one species
                                        43,44,82,215,95, # equal N and S
                                        49, # low coverage
                                        291)] 
ViaMarisInv_Cov <- iNEXT::DataInfo(ViaMarisInvasive, datatype = "abundance") 
ViaMarisInv_Cov$Category <- ViaMarisSpeciesMerched$Category[-c(2,15,16,17,19,20,30,40,45,48,52,76,188,254, # zero species
                                                               6,7,14,18,28,31,34,39,41,78,103,105,127,181,
                                                               182,185,221,225,238,247,248,251,284, # one species
                                                               43,44,82,215,95, # equal N and S
                                                               49)] # # low coverage
colnames(ViaMarisInv_Cov)[2] <- c("IndCount_NIS")
ViaMarisInvSp <- estimateD(ViaMarisInvasive, q = c(0), datatype = "abundance", base="coverage",level = 0.4857)
colnames(ViaMarisInvSp)[6:8] <- c("SpRichNIS","Low_conf_NIS","High_conf_NIS")

ViaMarisIndigenous <- ViaMarisTranspose[which(ViaMarisTranspose$Invasive =='0'),]
ViaMarisIndigenous <- ViaMarisIndigenous %>% mutate_if(is.character, as.numeric)
ViaMarisIndigenous <- ViaMarisIndigenous[-c(104,126,127,149,153,155,159,171,192,193,219, #zero species
                                            43,44,82,106,109,110,128,130,131,132,133,154,177,178,181,
                                            182,185,195,198,200,214,217,222,225,240,241,242,264,265,270,
                                            271,273,274,275,276,278,285, # one species
                                            26, 36,105,148,150,151,152,156,157,186,238,269,13,
                                            108,191,196,197,164,176, # equal N and S
                                            16,88,139,288,123,# low coverage
                                            291)]
ViaMarisInd_Cov <- iNEXT::DataInfo(ViaMarisIndigenous, datatype = "abundance") 
ViaMarisInd_Cov$Category <- ViaMarisSpeciesMerched$Category[-c(104,126,127,149,153,155,159,171,192,193,219, #zero species
                                                               43,44,82,106,109,110,128,130,131,132,133,154,177,178,181,
                                                               182,185,195,198,200,214,217,222,225,240,241,242,264,265,270,
                                                               271,273,274,275,276,278,285, # one species
                                                               26, 36,105,148,150,151,152,156,157,186,238,269,13,
                                                               108,191,196,197,164,176, # equal N and S
                                                               16,88,139,288,123)]# low coverage
colnames(ViaMarisInd_Cov)[2] <- c("IndCount_NAT")
ViaMarisIndSp <- iNEXT::estimateD(ViaMarisIndigenous, q = c(0), datatype = "abundance", base="coverage",level = 0.4857)
colnames(ViaMarisIndSp)[6:8] <- c("SpRichNAT","Low_conf_NAT","High_conf_NAT")

# Link back to the other categories
ViaMarisSpeciesMerched$Match <- colnames(ViaMarisTranspose[-c(291)])
ViaMarisSpeciesMerched <- ViaMarisSpeciesMerched[c(1, 139)]
ViaMarisSpeciesMerched <- merge(ViaMarisSpeciesMerched, ViaMarisIndSp,  by.x = "Match", by.y = "Assemblage", all.x = TRUE)
ViaMarisSpeciesMerched <- merge(ViaMarisSpeciesMerched, ViaMarisInvSp,  by.x = "Match", by.y = "Assemblage", all.x = TRUE)
ViaMarisSpeciesMerched <-  ViaMarisSpeciesMerched[c(1,2,7:9,14:16) ]
colnames(ViaMarisSpeciesMerched)[2] <- c("Category")

ViaMarisCategoriesSubset <- ViaMarisAllB[which(ViaMarisAllB$Replicate  =='1'),]
ViaMarisSpeciesMerched <- merge(ViaMarisSpeciesMerched, ViaMarisCategoriesSubset[c(2:4, 143)], 
                                by.x = "Category", by.y = "Merched_Replicates", all.x = TRUE)

ViaMarisSpeciesMerched$StationCode[c(30,231,261)] <- c("VM3","VM71","VM45")
ViaMarisSpeciesMerched$eventYear[c(30,231,261)] <- c("2010","2021","2023")
ViaMarisSpeciesMerched$eventSeason[c(30,231,261)] <- c("Spring")
# Only leave replicates with a zero when actually no individuals were present
# Clean out data that had only one species or the same species as number of individuals
# Loss of 80 replicates after cleaning data 
ViaMarisSpeciesMerched <- ViaMarisSpeciesMerched[-c(6,7,13,14,16,18,26,28,31,34,36,39,41,43,44,49,78,82,88,95,103,105,106,108,109,110,
                                                    123,127,128,130,131,132,133,139,148,150,151,152,154,156,157,164,176,177,178,181,
                                                    182,185,186,191,195,196,197,198,200,214,215,217,221,222,225,238, # extra line gets introduce with adding categories, updated numbers for this
                                                    239,241,242,243,248,249,252,
                                                    265,266,270,271,272,274,275,276,277,279,286,289),]
ViaMarisSpeciesMerched[is.na(ViaMarisSpeciesMerched)] <- 0

### Create categories to merch replicates
ViaMarisSpeciesMerched$Merched_Category <- factor(paste(ViaMarisSpeciesMerched$eventYear, 
                                                        ViaMarisSpeciesMerched$eventSeason,
                                                    sep = "_"))
### Mean values per Year*Season 
Mean_NIS <- aggregate(ViaMarisSpeciesMerched$SpRichNIS, 
                      list(ViaMarisSpeciesMerched$Merched_Category), FUN=mean) 
Mean_NIS[Mean_NIS == 0] <- 0.1
Mean_NAT <- aggregate(ViaMarisSpeciesMerched$SpRichNAT, 
                      list(ViaMarisSpeciesMerched$Merched_Category), FUN=mean)
### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS$Ratio2009 <-Ratios_NIS$`2009_Autumn`/Ratios_NIS$`2009_Spring`
Ratios_NIS$Ratio2010 <-Ratios_NIS$`2010_Autumn`/Ratios_NIS$`2010_Spring`
Ratios_NIS$Ratio2011 <-Ratios_NIS$`2011_Autumn`/Ratios_NIS$`2011_Spring`
Ratios_NIS$Ratio2012 <-Ratios_NIS$`2012_Autumn`/Ratios_NIS$`2012_Spring`
Ratios_NIS$Ratio2015<-Ratios_NIS$`2015_Autumn`/Ratios_NIS$`2015_Spring`
Ratios_NIS$Ratio2016 <-Ratios_NIS$`2016_Autumn`/Ratios_NIS$`2016_Spring`
Ratios_NIS$Ratio2017 <-Ratios_NIS$`2017_Autumn`/Ratios_NIS$`2017_Spring`
Ratios_NIS$Ratio2018<-Ratios_NIS$`2018_Autumn`/Ratios_NIS$`2018_Spring`
Ratios_NIS$Ratio2019 <-Ratios_NIS$`2019_Autumn`/Ratios_NIS$`2019_Spring`
Ratios_NIS$Ratio2020 <-Ratios_NIS$`2020_Autumn`/Ratios_NIS$`2020_Spring`
Ratios_NIS$Ratio2021<-Ratios_NIS$`2021_Autumn`/Ratios_NIS$`2021_Spring`
Ratios_NIS$Ratio2022 <-Ratios_NIS$`2022_Autumn`/Ratios_NIS$`2022_Spring`
Ratios_NIS$Ratio2024 <-Ratios_NIS$`2024_Autumn`/Ratios_NIS$`2024_Spring`
Ratios_NIS <- as.data.frame(t(Ratios_NIS))

Ratios_NAT <- as.data.frame(t(Mean_NAT))
colnames(Ratios_NAT) <- Ratios_NAT[1,]
Ratios_NAT<- Ratios_NAT[-c(1),-c(1)]
Ratios_NAT <- Ratios_NAT %>% 
  mutate_if(is.character, as.numeric)
Ratios_NAT$Ratio2009 <-Ratios_NAT$`2009_Autumn`/Ratios_NAT$`2009_Spring`
Ratios_NAT$Ratio2010 <-Ratios_NAT$`2010_Autumn`/Ratios_NAT$`2010_Spring`
Ratios_NAT$Ratio2011 <-Ratios_NAT$`2011_Autumn`/Ratios_NAT$`2011_Spring`
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2015 <-Ratios_NAT$`2015_Autumn`/Ratios_NAT$`2015_Spring`
Ratios_NAT$Ratio2016 <-Ratios_NAT$`2016_Autumn`/Ratios_NAT$`2016_Spring`
Ratios_NAT$Ratio2017 <-Ratios_NAT$`2017_Autumn`/Ratios_NAT$`2017_Spring`
Ratios_NAT$Ratio2018<-Ratios_NAT$`2018_Autumn`/Ratios_NAT$`2018_Spring`
Ratios_NAT$Ratio2019 <-Ratios_NAT$`2019_Autumn`/Ratios_NAT$`2019_Spring`
Ratios_NAT$Ratio2020 <-Ratios_NAT$`2020_Autumn`/Ratios_NAT$`2020_Spring`
Ratios_NAT$Ratio2021<-Ratios_NAT$`2021_Autumn`/Ratios_NAT$`2021_Spring`
Ratios_NAT$Ratio2022 <-Ratios_NAT$`2022_Autumn`/Ratios_NAT$`2022_Spring`
Ratios_NAT$Ratio2024 <-Ratios_NAT$`2024_Autumn`/Ratios_NAT$`2024_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for ratios
StdError_Data <- ViaMarisSpeciesMerched[ViaMarisSpeciesMerched$eventYear != c('2008'),]
StdError_Data <- StdError_Data[StdError_Data$eventYear != c('2013'),]
StdError_Data <- StdError_Data[StdError_Data$eventYear != c('2023'),]

StdError_NIS <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNIS, ES = "LRRi")
StdError_NIS$Ratio <- Ratios_NIS$x[c(29:41)]
StdError_NIS$Invasive <- "NIS"

StdError_NAT <- batch_calc_ES(dat = StdError_Data, grouping = eventYear, condition = eventSeason, 
                              outcome = SpRichNAT, ES = "LRRi")
StdError_NAT$Ratio <- Ratios_NAT$x[c(29:41)]
StdError_NAT$Invasive <- "NAT"

### Create dataframe
ViaMaris_Comm_SpRich <- rbind(StdError_NIS[c(1,4,7,8)],StdError_NAT[c(1,4,7,8)])
ViaMaris_Comm_SpRich <-na.omit(ViaMaris_Comm_SpRich)
ViaMaris_Comm_SpRich$Ratio <- log(ViaMaris_Comm_SpRich$Ratio)
