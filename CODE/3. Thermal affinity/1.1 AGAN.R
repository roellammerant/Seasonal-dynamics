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
ReplicateNumber <- as.data.frame(table(AganCategories$Merched_Category))

AganSpecies$Merched_Category <- AganCategories$Merched_Category

### Create Community level abundance columns
AganTranspose <- t(AganSpecies)
AganTranspose <- as.data.frame(AganTranspose)
AganTranspose <- AganTranspose[-c(106),]
AganTranspose$Invasive <- SpAganSoft$`Indigenous/Invasive (0/1)`
AganNIS  <- AganTranspose[which(AganTranspose$Invasive  =='1'),]
AganNAT  <- AganTranspose[which(AganTranspose$Invasive  =='0'),]
AganNIS <- AganNIS %>% 
  mutate_if(is.character, as.numeric)
AganNAT <- AganNAT %>% 
  mutate_if(is.character, as.numeric)

### Mean values per Year*Season 
AganNIS <- t(AganNIS)
AganNIS <- as.data.frame(AganNIS)
AganNIS <- AganNIS[-c(391),]
AganNIS$Merched_Category <- AganCategories$Merched_Category
Mean_NIS <- aggregate(AganNIS[c(1:31)], 
                      list(AganNIS$Merched_Category), FUN=mean) 

AganNAT <- t(AganNAT)
AganNAT <- as.data.frame(AganNAT)
AganNAT <- AganNAT[-c(391),]
AganNAT$Merched_Category <- AganCategories$Merched_Category
Mean_NAT <- aggregate(AganNAT[c(1:68)], 
                      list(AganNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),-c(1)]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
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
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio2007 <-Ratios_NAT$`2007_Autumn`/Ratios_NAT$`2007_Spring`
Ratios_NAT$Ratio2008 <-Ratios_NAT$`2008_Autumn`/Ratios_NAT$`2008_Spring`
Ratios_NAT$Ratio2009 <-Ratios_NAT$`2009_Autumn`/Ratios_NAT$`2009_Spring`
Ratios_NAT$Ratio2010 <-Ratios_NAT$`2010_Autumn`/Ratios_NAT$`2010_Spring`
Ratios_NAT$Ratio2011<-Ratios_NAT$`2011_Autumn`/Ratios_NAT$`2011_Spring`
Ratios_NAT$Ratio2012 <-Ratios_NAT$`2012_Autumn`/Ratios_NAT$`2012_Spring`
Ratios_NAT$Ratio2013 <-Ratios_NAT$`2013_Autumn`/Ratios_NAT$`2013_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

##### StdError values for ratios NIS species #####
AganNIS$eventYear <- AganCategories$eventYear
AganNIS$eventSeason <-  AganCategories$eventSeason
StdError_DataNIS <- AganNIS[AganNIS$Merched_Category != c('2006_Autumn'),]
StdError_DataNIS[StdError_DataNIS == 0] <- 0.1

StdError_B007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B007, ES = "LRRi")
StdError_B007$Ratio <- log(Ratios_NIS$B007[c(15:21)])
StdError_B007$TempPrefMax <- "25.43"
StdError_B007$TempPrefMean <- "25.43"
  
StdError_B009 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B009, ES = "LRRi")
StdError_B009$Ratio <- log(Ratios_NIS$B009[c(15:21)])
StdError_B009$TempPrefMax <- "NA"
StdError_B009$TempPrefMean <- "NA"

StdError_B029 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B029, ES = "LRRi")
StdError_B029$Ratio <- log(Ratios_NIS$B029[c(15:21)])
StdError_B029$TempPrefMax <- "29.67"
StdError_B029$TempPrefMean <- "25.15"

StdError_B030 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B030, ES = "LRRi")
StdError_B030$Ratio <- log(Ratios_NIS$B030[c(15:21)])
StdError_B030$TempPrefMax <- "27.94"
StdError_B030$TempPrefMean <- "25.20"

StdError_B078 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(15:21)])
StdError_B078$TempPrefMax <- "30.24"
StdError_B078$TempPrefMean <- "27.18"

StdError_B094 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B094, ES = "LRRi")
StdError_B094$Ratio <- log(Ratios_NIS$B094[c(15:21)])
StdError_B094$TempPrefMax <- "NA"
StdError_B094$TempPrefMean <- "NA"

StdError_B124 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(15:21)])
StdError_B124$TempPrefMax <- "28.36"
StdError_B124$TempPrefMean <- "22.87"

StdError_B132 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B132, ES = "LRRi")
StdError_B132$Ratio <- log(Ratios_NIS$B132[c(15:21)])
StdError_B132$TempPrefMax <- "30.28"
StdError_B132$TempPrefMean <- "24.95"

StdError_B168 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B168, ES = "LRRi")
StdError_B168$Ratio <- log(Ratios_NIS$B168[c(15:21)])
StdError_B168$TempPrefMax <- "22.44"
StdError_B168$TempPrefMean <- "20.93"

StdError_B227 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(15:21)])
StdError_B227$TempPrefMax <- "NA"
StdError_B227$TempPrefMean <- "NA"

StdError_G001 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G001, ES = "LRRi")
StdError_G001$Ratio <- log(Ratios_NIS$G001[c(15:21)])
StdError_G001$TempPrefMax <- "NA"
StdError_G001$TempPrefMean <- "NA"

StdError_G002 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G002, ES ="LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(15:21)])
StdError_G002$TempPrefMax <- "30.37"
StdError_G002$TempPrefMean <- "26.44"

StdError_G003 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G003, ES ="LRRi")
StdError_G003$Ratio <- log(Ratios_NIS$G003[c(15:21)])
StdError_G003$TempPrefMax <- "NA"
StdError_G003$TempPrefMean <- "NA"

StdError_G004 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G004, ES ="LRRi")
StdError_G004$Ratio <- log(Ratios_NIS$G004[c(15:21)])
StdError_G004$TempPrefMax <- "NA"
StdError_G004$TempPrefMean <- "NA"

StdError_G006 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G006, ES = "LRRi")
StdError_G006$Ratio <- log(Ratios_NIS$G006[c(15:21)])
StdError_G006$TempPrefMax <- "NA"
StdError_G006$TempPrefMean <- "NA"

StdError_G007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(15:21)])
StdError_G007$TempPrefMax <- "NA"
StdError_G007$TempPrefMean <- "NA"  

StdError_G008 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G008,ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(15:21)])
StdError_G008$TempPrefMax <- "NA"
StdError_G008$TempPrefMean <- "NA"

StdError_G009 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G009, ES = "LRRi")
StdError_G009$Ratio <- log(Ratios_NIS$G009[c(15:21)])
StdError_G009$TempPrefMax <- "NA"
StdError_G009$TempPrefMean <- "NA"

StdError_G012 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G012, ES ="LRRi")
StdError_G012$Ratio <- log(Ratios_NIS$G012[c(15:21)])
StdError_G012$TempPrefMax <- "NA"
StdError_G012$TempPrefMean <- "NA"

StdError_G063 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G063, ES ="LRRi")
StdError_G063$Ratio <- log(Ratios_NIS$G063[c(15:21)])
StdError_G063$TempPrefMax <- "NA"
StdError_G063$TempPrefMean <- "NA"

StdError_G081 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G081, ES ="LRRi")
StdError_G081$Ratio <- log(Ratios_NIS$G081[c(15:21)])
StdError_G081$TempPrefMax <- "NA"
StdError_G081$TempPrefMean <- "NA"

StdError_G089 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G089, ES ="LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(15:21)])
StdError_G089$TempPrefMax <- "28.27"
StdError_G089$TempPrefMean <- "27.26"

StdError_G097 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G097, ES ="LRRi")
StdError_G097$Ratio <- log(Ratios_NIS$G097[c(15:21)])
StdError_G097$TempPrefMax <- "NA"
StdError_G097$TempPrefMean <- "NA"

StdError_G104 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G104, ES ="LRRi")
StdError_G104$Ratio <- log(Ratios_NIS$G104[c(15:21)])
StdError_G104$TempPrefMax <- "NA"
StdError_G104$TempPrefMean <- "NA"

StdError_G128 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G128, ES ="LRRi")
StdError_G128$Ratio <- log(Ratios_NIS$G128[c(15:21)])
StdError_G128$TempPrefMax <- "NA"
StdError_G128$TempPrefMean <- "NA"

StdError_G133 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G133, ES ="LRRi")
StdError_G133$Ratio <- log(Ratios_NIS$G133[c(15:21)])
StdError_G133$TempPrefMax <- "NA"
StdError_G133$TempPrefMean <- "NA"

StdError_G237 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G237, ES ="LRRi")
StdError_G237$Ratio <- log(Ratios_NIS$G237[c(15:21)])
StdError_G237$TempPrefMax <- "30.43"
StdError_G237$TempPrefMean <- "26.84"

StdError_G246 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G246, ES ="LRRi")
StdError_G246$Ratio <- log(Ratios_NIS$G246[c(15:21)])
StdError_G246$TempPrefMax <- "NA"
StdError_G246$TempPrefMean <- "NA"

StdError_G322 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G322, ES ="LRRi")
StdError_G322$Ratio <- log(Ratios_NIS$G322[c(15:21)])
StdError_G322$TempPrefMax <- "NA"
StdError_G322$TempPrefMean <- "NA"

StdError_G343 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G343, ES ="LRRi")
StdError_G343$Ratio <- log(Ratios_NIS$G343[c(15:21)])
StdError_G343$TempPrefMax <- "NA"
StdError_G343$TempPrefMean <- "NA"

StdError_G398 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G398, ES ="LRRi")
StdError_G398$Ratio <- log(Ratios_NIS$G322[c(15:21)])
StdError_G398$TempPrefMax <- "NA"
StdError_G398$TempPrefMean <- "NA"

StdError_NIS<- rbind(StdError_B007, StdError_B009,StdError_B029,StdError_B030,StdError_B078,StdError_B094,StdError_B124,
                     StdError_B132,StdError_B168,StdError_B227,StdError_G001, StdError_G002,StdError_G003,StdError_G004,
                     StdError_G006,StdError_G007,StdError_G008,StdError_G009,StdError_G012,StdError_G063,StdError_G081,StdError_G089,
                     StdError_G097,StdError_G104,StdError_G128,StdError_G133,StdError_G237,StdError_G246,StdError_G322,StdError_G343,
                     StdError_G398)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

#### StdError values for ratios NIS species ####
AganNAT$eventYear <- AganCategories$eventYear
AganNAT$eventSeason <-  AganCategories$eventSeason
StdError_DataNAT <- AganNAT[AganNAT$Merched_Category != c('2006_Autumn'),]
StdError_DataNAT[StdError_DataNAT == 0] <- 0.1

StdError_B004 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B004, ES = "LRRi")
StdError_B004$Ratio <- log(Ratios_NAT$B004[c(15:21)])
StdError_B004$TempPrefMax <- "27.43"
StdError_B004$TempPrefMean <- "11.44"

StdError_B005 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(15:21)])
StdError_B005$TempPrefMax <- "16.87"
StdError_B005$TempPrefMean <- "11.55"

StdError_B006 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B006, ES = "LRRi")
StdError_B006$Ratio <- log(Ratios_NAT$B006[c(15:21)])
StdError_B006$TempPrefMax <- "28.14"
StdError_B006$TempPrefMean <- "11.22"

StdError_B013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B013, ES = "LRRi")
StdError_B013$Ratio <- log(Ratios_NAT$B013[c(15:21)])
StdError_B013$TempPrefMax <- "29.71"
StdError_B013$TempPrefMean <- "11.00"

StdError_B014 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B014, ES = "LRRi")
StdError_B014$Ratio <- log(Ratios_NAT$B014[c(15:21)])
StdError_B014$TempPrefMax <- "NA"
StdError_B014$TempPrefMean <- "NA"

StdError_B019 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(15:21)])
StdError_B019$TempPrefMax <- "25.69"
StdError_B019$TempPrefMean <- "14.39"

StdError_B020 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B020, ES = "LRRi")
StdError_B020$Ratio <- log(Ratios_NAT$B020[c(15:21)])
StdError_B020$TempPrefMax <- "28.041"
StdError_B020$TempPrefMean <- "14.77"

StdError_B023 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(15:21)])
StdError_B023$TempPrefMax <- "20.53"
StdError_B023$TempPrefMean <- "13.49"

StdError_B033 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(15:21)])
StdError_B033$TempPrefMax <- "24.45"
StdError_B033$TempPrefMean <- "11.58"

StdError_B036 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B036, ES = "LRRi")
StdError_B036$Ratio <- log(Ratios_NAT$B036[c(15:21)])
StdError_B036$TempPrefMax <- "18.63"
StdError_B036$TempPrefMean <- "13.62"

StdError_B039 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B039, ES = "LRRi")
StdError_B039$Ratio <- log(Ratios_NAT$B039[c(15:21)])
StdError_B039$TempPrefMax <- "NA"
StdError_B039$TempPrefMean <- "NA"

StdError_B044 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B044, ES = "LRRi")
StdError_B044$Ratio <- log(Ratios_NAT$B044[c(15:21)])
StdError_B044$TempPrefMax <- "20.20"
StdError_B044$TempPrefMean <- "15.00"

StdError_B046 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(15:21)])
StdError_B046$TempPrefMax <- "28.59"
StdError_B046$TempPrefMean <- "12.02"

StdError_B052 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B052, ES = "LRRi")
StdError_B052$Ratio <- log(Ratios_NAT$B052[c(15:21)])
StdError_B052$TempPrefMax <- "20.53"
StdError_B052$TempPrefMean <- "12.36"

StdError_B055 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(15:21)])
StdError_B055$TempPrefMax <- "29.24"
StdError_B055$TempPrefMean <- "13.61"

StdError_B068 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B068, ES = "LRRi")
StdError_B068$Ratio <- log(Ratios_NAT$B068[c(15:21)])
StdError_B068$TempPrefMax <- "24.43"
StdError_B068$TempPrefMean <- "18.22"

StdError_B074 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B074, ES = "LRRi")
StdError_B074$Ratio <- log(Ratios_NAT$B074[c(15:21)])
StdError_B074$TempPrefMax <- "27.47"
StdError_B074$TempPrefMean <- "18.41"

StdError_B084 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B084, ES = "LRRi")
StdError_B084$Ratio <- log(Ratios_NAT$B084[c(15:21)])
StdError_B084$TempPrefMax <- "19.43"
StdError_B084$TempPrefMean <- "13.31"

StdError_B121 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B121, ES = "LRRi")
StdError_B121$Ratio <- log(Ratios_NAT$B121[c(15:21)])
StdError_B121$TempPrefMax <- "18.78"
StdError_B121$TempPrefMean <- "11.28"

StdError_B158 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B158, ES = "LRRi")
StdError_B158$Ratio <- log(Ratios_NAT$B158[c(15:21)])
StdError_B158$TempPrefMax <- "29.78"
StdError_B158$TempPrefMean <- "17.30"

StdError_B173 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B173, ES = "LRRi")
StdError_B173$Ratio <- log(Ratios_NAT$B173[c(15:21)])
StdError_B173$TempPrefMax <- "18.63"
StdError_B173$TempPrefMean <- "14.56"

StdError_B182 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B182, ES = "LRRi")
StdError_B182$Ratio <- log(Ratios_NAT$B182[c(15:21)])
StdError_B182$TempPrefMax <- "23.25"
StdError_B182$TempPrefMean <- "11.64"

StdError_B186 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B186, ES = "LRRi")
StdError_B186$Ratio <- log(Ratios_NAT$B186[c(15:21)])
StdError_B186$TempPrefMax <- "NA"
StdError_B186$TempPrefMean <- "NA"

StdError_B187 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B187, ES = "LRRi")
StdError_B187$Ratio <- log(Ratios_NAT$B187[c(15:21)])
StdError_B187$TempPrefMax <- "18.63"
StdError_B187$TempPrefMean <- "13.36"

StdError_B273 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B273, ES = "LRRi")
StdError_B273$Ratio <- log(Ratios_NAT$B273[c(15:21)])
StdError_B273$TempPrefMax <- "18.78"
StdError_B273$TempPrefMean <- "17.35"

StdError_B283 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B283, ES = "LRRi")
StdError_B283$Ratio <- log(Ratios_NAT$B283[c(15:21)])
StdError_B283$TempPrefMax <- "20.20"
StdError_B283$TempPrefMean <- "11.87" 

StdError_B284 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B284, ES = "LRRi")
StdError_B284$Ratio <- log(Ratios_NAT$B284[c(15:21)])
StdError_B284$TempPrefMax <- "19.70"
StdError_B284$TempPrefMean <- "16.68"

StdError_B321 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B321, ES = "LRRi")
StdError_B321$Ratio <- log(Ratios_NAT$B321[c(15:21)])
StdError_B321$TempPrefMax <- "NA"
StdError_B321$TempPrefMean <- "NA"

StdError_B451 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B451, ES = "LRRi")
StdError_B451$Ratio <- log(Ratios_NAT$B451[c(15:21)])
StdError_B451$TempPrefMax <- "18.65"
StdError_B451$TempPrefMean <- "13.91"

StdError_B454 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B454, ES = "LRRi")
StdError_B454$Ratio <- log(Ratios_NAT$B454[c(15:21)])
StdError_B454$TempPrefMax <- "20.78"
StdError_B454$TempPrefMean <- "16.67"

StdError_B478 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B478, ES = "LRRi")
StdError_B478$Ratio <- log(Ratios_NAT$B478[c(15:21)])
StdError_B478$TempPrefMax <- "26.79"
StdError_B478$TempPrefMean <- "22.27"

StdError_B491 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B491, ES = "LRRi")
StdError_B491$Ratio <- log(Ratios_NAT$B491[c(15:21)])
StdError_B491$TempPrefMax <- "NA"
StdError_B491$TempPrefMean <- "NA"

StdError_G010 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(15:21)])
StdError_G010$TempPrefMax <- "NA"
StdError_G010$TempPrefMean <- "NA"

StdError_G013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G013, ES = "LRRi")
StdError_G013$Ratio <- log(Ratios_NAT$G013[c(15:21)])
StdError_G013$TempPrefMax <- "NA"
StdError_G013$TempPrefMean <- "NA"

StdError_G015 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G015, ES = "LRRi")
StdError_G015$Ratio <- log(Ratios_NAT$G015[c(15:21)])
StdError_G015$TempPrefMax <- "18.78"
StdError_G015$TempPrefMean <- "12.35"

StdError_G017 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(15:21)])
StdError_G017$TempPrefMax <- "21.80"
StdError_G017$TempPrefMean <- "19.23"

StdError_G027 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(15:21)])
StdError_G027$TempPrefMax <- "NA"
StdError_G027$TempPrefMean <- "NA"

StdError_G045 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G045, ES = "LRRi")
StdError_G045$Ratio <- log(Ratios_NAT$G045[c(15:21)])
StdError_G045$TempPrefMax <- "20.50"
StdError_G045$TempPrefMean <- "10.77"

StdError_G047 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G047, ES = "LRRi")
StdError_G047$Ratio <- log(Ratios_NAT$G047[c(15:21)])
StdError_G047$TempPrefMax <- "NA"
StdError_G047$TempPrefMean <- "NA"

StdError_G053 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G053, ES = "LRRi")
StdError_G053$Ratio <- log(Ratios_NAT$G053[c(15:21)])
StdError_G053$TempPrefMax <- "18.63"
StdError_G053$TempPrefMean <- "16.64"

StdError_G064 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G064, ES = "LRRi")
StdError_G064$Ratio <- log(Ratios_NAT$G064[c(15:21)])
StdError_G064$TempPrefMax <- "NA"
StdError_G064$TempPrefMean <- "NA"

StdError_G066 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G066, ES = "LRRi")
StdError_G066$Ratio <- log(Ratios_NAT$G066[c(15:21)])
StdError_G066$TempPrefMax <- "14.73"
StdError_G066$TempPrefMean <- "11.45"

StdError_G073 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G073, ES = "LRRi")
StdError_G073$Ratio <- log(Ratios_NAT$G073[c(15:21)])
StdError_G073$TempPrefMax <- "NA"
StdError_G073$TempPrefMean <- "NA"

StdError_G075 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G075, ES = "LRRi")
StdError_G075$Ratio <- log(Ratios_NAT$G075[c(15:21)])
StdError_G075$TempPrefMax <- "23.25"
StdError_G075$TempPrefMean <- "11.86"

StdError_G080 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G080, ES = "LRRi")
StdError_G080$Ratio <- log(Ratios_NAT$G080[c(15:21)])
StdError_G080$TempPrefMax <- "NA"
StdError_G080$TempPrefMean <- "NA"

StdError_G105 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G105, ES = "LRRi")
StdError_G105$Ratio <- log(Ratios_NAT$G105[c(15:21)])
StdError_G105$TempPrefMax <- "NA"
StdError_G105$TempPrefMean <- "NA"

StdError_G107 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G107, ES = "LRRi")
StdError_G107$Ratio <- log(Ratios_NAT$G107[c(15:21)])
StdError_G107$TempPrefMax <- "NA"
StdError_G107$TempPrefMean <- "NA"

StdError_G112 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G112, ES = "LRRi")
StdError_G112$Ratio <- log(Ratios_NAT$G112[c(15:21)])
StdError_G112$TempPrefMax <- "NA"
StdError_G112$TempPrefMean <- "NA"

StdError_G134 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G134, ES = "LRRi")
StdError_G134$Ratio <- log(Ratios_NAT$G134[c(15:21)])
StdError_G134$TempPrefMax <- "NA"
StdError_G134$TempPrefMean <- "NA"

StdError_G145 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G145, ES = "LRRi")
StdError_G145$Ratio <- log(Ratios_NAT$G145[c(15:21)])
StdError_G145$TempPrefMax <- "NA"
StdError_G145$TempPrefMean <- "NA"

StdError_G205 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G205, ES = "LRRi")
StdError_G205$Ratio <- log(Ratios_NAT$G205[c(15:21)])
StdError_G205$TempPrefMax <- "21.19"
StdError_G205$TempPrefMean <- "11.73"

StdError_G232 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G232, ES = "LRRi")
StdError_G232$Ratio <- log(Ratios_NAT$G232[c(15:21)])
StdError_G232$TempPrefMax <- "NA"
StdError_G232$TempPrefMean <- "NA"

StdError_G338 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(15:21)])
StdError_G338$TempPrefMax <- "NA"
StdError_G338$TempPrefMean <- "NA"

StdError_G357 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G357, ES = "LRRi")
StdError_G357$Ratio <- log(Ratios_NAT$G357[c(15:21)])
StdError_G357$TempPrefMax <- "NA"
StdError_G357$TempPrefMean <- "NA"

StdError_G457 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G457, ES = "LRRi")
StdError_G457$Ratio <- log(Ratios_NAT$G457[c(15:21)])
StdError_G457$TempPrefMax <- "NA"
StdError_G457$TempPrefMean <- "NA"

StdError_G483 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G483, ES = "LRRi")
StdError_G483$Ratio <- log(Ratios_NAT$G483[c(15:21)])
StdError_G483$TempPrefMax <- "NA"
StdError_G483$TempPrefMean <- "NA"

StdError_G514 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G514, ES = "LRRi")
StdError_G514$Ratio <- log(Ratios_NAT$G514[c(15:21)])
StdError_G514$TempPrefMax <- "NA"
StdError_G514$TempPrefMean <- "NA"

StdError_G549 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G549, ES = "LRRi")
StdError_G549$Ratio <- log(Ratios_NAT$G549[c(15:21)])
StdError_G549$TempPrefMax <- "NA"
StdError_G549$TempPrefMean <- "NA"

StdError_G673 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G673, ES = "LRRi")
StdError_G673$Ratio <- log(Ratios_NAT$G673[c(15:21)])
StdError_G673$TempPrefMax <- "NA"
StdError_G673$TempPrefMean <- "NA"

StdError_G699 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(15:21)])
StdError_G699$TempPrefMax <- "NA"
StdError_G699$TempPrefMean <- "NA"

StdError_G705 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G705, ES = "LRRi")
StdError_G705$Ratio <- log(Ratios_NAT$G705[c(15:21)])
StdError_G705$TempPrefMax <- "NA"
StdError_G705$TempPrefMean <- "NA"

StdError_G707 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G707, ES = "LRRi")
StdError_G707$Ratio <- log(Ratios_NAT$G707[c(15:21)])
StdError_G707$TempPrefMax <- "NA"
StdError_G707$TempPrefMean <- "NA"

StdError_G711 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G711, ES = "LRRi")
StdError_G711$Ratio <- log(Ratios_NAT$G711[c(15:21)])
StdError_G711$TempPrefMax <- "NA"
StdError_G711$TempPrefMean <- "NA"

StdError_G714 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G714, ES = "LRRi")
StdError_G714$Ratio <- log(Ratios_NAT$G714[c(15:21)])
StdError_G714$TempPrefMax <- "NA"
StdError_G714$TempPrefMean <- "NA"

StdError_G717 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G717, ES = "LRRi")
StdError_G717$Ratio <- log(Ratios_NAT$G717[c(15:21)])
StdError_G717$TempPrefMax <- "26.88"
StdError_G717$TempPrefMean <- "10.63"

StdError_G724 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G724, ES = "LRRi")
StdError_G724$Ratio <- log(Ratios_NAT$G724[c(15:21)])
StdError_G724$TempPrefMax <- "NA"
StdError_G724$TempPrefMean <- "NA"

StdError_G913 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G913, ES = "LRRi")
StdError_G913$Ratio <- log(Ratios_NAT$G913[c(15:21)])
StdError_G913$TempPrefMax <- "NA"
StdError_G913$TempPrefMean <- "NA"

StdError_G916 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G916, ES = "LRRi")
StdError_G916$Ratio <- log(Ratios_NAT$G916[c(15:21)])
StdError_G916$TempPrefMax <- "NA"
StdError_G916$TempPrefMean <- "NA"

StdError_NAT<- rbind(StdError_B004,StdError_B005,StdError_B006,StdError_B013,StdError_B014,StdError_B019,StdError_B020,StdError_B023,StdError_B033,
                     StdError_B036,StdError_B039,StdError_B044,StdError_B046,StdError_B052,StdError_B055,StdError_B068,StdError_B074,StdError_B084,
                     StdError_B121,StdError_B158,StdError_B173,StdError_B182,StdError_B186,StdError_B187,StdError_B273,StdError_B283,StdError_B284,
                     StdError_B321,StdError_B451,StdError_B454,StdError_B478,StdError_B491,StdError_G010,StdError_G013,StdError_G015,StdError_G017,
                     StdError_G027,StdError_G045,StdError_G047,StdError_G053,StdError_G064,StdError_G066,StdError_G073,StdError_G075,StdError_G080,
                     StdError_G105,StdError_G107,StdError_G112,StdError_G134,StdError_G145,StdError_G205,StdError_G232,StdError_G338,StdError_G357,
                     StdError_G457,StdError_G483,StdError_G514,StdError_G549,StdError_G673,StdError_G699,StdError_G705,StdError_G711,StdError_G714,
                     StdError_G717,StdError_G724,StdError_G913,StdError_G916)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]


### Create dataframe
Agan_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9,10)],StdError_NAT[c(1,4,7,8,9,10)])
Agan_Species_Abundance <- Agan_Species_Abundance[Agan_Species_Abundance$TempPrefMax != c('NA'),]
