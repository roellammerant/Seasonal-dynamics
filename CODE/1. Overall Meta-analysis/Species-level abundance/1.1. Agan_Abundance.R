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
StdError_B007$Species <- "B007"

StdError_B009 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B009, ES = "LRRi")
StdError_B009$Ratio <- log(Ratios_NIS$B009[c(15:21)])
StdError_B009$Species <- "B009"

StdError_B029 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B029, ES = "LRRi")
StdError_B029$Ratio <- log(Ratios_NIS$B029[c(15:21)])
StdError_B029$Species <- "B029"

StdError_B030 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B030, ES = "LRRi")
StdError_B030$Ratio <- log(Ratios_NIS$B030[c(15:21)])
StdError_B030$Species <- "B030"

StdError_B078 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(15:21)])
StdError_B078$Species <- "B078"

StdError_B094 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B094, ES = "LRRi")
StdError_B094$Ratio <- log(Ratios_NIS$B094[c(15:21)])
StdError_B094$Species <- "B094"

StdError_B124 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B124, ES = "LRRi")
StdError_B124$Ratio <- log(Ratios_NIS$B124[c(15:21)])
StdError_B124$Species <- "B124"

StdError_B132 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B132, ES = "LRRi")
StdError_B132$Ratio <- log(Ratios_NIS$B132[c(15:21)])
StdError_B132$Species <- "B132"

StdError_B168 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B168, ES = "LRRi")
StdError_B168$Ratio <- log(Ratios_NIS$B168[c(15:21)])
StdError_B168$Species <- "B168"

StdError_B227 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = B227, ES = "LRRi")
StdError_B227$Ratio <- log(Ratios_NIS$B227[c(15:21)])
StdError_B227$Species <- "B227"

StdError_G001 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G001, ES = "LRRi")
StdError_G001$Ratio <- log(Ratios_NIS$G001[c(15:21)])
StdError_G001$Species <- "G001"

StdError_G002 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G002, ES ="LRRi")
StdError_G002$Ratio <- log(Ratios_NIS$G002[c(15:21)])
StdError_G002$Species <- "G002"

StdError_G003 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G003, ES ="LRRi")
StdError_G003$Ratio <- log(Ratios_NIS$G003[c(15:21)])
StdError_G003$Species <- "G003"

StdError_G004 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G004, ES ="LRRi")
StdError_G004$Ratio <- log(Ratios_NIS$G004[c(15:21)])
StdError_G004$Species <- "G004"

StdError_G006 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G006, ES = "LRRi")
StdError_G006$Ratio <- log(Ratios_NIS$G006[c(15:21)])
StdError_G006$Species <- "G006"

StdError_G007 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G007, ES = "LRRi")
StdError_G007$Ratio <- log(Ratios_NIS$G007[c(15:21)])
StdError_G007$Species <- "G007"

StdError_G008 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G008,ES = "LRRi")
StdError_G008$Ratio <- log(Ratios_NIS$G008[c(15:21)])
StdError_G008$Species <- "G008"

StdError_G009 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G009, ES = "LRRi")
StdError_G009$Ratio <- log(Ratios_NIS$G009[c(15:21)])
StdError_G009$Species <- "G009"

StdError_G012 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G012, ES ="LRRi")
StdError_G012$Ratio <- log(Ratios_NIS$G012[c(15:21)])
StdError_G012$Species <- "G012"

StdError_G063 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G063, ES ="LRRi")
StdError_G063$Ratio <- log(Ratios_NIS$G063[c(15:21)])
StdError_G063$Species <- "G063"

StdError_G081 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G081, ES ="LRRi")
StdError_G081$Ratio <- log(Ratios_NIS$G081[c(15:21)])
StdError_G081$Species <- "G081"

StdError_G089 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G089, ES ="LRRi")
StdError_G089$Ratio <- log(Ratios_NIS$G089[c(15:21)])
StdError_G089$Species <- "G089"

StdError_G097 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G097, ES ="LRRi")
StdError_G097$Ratio <- log(Ratios_NIS$G097[c(15:21)])
StdError_G097$Species <- "G097"

StdError_G104 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G104, ES ="LRRi")
StdError_G104$Ratio <- log(Ratios_NIS$G104[c(15:21)])
StdError_G104$Species <- "G104"

StdError_G128 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G128, ES ="LRRi")
StdError_G128$Ratio <- log(Ratios_NIS$G128[c(15:21)])
StdError_G128$Species <- "G128"

StdError_G133 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G133, ES ="LRRi")
StdError_G133$Ratio <- log(Ratios_NIS$G133[c(15:21)])
StdError_G133$Species <- "G133"

StdError_G237 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G237, ES ="LRRi")
StdError_G237$Ratio <- log(Ratios_NIS$G237[c(15:21)])
StdError_G237$Species <- "G237"

StdError_G246 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G246, ES ="LRRi")
StdError_G246$Ratio <- log(Ratios_NIS$G246[c(15:21)])
StdError_G246$Species <- "G246"

StdError_G322 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G322, ES ="LRRi")
StdError_G322$Ratio <- log(Ratios_NIS$G322[c(15:21)])
StdError_G322$Species <- "G322"

StdError_G343 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G343, ES ="LRRi")
StdError_G343$Ratio <- log(Ratios_NIS$G343[c(15:21)])
StdError_G343$Species <- "G343"

StdError_G398 <- batch_calc_ES(dat = StdError_DataNIS, grouping = eventYear, condition = eventSeason, 
                               outcome = G398, ES ="LRRi")
StdError_G398$Ratio <- log(Ratios_NIS$G322[c(15:21)])
StdError_G398$Species <- "G398"

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
StdError_B004$Species <- "B004"

StdError_B005 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(15:21)])
StdError_B005$Species <- "B005"

StdError_B006 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B006, ES = "LRRi")
StdError_B006$Ratio <- log(Ratios_NAT$B006[c(15:21)])
StdError_B006$Species <- "B006"

StdError_B013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B013, ES = "LRRi")
StdError_B013$Ratio <- log(Ratios_NAT$B013[c(15:21)])
StdError_B013$Species <- "B013"

StdError_B014 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                                outcome = B014, ES = "LRRi")
StdError_B014$Ratio <- log(Ratios_NAT$B014[c(15:21)])
StdError_B014$Species <- "B014"

StdError_B019 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                                outcome = B019, ES = "LRRi")
StdError_B019$Ratio <- log(Ratios_NAT$B019[c(15:21)])
StdError_B019$Species <- "B019"

StdError_B020 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                                outcome = B020, ES = "LRRi")
StdError_B020$Ratio <- log(Ratios_NAT$B020[c(15:21)])
StdError_B020$Species <- "B020"

StdError_B023 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B023, ES = "LRRi")
StdError_B023$Ratio <- log(Ratios_NAT$B023[c(15:21)])
StdError_B023$Species <- "B023"

StdError_B033 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B033, ES = "LRRi")
StdError_B033$Ratio <- log(Ratios_NAT$B033[c(15:21)])
StdError_B033$Species <- "B033"

StdError_B036 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B036, ES = "LRRi")
StdError_B036$Ratio <- log(Ratios_NAT$B036[c(15:21)])
StdError_B036$Species <- "B036"

StdError_B039 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B039, ES = "LRRi")
StdError_B039$Ratio <- log(Ratios_NAT$B039[c(15:21)])
StdError_B039$Species <- "B039"

StdError_B044 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B044, ES = "LRRi")
StdError_B044$Ratio <- log(Ratios_NAT$B044[c(15:21)])
StdError_B044$Species <- "B044"

StdError_B046 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B046, ES = "LRRi")
StdError_B046$Ratio <- log(Ratios_NAT$B046[c(15:21)])
StdError_B046$Species <- "B046"

StdError_B052 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B052, ES = "LRRi")
StdError_B052$Ratio <- log(Ratios_NAT$B052[c(15:21)])
StdError_B052$Species <- "B052"

StdError_B055 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(15:21)])
StdError_B055$Species <- "B055"

StdError_B068 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B068, ES = "LRRi")
StdError_B068$Ratio <- log(Ratios_NAT$B068[c(15:21)])
StdError_B068$Species <- "B068"

StdError_B074 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B074, ES = "LRRi")
StdError_B074$Ratio <- log(Ratios_NAT$B074[c(15:21)])
StdError_B074$Species <- "B074"

StdError_B084 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B084, ES = "LRRi")
StdError_B084$Ratio <- log(Ratios_NAT$B084[c(15:21)])
StdError_B084$Species <- "B084"

StdError_B121 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B121, ES = "LRRi")
StdError_B121$Ratio <- log(Ratios_NAT$B121[c(15:21)])
StdError_B121$Species <- "B121"

StdError_B158 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B158, ES = "LRRi")
StdError_B158$Ratio <- log(Ratios_NAT$B158[c(15:21)])
StdError_B158$Species <- "B158"

StdError_B173 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B173, ES = "LRRi")
StdError_B173$Ratio <- log(Ratios_NAT$B173[c(15:21)])
StdError_B173$Species <- "B173"

StdError_B182 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B182, ES = "LRRi")
StdError_B182$Ratio <- log(Ratios_NAT$B182[c(15:21)])
StdError_B182$Species <- "B182"

StdError_B186 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B186, ES = "LRRi")
StdError_B186$Ratio <- log(Ratios_NAT$B186[c(15:21)])
StdError_B186$Species <- "B186"

StdError_B187 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B187, ES = "LRRi")
StdError_B187$Ratio <- log(Ratios_NAT$B187[c(15:21)])
StdError_B187$Species <- "B187"

StdError_B273 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B273, ES = "LRRi")
StdError_B273$Ratio <- log(Ratios_NAT$B273[c(15:21)])
StdError_B273$Species <- "B273"

StdError_B283 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B283, ES = "LRRi")
StdError_B283$Ratio <- log(Ratios_NAT$B283[c(15:21)])
StdError_B283$Species <- "B283"

StdError_B284 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B284, ES = "LRRi")
StdError_B284$Ratio <- log(Ratios_NAT$B284[c(15:21)])
StdError_B284$Species <- "B284"

StdError_B321 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B321, ES = "LRRi")
StdError_B321$Ratio <- log(Ratios_NAT$B321[c(15:21)])
StdError_B321$Species <- "B321"

StdError_B451 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B451, ES = "LRRi")
StdError_B451$Ratio <- log(Ratios_NAT$B451[c(15:21)])
StdError_B451$Species <- "B451"

StdError_B454 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B454, ES = "LRRi")
StdError_B454$Ratio <- log(Ratios_NAT$B454[c(15:21)])
StdError_B454$Species <- "B454"

StdError_B478 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B478, ES = "LRRi")
StdError_B478$Ratio <- log(Ratios_NAT$B478[c(15:21)])
StdError_B478$Species <- "B478"

StdError_B491 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = B491, ES = "LRRi")
StdError_B491$Ratio <- log(Ratios_NAT$B491[c(15:21)])
StdError_B491$Species <- "B491"

StdError_G010 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(15:21)])
StdError_G010$Species <- "G010"

StdError_G013 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G013, ES = "LRRi")
StdError_G013$Ratio <- log(Ratios_NAT$G013[c(15:21)])
StdError_G013$Species <- "G013"

StdError_G015 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G015, ES = "LRRi")
StdError_G015$Ratio <- log(Ratios_NAT$G015[c(15:21)])
StdError_G015$Species <- "G015"

StdError_G017 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(15:21)])
StdError_G017$Species <- "G017"

StdError_G027 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G027, ES = "LRRi")
StdError_G027$Ratio <- log(Ratios_NAT$G027[c(15:21)])
StdError_G027$Species <- "G027"

StdError_G045 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G045, ES = "LRRi")
StdError_G045$Ratio <- log(Ratios_NAT$G045[c(15:21)])
StdError_G045$Species <- "G045"

StdError_G047 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G047, ES = "LRRi")
StdError_G047$Ratio <- log(Ratios_NAT$G047[c(15:21)])
StdError_G047$Species <- "G047"

StdError_G053 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G053, ES = "LRRi")
StdError_G053$Ratio <- log(Ratios_NAT$G053[c(15:21)])
StdError_G053$Species <- "G053"

StdError_G064 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G064, ES = "LRRi")
StdError_G064$Ratio <- log(Ratios_NAT$G064[c(15:21)])
StdError_G064$Species <- "G064"

StdError_G066 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G066, ES = "LRRi")
StdError_G066$Ratio <- log(Ratios_NAT$G066[c(15:21)])
StdError_G066$Species <- "G066"

StdError_G073 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G073, ES = "LRRi")
StdError_G073$Ratio <- log(Ratios_NAT$G073[c(15:21)])
StdError_G073$Species <- "G073"

StdError_G075 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G075, ES = "LRRi")
StdError_G075$Ratio <- log(Ratios_NAT$G075[c(15:21)])
StdError_G075$Species <- "G075"

StdError_G080 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G080, ES = "LRRi")
StdError_G080$Ratio <- log(Ratios_NAT$G080[c(15:21)])
StdError_G080$Species <- "G080"

StdError_G105 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G105, ES = "LRRi")
StdError_G105$Ratio <- log(Ratios_NAT$G105[c(15:21)])
StdError_G105$Species <- "G105"

StdError_G107 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G107, ES = "LRRi")
StdError_G107$Ratio <- log(Ratios_NAT$G107[c(15:21)])
StdError_G107$Species <- "G107"

StdError_G112 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G112, ES = "LRRi")
StdError_G112$Ratio <- log(Ratios_NAT$G112[c(15:21)])
StdError_G112$Species <- "G112"

StdError_G134 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G134, ES = "LRRi")
StdError_G134$Ratio <- log(Ratios_NAT$G134[c(15:21)])
StdError_G134$Species <- "G134"

StdError_G145 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G145, ES = "LRRi")
StdError_G145$Ratio <- log(Ratios_NAT$G145[c(15:21)])
StdError_G145$Species <- "G145"

StdError_G205 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G205, ES = "LRRi")
StdError_G205$Ratio <- log(Ratios_NAT$G205[c(15:21)])
StdError_G205$Species <- "G205"

StdError_G232 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G232, ES = "LRRi")
StdError_G232$Ratio <- log(Ratios_NAT$G232[c(15:21)])
StdError_G232$Species <- "G232"

StdError_G338 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G338, ES = "LRRi")
StdError_G338$Ratio <- log(Ratios_NAT$G338[c(15:21)])
StdError_G338$Species <- "G338"

StdError_G357 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G357, ES = "LRRi")
StdError_G357$Ratio <- log(Ratios_NAT$G357[c(15:21)])
StdError_G357$Species <- "G357"

StdError_G457 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G457, ES = "LRRi")
StdError_G457$Ratio <- log(Ratios_NAT$G457[c(15:21)])
StdError_G457$Species <- "G457"

StdError_G483 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G483, ES = "LRRi")
StdError_G483$Ratio <- log(Ratios_NAT$G483[c(15:21)])
StdError_G483$Species <- "G483"

StdError_G514 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G514, ES = "LRRi")
StdError_G514$Ratio <- log(Ratios_NAT$G514[c(15:21)])
StdError_G514$Species <- "G514"

StdError_G549 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G549, ES = "LRRi")
StdError_G549$Ratio <- log(Ratios_NAT$G549[c(15:21)])
StdError_G549$Species <- "G549"

StdError_G673 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G673, ES = "LRRi")
StdError_G673$Ratio <- log(Ratios_NAT$G673[c(15:21)])
StdError_G673$Species <- "G673"

StdError_G699 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G699, ES = "LRRi")
StdError_G699$Ratio <- log(Ratios_NAT$G699[c(15:21)])
StdError_G699$Species <- "G699"

StdError_G705 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G705, ES = "LRRi")
StdError_G705$Ratio <- log(Ratios_NAT$G705[c(15:21)])
StdError_G705$Species <- "G705"

StdError_G707 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G707, ES = "LRRi")
StdError_G707$Ratio <- log(Ratios_NAT$G707[c(15:21)])
StdError_G707$Species <- "G707"

StdError_G711 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G711, ES = "LRRi")
StdError_G711$Ratio <- log(Ratios_NAT$G711[c(15:21)])
StdError_G711$Species <- "G711"

StdError_G714 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G714, ES = "LRRi")
StdError_G714$Ratio <- log(Ratios_NAT$G714[c(15:21)])
StdError_G714$Species <- "G714"

StdError_G717 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G717, ES = "LRRi")
StdError_G717$Ratio <- log(Ratios_NAT$G717[c(15:21)])
StdError_G717$Species <- "G717"

StdError_G724 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G724, ES = "LRRi")
StdError_G724$Ratio <- log(Ratios_NAT$G724[c(15:21)])
StdError_G724$Species <- "G724"

StdError_G913 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G913, ES = "LRRi")
StdError_G913$Ratio <- log(Ratios_NAT$G913[c(15:21)])
StdError_G913$Species <- "G913"

StdError_G916 <- batch_calc_ES(dat = StdError_DataNAT, grouping = eventYear, condition = eventSeason, 
                               outcome = G916, ES = "LRRi")
StdError_G916$Ratio <- log(Ratios_NAT$G916[c(15:21)])
StdError_G916$Species <- "G916"

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
Agan_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9)],StdError_NAT[c(1,4,7,8,9)])
