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

### Mean values per Year*Season 
CreteNIS <- t(CreteNIS)
CreteNIS <- as.data.frame(CreteNIS)
CreteNIS <- CreteNIS[-c(25),]
CreteNIS <- CreteNIS %>% 
  mutate_if(is.character, as.numeric)
CreteNIS$Merched_Category <- Crete$Merched_Replicates
Mean_NIS <- aggregate(CreteNIS[c(1:4)], 
                      list(CreteNIS$Merched_Category), FUN=mean) 

CreteNAT <- t(CreteNAT)
CreteNAT <- as.data.frame(CreteNAT)
CreteNAT <- CreteNAT[-c(25),]
CreteNAT <- CreteNAT %>% 
  mutate_if(is.character, as.numeric)
CreteNAT$Merched_Category <- Crete$Merched_Replicates
Mean_NAT <- aggregate(CreteNAT[c(1:102)], 
                      list(CreteNAT$Merched_Category), FUN=mean) 

### Calculate Ratios
Ratios_NIS <- as.data.frame(t(Mean_NIS))
colnames(Ratios_NIS) <- Ratios_NIS[1,]
Ratios_NIS<- Ratios_NIS[-c(1),]
Ratios_NIS <- Ratios_NIS %>% 
  mutate_if(is.character, as.numeric)
Ratios_NIS[Ratios_NIS == 0] <- 0.001
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
Ratios_NAT[Ratios_NAT == 0] <- 0.001
Ratios_NAT$Ratio10 <-Ratios_NAT$`10_Autumn`/Ratios_NAT$`10_Spring`
Ratios_NAT$Ratio15 <-Ratios_NAT$`15_Autumn`/Ratios_NAT$`15_Spring`
Ratios_NAT$Ratio20 <-Ratios_NAT$`20_Autumn`/Ratios_NAT$`20_Spring`
Ratios_NAT$Ratio5 <- Ratios_NAT$`5_Autumn`/Ratios_NAT$`5_Spring`
Ratios_NAT <- as.data.frame(t(Ratios_NAT))

### StdError values for NIS ratios
CreteNIS$Depth <- Crete$Depth
CreteNIS$Season <-  Crete$Season
CreteNIS[CreteNIS == 0] <- 0.1

StdError_B078 <- batch_calc_ES(dat = CreteNIS, grouping = Depth, condition = Season, 
                               outcome = B078, ES = "LRRi")
StdError_B078$Ratio <- log(Ratios_NIS$B078[c(9:12)])
StdError_B078$Species <- "B078"

StdError_B132 <- batch_calc_ES(dat = CreteNIS, grouping = Depth, condition = Season, 
                               outcome = B132, ES = "LRRi")
StdError_B132$Ratio <- log(Ratios_NIS$B132[c(9:12)])
StdError_B132$Species <- "B132"

StdError_B464 <- batch_calc_ES(dat = CreteNIS, grouping = Depth, condition = Season, 
                               outcome = B464, ES = "LRRi")
StdError_B464$Ratio <- log(Ratios_NIS$B464[c(9:12)])
StdError_B464$Species <- "B464"

StdError_G352 <- batch_calc_ES(dat = CreteNIS, grouping = Depth, condition = Season, 
                               outcome = G352, ES = "LRRi")
StdError_G352 $Ratio <- log(Ratios_NIS$G352[c(9:12)])
StdError_G352$Species <- "G352"

StdError_NIS<- rbind(StdError_B078,StdError_B132,StdError_B464,StdError_G352)
StdError_NIS$Invasive <- "NIS"
StdError_NIS <- StdError_NIS[StdError_NIS$Ratio != c('0'),]

### StdError values for NAT ratios
CreteNAT$Depth <- Crete$Depth
CreteNAT$Season <-  Crete$Season
CreteNAT[CreteNAT == 0] <- 0.1

StdError_B005 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B005, ES = "LRRi")
StdError_B005$Ratio <- log(Ratios_NAT$B005[c(9:12)])
StdError_B005$Species <- "B005"

StdError_B018 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B018, ES = "LRRi")
StdError_B018$Ratio <- log(Ratios_NAT$B018[c(9:12)])
StdError_B018$Species <- "B018"

StdError_B045 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B045, ES = "LRRi")
StdError_B045$Ratio <- log(Ratios_NAT$B045[c(9:12)])
StdError_B045$Species <- "B045"

StdError_B049 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B049, ES = "LRRi")
StdError_B049$Ratio <- log(Ratios_NAT$B049[c(9:12)])
StdError_B049$Species <- "B049"

StdError_B055 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B055, ES = "LRRi")
StdError_B055$Ratio <- log(Ratios_NAT$B055[c(9:12)])
StdError_B055$Species <- "B055"

StdError_B060 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B060, ES = "LRRi")
StdError_B060$Ratio <- log(Ratios_NAT$B060[c(9:12)])
StdError_B060$Species <- "B060"

StdError_B068 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B068, ES = "LRRi")
StdError_B068$Ratio <- log(Ratios_NAT$B068[c(9:12)])
StdError_B068$Species <- "B068"

StdError_B088 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B088, ES = "LRRi")
StdError_B088$Ratio <- log(Ratios_NAT$B088[c(9:12)])
StdError_B088$Species <- "B088"

StdError_B125 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B125, ES = "LRRi")
StdError_B125$Ratio <- log(Ratios_NAT$B138[c(9:12)])
StdError_B125$Species <- "B125"

StdError_B138 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B138, ES = "LRRi")
StdError_B138$Ratio <- log(Ratios_NAT$B138[c(9:12)])
StdError_B138$Species <- "B138"

StdError_B139 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B139, ES = "LRRi")
StdError_B139$Ratio <- log(Ratios_NAT$B139[c(9:12)])
StdError_B139$Species <- "B139"

StdError_B140 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B140, ES = "LRRi")
StdError_B140$Ratio <- log(Ratios_NAT$B140[c(9:12)])
StdError_B140$Species <- "B140"

StdError_B141 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B141, ES = "LRRi")
StdError_B141$Ratio <- log(Ratios_NAT$B141[c(9:12)])
StdError_B141$Species <- "B141"

StdError_B142 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B142, ES = "LRRi")
StdError_B142$Ratio <- log(Ratios_NAT$B142[c(9:12)])
StdError_B142$Species <- "B142"

StdError_B143 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B143, ES = "LRRi")
StdError_B143$Ratio <- log(Ratios_NAT$B143[c(9:12)])
StdError_B143$Species <- "B143"

StdError_B144 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B144, ES = "LRRi")
StdError_B144$Ratio <- log(Ratios_NAT$B144[c(9:12)])
StdError_B144$Species <- "B144"

StdError_B147 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B147, ES = "LRRi")
StdError_B147$Ratio <- log(Ratios_NAT$B147[c(9:12)])
StdError_B147$Species <- "B147"

StdError_B150 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B150, ES = "LRRi")
StdError_B150$Ratio <- log(Ratios_NAT$B150[c(9:12)])
StdError_B150$Species <- "B150"

StdError_B152 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B152, ES = "LRRi")
StdError_B152$Ratio <- log(Ratios_NAT$B152[c(9:12)])
StdError_B152$Species <- "B152"

StdError_B158 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B158, ES = "LRRi")
StdError_B158$Ratio <- log(Ratios_NAT$B158[c(9:12)])
StdError_B158$Species <- "B158"

StdError_B161 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B161, ES = "LRRi")
StdError_B161$Ratio <- log(Ratios_NAT$B161[c(9:12)])
StdError_B161$Species <- "B161"

StdError_B167 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B167, ES = "LRRi")
StdError_B167$Ratio <- log(Ratios_NAT$B167[c(9:12)])
StdError_B167$Species <- "B167"

StdError_B181 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B181, ES = "LRRi")
StdError_B181$Ratio <- log(Ratios_NAT$B181[c(9:12)])
StdError_B181$Species <- "B181"

StdError_B188 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B188, ES = "LRRi")
StdError_B188$Ratio <- log(Ratios_NAT$B188[c(9:12)])
StdError_B188$Species <- "B188"

StdError_B200 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B200, ES = "LRRi")
StdError_B200$Ratio <- log(Ratios_NAT$B200[c(9:12)])
StdError_B200$Species <- "B200"

StdError_B247 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = B247, ES = "LRRi")
StdError_B247$Ratio <- log(Ratios_NAT$B247[c(9:12)])
StdError_B247$Species <- "B247"

StdError_G010 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G010, ES = "LRRi")
StdError_G010$Ratio <- log(Ratios_NAT$G010[c(9:12)])
StdError_G010$Species <- "G010"

StdError_G017 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G017, ES = "LRRi")
StdError_G017$Ratio <- log(Ratios_NAT$G017[c(9:12)])
StdError_G017$Species <- "G017"

StdError_G021 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G021, ES = "LRRi")
StdError_G021$Ratio <- log(Ratios_NAT$G021[c(9:12)])
StdError_G021$Species <- "G021"

StdError_G026 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G026, ES = "LRRi")
StdError_G026$Ratio <- log(Ratios_NAT$G026[c(9:12)])
StdError_G026$Species <- "G026"

StdError_G037 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G037, ES = "LRRi")
StdError_G037$Ratio <- log(Ratios_NAT$G037[c(9:12)])
StdError_G037$Species <- "G037"

StdError_G044 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G044, ES = "LRRi")
StdError_G044$Ratio <- log(Ratios_NAT$G044[c(9:12)])
StdError_G044$Species <- "G044"

StdError_G046 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G046, ES = "LRRi")
StdError_G046$Ratio <- log(Ratios_NAT$G046[c(9:12)])
StdError_G046$Species <- "G046"

StdError_G047 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G047, ES = "LRRi")
StdError_G047$Ratio <- log(Ratios_NAT$G047[c(9:12)])
StdError_G047$Species <- "G047"

StdError_G050 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G050, ES = "LRRi")
StdError_G050$Ratio <- log(Ratios_NAT$G050[c(9:12)])
StdError_G050$Species <- "G050"

StdError_G100 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G100, ES = "LRRi")
StdError_G100$Ratio <- log(Ratios_NAT$G100[c(9:12)])
StdError_G100$Species <- "G100"

StdError_G112 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G112, ES = "LRRi")
StdError_G112$Ratio <- log(Ratios_NAT$G112[c(9:12)])
StdError_G112$Species <- "G112"

StdError_G140 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G140, ES = "LRRi")
StdError_G140$Ratio <- log(Ratios_NAT$G140[c(9:12)])
StdError_G140$Species <- "G140"

StdError_G152 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G152, ES = "LRRi")
StdError_G152$Ratio <- log(Ratios_NAT$G152[c(9:12)])
StdError_G152$Species <- "G152"

StdError_G153 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G153, ES = "LRRi")
StdError_G153$Ratio <- log(Ratios_NAT$G153[c(9:12)])
StdError_G153$Species <- "G153"

StdError_G154 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G154, ES = "LRRi")
StdError_G154$Ratio <- log(Ratios_NAT$G154[c(9:12)])
StdError_G154$Species <- "G154"

StdError_G155 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G155, ES = "LRRi")
StdError_G155$Ratio <- log(Ratios_NAT$G155[c(9:12)])
StdError_G155$Species <- "G155"

StdError_G158 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G158, ES = "LRRi")
StdError_G158$Ratio <- log(Ratios_NAT$G158[c(9:12)])
StdError_G158$Species <- "G158"

StdError_G159 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G159, ES = "LRRi")
StdError_G159$Ratio <- log(Ratios_NAT$G159[c(9:12)])
StdError_G159$Species <- "G159"

StdError_G165 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G165, ES = "LRRi")
StdError_G165$Ratio <- log(Ratios_NAT$G165[c(9:12)])
StdError_G165$Species <- "G165"

StdError_G168 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G168, ES = "LRRi")
StdError_G168$Ratio <- log(Ratios_NAT$G168[c(9:12)])
StdError_G168$Species <- "G168"

StdError_G169 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G169, ES = "LRRi")
StdError_G169$Ratio <- log(Ratios_NAT$G169[c(9:12)])
StdError_G169$Species <- "G169"

StdError_G170 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G170, ES = "LRRi")
StdError_G170$Ratio <- log(Ratios_NAT$G170[c(9:12)])
StdError_G170$Species <- "G170"

StdError_G172 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G172, ES = "LRRi")
StdError_G172$Ratio <- log(Ratios_NAT$G172[c(9:12)])
StdError_G172$Species <- "G172"

StdError_G173 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G173, ES = "LRRi")
StdError_G173$Ratio <- log(Ratios_NAT$G173[c(9:12)])
StdError_G173$Species <- "G173"

StdError_G174 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G174, ES = "LRRi")
StdError_G174$Ratio <- log(Ratios_NAT$G174[c(9:12)])
StdError_G174$Species <- "G174"

StdError_G180 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G180, ES = "LRRi")
StdError_G180$Ratio <- log(Ratios_NAT$G180[c(9:12)])
StdError_G180$Species <- "G180"

StdError_G183 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G183, ES = "LRRi")
StdError_G183$Ratio <- log(Ratios_NAT$G183[c(9:12)])
StdError_G183$Species <- "G183"

StdError_G184 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G184, ES = "LRRi")
StdError_G184$Ratio <- log(Ratios_NAT$G184[c(9:12)])
StdError_G184$Species <- "G184"

StdError_G187 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G187, ES = "LRRi")
StdError_G187$Ratio <- log(Ratios_NAT$G187[c(9:12)])
StdError_G187$Species <- "G187"

StdError_G188 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G188, ES = "LRRi")
StdError_G188$Ratio <- log(Ratios_NAT$G188[c(9:12)])
StdError_G188$Species <- "G188"

StdError_G194 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G194, ES = "LRRi")
StdError_G194$Ratio <- log(Ratios_NAT$G194[c(9:12)])
StdError_G194$Species <- "G194"

StdError_G196 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G196, ES = "LRRi")
StdError_G196$Ratio <- log(Ratios_NAT$G196[c(9:12)])
StdError_G196$Species <- "G196"

StdError_G197 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G197, ES = "LRRi")
StdError_G197$Ratio <- log(Ratios_NAT$G197[c(9:12)])
StdError_G197$Species <- "G197"

StdError_G199 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G199, ES = "LRRi")
StdError_G199$Ratio <- log(Ratios_NAT$G199[c(9:12)])
StdError_G199$Species <- "G199"

StdError_G201 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G201, ES = "LRRi")
StdError_G201$Ratio <- log(Ratios_NAT$G201[c(9:12)])
StdError_G201$Species <- "G201"

StdError_G202 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G202, ES = "LRRi")
StdError_G202$Ratio <- log(Ratios_NAT$G202[c(9:12)])
StdError_G202$Species <- "G202"

StdError_G208 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G208, ES = "LRRi")
StdError_G208$Ratio <- log(Ratios_NAT$G208[c(9:12)])
StdError_G208$Species <- "G208"

StdError_G215 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G215, ES = "LRRi")
StdError_G215$Ratio <- log(Ratios_NAT$G215[c(9:12)])
StdError_G215$Species <- "G215"

StdError_G216 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G216, ES = "LRRi")
StdError_G216$Ratio <- log(Ratios_NAT$G216[c(9:12)])
StdError_G216$Species <- "G216"

StdError_G219 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G219, ES = "LRRi")
StdError_G219$Ratio <- log(Ratios_NAT$G219[c(9:12)])
StdError_G219$Species <- "G219"

StdError_G255 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G255, ES = "LRRi")
StdError_G255$Ratio <- log(Ratios_NAT$G255[c(9:12)])
StdError_G255$Species <- "G255"

StdError_G282 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G282, ES = "LRRi")
StdError_G282$Ratio <- log(Ratios_NAT$G282[c(9:12)])
StdError_G282$Species <- "G282"

StdError_G287 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G287, ES = "LRRi")
StdError_G287$Ratio <- log(Ratios_NAT$G287[c(9:12)])
StdError_G287$Species <- "G287"

StdError_G288 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G288, ES = "LRRi")
StdError_G288$Ratio <- log(Ratios_NAT$G288[c(9:12)])
StdError_G288$Species <- "G288"

StdError_G289 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G289, ES = "LRRi")
StdError_G289$Ratio <- log(Ratios_NAT$G289[c(9:12)])
StdError_G289$Species <- "G289"

StdError_G290 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G290, ES = "LRRi")
StdError_G290$Ratio <- log(Ratios_NAT$G290[c(9:12)])
StdError_G290$Species <- "G290"

StdError_G291 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G291, ES = "LRRi")
StdError_G291$Ratio <- log(Ratios_NAT$G291[c(9:12)])
StdError_G291$Species <- "G291"

StdError_G294 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G294, ES = "LRRi")
StdError_G294$Ratio <- log(Ratios_NAT$G294[c(9:12)])
StdError_G294$Species <- "G294"

StdError_G295 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G295, ES = "LRRi")
StdError_G295$Ratio <- log(Ratios_NAT$G295[c(9:12)])
StdError_G295$Species <- "G295"

StdError_G304 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G304, ES = "LRRi")
StdError_G304$Ratio <- log(Ratios_NAT$G304[c(9:12)])
StdError_G304$Species <- "G304"

StdError_G305 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G305, ES = "LRRi")
StdError_G305$Ratio <- log(Ratios_NAT$G305[c(9:12)])
StdError_G305$Species <- "G305"

StdError_G332 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G332, ES = "LRRi")
StdError_G332$Ratio <- log(Ratios_NAT$G332[c(9:12)])
StdError_G332$Species <- "G332"

StdError_G339 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G339, ES = "LRRi")
StdError_G339$Ratio <- log(Ratios_NAT$G339[c(9:12)])
StdError_G339$Species <- "G339"

StdError_G340 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G340, ES = "LRRi")
StdError_G340$Ratio <- log(Ratios_NAT$G340[c(9:12)])
StdError_G340$Species <- "G340"

StdError_G345 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G345, ES = "LRRi")
StdError_G345$Ratio <- log(Ratios_NAT$G345[c(9:12)])
StdError_G345$Species <- "G345"

StdError_G347 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G347, ES = "LRRi")
StdError_G347$Ratio <- log(Ratios_NAT$G347[c(9:12)])
StdError_G347$Species <- "G347"

StdError_G349 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G349, ES = "LRRi")
StdError_G349$Ratio <- log(Ratios_NAT$G349[c(9:12)])
StdError_G349$Species <- "G349"

StdError_G350 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G350, ES = "LRRi")
StdError_G350$Ratio <- log(Ratios_NAT$G350[c(9:12)])
StdError_G350$Species <- "G350"

StdError_G351 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G351, ES = "LRRi")
StdError_G351$Ratio <- log(Ratios_NAT$G351[c(9:12)])
StdError_G351$Species <- "G351"

StdError_G353 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G353, ES = "LRRi")
StdError_G353$Ratio <- log(Ratios_NAT$G353[c(9:12)])
StdError_G353$Species <- "G353"

StdError_G357 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G357, ES = "LRRi")
StdError_G357$Ratio <- log(Ratios_NAT$G357[c(9:12)])
StdError_G357$Species <- "G357"

StdError_G360 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G360, ES = "LRRi")
StdError_G360$Ratio <- log(Ratios_NAT$G360[c(9:12)])
StdError_G360$Species <- "G360"

StdError_G362 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G362, ES = "LRRi")
StdError_G362$Ratio <- log(Ratios_NAT$G362[c(9:12)])
StdError_G362$Species <- "G362"

StdError_G363 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G363, ES = "LRRi")
StdError_G363$Ratio <- log(Ratios_NAT$G363[c(9:12)])
StdError_G363$Species <- "G363"

StdError_G364 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G364, ES = "LRRi")
StdError_G364$Ratio <- log(Ratios_NAT$G364[c(9:12)])
StdError_G364$Species <- "G364"

StdError_G367 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G367, ES = "LRRi")
StdError_G367$Ratio <- log(Ratios_NAT$G367[c(9:12)])
StdError_G367$Species <- "G367"

StdError_G422 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G422, ES = "LRRi")
StdError_G422$Ratio <- log(Ratios_NAT$G422[c(9:12)])
StdError_G422$Species <- "G422"

StdError_G445 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G445, ES = "LRRi")
StdError_G445$Ratio <- log(Ratios_NAT$G445[c(9:12)])
StdError_G445$Species <- "G445"

StdError_G446 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G446, ES = "LRRi")
StdError_G446$Ratio <- log(Ratios_NAT$G446[c(9:12)])
StdError_G446$Species <- "G446"

StdError_G706 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G706, ES = "LRRi")
StdError_G706$Ratio <- log(Ratios_NAT$G706[c(9:12)])
StdError_G706$Species <- "G706"

StdError_G809 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = G809, ES = "LRRi")
StdError_G809$Ratio <- log(Ratios_NAT$G809[c(9:12)])
StdError_G809$Species <- "G809"

StdError_P001 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = P001, ES = "LRRi")
StdError_P001$Ratio <- log(Ratios_NAT$P001[c(9:12)])
StdError_P001$Species <- "P001"

StdError_P002 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = P002, ES = "LRRi")
StdError_P002$Ratio <- log(Ratios_NAT$P002[c(9:12)])
StdError_P002$Species <- "P002"

StdError_P003 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = P003, ES = "LRRi")
StdError_P003$Ratio <- log(Ratios_NAT$P003[c(9:12)])
StdError_P003$Species <- "P003"

StdError_P004 <- batch_calc_ES(dat = CreteNAT, grouping = Depth, condition = Season, 
                               outcome = P004, ES = "LRRi")
StdError_P004$Ratio <- log(Ratios_NAT$P004[c(9:12)])
StdError_P004$Species <- "P004"


StdError_NAT<- rbind(StdError_B005,StdError_B018,StdError_B045,StdError_B049,StdError_B055,StdError_B060,StdError_B068,
                     StdError_B088,StdError_B125,StdError_B138,StdError_B139,StdError_B140,StdError_B141,StdError_B142,
                     StdError_B143,StdError_B144,StdError_B147,StdError_B150,StdError_B152,StdError_B158,
                     StdError_B161,StdError_B167,StdError_B181,StdError_B188,StdError_B200,StdError_B247,StdError_G010,
                     StdError_G017,StdError_G021,StdError_G026,StdError_G037,StdError_G044,StdError_G046,StdError_G047,
                     StdError_G050,StdError_G100,StdError_G112,StdError_G140,StdError_G152,StdError_G153,StdError_G154,
                     StdError_G155,StdError_G158,StdError_G159,StdError_G165,StdError_G168,StdError_G169,StdError_G170,
                     StdError_G172,StdError_G173,StdError_G174,StdError_G180,StdError_G183,StdError_G184,StdError_G187,
                     StdError_G188,StdError_G194,StdError_G196,StdError_G197,StdError_G199,StdError_G201,StdError_G202,
                     StdError_G208,StdError_G215,StdError_G216,StdError_G219,StdError_G255,StdError_G282,StdError_G287,
                     StdError_G288,StdError_G289,StdError_G290,StdError_G291,StdError_G194,StdError_G295,StdError_G304,
                     StdError_G305,StdError_G332,StdError_G339,StdError_G340,StdError_G345,StdError_G347,StdError_G349,
                     StdError_G350,StdError_G351,StdError_G353,StdError_G357,StdError_G360,StdError_G362,StdError_G363,
                     StdError_G364,StdError_G367,StdError_G422,StdError_G445,StdError_G446,StdError_G706,StdError_G809,
                     StdError_P001,StdError_P002,StdError_P003,StdError_P004)
StdError_NAT$Invasive <- "NAT"
StdError_NAT <- StdError_NAT[StdError_NAT$Ratio != c('0'),]

### Create dataframe
Crete_Species_Abundance <- rbind(StdError_NIS[c(1,4,7,8,9)],
                                 StdError_NAT[c(1,4,7,8,9)])

