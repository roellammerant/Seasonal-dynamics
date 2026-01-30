##%######################################################%##
#                                                          #
####           Load Fish data and summarise             ####
#                                                          #
##%######################################################%##

#### BRUV
BRUV_meta <- BRUV_Sp_Abundance 
BRUV_meta$Study <- "BRUV"

#### Trawling40
Trawling40_meta <- Trawl40_Sp_Abundance
Trawling40_meta$Study <- "Trawling40"

#### Trawling100
Trawling100_meta <- Trawl100_Sp_Abundance
Trawling100_meta$Study <- "Trawling100"

#### DivingTransect
DivingTransect_meta <- Transect_Species_Abundance
DivingTransect_meta$Study <- "DivingTransect"

FishThermalAffinity <- rbind(BRUV_meta[-c(1)],Trawling40_meta[-c(1)],
                             Trawling100_meta[-c(1)], DivingTransect_meta[-c(1)])

FishThermalAffinity$TempPrefMax <- 
  as.numeric(as.character(FishThermalAffinity$TempPrefMax))
FishThermalAffinity$TempPrefMean <- 
  as.numeric(as.character(FishThermalAffinity$TempPrefMean))
FishThermalAffinity <- na.omit(FishThermalAffinity)

##%######################################################%##
#                                                          #
####        Load Mollusc data and summarise             ####
#                                                          #
##%######################################################%##

#### AGAN
AGAN_meta <- Agan_Species_Abundance 
AGAN_meta$Organism <- "Mollusc"
AGAN_meta$Habitat <- "Soft-bottom"
AGAN_meta$Study <- "AGAN"

#### ViaMaris
ViaMaris_meta <- ViaMaris_Species_Abundance
ViaMaris_meta$Organism <- "Mollusc"
ViaMaris_meta$Habitat <- "Soft-bottom"
ViaMaris_meta$Study <- "ViaMaris"

#### Soreq
Soreq_meta <- Soreq_Species_Abundance
Soreq_meta$Organism <- "Mollusc"
Soreq_meta$Habitat <- "Soft-bottom"
Soreq_meta$Study <- "Soreq"


#### LessepsianH
LessepsianH_meta <- LessepsianH_Species_Abundance
LessepsianH_meta$Organism <- "Mollusc"
LessepsianH_meta$Habitat <- "Hard-bottom"
LessepsianH_meta$Study <- "LessepsianH"


MolluscThermalAffinity <- rbind(AGAN_meta[-c(1)],ViaMaris_meta[-c(1)],Soreq_meta[-c(1)],
                                LessepsianH_meta[-c(1)])

MolluscThermalAffinity$TempPrefMax <- 
  as.numeric(as.character(MolluscThermalAffinity$TempPrefMax))
MolluscThermalAffinity$TempPrefMean <- 
  as.numeric(as.character(MolluscThermalAffinity$TempPrefMean))
MolluscThermalAffinity <- na.omit(MolluscThermalAffinity)

##%######################################################%##
#                                                          #
####        Meta-analysis Mollusc thermal affinity      ####
#                                                          #
##%######################################################%##
library(metafor)

MolluscThermalAffinity$SE[MolluscThermalAffinity$SE == 0] <- 1.31

###subset Native and  NIS response
ThermalNIS_Mollusc  <- MolluscThermalAffinity[which(MolluscThermalAffinity$Invasive =='NIS'),]
ThermalNAT_Mollusc  <- MolluscThermalAffinity[which(MolluscThermalAffinity$Invasive  =='NAT'),]

##NAT
Mollusc_NAT <- rma.mv(Ratio ~  TempPrefMax, random = ~ 1 | Study, SE^2, data=ThermalNAT_Mollusc)
summary(Mollusc_NAT)

ModelPrediction_MolluscNAT <- predict(Mollusc_NAT) 
Modeloutput_MolluscNAT <- Mollusc_NAT[["data"]]
Modeloutput_MolluscNAT$Pred <- ModelPrediction_MolluscNAT[["pred"]]
Modeloutput_MolluscNAT$ci.l <- ModelPrediction_MolluscNAT[["ci.lb"]]
Modeloutput_MolluscNAT$ci.u <- ModelPrediction_MolluscNAT[["ci.ub"]]
StandardErrorbars_MolluscNAT <- as.data.frame(table(Modeloutput_MolluscNAT$TempPrefMax,Modeloutput_MolluscNAT$ci.l,Modeloutput_MolluscNAT$ci.u))
StandardErrorbars_MolluscNAT <- StandardErrorbars_MolluscNAT[StandardErrorbars_MolluscNAT$Freq > 0,]
StandardErrorbars_MolluscNAT <- StandardErrorbars_MolluscNAT[c(1:3)]
colnames(StandardErrorbars_MolluscNAT) <- c("TempPrefMax","ci.lb","ci.ub")
StandardErrorbars_MolluscNAT$TempPrefMax <- as.numeric(as.character(StandardErrorbars_MolluscNAT$TempPrefMax))
StandardErrorbars_MolluscNAT$ci.lb <- as.numeric(as.character(StandardErrorbars_MolluscNAT$ci.lb))
StandardErrorbars_MolluscNAT$ci.ub <- as.numeric(as.character(StandardErrorbars_MolluscNAT$ci.ub))
StandardErrorbars_MolluscNAT <- StandardErrorbars_MolluscNAT[order(StandardErrorbars_MolluscNAT$TempPrefMax),]

##NIS
Mollusc_NIS <- rma.mv(Ratio ~  TempPrefMax, random = ~ 1 | Study, SE^2, data=ThermalNIS_Mollusc)
summary(Mollusc_NIS)

ModelPrediction_MolluscNIS <- predict(Mollusc_NIS) 
Modeloutput_MolluscNIS <- Mollusc_NIS[["data"]]
Modeloutput_MolluscNIS$Pred <- ModelPrediction_MolluscNIS[["pred"]]
Modeloutput_MolluscNIS$ci.l <- ModelPrediction_MolluscNIS[["ci.lb"]]
Modeloutput_MolluscNIS$ci.u <- ModelPrediction_MolluscNIS[["ci.ub"]]
StandardErrorbars_MolluscNIS <- as.data.frame(table(Modeloutput_MolluscNIS$TempPrefMax,Modeloutput_MolluscNIS$ci.l,Modeloutput_MolluscNIS$ci.u))
StandardErrorbars_MolluscNIS <- StandardErrorbars_MolluscNIS[StandardErrorbars_MolluscNIS$Freq > 0,]
StandardErrorbars_MolluscNIS <- StandardErrorbars_MolluscNIS[c(1:3)]
colnames(StandardErrorbars_MolluscNIS) <- c("TempPrefMax","ci.lb","ci.ub")
StandardErrorbars_MolluscNIS$TempPrefMax <- as.numeric(as.character(StandardErrorbars_MolluscNIS$TempPrefMax))
StandardErrorbars_MolluscNIS$ci.lb <- as.numeric(as.character(StandardErrorbars_MolluscNIS$ci.lb))
StandardErrorbars_MolluscNIS$ci.ub <- as.numeric(as.character(StandardErrorbars_MolluscNIS$ci.ub))
StandardErrorbars_MolluscNIS <- StandardErrorbars_MolluscNIS[order(StandardErrorbars_MolluscNIS$TempPrefMax),]


##%######################################################%##
#                                                          #
####        Meta-analysis Fish thermal affinity         ####
#                                                          #
##%######################################################%##

###Fig subset Native and  NIS response
ThermalNIS_Fish  <- FishThermalAffinity[which(FishThermalAffinity$Invasive =='NIS'),]
ThermalNAT_Fish  <- FishThermalAffinity[which(FishThermalAffinity$Invasive  =='NAT'),]

##NAT
Fish_NAT <- rma.mv(Ratio ~  TempPrefMax, random = ~ 1 | Study, SE^2, data=ThermalNAT_Fish)
summary(Fish_NAT)

ModelPrediction_FishNAT <- predict(Fish_NAT) 
Modeloutput_FishNAT <- Fish_NAT[["data"]]
Modeloutput_FishNAT$Pred <- ModelPrediction_FishNAT[["pred"]]
Modeloutput_FishNAT$ci.l <- ModelPrediction_FishNAT[["ci.lb"]]
Modeloutput_FishNAT$ci.u <- ModelPrediction_FishNAT[["ci.ub"]]
StandardErrorbars_FishNAT <- as.data.frame(table(Modeloutput_FishNAT$TempPrefMax,Modeloutput_FishNAT$ci.l,Modeloutput_FishNAT$ci.u))
StandardErrorbars_FishNAT <- StandardErrorbars_FishNAT[StandardErrorbars_FishNAT$Freq > 0,]
StandardErrorbars_FishNAT <- StandardErrorbars_FishNAT[c(1:3)]
colnames(StandardErrorbars_FishNAT) <- c("TempPrefMax","ci.lb","ci.ub")
StandardErrorbars_FishNAT$TempPrefMax <- as.numeric(as.character(StandardErrorbars_FishNAT$TempPrefMax))
StandardErrorbars_FishNAT$ci.lb <- as.numeric(as.character(StandardErrorbars_FishNAT$ci.lb))
StandardErrorbars_FishNAT$ci.ub <- as.numeric(as.character(StandardErrorbars_FishNAT$ci.ub))
StandardErrorbars_FishNAT <- StandardErrorbars_FishNAT[order(StandardErrorbars_FishNAT$TempPrefMax),]

##NIS
Fish_NIS <- rma.mv(Ratio ~  TempPrefMax, random = ~ 1 | Study, SE^2, data=ThermalNIS_Fish)
summary(Fish_NIS)

ModelPrediction_FishNIS <- predict(Fish_NIS) 
Modeloutput_FishNIS <- Fish_NIS[["data"]]
Modeloutput_FishNIS$Pred <- ModelPrediction_FishNIS[["pred"]]
Modeloutput_FishNIS$ci.l <- ModelPrediction_FishNIS[["ci.lb"]]
Modeloutput_FishNIS$ci.u <- ModelPrediction_FishNIS[["ci.ub"]]
StandardErrorbars_FishNIS <- as.data.frame(table(Modeloutput_FishNIS$TempPrefMax,Modeloutput_FishNIS$ci.l,Modeloutput_FishNIS$ci.u))
StandardErrorbars_FishNIS <- StandardErrorbars_FishNIS[StandardErrorbars_FishNIS$Freq > 0,]
StandardErrorbars_FishNIS <- StandardErrorbars_FishNIS[c(1:3)]
colnames(StandardErrorbars_FishNIS) <- c("TempPrefMax","ci.lb","ci.ub")
StandardErrorbars_FishNIS$TempPrefMax <- as.numeric(as.character(StandardErrorbars_FishNIS$TempPrefMax))
StandardErrorbars_FishNIS$ci.lb <- as.numeric(as.character(StandardErrorbars_FishNIS$ci.lb))
StandardErrorbars_FishNIS$ci.ub <- as.numeric(as.character(StandardErrorbars_FishNIS$ci.ub))
StandardErrorbars_FishNIS <- StandardErrorbars_FishNIS[order(StandardErrorbars_FishNIS$TempPrefMax),]
