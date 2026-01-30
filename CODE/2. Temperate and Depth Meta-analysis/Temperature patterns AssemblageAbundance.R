##%######################################################%##
#                                                          #
####             Temperature Meta-analysis              ####
#                                                          #
##%######################################################%##
library(readr)
MolluscDataAbundance_Comm <- read_csv("output_data/MolluscDataAbundance_Comm.csv", 
                                      col_types = cols(...1 = col_skip()))
All_AbundanceComm <- MolluscDataAbundance_Comm

library(metafor)

All_AbundanceComm$Habitat <- factor(All_AbundanceComm$Habitat, 
                                    c( "Soft-bottom", "Hard-bottom", "Posidonia meadow"))

All_AbundanceComm$Invasive <- factor(All_AbundanceComm$Invasive, 
                                     c( "NAT", "NIS"))

All_AbundanceComm$Organism <- factor(All_AbundanceComm$Organism, 
                                     c( "Mollusc", "Fish"))

All_AbundanceComm$Location <- factor(All_AbundanceComm$Location, 
                                     c( "Israeli Shelf", "CapeGreco", "Cyprus_North", "Akrotori","Crete"))

All_AbundanceComm$Study <- factor(All_AbundanceComm$Study, 
                                  c( "AGAN", "Soreq", "ViaMaris", "LessepsianS","LessepsianH","Cyprus_North", "Cyprus_Warm", "Cyprus_Refuge","Crete",
                                     "DivingTransects", "BRUV", "Trawling_40", "Trawling_100" ))



All_AbundanceComm$Temp <- as.numeric(All_AbundanceComm$Temp)

All_AbundanceComm <- All_AbundanceComm[All_AbundanceComm$Location!= "Crete",]

###Fig temp native versus NIS

All_AbundanceComm_NAT <- All_AbundanceComm[which(All_AbundanceComm$Invasive  =='NAT'),]
All_AbundanceComm_NIS <- All_AbundanceComm[which(All_AbundanceComm$Invasive  =='NIS'),]

Temp<- rma.mv(Ratio ~  Temp*Invasive, random = ~ 1 | Study , SE^2, data=All_AbundanceComm)
summary(Temp)

Temp_Nat<- rma.mv(Ratio ~  Temp, random = ~ 1 | Study , SE^2, data=All_AbundanceComm_NAT)
summary(Temp_Nat)

Temp_NIS<- rma.mv(Ratio ~  Temp, random = ~ 1 | Study , SE^2, data=All_AbundanceComm_NIS)
summary(Temp_NIS)

preds_Nat <- predict(Temp_Nat)
preds_NIS <- predict(Temp_NIS)
PredictNIS_Abun <- Temp_NIS$data
PredictNIS_Abun$Pred <-preds_NIS$pred
PredictNIS_Abun$ci.lb <-preds_NIS$ci.lb
PredictNIS_Abun$ci.ub <-preds_NIS$ci.ub
PredictNAT_Abun <- Temp_Nat$data
PredictNAT_Abun$Pred <-preds_Nat$pred
PredictNAT_Abun$ci.lb <-preds_Nat$ci.lb
PredictNAT_Abun$ci.ub <-preds_Nat$ci.ub

### Create dataframe for confidence intervals
ci.lb_NIS <- as.data.frame(table(PredictNIS_Abun$Temp, PredictNIS_Abun$ci.lb))
ci.lb_NIS <- ci.lb_NIS[ci.lb_NIS$Freq > 0,]
ci.ub_NIS <- as.data.frame(table(PredictNIS_Abun$Temp, PredictNIS_Abun$ci.ub))
ci.ub_NIS <- ci.ub_NIS[ci.ub_NIS$Freq > 0,]
CI_NIS_Abun<- cbind(ci.lb_NIS,ci.ub_NIS)
CI_NIS_Abun <- CI_NIS_Abun[c(1,2,5)]
colnames(CI_NIS_Abun) <- c("Temp","ci.lb","ci.ub")
CI_NIS_Abun$Temp <- as.numeric(as.character(CI_NIS_Abun$Temp))
CI_NIS_Abun$ci.lb <- as.numeric(as.character(CI_NIS_Abun$ci.lb))
CI_NIS_Abun$ci.ub <- as.numeric(as.character(CI_NIS_Abun$ci.ub))

ci.lb_NAT <- as.data.frame(table(PredictNAT_Abun$Temp, PredictNAT_Abun$ci.lb))
ci.lb_NAT <- ci.lb_NAT[ci.lb_NAT$Freq != c('0'),]
ci.ub_NAT <- as.data.frame(table(PredictNAT_Abun$Temp, PredictNAT_Abun$ci.ub))
ci.ub_NAT <- ci.ub_NAT[ci.ub_NAT$Freq != c('0'),]
CI_NAT_Abun <- cbind(ci.lb_NAT,ci.ub_NAT)
CI_NAT_Abun <- CI_NAT_Abun[c(1,2,5)]
colnames(CI_NAT_Abun) <- c("Temp","ci.lb","ci.ub")
CI_NAT_Abun$Temp <- as.numeric(as.character(CI_NAT_Abun$Temp))
CI_NAT_Abun$ci.lb <- as.numeric(as.character(CI_NAT_Abun$ci.lb))
CI_NAT_Abun$ci.ub <- as.numeric(as.character(CI_NAT_Abun$ci.ub))

