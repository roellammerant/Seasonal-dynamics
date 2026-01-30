##%######################################################%##
#                                                          #
####             Temperature Meta-analysis              ####
#                                                          #
##%######################################################%##
library(readr)
All_SpRich <- read_csv("output_data/MolluscDataSpRich.csv", 
                              col_types = cols(...1 = col_skip()))

All_SpRich$Habitat <- factor(All_SpRich$Habitat, 
                             c( "Soft-bottom", "Hard-bottom", "Posidonia meadow"))

All_SpRich$Organism <- factor(All_SpRich$Organism,
                              c( "Mollusc", "Fish"))


All_SpRich$Invasive <- factor(All_SpRich$Invasive, 
                              c( "NAT", "NIS"))

All_SpRich$Location <- factor(All_SpRich$Location, 
                              c( "Israeli Shelf", "Cape Greco", "Cyprus_North", "Akrotori","Crete"))

All_SpRich$Temp <- as.numeric(All_SpRich$Temp)
All_SpRich$`SpRich_S:A` <- as.numeric(All_SpRich$`SpRich_S:A`)
All_SpRich$SE <- as.numeric(All_SpRich$SE)

###Fig temp native versus NIS
All_SpRich_NAT <- All_SpRich[which(All_SpRich$Invasive  =='NAT'),]
All_SpRich_NIS <- All_SpRich[which(All_SpRich$Invasive  =='NIS'),]

Temp<- rma.mv(`SpRich_S:A` ~  Temp*Invasive, random = ~ 1 | Study , SE, data=All_SpRich)
summary(Temp)

Temp_Nat<- rma.mv(`SpRich_S:A` ~  Temp, random = ~ 1 | Study , SE, data=All_SpRich_NAT)
summary(Temp_Nat)

Temp_NIS<- rma.mv(`SpRich_S:A`~  Temp, random = ~ 1 | Study , SE, data=All_SpRich_NIS)
summary(Temp_NIS)

preds_Nat <- predict(Temp_Nat)
preds_NIS <- predict(Temp_NIS)
PredictNIS_SpRich <- Temp_NIS$data
PredictNIS_SpRich$Pred <-preds_NIS$pred
PredictNIS_SpRich$ci.lb <-preds_NIS$ci.lb
PredictNIS_SpRich$ci.ub <-preds_NIS$ci.ub
PredictNAT_SpRich <- Temp_Nat$data
PredictNAT_SpRich$Pred <-preds_Nat$pred
PredictNAT_SpRich$ci.lb <-preds_Nat$ci.lb
PredictNAT_SpRich$ci.ub <-preds_Nat$ci.ub

### Create dataframe for confidence intervals
ci.lb_NIS <- as.data.frame(table(PredictNIS_SpRich$Temp, PredictNIS_SpRich$ci.lb))
ci.lb_NIS <- ci.lb_NIS[ci.lb_NIS$Freq > 0,]
ci.lb_NIS <- ci.lb_NIS[order(ci.lb_NIS$Var1),]
ci.ub_NIS <- as.data.frame(table(PredictNIS_SpRich$Temp, PredictNIS_SpRich$ci.ub))
ci.ub_NIS <- ci.ub_NIS[ci.ub_NIS$Freq > 0,]
ci.ub_NIS <- ci.ub_NIS[order(ci.ub_NIS$Var1),]
CI_NIS_SpRich<- cbind(ci.lb_NIS,ci.ub_NIS)
CI_NIS_SpRich <- CI_NIS_SpRich[c(1,2,5)]
colnames(CI_NIS_SpRich) <- c("Temp","ci.lb","ci.ub")
CI_NIS_SpRich$Temp <- as.numeric(as.character(CI_NIS_SpRich$Temp))
CI_NIS_SpRich$ci.lb <- as.numeric(as.character(CI_NIS_SpRich$ci.lb))
CI_NIS_SpRich$ci.ub <- as.numeric(as.character(CI_NIS_SpRich$ci.ub))

ci.lb_NAT <- as.data.frame(table(PredictNAT_SpRich$Temp, PredictNAT_SpRich$ci.lb))
ci.lb_NAT <- ci.lb_NAT[ci.lb_NAT$Freq != c('0'),]
ci.lb_NAT <- ci.lb_NAT[order(ci.lb_NAT$Var1),]
ci.ub_NAT <- as.data.frame(table(PredictNAT_SpRich$Temp, PredictNAT_SpRich$ci.ub))
ci.ub_NAT <- ci.ub_NAT[ci.ub_NAT$Freq != c('0'),]
ci.ub_NAT <- ci.ub_NAT[order(ci.ub_NAT$Var1),]
CI_NAT_SpRich <- cbind(ci.lb_NAT,ci.ub_NAT)
CI_NAT_SpRich <- CI_NAT_SpRich[c(1,2,5)]
colnames(CI_NAT_SpRich) <- c("Temp","ci.lb","ci.ub")
CI_NAT_SpRich$Temp <- as.numeric(as.character(CI_NAT_SpRich$Temp))
CI_NAT_SpRich$ci.lb <- as.numeric(as.character(CI_NAT_SpRich$ci.lb))
CI_NAT_SpRich$ci.ub <- as.numeric(as.character(CI_NAT_SpRich$ci.ub))

