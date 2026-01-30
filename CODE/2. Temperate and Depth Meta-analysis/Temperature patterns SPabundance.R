##%######################################################%##
#                                                          #
####             Temperature Meta-analysis              ####
#                                                          #
##%######################################################%##
library(readr)
library(metafor)

All_SpAbundance <- read_csv("output_data/MolluscDataSpAbundance.csv", 
                               col_types = cols(...1 = col_skip()))

All_SpAbundance$Habitat <- factor(All_SpAbundance$Habitat, 
                                  c( "Soft-bottom", "Hard-bottom", "Posidonia meadow"))

All_SpAbundance$Invasive <- factor(All_SpAbundance$Invasive, 
                                   c( "NAT", "NIS"))

All_SpAbundance$Location <- factor(All_SpAbundance$Location, 
                                   c( "Israeli Shelf", "Cape Greco", "Akrotori", "Cyprus","Crete"))


All_SpAbundance$Temp <- as.numeric(All_SpAbundance$Temp)

# standard error of zero -> only one datapoint
# set it equal to the highest SE in the dataset, to give it lower weight
All_SpAbundance$SE[All_SpAbundance$SE == 0] <- 1.4
All_SpAbundance <- All_SpAbundance[All_SpAbundance$Location!= "Crete",]
###Fig temp native versus NIS

All_SpAbundance_NAT <- All_SpAbundance[which(All_SpAbundance$Invasive  =='NAT'),]
All_SpAbundance_NIS <- All_SpAbundance[which(All_SpAbundance$Invasive  =='NIS'),]

Temp_Native<- rma.mv(Ratio ~  Temp , random = list(~ 1 | Study, ~ 1 | Species)
                     , SE^2, data=All_SpAbundance_NAT)
summary(Temp_Native)

Temp_NIS<- rma.mv(Ratio ~  Temp ,random = list(~ 1 | Study, ~ 1 | Species)
                  , SE^2, data=All_SpAbundance_NIS)
summary(Temp_NIS)

preds_Nat <- predict(Temp_Native)
preds_NIS <- predict(Temp_NIS)
PredictNIS_AbunSP <- Temp_NIS$data
PredictNIS_AbunSP$Pred <-preds_NIS$pred
PredictNIS_AbunSP$ci.lb <-preds_NIS$ci.lb
PredictNIS_AbunSP$ci.ub <-preds_NIS$ci.ub
PredictNAT_AbunSP <- Temp_Native$data
PredictNAT_AbunSP$Pred <-preds_Nat$pred
PredictNAT_AbunSP$ci.lb <-preds_Nat$ci.lb
PredictNAT_AbunSP$ci.ub <-preds_Nat$ci.ub


### Create dataframe for confidence intervals
ci.lb_NIS <- as.data.frame(table(PredictNIS_AbunSP$Temp, PredictNIS_AbunSP$ci.lb))
ci.lb_NIS <- ci.lb_NIS[ci.lb_NIS$Freq > 0,]
ci.lb_NIS <- ci.lb_NIS[order(ci.lb_NIS$Var1),]
ci.ub_NIS <- as.data.frame(table(PredictNIS_AbunSP$Temp, PredictNIS_AbunSP$ci.ub))
ci.ub_NIS <- ci.ub_NIS[ci.ub_NIS$Freq > 0,]
ci.ub_NIS <- ci.ub_NIS[order(ci.ub_NIS$Var1),]
CI_NIS_AbunSP<- cbind(ci.lb_NIS,ci.ub_NIS)
CI_NIS_AbunSP <- CI_NIS_AbunSP[c(1,2,5)]
colnames(CI_NIS_AbunSP) <- c("Temp","ci.lb","ci.ub")
CI_NIS_AbunSP$Temp <- as.numeric(as.character(CI_NIS_AbunSP$Temp))
CI_NIS_AbunSP$ci.lb <- as.numeric(as.character(CI_NIS_AbunSP$ci.lb))
CI_NIS_AbunSP$ci.ub <- as.numeric(as.character(CI_NIS_AbunSP$ci.ub))

ci.lb_NAT <- as.data.frame(table(PredictNAT_AbunSP$Temp, PredictNAT_AbunSP$ci.lb))
ci.lb_NAT <- ci.lb_NAT[ci.lb_NAT$Freq != c('0'),]
ci.lb_NAT <- ci.lb_NAT[order(ci.lb_NAT$Var1),]
ci.ub_NAT <- as.data.frame(table(PredictNAT_AbunSP$Temp, PredictNAT_AbunSP$ci.ub))
ci.ub_NAT <- ci.ub_NAT[ci.ub_NAT$Freq != c('0'),]
ci.ub_NAT <- ci.ub_NAT[order(ci.ub_NAT$Var1),]
CI_NAT_AbunSP <- cbind(ci.lb_NAT,ci.ub_NAT)
CI_NAT_AbunSP <- CI_NAT_AbunSP[c(1,2,5)]
colnames(CI_NAT_AbunSP) <- c("Temp","ci.lb","ci.ub")
CI_NAT_AbunSP$Temp <- as.numeric(as.character(CI_NAT_AbunSP$Temp))
CI_NAT_AbunSP$ci.lb <- as.numeric(as.character(CI_NAT_AbunSP$ci.lb))
CI_NAT_AbunSP$ci.ub <- as.numeric(as.character(CI_NAT_AbunSP$ci.ub))

