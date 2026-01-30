##%######################################################%##
#                                                          #
####               Depth Meta-analysis                  ####
#                                                          #
##%######################################################%##
library(readr)
library(metafor)

MolluscDepthAbundance <- read_csv("output_data/MolluscDataDepthAbundance.csv", 
                               col_types = cols(...1 = col_skip()))


MolluscDepthAbundance$Habitat <- factor(MolluscDepthAbundance$Habitat, 
                                  c( "Soft-bottom", "Hard-bottom", "Posidonia meadow"))


MolluscDepthAbundance$Invasive <- factor(MolluscDepthAbundance$Invasive, 
                                   c( "NAT", "NIS"))

MolluscDepthAbundance$Location <- factor(MolluscDepthAbundance$Location, 
                                   c( "Israeli Shelf", "Cape Greco", "Akrotori", "Cyprus","Crete"))

MolluscDepthAbundance$Temp <- as.numeric(MolluscDepthAbundance$Temp)

MolluscDepthAbundance$Depth <- as.numeric(MolluscDepthAbundance$Depth)

MolluscDepthAbundance$SE[MolluscDepthAbundance$SE == 0] <- 1.33

###Fig Depth native versus NIS

MolluscDepthAbundance_NAT <- MolluscDepthAbundance[which(MolluscDepthAbundance$Invasive  =='NAT'),]
MolluscDepthAbundance_NIS <- MolluscDepthAbundance[which(MolluscDepthAbundance$Invasive  =='NIS'),]

Depth_Native<- rma.mv(Ratio ~  Depth ,random = list(~ 1 | Study, ~ 1 | Species) , 
                      SE^2, data=MolluscDepthAbundance_NAT)
summary(Depth_Native)

Depth_NIS<- rma.mv(Ratio ~ Depth , random = list(~ 1 | Study, ~ 1 | Species) 
                   , SE^2, data=MolluscDepthAbundance_NIS)
summary(Depth_NIS)

preds_Nat <- predict(Depth_Native)
preds_NIS <- predict(Depth_NIS)
PredictNIS_AbunDepth <- Depth_NIS$data
PredictNIS_AbunDepth$Pred <-preds_NIS$pred
PredictNIS_AbunDepth$ci.lb <-preds_NIS$ci.lb
PredictNIS_AbunDepth$ci.ub <-preds_NIS$ci.ub
PredictNAT_AbunDepth <- Depth_Native$data
PredictNAT_AbunDepth$Pred <-preds_Nat$pred
PredictNAT_AbunDepth$ci.lb <-preds_Nat$ci.lb
PredictNAT_AbunDepth$ci.ub <-preds_Nat$ci.ub

### Create dataframe for confidence intervals
ci.lb_NIS <- as.data.frame(table(PredictNIS_AbunDepth$Depth, PredictNIS_AbunDepth$ci.lb))
ci.lb_NIS <- ci.lb_NIS[ci.lb_NIS$Freq > 0,]
ci.ub_NIS <- as.data.frame(table(PredictNIS_AbunDepth$Depth, PredictNIS_AbunDepth$ci.ub))
ci.ub_NIS <- ci.ub_NIS[ci.ub_NIS$Freq > 0,]
CI_NIS <- cbind(ci.lb_NIS,ci.ub_NIS)
CI_NIS <- CI_NIS[c(1,2,5)]
colnames(CI_NIS) <- c("Depth","ci.lb","ci.ub")
CI_NIS$Depth <- as.numeric(as.character(CI_NIS$Depth))
CI_NIS$ci.lb <- as.numeric(as.character(CI_NIS$ci.lb))
CI_NIS$ci.ub <- as.numeric(as.character(CI_NIS$ci.ub))
CI_NIS <- CI_NIS[order(CI_NIS$Depth),]

ci.lb_NAT <- as.data.frame(table(PredictNAT_AbunDepth$Depth, PredictNAT_AbunDepth$ci.lb))
ci.lb_NAT <- ci.lb_NAT[ci.lb_NAT$Freq != c('0'),]
ci.ub_NAT <- as.data.frame(table(PredictNAT_AbunDepth$Depth, PredictNAT_AbunDepth$ci.ub))
ci.ub_NAT <- ci.ub_NAT[ci.ub_NAT$Freq != c('0'),]
CI_NAT <- cbind(ci.lb_NAT,ci.ub_NAT)
CI_NAT <- CI_NAT[c(1,2,5)]
colnames(CI_NAT) <- c("Depth","ci.lb","ci.ub")
CI_NAT$Depth <- as.numeric(as.character(CI_NIS$Depth))
CI_NAT$ci.lb <- as.numeric(as.character(CI_NAT$ci.lb))
CI_NAT$ci.ub <- as.numeric(as.character(CI_NAT$ci.ub))
CI_NAT <- CI_NAT[order(CI_NAT$Depth),]
