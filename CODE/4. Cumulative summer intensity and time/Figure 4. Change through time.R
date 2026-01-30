##%######################################################%##
#                                                          #
####          Load Fish data and summarise              ####
#                                                          #
##%######################################################%##

SpAbundance_FishNAT <- Transect_Species_Abundance[which(Transect_Species_Abundance$Invasive  =='NAT'),]
SpAbundance_FishNAT$eventYear <- as.numeric(as.character(SpAbundance_FishNAT$eventYear))
SpAbundance_FishNIS <- Transect_Species_Abundance[which(Transect_Species_Abundance$Invasive  =='NIS'),]
SpAbundance_FishNIS$eventYear <- as.numeric(as.character(SpAbundance_FishNIS$eventYear))

library(metafor)


Year_FishNat<- rma.mv(Ratio ~  eventYear, random =  ~ 1 | Species,
                      SE^2, data=SpAbundance_FishNAT)
summary(Year_FishNat)

Year_FishNIS<- rma.mv(Ratio ~  eventYear, random =  ~ 1 | Species,
                      SE^2, data=SpAbundance_FishNIS)
summary(Year_FishNIS)

preds_FishNat <- predict(Year_FishNat)
preds_FishNIS <- predict(Year_FishNIS)
PredictNIS_FishAbun <- Year_FishNIS$data
PredictNIS_FishAbun$Pred <-preds_FishNIS$pred
PredictNIS_FishAbun$ci.lb <-preds_FishNIS$ci.lb
PredictNIS_FishAbun$ci.ub <-preds_FishNIS$ci.ub

PredictNAT_FishAbun <- Year_FishNat$data
PredictNAT_FishAbun$Pred <-preds_FishNat$pred
PredictNAT_FishAbun$ci.lb <-preds_FishNat$ci.lb
PredictNAT_FishAbun$ci.ub <-preds_FishNat$ci.ub


### Create dataframe for confidence intervals
ci.lb_FishNIS <- as.data.frame(table(PredictNIS_FishAbun$eventYear, PredictNIS_FishAbun$ci.lb))
ci.lb_FishNIS <- ci.lb_FishNIS[ci.lb_FishNIS$Freq > 0,]
ci.lb_FishNIS <- ci.lb_FishNIS[order(ci.lb_FishNIS$Var1, decreasing = TRUE),]
ci.ub_FishNIS <- as.data.frame(table(PredictNIS_FishAbun$eventYear, PredictNIS_FishAbun$ci.ub))
ci.ub_FishNIS <- ci.ub_FishNIS[ci.ub_FishNIS$Freq > 0,]
ci.ub_FishNIS <- ci.ub_FishNIS[order(ci.ub_FishNIS$Var1, decreasing = TRUE),]
CI_FishNIS <- cbind(ci.lb_FishNIS,ci.ub_FishNIS)
CI_FishNIS <- CI_FishNIS[c(1,2,5)]
colnames(CI_FishNIS) <- c("eventYear","ci.lb","ci.ub")
CI_FishNIS$eventYear <- as.numeric(as.character(CI_FishNIS$eventYear))
CI_FishNIS$ci.lb <- as.numeric(as.character(CI_FishNIS$ci.lb))
CI_FishNIS$ci.ub <- as.numeric(as.character(CI_FishNIS$ci.ub))
CI_FishNIS <- CI_FishNIS[order(CI_FishNIS$eventYear),]

ci.lb_FishNAT <- as.data.frame(table(PredictNAT_FishAbun$eventYear, PredictNAT_FishAbun$ci.lb))
ci.lb_FishNAT <- ci.lb_FishNAT[ci.lb_FishNAT$Freq != c('0'),]
ci.lb_FishNAT <- ci.lb_FishNAT[order(ci.lb_FishNAT$Var1, decreasing = TRUE),]
ci.ub_FishNAT <- as.data.frame(table(PredictNAT_FishAbun$eventYear, PredictNAT_FishAbun$ci.ub))
ci.ub_FishNAT <- ci.ub_FishNAT[ci.ub_FishNAT$Freq != c('0'),]
ci.ub_FishNAT <- ci.ub_FishNAT[order(ci.ub_FishNAT$Var1, decreasing = TRUE),]
CI_FishNAT <- cbind(ci.lb_FishNAT,ci.ub_FishNAT)
CI_FishNAT <- CI_FishNAT[c(1,2,5)]
colnames(CI_FishNAT) <- c("eventYear","ci.lb","ci.ub")
CI_FishNAT$eventYear <- as.numeric(as.character(CI_FishNAT$eventYear))
CI_FishNAT$ci.lb <- as.numeric(as.character(CI_FishNAT$ci.lb))
CI_FishNAT$ci.ub <- as.numeric(as.character(CI_FishNAT$ci.ub))
CI_FishNAT <- CI_FishNAT[order(CI_FishNAT$eventYear),]


##%######################################################%##
#                                                          #
####        Load Mollusc data and summarise             ####
#                                                          #
##%######################################################%##

#### AGAN
AGAN_meta <- Agan_Species_Abundance 
AGAN_meta$Study <- "AGAN"

#### ViaMaris
ViaMaris_meta <- ViaMaris_Species_Abundance
ViaMaris_meta$Study <- "ViaMaris"

#### Soreq
Soreq_meta <- Soreq_Species_Abundance
Soreq_meta$Study <- "Soreq"

MolluscSpAbundance <- rbind(AGAN_meta,ViaMaris_meta,Soreq_meta)
MolluscSpAbundance$SE[MolluscSpAbundance$SE == 0] <- 1.1

SpAbundance_MolluscNAT <- MolluscSpAbundance[which(MolluscSpAbundance$Invasive  =='NAT'),]
SpAbundance_MolluscNIS <- MolluscSpAbundance[which(MolluscSpAbundance$Invasive  =='NIS'),]

Year_MolluscNat<- rma.mv(Ratio ~  eventYear, random = list(~ 1 | Study, ~ 1 | Species),
                         SE^2, data=SpAbundance_MolluscNAT)
summary(Year_MolluscNat)

Year_MolluscNIS<- rma.mv(Ratio ~  eventYear, random =list(~ 1 | Study, ~ 1 | Species), 
                         SE^2, data=SpAbundance_MolluscNIS)
summary(Year_MolluscNIS)

preds_MolluscNat <- predict(Year_MolluscNat)
preds_MolluscNIS <- predict(Year_MolluscNIS)
PredictNIS_MolluscAbun <- Year_MolluscNIS$data
PredictNIS_MolluscAbun$Pred <-preds_MolluscNIS$pred
PredictNIS_MolluscAbun$ci.lb <-preds_MolluscNIS$ci.lb
PredictNIS_MolluscAbun$ci.ub <-preds_MolluscNIS$ci.ub
PredictNAT_MolluscAbun <- Year_MolluscNat$data
PredictNAT_MolluscAbun$Pred <-preds_MolluscNat$pred
PredictNAT_MolluscAbun$ci.lb <-preds_MolluscNat$ci.lb
PredictNAT_MolluscAbun$ci.ub <-preds_MolluscNat$ci.ub

### Create dataframe for confidence intervals
ci.lb_MolluscNIS <- as.data.frame(table(PredictNIS_MolluscAbun$eventYear, PredictNIS_MolluscAbun$ci.lb))
ci.lb_MolluscNIS <- ci.lb_MolluscNIS[ci.lb_MolluscNIS$Freq > 0,]
ci.ub_MolluscNIS <- as.data.frame(table(PredictNIS_MolluscAbun$eventYear, PredictNIS_MolluscAbun$ci.ub))
ci.ub_MolluscNIS <- ci.ub_MolluscNIS[ci.ub_MolluscNIS$Freq > 0,]
CI_MolluscNIS <- cbind(ci.lb_MolluscNIS,ci.ub_MolluscNIS)
CI_MolluscNIS <- CI_MolluscNIS[c(1,2,5)]
colnames(CI_MolluscNIS) <- c("eventYear","ci.lb","ci.ub")
CI_MolluscNIS$eventYear <- as.numeric(as.character(CI_MolluscNIS$eventYear))
CI_MolluscNIS$ci.lb <- as.numeric(as.character(CI_MolluscNIS$ci.lb))
CI_MolluscNIS$ci.ub <- as.numeric(as.character(CI_MolluscNIS$ci.ub))


ci.lb_MolluscNAT <- as.data.frame(table(PredictNAT_MolluscAbun$eventYear, PredictNAT_MolluscAbun$ci.lb))
ci.lb_MolluscNAT <- ci.lb_MolluscNAT[ci.lb_MolluscNAT$Freq != c('0'),]
ci.ub_MolluscNAT <- as.data.frame(table(PredictNAT_MolluscAbun$eventYear, PredictNAT_MolluscAbun$ci.ub))
ci.ub_MolluscNAT <- ci.ub_MolluscNAT[ci.ub_MolluscNAT$Freq != c('0'),]
CI_MolluscNAT <- cbind(ci.lb_MolluscNAT,ci.ub_MolluscNAT)
CI_MolluscNAT <- CI_MolluscNAT[c(1,2,5)]
colnames(CI_MolluscNAT) <- c("eventYear","ci.lb","ci.ub")
CI_MolluscNAT$eventYear <- as.numeric(as.character(CI_MolluscNAT$eventYear))
CI_MolluscNAT$ci.lb <- as.numeric(as.character(CI_MolluscNAT$ci.lb))
CI_MolluscNAT$ci.ub <- as.numeric(as.character(CI_MolluscNAT$ci.ub))

##%######################################################%##
#                                                          #
####     Load summer intensity data and summarise       ####
#                                                          #
##%######################################################%##

summer_c_int <- read.csv("~/RecruitmentMediterannean/DATA/summer_c_int.csv")

#### Mollusc

MolluscSummerIntensity <- merge(MolluscSpAbundance, summer_c_int, 
                                by.x = "eventYear", by.y = "year")

SummerIntensity_MolluscNAT <- MolluscSummerIntensity[which(MolluscSummerIntensity$Invasive  =='NAT'),]
SummerIntensity_MolluscNIS <- MolluscSummerIntensity[which(MolluscSummerIntensity$Invasive  =='NIS'),]

CIntensity_MolluscNat<- rma.mv(Ratio ~  c_intensity, random = list(~ 1 | Study, ~ 1 | Species),
                               SE^2, data=SummerIntensity_MolluscNAT)
summary(CIntensity_MolluscNat)

CIntensity_MolluscNIS<- rma.mv(Ratio ~  c_intensity, random = list(~ 1 | Study, ~ 1 | Species), 
                               SE^2, data=SummerIntensity_MolluscNIS)
summary(CIntensity_MolluscNIS)

preds_MolluscNat <- predict(CIntensity_MolluscNat)
preds_MolluscNIS <- predict(CIntensity_MolluscNIS)
PredictNIS_MolluscIntensity <- CIntensity_MolluscNIS$data
PredictNIS_MolluscIntensity$Pred <-preds_MolluscNIS$pred
PredictNIS_MolluscIntensity$ci.lb <-preds_MolluscNIS$ci.lb
PredictNIS_MolluscIntensity$ci.ub <-preds_MolluscNIS$ci.ub
PredictNAT_MolluscIntensity <- CIntensity_MolluscNat$data
PredictNAT_MolluscIntensity$Pred <-preds_MolluscNat$pred
PredictNAT_MolluscIntensity$ci.lb <-preds_MolluscNat$ci.lb
PredictNAT_MolluscIntensity$ci.ub <-preds_MolluscNat$ci.ub

### Create dataframe for confidence intervals
ci.lb_MolluscNIS_Int<- as.data.frame(table(PredictNIS_MolluscIntensity$c_intensity, PredictNIS_MolluscIntensity$ci.lb))
ci.lb_MolluscNIS_Int <- ci.lb_MolluscNIS_Int[ci.lb_MolluscNIS_Int$Freq > 0,]
ci.lb_MolluscNIS_Int <- ci.lb_MolluscNIS_Int[order(ci.lb_MolluscNIS_Int$Var1),]
ci.ub_MolluscNIS_Int <- as.data.frame(table(PredictNIS_MolluscIntensity$c_intensity, PredictNIS_MolluscIntensity$ci.ub))
ci.ub_MolluscNIS_Int <- ci.ub_MolluscNIS_Int[ci.ub_MolluscNIS_Int$Freq > 0,]
ci.ub_MolluscNIS_Int <- ci.ub_MolluscNIS_Int[order(ci.ub_MolluscNIS_Int$Var1),]
CI_MolluscNIS_Int <- cbind(ci.lb_MolluscNIS_Int,ci.ub_MolluscNIS_Int)
CI_MolluscNIS_Int <- CI_MolluscNIS_Int[c(1,2,5)]
colnames(CI_MolluscNIS_Int) <- c("c_intensity","ci.lb","ci.ub")
CI_MolluscNIS_Int$c_intensity <- as.numeric(as.character(CI_MolluscNIS_Int$c_intensity))
CI_MolluscNIS_Int$ci.lb <- as.numeric(as.character(CI_MolluscNIS_Int$ci.lb))
CI_MolluscNIS_Int$ci.ub <- as.numeric(as.character(CI_MolluscNIS_Int$ci.ub))
CI_MolluscNIS_Int <- CI_MolluscNIS_Int[order(CI_MolluscNIS_Int$c_intensity),]

ci.lb_MolluscNAT_Int <- as.data.frame(table(PredictNAT_MolluscIntensity$c_intensity, PredictNAT_MolluscIntensity$ci.lb))
ci.lb_MolluscNAT_Int <- ci.lb_MolluscNAT_Int[ci.lb_MolluscNAT_Int$Freq != c('0'),]
ci.lb_MolluscNAT_Int <- ci.lb_MolluscNAT_Int[order(ci.lb_MolluscNAT_Int$Var1),]
ci.ub_MolluscNAT_Int <- as.data.frame(table(PredictNAT_MolluscIntensity$c_intensity, PredictNAT_MolluscIntensity$ci.ub))
ci.ub_MolluscNAT_Int <- ci.ub_MolluscNAT_Int[ci.ub_MolluscNAT_Int$Freq != c('0'),]
ci.ub_MolluscNAT_Int <- ci.ub_MolluscNAT_Int[order(ci.ub_MolluscNAT_Int$Var1),]
CI_MolluscNAT_Int <- cbind(ci.lb_MolluscNAT_Int,ci.ub_MolluscNAT_Int)
CI_MolluscNAT_Int <- CI_MolluscNAT_Int[c(1,2,5)]
colnames(CI_MolluscNAT_Int) <- c("c_intensity","ci.lb","ci.ub")
CI_MolluscNAT_Int$c_intensity <- as.numeric(as.character(CI_MolluscNAT_Int$c_intensity))
CI_MolluscNAT_Int$ci.lb <- as.numeric(as.character(CI_MolluscNAT_Int$ci.lb))
CI_MolluscNAT_Int$ci.ub <- as.numeric(as.character(CI_MolluscNAT_Int$ci.ub))
CI_MolluscNAT_Int <- CI_MolluscNAT_Int[order(CI_MolluscNAT_Int$c_intensity),]

#### Fish

SummerIntensity_FishNAT <- Transect_Species_Abundance[which(Transect_Species_Abundance$Invasive  =='NAT'),]
SummerIntensity_FishNAT <- merge(SummerIntensity_FishNAT, summer_c_int, 
                                by.x = "eventYear", by.y = "year")
SummerIntensity_FishNAT$eventYear <- as.numeric(as.character(SummerIntensity_FishNAT$eventYear))

SummerIntensity_FishNIS <- Transect_Species_Abundance[which(Transect_Species_Abundance$Invasive  =='NIS'),]
SummerIntensity_FishNIS <- merge(SummerIntensity_FishNIS, summer_c_int, 
                                by.x = "eventYear", by.y = "year")
SummerIntensity_FishNIS$eventYear <- as.numeric(as.character(SummerIntensity_FishNIS$eventYear))

Cintensity_FishNat<- rma.mv(Ratio ~  c_intensity, random =  ~ 1 | Species,
                            SE^2, data=SummerIntensity_FishNAT)
summary(Cintensity_FishNat)

Cintensity_FishNIS<- rma.mv(Ratio ~  c_intensity, random =  ~ 1 | Species,
                            SE^2, data=SummerIntensity_FishNIS)
summary(Cintensity_FishNIS)

preds_FishNat <- predict(Cintensity_FishNat)
preds_FishNIS <- predict(Cintensity_FishNIS)
PredictNIS_FishInt <- Cintensity_FishNIS$data
PredictNIS_FishInt$Pred <-preds_FishNIS$pred
PredictNIS_FishInt$ci.lb <-preds_FishNIS$ci.lb
PredictNIS_FishInt$ci.ub <-preds_FishNIS$ci.ub

PredictNAT_FishInt <- Cintensity_FishNat$data
PredictNAT_FishInt$Pred <-preds_FishNat$pred
PredictNAT_FishInt$ci.lb <-preds_FishNat$ci.lb
PredictNAT_FishInt$ci.ub <-preds_FishNat$ci.ub


### Create dataframe for confidence intervals
ci.lb_FishNIS_Int <- as.data.frame(table(PredictNIS_FishInt$c_intensity, PredictNIS_FishInt$ci.lb))
ci.lb_FishNIS_Int <- ci.lb_FishNIS_Int[ci.lb_FishNIS_Int$Freq > 0,]
ci.lb_FishNIS_Int <- ci.lb_FishNIS_Int[order(ci.lb_FishNIS_Int$Var1, decreasing = TRUE),]
ci.ub_FishNIS_Int <- as.data.frame(table(PredictNIS_FishInt$c_intensity, PredictNIS_FishInt$ci.ub))
ci.ub_FishNIS_Int <- ci.ub_FishNIS_Int[ci.ub_FishNIS_Int$Freq > 0,]
ci.ub_FishNIS_Int <- ci.ub_FishNIS_Int[order(ci.ub_FishNIS_Int$Var1, decreasing = TRUE),]
CI_FishNIS_Int <- cbind(ci.lb_FishNIS_Int,ci.ub_FishNIS_Int)
CI_FishNIS_Int <- CI_FishNIS_Int[c(1,2,5)]
colnames(CI_FishNIS_Int) <- c("c_intensity","ci.lb","ci.ub")
CI_FishNIS_Int$c_intensity <- as.numeric(as.character(CI_FishNIS_Int$c_intensity))
CI_FishNIS_Int$ci.lb <- as.numeric(as.character(CI_FishNIS_Int$ci.lb))
CI_FishNIS_Int$ci.ub <- as.numeric(as.character(CI_FishNIS_Int$ci.ub))
CI_FishNIS_Int <- CI_FishNIS_Int[order(CI_FishNIS_Int$c_intensity),]

ci.lb_FishNAT_int <- as.data.frame(table(PredictNAT_FishInt$c_intensity, PredictNAT_FishInt$ci.lb))
ci.lb_FishNAT_int <- ci.lb_FishNAT_int[ci.lb_FishNAT_int$Freq != c('0'),]
ci.lb_FishNAT_int <- ci.lb_FishNAT_int[order(ci.lb_FishNAT_int$Var1, decreasing = TRUE),]
ci.ub_FishNAT_int <- as.data.frame(table(PredictNAT_FishInt$c_intensity, PredictNAT_FishInt$ci.ub))
ci.ub_FishNAT_int <- ci.ub_FishNAT_int[ci.ub_FishNAT_int$Freq != c('0'),]
ci.ub_FishNAT_int <- ci.ub_FishNAT_int[order(ci.ub_FishNAT_int$Var1, decreasing = TRUE),]
CI_FishNAT_int <- cbind(ci.lb_FishNAT_int,ci.ub_FishNAT_int)
CI_FishNAT_int <- CI_FishNAT_int[c(1,2,5)]
colnames(CI_FishNAT_int) <- c("c_intensity","ci.lb","ci.ub")
CI_FishNAT_int$c_intensity <- as.numeric(as.character(CI_FishNAT_int$c_intensity))
CI_FishNAT_int$ci.lb <- as.numeric(as.character(CI_FishNAT_int$ci.lb))
CI_FishNAT_int$ci.ub <- as.numeric(as.character(CI_FishNAT_int$ci.ub))
CI_FishNAT_int <- CI_FishNAT_int[order(CI_FishNAT_int$c_intensity),]


