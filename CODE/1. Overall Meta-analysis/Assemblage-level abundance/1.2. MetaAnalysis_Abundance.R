##%######################################################%##
#                                                          #
####             Overall Meta-analysis                  ####
#                                                          #
##%######################################################%##
library(readr)
MolluscDataAbundance_Comm <- read_csv("output_data/MolluscDataAbundance_Comm.csv", 
                                      col_types = cols(...1 = col_skip()))
FishDataAbundance_Comm <- read_csv("output_data/FishDataAbundance_Comm.csv", 
                                      col_types = cols(...1 = col_skip()))

All_AbundanceComm <- rbind(MolluscDataAbundance_Comm, FishDataAbundance_Comm)
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
                                     c( "AGAN", "Soreq", "ViaMaris", "LessepsianH","Cyprus_North", "Cyprus_Warm", "Cyprus_Refuge","Crete",
                                        "DivingTransects", "BRUV", "Trawling_40", "Trawling_100" ))



All_AbundanceComm$Temp <- as.numeric(All_AbundanceComm$Temp)

###Fig Native VS NIS
Study <- rma.mv(Ratio ~  Invasive + Study + Habitat, SE^2, data=All_AbundanceComm)
summary(Study)

ModelPrediction <- predict(Study) 
Modeloutput <- Study[["data"]]
Modeloutput$Pred <- ModelPrediction[["pred"]]
Modeloutput$ci.l <- ModelPrediction[["ci.lb"]]
Modeloutput$ci.u <- ModelPrediction[["ci.ub"]]


