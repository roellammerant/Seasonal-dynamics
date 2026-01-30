##%######################################################%##
#                                                          #
####             Overall Meta-analysis                  ####
#                                                          #
##%######################################################%##
library(readr)
MolluscDataSpRich <- read_csv("output_data/MolluscDataSpRich.csv", 
                              col_types = cols(...1 = col_skip()))
FishDataSpRich <- read_csv("output_data/FishDataSpRich.csv", 
                              col_types = cols(...1 = col_skip()))

All_SpRich <- rbind(MolluscDataSpRich, FishDataSpRich)

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

library(metafor)

Study <- rma.mv(`SpRich_S:A` ~  Invasive + Study + Habitat, SE^2, data=All_SpRich)
summary(Study)

ModelPrediction <- predict(Study) 
Modeloutput <- Study[["data"]]
Modeloutput$Pred <- ModelPrediction[["pred"]]
Modeloutput$ci.l <- ModelPrediction[["ci.lb"]]
Modeloutput$ci.u <- ModelPrediction[["ci.ub"]]


