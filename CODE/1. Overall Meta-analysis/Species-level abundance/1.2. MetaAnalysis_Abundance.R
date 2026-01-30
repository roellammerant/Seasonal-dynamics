##%######################################################%##
#                                                          #
####             Overall Meta-analysis                  ####
#                                                          #
##%######################################################%##
library(readr)
library(metafor)

MolluscSpAbundance <- read_csv("output_data/MolluscDataSpAbundance.csv", 
                                      col_types = cols(...1 = col_skip()))
FishDataSpAbundance <- read_csv("output_data/FishDataSpAbundance.csv", 
                                   col_types = cols(...1 = col_skip()))

MolluscSpAbundance <- MolluscSpAbundance[, -which(names(MolluscSpAbundance) == "Species")]

All_SpAbundance <- rbind(MolluscSpAbundance, FishDataSpAbundance)


All_SpAbundance$Habitat <- factor(All_SpAbundance$Habitat, 
                             c( "Soft-bottom", "Hard-bottom", "Posidonia meadow"))

All_SpAbundance$Organism <- factor(All_SpAbundance$Organism,
                              c( "Mollusc", "Fish"))


All_SpAbundance$Invasive <- factor(All_SpAbundance$Invasive, 
                              c( "NAT", "NIS"))

All_SpAbundance$Location <- factor(All_SpAbundance$Location, 
                              c( "Israeli Shelf", "Cape Greco", "Akrotori", "Cyprus","Crete"))


All_SpAbundance$Temp <- as.numeric(All_SpAbundance$Temp)

# standard error of zero -> only one datapoint
# set it equal to the highest SE in the dataset, to give it lower weight
All_SpAbundance$SE[All_SpAbundance$SE == 0] <- 1.4

### to get all the NIS and native points calculated for each study
Study <- rma.mv(Ratio ~  Invasive + Study + Habitat, SE^2, data=All_SpAbundance)
summary(Study)

ModelPrediction <- predict(Study) 
Modeloutput <- Study[["data"]]
Modeloutput$Pred <- ModelPrediction[["pred"]]
Modeloutput$ci.l <- ModelPrediction[["ci.lb"]]
Modeloutput$ci.u <- ModelPrediction[["ci.ub"]]





