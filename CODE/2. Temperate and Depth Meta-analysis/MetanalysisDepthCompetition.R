##%######################################################%##
#                                                          #
####            Depth VS Comp Meta-analysis             ####
#                                                          #
##%######################################################%##
library(readr)
library(metafor)

All_Depth_Comp <- read_csv("output_data/All_Depth_Comp.csv", 
                                  col_types = cols(...1 = col_skip()))


All_Depth_Comp$Study <- factor(All_Depth_Comp$Study, 
                                         c( "LessepsianH", "CyprusWarm", "CyprusCool", "CyprusNorth","Crete"))

All_Depth_Comp$Depth <- as.numeric(All_Depth_Comp$Depth)

All_Depth_Comp$Ratios <- as.numeric(All_Depth_Comp$Ratios)

All_Depth_Comp$SE <- as.numeric(All_Depth_Comp$SE)

All_Depth_Comp$SpringAbundanceNIS <- as.numeric(All_Depth_Comp$SpringAbundanceNIS)

All_Depth_Comp$AutumnAbundanceNIS <- as.numeric(All_Depth_Comp$AutumnAbundanceNIS)

All_Depth_Comp$SE[All_Depth_Comp$SE == 0] <- 1.33

###The role of competition for native species

MolluscDepthAbundance_NAT <- All_Depth_Comp[which(All_Depth_Comp$Invasive  =='NAT'),]

Depth_Native<- rma.mv(Ratios ~  Depth*SpringAbundanceNIS,random = list(~ 1 | Study, ~ 1 | Species) , 
                      SE^2, data=MolluscDepthAbundance_NAT)
summary(Depth_Native)

