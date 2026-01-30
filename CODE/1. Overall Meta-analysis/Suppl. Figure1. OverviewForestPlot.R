##%######################################################%##
#                                                          #
####        Load R packages and set-up workspace        ####
#                                                          #
##%######################################################%##
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c('readxl', 'writexl', 'vioplot', 'iNEXT', 'reshape2', 'dplyr')
ipak(packages)

##%######################################################%##
#                                                          #
####           Load Abundance data and combine          ####
#                                                          #
##%######################################################%##
Crete <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                    sheet = "Crete", col_types = c("text","text", "text",
                                                   "numeric", "numeric","numeric"))

Cyprus_S <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                       sheet = "Cyprus_S", col_types = c("text","text", "text",
                                                         "numeric", "numeric","numeric"))

Cyprus_N <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                       sheet = "Cyprus_N", col_types = c("text","text", "text",
                                                         "numeric", "numeric","numeric"))

Cyprus_E <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                       sheet = "Cyprus_E", col_types = c("text","text", "text",
                                                         "numeric", "numeric","numeric"))


Israel_Mollusc <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                             sheet = "Israel_Mollusc", col_types = c("text","text", "text",
                                                                     "numeric", "numeric","numeric"))

Israel_Fish <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                          sheet = "Israel_Fish", col_types = c("text","text", "text",
                                                               "numeric", "numeric","numeric"))

Overall <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                      sheet = "Overall", col_types = c("text","text", "text",
                                                       "numeric", "numeric","numeric"))

SpeciesAbudance <- rbind(Overall[c(1,2),],Crete[c(1,2),],Cyprus_S[c(1:4),],
                         Cyprus_E[c(1:4),],Cyprus_N[c(1:4),],
                         Israel_Mollusc[c(1:8),],Israel_Fish[c(1:8),])
SpeciesAbudance$YPosition <-  c(66,65, # Overall
                                60,59, # Crete
                                56,55,52,51, # Cyprus_S
                                48,47,44,43, # Cyprus_E
                                40,39,36,35, # Cyprus_N
                                32,31,28,27,24,23, 20,19, # Mollusc Israel
                                14,13,10,9, 6,5, 2,1) # Fish Israel

CommAbundance <-rbind(Overall[c(3,4),],Crete[c(3,4),],Cyprus_S[c(5:8),],
                      Cyprus_E[c(5:8),],Cyprus_N[c(5:8),],
                      Israel_Mollusc[c(9:16),],Israel_Fish[c(9:16),])
CommAbundance$YPosition <-  c(66,65, # Overall
                              60,59, # Crete
                              56,55,52,51, # Cyprus_S
                              48,47,44,43, # Cyprus_E
                              40,39,36,35, # Cyprus_N
                              32,31,28,27,24,23, 20,19, # Mollusc Israel
                              14,13,10,9, 6,5, 2,1) # Fish Israel

SpRich <- rbind(Overall[c(5,6),],Crete[c(5,6),],Cyprus_S[c(9:12),],
                Cyprus_E[c(9:12),],Cyprus_N[c(9:12),],
                Israel_Mollusc[c(17:24),],Israel_Fish[c(17:24),])
SpRich$YPosition <-  c(66,65, # Overall
                       60,59, # Crete
                       56,55,52,51, # Cyprus_S
                       48,47,44,43, # Cyprus_E
                       40,39,36,35, # Cyprus_N
                       32,31,28,27,24,23, 20,19, # Mollusc Israel
                       14,13,10,9, 6,5, 2,1) # Fish Israel


##%######################################################%##
#                                                          #
####           Overview Forestplot figure S1            ####
#                                                          #
##%######################################################%##


png(
  "output_plot/Supplemental Figure1.jpg",
  width = 20,
  height = 15,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,3))
par(mar = c(4, 4, 4, 4))
par(cex.lab = 2, cex.axis = 3)

#### Species_Abundance plot

plot(SpeciesAbudance$YPosition ~ SpeciesAbudance$Pred, las = 1,
     ylim = c(0, 70),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3.5,
     pch = 19,cex.axis= 2.5, cex = 3.5,
     main="A. Species abundance ratio",
     col = c("indianred1","lightskyblue1"))


segments(SpeciesAbudance$Ci.l, SpeciesAbudance$YPosition, 
         SpeciesAbudance$Ci.b, SpeciesAbudance$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))


abline( v= 0,col='grey', lty=1, lwd=2) 

abline( h= 62,col='black', lty=2, lwd=2) 

abline( h= 17.5,col='black', lty=2, lwd=2) 

legend("topleft", legend = c("Non-indigenous", "Native"), 
       col=c("indianred1","lightskyblue1"),
       pch = 15, bty = "n", pt.cex = 3, cex = 3.5,  horiz = F)#

text(-5.5 ,59.5, expression(GR_Crete), font = 2, cex =2.75, col = "seagreen3")

text(-5.5 ,55.5, expression(CY_South), font = 2, cex =2.75, col = "seagreen3")
text(-5.5 ,51.5, expression(CY_South), font = 2, cex =2.75, col = "lightsteelblue4")

text(-5.5 ,47.5, expression(CY_East), font = 2, cex =2.75, col = "seagreen3")
text(-5.5 ,43.5, expression(CY_East), font = 2, cex =2.75, col = "lightsteelblue4")

text(-5.5 ,39.5, expression(CY_North), font = 2, cex =2.75, col = "seagreen3")
text(-5.5 ,35.5, expression(CY_North), font = 2, cex =2.75, col = "lightsteelblue4")

text(-5.5 ,31.5, expression(IS_Agan), font = 2, cex =2.75, col = "sandybrown")
text(-5.5 ,27.5, expression(IS_ViaMaris), font = 2, cex =2.75, col = "sandybrown")
text(-5.5 ,23.5, expression(IS_Soreq), font = 2, cex =2.75, col = "sandybrown")
text( 5.5 ,19.5, expression(IS_1), font = 2, cex =2.75, col = "lightsteelblue4")

text(-5.5 ,13.5, expression(IS_Diving), font = 2, cex =2.75, col = "lightsteelblue4")
text(-5.5 ,9.5, expression(IS_BRUV), font = 2, cex =2.75, col = "sandybrown")
text(-5.4 ,5.5, expression(IS_Trawl40), font = 2, cex =2.75, col = "sandybrown")
text(-5.4 ,1.5, expression(IS_Trawl_100), font = 2, cex =2.75, col = "sandybrown")

#### Comm_Abundance plot

plot(CommAbundance$YPosition ~ CommAbundance$Pred, las = 1,
     ylim = c(0, 70),
     xlim = c(-4, 4),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3.5,
     pch = 19,cex.axis= 2.5, cex = 3.5,
     main="B. Assemblage abundance ratio",
     col = c("indianred1","lightskyblue1"))


segments(CommAbundance$Ci.l, CommAbundance$YPosition, 
         CommAbundance$Ci.b, CommAbundance$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 

abline( h= 62,col='black', lty=2, lwd=2) 

abline( h= 17.5,col='black', lty=2, lwd=2) 

legend("topleft", legend = c("Soft bottom", "Hard bottom", "Seagrass"), 
       col=c("sandybrown","lightsteelblue4", "seagreen3"),
       pch = 15, bty = "n", pt.cex = 3, cex = 3.5,  horiz = F)

#### Comm_SpRich plot

plot(SpRich$YPosition ~ SpRich$Pred, las = 1,
     ylim = c(0, 70),
     xlim = c(-4, 4),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3.5,
     pch = 19,cex.axis= 2.5, cex = 3.5,
     main="C. Species richness ratio",
     col = c("indianred1","lightskyblue1"))


segments(SpRich$Ci.l, SpRich$YPosition, 
         SpRich$Ci.b, SpRich$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 

abline( h= 62,col='black', lty=2, lwd=2) 

abline( h= 17.5,col='black', lty=2, lwd=2) 


dev.off()


