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
CreteSP <- Crete[c(1,2),]
CreteSP$YPosition <-  c(2,1)

png(
  "output_plot/Figure1_CreteSubPlot.jpg",
  width = 8,
  height = 8,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,1))
par(mar = c(3, 3, 4, 2))
par(cex.lab = 2, cex.axis = 3)

plot(CreteSP$YPosition ~ CreteSP$Pred, las = 1,
     ylim = c(0, 5),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3,
     pch = 19,cex.axis= 2.5, cex = 5,
     main= "GR_Crete",
     col = c("indianred1","lightskyblue1"))


segments(CreteSP$Ci.l, CreteSP$YPosition, 
         CreteSP$Ci.b, CreteSP$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 


legend("topleft", legend = c("Non-indigenous", "Native"), 
       col=c("indianred1","lightskyblue1"),
       pch = 15, bty = "n", pt.cex = 3, cex = 3,  horiz = F)


dev.off()

Cyprus_S <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                       sheet = "Cyprus_S", col_types = c("text","text", "text",
                                                         "numeric", "numeric","numeric"))
CyprusSP_S <- Cyprus_S[c(1:4),]
CyprusSP_S$YPosition <-  c(4.5,3.5,2,1)


png(
  "output_plot/Figure1_CyprusSSubPlot.jpg",
  width = 8,
  height = 8,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,1))
par(mar = c(3, 3, 4, 2))
par(cex.lab = 2, cex.axis = 3)

plot(CyprusSP_S$YPosition ~ CyprusSP_S$Pred, las = 1,
     ylim = c(0, 6),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3,
     pch = 19,cex.axis= 2.5, cex = 5,
     main= "CY_South",
     col = c("indianred1","lightskyblue1"))


segments(CyprusSP_S$Ci.l, CyprusSP_S$YPosition, 
         CyprusSP_S$Ci.b, CyprusSP_S$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 



dev.off()

Cyprus_N <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                       sheet = "Cyprus_N", col_types = c("text","text", "text",
                                                         "numeric", "numeric","numeric"))
CyprusSP_N <- Cyprus_N[c(1:4),]
CyprusSP_N$YPosition <-  c(4.5,3.5,2,1)


png(
  "output_plot/Figure1_CyprusNSubPlot.jpg",
  width = 8,
  height = 8,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,1))
par(mar = c(3, 3, 4, 2))
par(cex.lab = 2, cex.axis = 3)

plot(CyprusSP_N$YPosition ~ CyprusSP_N$Pred, las = 1,
     ylim = c(0, 6),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3,
     pch = 19,cex.axis= 2.5, cex = 5,
     main= "CY_North",
     col = c("indianred1","lightskyblue1"))


segments(CyprusSP_N$Ci.l, CyprusSP_N$YPosition, 
         CyprusSP_N$Ci.b, CyprusSP_N$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 



dev.off()

Cyprus_E <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                       sheet = "Cyprus_E", col_types = c("text","text", "text",
                                                         "numeric", "numeric","numeric"))
CyprusSP_E <- Cyprus_E[c(1:4),]
CyprusSP_E$YPosition <-  c(4.5,3.5,2,1)


png(
  "output_plot/Figure1_CyprusESubPlot.jpg",
  width = 8,
  height = 8,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,1))
par(mar = c(3, 3, 4, 2))
par(cex.lab = 2, cex.axis = 3)

plot(CyprusSP_E$YPosition ~ CyprusSP_E$Pred, las = 1,
     ylim = c(0, 6),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3,
     pch = 19,cex.axis= 2.5, cex = 5,
     main= "CY_East",
     col = c("indianred1","lightskyblue1"))


segments(CyprusSP_E$Ci.l, CyprusSP_E$YPosition, 
         CyprusSP_E$Ci.b, CyprusSP_E$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 



dev.off()


##%######################################################%##
#                                                          #
####                      Israel plot                   ####
#                                                          #
##%######################################################%##
Israel_Mollusc <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                             sheet = "Israel_Mollusc", col_types = c("text","text", "text",
                                                                     "numeric", "numeric","numeric"))
Israel_Mollusc$YPosition <-  c(26,25,24,23,22,21,20,19,
                               17,16,15,14,13,12,11,10,
                               8,7,6,5,4,3,2,1)

Israel_MolluscSP <- Israel_Mollusc[c(1:8),]
Israel_MolluscSP$YPosition <-  c(8,7,6,5,4,3,2,1)

Israel_Fish <- read_excel("output_data/Meta-analysis_Summary.xlsx", 
                          sheet = "Israel_Fish", col_types = c("text","text", "text",
                                                               "numeric", "numeric","numeric"))
Israel_Fish$YPosition <-  c(26,25,24,23,22,21,20,19,
                            17,16,15,14,13,12,11,10,
                            8,7,6,5,4,3,2,1)

Israel_FishSP <- Israel_Fish[c(1:8),]
Israel_FishSP$YPosition <-  c(8,7,6,5,4,3,2,1)


png(
  "output_plot/Figure1_IsraelMolluscSubPlot.jpg",
  width = 8,
  height = 10,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,1))
par(mar = c(3, 3, 4, 2))
par(cex.lab = 2, cex.axis = 3)

plot(Israel_MolluscSP$YPosition ~ Israel_MolluscSP$Pred, las = 1,
     ylim = c(0, 9),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3,
     pch = 19,cex.axis= 2.5, cex = 3.5,
     main= "IS",
     col = c("indianred1","lightskyblue1"))


segments(Israel_MolluscSP$Ci.l, Israel_MolluscSP$YPosition, 
         Israel_MolluscSP$Ci.b, Israel_MolluscSP$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 

dev.off()


png(
  "output_plot/Figure1_IsraelFishSubPlot.jpg",
  width = 8,
  height = 10,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,1))
par(mar = c(3, 3, 4, 2))
par(cex.lab = 2, cex.axis = 3)

plot(Israel_FishSP$YPosition ~ Israel_FishSP$Pred, las = 1,
     ylim = c(0, 9),
     xlim = c(-8, 8),
     ylab = NA, xlab = NA, axes = TRUE, yaxt = "n", cex.main = 3,
     pch = 19,cex.axis= 2.5, cex = 3.5,
     main= "IS",
     col = c("indianred1","lightskyblue1"))


segments(Israel_FishSP$Ci.l, Israel_FishSP$YPosition, 
         Israel_FishSP$Ci.b, Israel_FishSP$YPosition, 
         lty = 2, lwd = 3.5,
         col = c("indianred1","lightskyblue1"))

abline( v= 0,col='grey', lty=1, lwd=2) 

dev.off()