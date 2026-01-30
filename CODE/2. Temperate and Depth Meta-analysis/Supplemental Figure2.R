##%######################################################%##
#                                                          #
####        Overview plot temperature and depth         ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Supplemental Figure2.jpg",
  width = 16,
  height = 6,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,2))
par(mar = c(5,5,3,2))

##%######################################################%##
#                                                          #
####        Assemblage level abundance across SST       ####
#                                                          #
##%######################################################%##

plot(PredictNIS_Abun$Temp, PredictNIS_Abun$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2,cex.main = 2,cex.axis = 1.8, ylim = c(-3, 3),main="A. Assemblage abundance across SST", 
     ylab = "Autumn to spring ratio", xlab = "Max monthly SST (°C)")

polygon(c(CI_NIS_Abun$Temp, rev(CI_NIS_Abun$Temp)), c(CI_NIS_Abun$ci.lb, rev(CI_NIS_Abun$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

#add second data series to the same chart using points() and lines()
lines(PredictNAT_Abun$Temp, PredictNAT_Abun$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_NAT_Abun$Temp, rev(CI_NAT_Abun$Temp)), c(CI_NAT_Abun$ci.lb, rev(CI_NAT_Abun$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

legend("topright", legend = c("Non-indigenous", "Native"), 
       col=c("indianred1","skyblue1"),
       pch = 15, bty = "n", pt.cex = 3.5, cex = 2,  horiz = F)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNIS_Abun$Temp, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(PredictNIS_Abun$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

##%######################################################%##
#                                                          #
####         Community level Sprich across SST          ####
#                                                          #
##%######################################################%##

plot(PredictNIS_SpRich$Temp, PredictNIS_SpRich$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2,cex.main = 2,cex.axis = 1.8, ylim = c(-3, 3),main="B. Species richness across SST",
     ylab = "Autumn to spring ratio", xlab = "Max monthly SST (°C)")

polygon(c(CI_NIS_SpRich$Temp, rev(CI_NIS_SpRich$Temp)), c(CI_NIS_SpRich$ci.lb, rev(CI_NIS_SpRich$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

#add second data series to the same chart using points() and lines()
lines(PredictNAT_SpRich$Temp, PredictNAT_SpRich$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_NAT_SpRich$Temp, rev(CI_NAT_SpRich$Temp)), c(CI_NAT_SpRich$ci.lb, rev(CI_NAT_SpRich$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNIS_SpRich$Temp, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(PredictNIS_SpRich$`SpRich_S:A`, ticksize=0.04, side=2, lwd=2,col="darkgrey")

dev.off()