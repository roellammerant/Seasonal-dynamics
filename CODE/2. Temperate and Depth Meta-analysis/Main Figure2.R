##%######################################################%##
#                                                          #
####        Overview plot temperature and depth         ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Main Figure2.jpg",
  width = 16,
  height = 6,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,2))
par(mar = c(5,5,3,2))

##%######################################################%##
#                                                          #
####         Species level abundance across SST         ####
#                                                          #
##%######################################################%##

plot(PredictNIS_AbunSP$Temp, PredictNIS_AbunSP$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     xlim = c(28, 30), ylim = c(-6, 6),main="A. Species abundance across SST",cex.lab = 2,cex.main = 2,cex.axis = 1.8,
     ylab = "Autumn to spring ratio", xlab = "Max monthly SST (°C)")


polygon(c(CI_NIS_AbunSP$Temp, rev(CI_NIS_AbunSP$Temp)), c(CI_NIS_AbunSP$ci.lb, rev(CI_NIS_AbunSP$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

#add second data series to the same chart using points() and lines()
lines(PredictNAT_AbunSP$Temp, PredictNAT_AbunSP$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_NAT_AbunSP$Temp, rev(CI_NAT_AbunSP$Temp)), c(CI_NAT_AbunSP$ci.lb, rev(CI_NAT_AbunSP$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNIS_AbunSP$Temp, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(PredictNIS_AbunSP$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

legend("topright", legend = c("Non-indigenous", "Native"), 
       col=c("indianred1","lightskyblue1"),
       pch = 15, bty = "n", pt.cex = 2.5, cex = 2,  horiz = F)

##%######################################################%##
#                                                          #
####         Species level abundance across Depth       ####
#                                                          #
##%######################################################%##

plot(PredictNIS_AbunDepth$Depth, PredictNIS_AbunDepth$Pred, type="o", col="indianred1",pch = NA, lty=1 , lwd=4,
     cex.lab = 2,cex.main = 2,cex.axis = 1.8, ylim = c(-6, 6),main="B. Species abundance across depth",
     ylab = NA, xlab = "Depth (m)")

polygon(c(CI_NIS$Depth, rev(CI_NIS$Depth)), c(CI_NIS$ci.lb, rev(CI_NIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

#add second data series to the same chart using points() and lines()
lines(PredictNAT_AbunDepth$Depth, PredictNAT_AbunDepth$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_NAT$Depth, rev(CI_NAT$Depth)), c(CI_NAT$ci.lb, rev(CI_NAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNAT_AbunDepth$Depth, ticksize=0.03, side=1, lwd=2,col="darkgrey")
rug(PredictNAT_AbunDepth$Ratio, ticksize=0.03, side=2, lwd=2,col="darkgrey")


dev.off()