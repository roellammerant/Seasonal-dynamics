##%######################################################%##
#                                                          #
####        Supplemental plot temperature and depth     ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Supplemental Figure3.jpg",
  width = 14,
  height = 10,
  units = 'in',
  res = 800
)     
par(mfcol=c(2,2))
par(mar = c(5,5,3,2))

##%######################################################%##
#                                                          #
####         Species level abundance across SST         ####
#                                                          #
##%######################################################%##

library("vioplot")

##### Native #####

plot(PredictNAT_AbunSP$Temp, PredictNAT_AbunSP$Pred,
     type = "n",
     xlim = c(27.5,30.5),
     ylim = c(-12,12),main="A. Native mollusc abundance across SST",cex.lab = 2,cex.main = 2,cex.axis = 1.8,
     ylab = "Autumn to spring ratio", xlab = NA)

vioplot(split(PredictNAT_AbunSP$Ratio, PredictNAT_AbunSP$Temp),
        at = c(28.06, 28.56, 29.01, 30.06),
        add = TRUE,
        col = adjustcolor("skyblue1", alpha.f = 0.10))

lines(PredictNAT_AbunSP$Temp, PredictNAT_AbunSP$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_NAT_AbunSP$Temp, rev(CI_NAT_AbunSP$Temp)), c(CI_NAT_AbunSP$ci.lb, rev(CI_NAT_AbunSP$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2)

##### Non-Indigenous #####

plot(PredictNIS_AbunSP$Temp, PredictNIS_AbunSP$Pred,
     type = "n",
     xlim = c(27.5,30.5),
     ylim = c(-12,12),main="C. NIS mollusc abundance across SST",cex.lab = 2,cex.main = 2,cex.axis = 1.8,
     ylab = "Autumn to spring ratio", xlab = "Max monthly SST (°C)")

vioplot(split(PredictNIS_AbunSP$Ratio, PredictNIS_AbunSP$Temp),
        at = c(28.06, 28.56, 29.01, 30.06),
        add = TRUE,
        col = adjustcolor("indianred1", alpha.f = 0.10))

lines(PredictNIS_AbunSP$Temp, PredictNIS_AbunSP$Pred, col="indianred1",lty=1, lwd=4)

polygon(c(CI_NIS_AbunSP$Temp, rev(CI_NIS_AbunSP$Temp)), c(CI_NIS_AbunSP$ci.lb, rev(CI_NIS_AbunSP$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

##%######################################################%##
#                                                          #
####         Species level abundance across Depth       ####
#                                                          #
##%######################################################%##

##### Native #####
plot(PredictNAT_AbunDepth$Depth, PredictNAT_AbunDepth$Pred,
     type = "n",
     xlim = c(5,30),
     ylim = c(-12,12),main="B. Native mollusc abundance across Depth",cex.lab = 2,cex.main = 2,cex.axis = 1.8,
     ylab = NA, xlab = NA)

vioplot(split(PredictNAT_AbunDepth$Ratio, PredictNAT_AbunDepth$Depth),
        at = c(5, 10, 12, 15, 20, 25, 30),
        add = TRUE,
        col = adjustcolor("skyblue1", alpha.f = 0.10))

lines(PredictNAT_AbunDepth$Depth, PredictNAT_AbunDepth$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_NAT$Depth, rev(CI_NAT$Depth)), c(CI_NAT$ci.lb, rev(CI_NAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

##### Non-Indigenous #####
plot(PredictNIS_AbunDepth$Depth, PredictNIS_AbunDepth$Pred,
     type = "n",
     xlim = c(5,30),
     ylim = c(-12,12),main="D. NIS mollusc abundance across Depth",cex.lab = 2,cex.main = 2,cex.axis = 1.8,
     ylab = NA, xlab = "Depth (m)")

vioplot(split(PredictNIS_AbunDepth$Ratio, PredictNIS_AbunDepth$Depth),
        at = c(5, 10, 12, 15, 20, 25, 30),
        add = TRUE,
        col = adjustcolor("indianred1", alpha.f = 0.10))

lines(PredictNIS_AbunDepth$Depth, PredictNIS_AbunDepth$Pred, col="indianred1",lty=1, lwd=4)

polygon(c(CI_NIS$Depth, rev(CI_NIS$Depth)), c(CI_NIS$ci.lb, rev(CI_NIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 



dev.off()