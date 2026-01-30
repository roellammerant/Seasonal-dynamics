##%######################################################%##
#                                                          #
####     Max thermal affinity and cummulative stress    ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Main Figure3.jpg",
  width = 16,
  height = 10,
  units = 'in',
  res = 800
)     
par(mfcol=c(2,2))
par(mar = c(5, 5, 2, 2))
par(cex.lab = 1.5, cex.axis = 1.5)

##%######################################################%##
#                                                          #
####             Mollusc Max thermal affinity           ####
#                                                          #
##%######################################################%##

plot(Modeloutput_MolluscNAT$TempPrefMax, Modeloutput_MolluscNAT$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),main="A. Mollusc abundance across thermal affinity",
     ylab = "Autumn to spring ratio", xlab = "Max thermal affinity (°C)")


polygon(c(StandardErrorbars_MolluscNAT$TempPrefMax, rev(StandardErrorbars_MolluscNAT$TempPrefMax)), c(StandardErrorbars_MolluscNAT$ci.lb, rev(StandardErrorbars_MolluscNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )


legend("topleft", legend = c("Non-indigenous", "Native"), 
       col=c("indianred1","skyblue1"),
       pch = 15, bty = "n", pt.cex = 2.5, cex = 2,  horiz = F)

lines(Modeloutput_MolluscNIS$TempPrefMax, Modeloutput_MolluscNIS$Pred, col="indianred1",lty=1, lwd=4)

polygon(c(StandardErrorbars_MolluscNIS$TempPrefMax, rev(StandardErrorbars_MolluscNIS$TempPrefMax)), c(StandardErrorbars_MolluscNIS$ci.lb, rev(StandardErrorbars_MolluscNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(MolluscThermalAffinity$TempPrefMax, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(MolluscThermalAffinity$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

##%######################################################%##
#                                                          #
####          Mollusc Cumulative summer intensity       ####
#                                                          #
##%######################################################%##

plot(PredictNIS_MolluscIntensity$c_intensity, PredictNIS_MolluscIntensity$Pred, type="o", col="indianred1",pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2, ylim = c(-4, 4),main="C. Mollusc abundance across summer intensity",
     ylab = "Autumn to spring ratio", xlab = "Cumulative summer intensity (°C-days)")

polygon(c(CI_MolluscNIS_Int$c_intensity, rev(CI_MolluscNIS_Int$c_intensity)), 
        c(CI_MolluscNIS_Int$ci.lb, rev(CI_MolluscNIS_Int$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

#add second data series to the same chart using points() and lines()
lines(PredictNAT_MolluscIntensity$c_intensity, PredictNAT_MolluscIntensity$Pred, col="skyblue1",lty=1, lwd=4)

polygon(c(CI_MolluscNAT_Int$c_intensity, rev(CI_MolluscNAT_Int$c_intensity)), 
        c(CI_MolluscNAT_Int$ci.lb, rev(CI_MolluscNAT_Int$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNIS_MolluscIntensity$c_intensity, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(MolluscSummerIntensity$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

##%######################################################%##
#                                                          #
####             Fish Max thermal affinity              ####
#                                                          #
##%######################################################%##

plot(Modeloutput_FishNAT$TempPrefMax, Modeloutput_FishNAT$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),main=" B. Fish abundance across thermal affinity",
     ylab = NA, xlab = "Max thermal affinity (°C)")

polygon(c(StandardErrorbars_FishNAT$TempPrefMax, rev(StandardErrorbars_FishNAT$TempPrefMax)), c(StandardErrorbars_FishNAT$ci.lb, rev(StandardErrorbars_FishNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

lines(Modeloutput_FishNIS$TempPrefMax, Modeloutput_FishNIS$Pred, col="indianred1",lty=1, lwd=4)

polygon(c(StandardErrorbars_FishNIS$TempPrefMax, rev(StandardErrorbars_FishNIS$TempPrefMax)), c(StandardErrorbars_FishNIS$ci.lb, rev(StandardErrorbars_FishNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(FishThermalAffinity$TempPrefMax, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(FishThermalAffinity$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

##%######################################################%##
#                                                          #
####          Fish Cumulative summer intensity          ####
#                                                          #
##%######################################################%##

plot(PredictNIS_FishInt$c_intensity, PredictNIS_FishInt$Pred, type="o", col="indianred1",pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2, ylim = c(-4, 4),main= "D. Fish abundance across summer intensity",
     ylab = NA, xlab = "Cumulative summer intensity (°C-days)")

polygon(c(CI_FishNIS_Int$c_intensity, rev(CI_FishNIS_Int$c_intensity)), 
        c(CI_FishNIS_Int$ci.lb, rev(CI_FishNIS_Int$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )


#add second data series to the same chart using points() and lines()
lines(PredictNAT_FishInt$c_intensity, PredictNAT_FishInt$Pred, col="lightskyblue1",lty=1, lwd=4)

polygon(c(CI_FishNAT_int$c_intensity, rev(CI_FishNAT_int$c_intensity)),
        c(CI_FishNAT_int$ci.lb, rev(CI_FishNAT_int$ci.ub)),
        col = adjustcolor("lightskyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNIS_FishInt$c_intensity, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(Transect_Species_Abundance$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

dev.off()