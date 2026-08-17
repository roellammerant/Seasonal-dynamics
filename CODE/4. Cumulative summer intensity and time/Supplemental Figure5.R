##%######################################################%##
#                                                          #
####          Suppl. cumulative summer intensity        ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Supplemental Figure5.jpg",
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
####            Mollusc cumulative summer intensity     ####
#                                                          #
##%######################################################%##

###Native species
plot(PredictNAT_MolluscIntensity$c_intensity, PredictNAT_MolluscIntensity$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),main="A. Native Mollusc abundance across summer intensity",
     ylab = "Autumn to spring ratio", xlab = NA)

polygon(c(CI_MolluscNAT_Int$c_intensity, rev(CI_MolluscNAT_Int$c_intensity)), c(CI_MolluscNAT_Int$ci.lb, rev(CI_MolluscNAT_Int$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNAT_MolluscIntensity$c_intensity, amount = 0.05),
       jitter(PredictNAT_MolluscIntensity$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("skyblue1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

###NIS species
plot(PredictNIS_MolluscIntensity$c_intensity, PredictNIS_MolluscIntensity$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),
     main="C. NIS Mollusc abundance across summer intensity",
     ylab = "Autumn to spring ratio", xlab = "Cumulative summer intensity (°C-days)")


polygon(c(CI_MolluscNIS_Int$c_intensity, rev(CI_MolluscNIS_Int$c_intensity)), c(CI_MolluscNIS_Int$ci.lb, rev(CI_MolluscNIS_Int$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNIS_MolluscIntensity$c_intensity, amount = 0.05),
       jitter(PredictNIS_MolluscIntensity$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("indianred1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 


##%######################################################%##
#                                                          #
####             Fish cumulative summer intensity       ####
#                                                          #
##%######################################################%##

###Native species
plot(PredictNAT_FishInt$c_intensity, PredictNAT_FishInt$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),main="B. Native Fish abundance across summer intensity",
     ylab = NA, xlab = NA)


polygon(c(CI_FishNAT_int$c_intensity, rev(CI_FishNAT_int$c_intensity)), c(CI_FishNAT_int$ci.lb, rev(CI_FishNAT_int$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNAT_FishInt$c_intensity, amount = 0.05),
       jitter(PredictNAT_FishInt$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("skyblue1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

###NIS species
plot(PredictNIS_FishInt$c_intensity, PredictNIS_FishInt$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),
     main="D. NIS Fish abundance across summer intensity",
     ylab = NA, xlab = "Cumulative summer intensity (°C-days)")


polygon(c(CI_FishNIS_Int$c_intensity, rev(CI_FishNIS_Int$c_intensity)), c(CI_FishNIS_Int$ci.lb, rev(CI_FishNIS_Int$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNIS_FishInt$c_intensity, amount = 0.05),
       jitter(PredictNIS_FishInt$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("indianred1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

dev.off()