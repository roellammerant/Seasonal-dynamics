##%######################################################%##
#                                                          #
####                      Suppl. Time                   ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Supplemental Figure6.jpg",
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
plot(PredictNAT_MolluscAbun$eventYear, PredictNAT_MolluscAbun$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),xlim = c(2005, 2025),
     main="A. Native Mollusc abundance through time",
     ylab = "Autumn to spring ratio", xlab = NA)

polygon(c(CI_MolluscNAT$eventYear, rev(CI_MolluscNAT$eventYear)), c(CI_MolluscNAT$ci.lb, rev(CI_MolluscNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNAT_MolluscAbun$eventYear, amount = 0.05),
       jitter(PredictNAT_MolluscAbun$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("skyblue1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

###NIS species
plot(PredictNIS_MolluscAbun$eventYear, PredictNIS_MolluscAbun$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),xlim = c(2005, 2025),
     main="C. NIS Mollusc abundance through time",
     ylab = "Autumn to spring ratio", xlab = "Year")


polygon(c(CI_MolluscNIS$eventYear, rev(CI_MolluscNIS$eventYear)), c(CI_MolluscNIS$ci.lb, rev(CI_MolluscNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNIS_MolluscAbun$eventYear, amount = 0.05),
       jitter(PredictNIS_MolluscAbun$Ratio, amount = 0.05),
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
plot(PredictNAT_FishAbun$eventYear, PredictNAT_FishAbun$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),xlim = c(2015, 2025),
     main="B. Native Fish abundance through time",
     ylab = NA, xlab = NA)


polygon(c(CI_FishNAT$eventYear, rev(CI_FishNAT$eventYear)), c(CI_FishNAT$ci.lb, rev(CI_FishNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNAT_FishAbun$eventYear, amount = 0.05),
       jitter(PredictNAT_FishAbun$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("skyblue1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

###NIS species
plot(PredictNIS_FishAbun$eventYear, PredictNIS_FishAbun$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-4, 4),xlim = c(2015, 2025),
     main="D. NIS Fish abundance through time",
     ylab = NA, xlab = "Year")


polygon(c(CI_FishNIS$eventYear, rev(CI_FishNIS$eventYear)), c(CI_FishNIS$ci.lb, rev(CI_FishNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

points(jitter(PredictNIS_FishAbun$eventYear, amount = 0.05),
       jitter(PredictNIS_FishAbun$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("indianred1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

dev.off()