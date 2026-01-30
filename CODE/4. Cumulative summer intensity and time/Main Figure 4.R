##%######################################################%##
#                                                          #
####        Overview plot change across time            ####
#                                                          #
##%######################################################%##
png(
  "output_plot/Main Figure4.jpg",
  width = 16,
  height = 6,
  units = 'in',
  res = 800
)     
par(mfcol=c(1,2))
par(mar = c(5,5,2,2))

##%######################################################%##
#                                                          #
####               Mollusc through time                 ####
#                                                          #
##%######################################################%##

plot(PredictNIS_MolluscAbun$eventYear, PredictNIS_MolluscAbun$Pred, type="o", col="indianred1",pch = NA, lty=1 , lwd=4,
     cex.lab = 2,cex.main = 2,cex.axis = 1.8,xlim = c(2005, 2025), ylim = c(-4, 4),main="A. Mollusc abundance through time",
     ylab = "Autumn to spring ratio", xlab = "Year")

polygon(c(CI_MolluscNIS$eventYear, rev(CI_MolluscNIS$eventYear)), c(CI_MolluscNIS$ci.lb, rev(CI_MolluscNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

#add second data series to the same chart using points() and lines()
lines(PredictNAT_MolluscAbun$eventYear, PredictNAT_MolluscAbun$Pred, col="skyblue1",lty=1, lwd=4)


polygon(c(CI_MolluscNAT$eventYear, rev(CI_MolluscNAT$eventYear)), c(CI_MolluscNAT$ci.lb, rev(CI_MolluscNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

rug(PredictNIS_MolluscAbun$eventYear, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(MolluscSpAbundance$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

##%######################################################%##
#                                                          #
####                 Fish through time                   ###
#                                                          #
##%######################################################%##

plot(PredictNIS_FishAbun$eventYear, PredictNIS_FishAbun$Pred, type="o", col="indianred1",pch = NA, lty=1 , lwd=4,
     cex.lab = 2,cex.main = 2,cex.axis = 1.8,xlim = c(2015, 2025), ylim = c(-4, 4),main="B. Fish abundance through time",
     ylab = NA, xlab = "Year")

polygon(c(CI_FishNIS$eventYear, rev(CI_FishNIS$eventYear)), c(CI_FishNIS$ci.lb, rev(CI_FishNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )


#add second data series to the same chart using points() and lines()
lines(PredictNAT_FishAbun$eventYear, PredictNAT_FishAbun$Pred, col="lightskyblue1",lty=1, lwd=4)

polygon(c(CI_FishNAT$eventYear, rev(CI_FishNAT$eventYear)), c(CI_FishNAT$ci.lb, rev(CI_FishNAT$ci.ub)),
        col = adjustcolor("lightskyblue1", alpha.f=0.25),lty = 0 )

rug(PredictNAT_FishAbun$eventYear, ticksize=0.04, side=1, lwd=2,col="darkgrey")
rug(Transect_Species_Abundance$Ratio, ticksize=0.04, side=2, lwd=2,col="darkgrey")

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 
legend("bottomleft", legend = c("Non-indigenous", "Native"), 
       col=c("indianred1","lightskyblue1"),
       pch = 15, bty = "n", pt.cex = 2.5, cex = 2,  horiz = F)


dev.off()