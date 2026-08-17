##%######################################################%##
#                                                          #
####            Suppl. Max thermal affinity             ####
#                                                          #
##%######################################################%##

png(
  "output_plot/Supplemental Figure4.jpg",
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

###Native species
plot(Modeloutput_MolluscNAT$TempPrefMax, Modeloutput_MolluscNAT$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),main="A. Native Mollusc abundance across thermal affinity",
     ylab = "Autumn to spring ratio", xlab = NA)

polygon(c(StandardErrorbars_MolluscNAT$TempPrefMax, rev(StandardErrorbars_MolluscNAT$TempPrefMax)), c(StandardErrorbars_MolluscNAT$ci.lb, rev(StandardErrorbars_MolluscNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

points(jitter(Modeloutput_MolluscNAT$TempPrefMax, amount = 0.05),
       jitter(Modeloutput_MolluscNAT$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("skyblue1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

###NIS species
plot(Modeloutput_MolluscNIS$TempPrefMax, Modeloutput_MolluscNIS$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),xlim = c(15, 31),
     main="C. NIS Mollusc abundance across thermal affinity",
     ylab = "Autumn to spring ratio", xlab = "Max thermal affinity (°C)")


polygon(c(StandardErrorbars_MolluscNIS$TempPrefMax, rev(StandardErrorbars_MolluscNIS$TempPrefMax)), c(StandardErrorbars_MolluscNIS$ci.lb, rev(StandardErrorbars_MolluscNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

points(jitter(Modeloutput_MolluscNIS$TempPrefMax, amount = 0.05),
       jitter(Modeloutput_MolluscNIS$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("indianred1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 


##%######################################################%##
#                                                          #
####             Mollusc Max thermal affinity           ####
#                                                          #
##%######################################################%##

###Native species
plot(Modeloutput_FishNAT$TempPrefMax, Modeloutput_FishNAT$Pred, type="o", col="skyblue1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),main="B. Native Fish abundance across thermal affinity",
     ylab = NA, xlab = NA)


polygon(c(StandardErrorbars_FishNAT$TempPrefMax, rev(StandardErrorbars_FishNAT$TempPrefMax)), c(StandardErrorbars_FishNAT$ci.lb, rev(StandardErrorbars_FishNAT$ci.ub)),
        col = adjustcolor("skyblue1", alpha.f=0.25),lty = 0 )

points(jitter(Modeloutput_FishNAT$TempPrefMax, amount = 0.05),
       jitter(Modeloutput_FishNAT$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("skyblue1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

###NIS species
plot(Modeloutput_FishNIS$TempPrefMax, Modeloutput_FishNIS$Pred, type="o", col="indianred1", pch = NA, lty=1 , lwd=4,
     cex.lab = 2.15,cex.main = 2.15,cex.axis = 2,ylim = c(-8, 8),xlim = c(15, 31),
     main="D. NIS Fish abundance across thermal affinity",
     ylab = NA, xlab = "Max thermal affinity (°C)")


polygon(c(StandardErrorbars_FishNIS$TempPrefMax, rev(StandardErrorbars_FishNIS$TempPrefMax)), c(StandardErrorbars_FishNIS$ci.lb, rev(StandardErrorbars_FishNIS$ci.ub)),
        col = adjustcolor("indianred1", alpha.f=0.25),lty = 0 )

points(jitter(Modeloutput_FishNIS$TempPrefMax, amount = 0.05),
       jitter(Modeloutput_FishNIS$Ratio, amount = 0.05),
       pch = 21,          # open circle
       bg = NA,
       col = adjustcolor("indianred1", alpha.f = 0.60),
       lwd = 1,
       cex = 1.0)

abline(h = 0, col = "darkgrey", lwd = 2, lty = 2) 

dev.off()
