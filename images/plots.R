#load data ----
library(Hmisc)
source("linearReg.R")

tick_ratio <- 0.5
grid_col<-"lightgrey"
par(mfrow = c(2,3))

#Background Estimation Graphs ----
#bg without correction
plot(bg_sans_correction ~ actual_mm, col = "red", pch = 1, xlim = (c(0, 600)), ylim = (c(0, 600)), main = "Background Checker", xlab = "Actual (mm)", ylab = "Estimated (mm)", asp = 1)
lmtmp <- lm(bg_sans_correction ~ actual_mm, data = dfAll, na.action = na.exclude)
abline(lmtmp, col = "red")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0("yNoCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]), " ")
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 1)
eq <- paste0("RMSENoCor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -2, cex = 0.7, adj = 1)

#bg with correction
lmtmp <- lm(bg_with_correction~actual_mm, na.action = na.exclude)
points(bg_with_correction~actual_mm, col = "blue", pch = 2)
abline(lmtmp, col = "blue")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0(" yCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]))
mtext(eq, side = 3, line = -6, cex = 0.7, adj = 0)
eq <- paste0(" RMSECor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 3, line = -7, cex = 0.7, adj = 0)

#add y=x line
abline(0, 1, col = "black", lty = 3, lwd = 2)
minor.tick(tick.ratio = tick_ratio)
grid(col = grid_col)
legend("topleft", c("corrected","uncorrected", "y=x"), lty=c(1,1,3), col=c("blue","red","black"), pch=c(2,1,NA))
#----


# Foreground Estimation Graphs ----
#foreground without correction
plot(fg_sans_correction ~ actual_mm, col = "red", pch = 1, xlim = (c(0, 600)), ylim = (c(0, 600)), main = "Foreground Checker", xlab = "Actual (mm)", ylab = "Estimated (mm)", asp = 1)
lmtmp <- lm(fg_sans_correction ~ actual_mm, na.action = na.omit)
abline(lmtmp, col = "red")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0("yNoCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]), " ")
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 1)
eq <- paste0("RMSENoCor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -2, cex = 0.7, adj = 1)

#foreground with correction
lmtmp <- lm(fg_with_correction ~ actual_mm, data=dfAll, na.action=na.omit)
points(fg_with_correction ~ actual_mm, col = "blue", pch = 2)
abline(lmtmp, col = "blue")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0(" yCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]))
mtext(eq, side = 3, line = -6, cex = 0.7, adj = 0)
eq <- paste0(" RMSECor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 3, line = -7, cex = 0.7, adj = 0)

#add y=x line
abline(0, 1, col = "black", lty = 3, lwd = 2)
minor.tick(tick.ratio = tick_ratio)
grid(col = grid_col)
legend("topleft", c("corrected","uncorrected", "y=x"), lty=c(1,1,3), col=c("blue","red","black"), pch=c(2,1,NA))
#----

# Laser Estimation Graphs ----
#laser without correction
plot(laser_sans_correction ~ actual_mm, col = "red", pch = 1, xlim = (c(0, 600)), ylim = (c(0, 600)), main = "Laser", xlab = "Actual (mm)", ylab = "Estimated (mm)", asp = 1)
lmtmp <- lm(laser_sans_correction ~ actual_mm, data = dfAll, na.action = na.omit)
abline(lmtmp, col = "red")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0("yNoCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]), " ")
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 1)
eq <- paste0("RMSENoCor=", round(summary(lmtmp)[["sigma"]], 2), "  ","R^2=", rsqr, " ")
mtext(eq, side = 1, line = -2, cex = 0.7, adj = 1)

#laser with correction
lmtmp <- lm(laser_with_correction ~ actual_mm, data = dfAll, na.action = na.omit)
points(laser_with_correction ~ actual_mm, col = "blue", pch = 2)
abline(lmtmp, col = "blue")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0(" yCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]))
mtext(eq, side = 3, line = -6, cex = 0.7, adj = 0)
eq <- paste0(" RMSECor=", round(summary(lmtmp)[["sigma"]], 2),"  ", "R^2=", rsqr, " ")
mtext(eq, side = 3, line = -7, cex = 0.7, adj = 0)

#add y=x line
abline(0, 1, col = "black", lty = 3, lwd = 2)
minor.tick(tick.ratio = tick_ratio)
grid(col = grid_col)
legend("topleft", c("corrected","uncorrected", "y=x"), lty=c(1,1,3), col=c("blue","red","black"), pch=c(2,1,NA))
#----


# Difference Graphs

# Background Checker ----
#bg without correction
plot(diff_bg_sans_correction ~ actual_mm, data = dfAll, col = "red", pch = 1, xlim = (c(250, 600)), ylim = (c(-150, 50)), main = "Background Checker", xlab = "Actual (mm)", ylab = "Estimated minus Actual (mm)")
lmtmp <- lm(diff_bg_sans_correction ~ actual_mm,, data = dfAll, na.action = na.omit)
abline(lmtmp, col = "red")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0("yNoCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]), " ")
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 1)
eq <- paste0("RMSENoCor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -2, cex = 0.7, adj = 1)

#bg with correction
lmtmp <- lm(diff_bg_with_correction ~ actual_mm, data = dfAll, na.action = na.omit)
points(diff_bg_with_correction ~ actual_mm, col = "blue", pch = 2, data=dfAll)
abline(lmtmp, col = "blue")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0(" yCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]))
mtext(eq, side = 1, line = -5, cex = 0.7, adj = 0)
eq <- paste0(" RMSECor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -4, cex = 0.7, adj = 0)
minor.tick(tick.ratio=tick_ratio)
grid(col = grid_col)
legend("topleft", c("corrected", "uncorrected"), lty = c(1, 1), col = c("blue", "red"), pch = c(2, 1))
#----
library(Hmisc)
# Foreground Estimation Graphs ----
#foreground without correction
plot(diff_fg_sans_correction ~ actual_mm, col = "red", pch = 1, xlim = (c(250, 600)), ylim = (c(-150, 50)), main = "Foreground Checker", xlab = "Actual (mm)", ylab = "Estimated minus Actual (mm)", data = dfAll)
lmtmp <- lm(diff_fg_sans_correction ~ actual_mm, na.action = na.omit, data = dfAll)
abline(lmtmp, col = "red")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0("yNoCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]), " ")
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 1)
eq <- paste0("RMSENoCor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -2, cex = 0.7, adj = 1)

#foreground with correction
lmtmp <- lm(diff_fg_with_correction ~ actual_mm, data = dfAll, na.action = na.exclude)
points(diff_fg_with_correction ~ actual_mm, col = "blue", pch = 2, data=dfAll)
abline(lmtmp, col = "blue")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0(" yCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]))
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 0)
eq <- paste0(" RMSECor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -4, cex = 0.7, adj = 0)

#add y=x line
abline(0, 1, col = "black", lty = 3, lwd = 2)
minor.tick(tick.ratio = tick_ratio)
grid(col = grid_col)
legend("topleft", c("corrected", "uncorrected"), lty = c(1, 1), col = c("blue", "red"), pch = c(2, 1))
#----


# Laser Estimation Graphs ----
#laser without correction
plot(diff_laser_sans_correction ~ actual_mm, col = "red", pch = 1, main = "Laser", xlim = (c(250, 600)), ylim = (c(-150, 50)), xlab = "Actual (mm)", ylab = "Estimated minus Actual (mm)", data = dfAll)
lmtmp <- lm(diff_laser_sans_correction ~ actual_mm, data = dfAll, na.action = na.omit)
abline(lmtmp, col = "red")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0("yNoCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]), " ")
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 1)
eq <- paste0("RMSENoCor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -2, cex = 0.7, adj = 1)

#laser with correction
lmtmp <- lm(diff_laser_with_correction ~ actual_mm, data = dfAll, na.action = na.omit)
points(diff_laser_with_correction ~ actual_mm, col = "blue", pch = 2, data=dfAll)
abline(lmtmp, col = "blue")
cf <- round(coefficients(lmtmp), 2)
rsqr = round(summary(lmtmp)$r.squared, 3)
eq <- paste0(" yCor=", cf[2], "x", ifelse(sign(cf[1]) == 1, " + ", " - "), abs(cf[1]))
mtext(eq, side = 1, line = -3, cex = 0.7, adj = 0)
eq <- paste0(" RMSECor=", round(summary(lmtmp)[["sigma"]], 2), "  ", "R^2=", rsqr, " ")
mtext(eq, side = 1, line = -4, cex = 0.7, adj = 0)

minor.tick(tick.ratio = tick_ratio)
grid(col = grid_col)
legend("topleft", c("corrected", "uncorrected"), lty = c(1, 1), col = c("blue", "red"), pch = c(2, 1))
#----


#summary(dfAll[9:14])