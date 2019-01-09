##Code for Figs. 3 and S2- cumulative frequency plots

library(dplyr)

sol <- readRDS("SoL_data/SoL_data.rds")

#getting the total number of observations per lake for each variable
chl <- sol[!is.na(sol$chla),]
chl.lake = aggregate(chl$chla, by = list(chl$lagoslakeid), FUN = length)
chl.lake <- rename(chl.lake, lagoslakeid = Group.1, chl.count = x)

color <- sol[!is.na(sol$colort),]
color.lake = aggregate(color$colort, by = list(color$lagoslakeid), FUN = length)
color.lake <- rename(color.lake, lagoslakeid = Group.1, color.count = x)

doc<- sol[!is.na(sol$doc),]
doc.lake = aggregate(doc$doc, by = list(doc$lagoslakeid), FUN = length)
doc.lake <- rename(doc.lake, lagoslakeid = Group.1, doc.count = x)

nh4<- sol[!is.na(sol$nh4),]
nh4.lake = aggregate(nh4$nh4, by = list(nh4$lagoslakeid), FUN = length)
nh4.lake <- rename(nh4.lake, lagoslakeid = Group.1, nh4.count = x)

no2no3<- sol[!is.na(sol$no2no3),]
no2no3.lake = aggregate(no2no3$no2no3, by = list(no2no3$lagoslakeid), FUN = length)
no2no3.lake <- rename(no2no3.lake, lagoslakeid = Group.1, no2no3.count = x)

secchi <- sol[!is.na(sol$secchi),]
secchi.lake = aggregate(secchi$secchi, by = list(secchi$lagoslakeid), FUN = length)
secchi.lake <- rename(secchi.lake, lagoslakeid = Group.1, secchi.count = x)

tn<- sol[!is.na(sol$tn),]
tn.lake = aggregate(tn$tn, by = list(tn$lagoslakeid), FUN = length)
tn.lake <- rename(tn.lake, lagoslakeid = Group.1, tn.count = x)

tp<- sol[!is.na(sol$tp),]
tp.lake = aggregate(tp$tp, by = list(tp$lagoslakeid), FUN = length)
tp.lake <- rename(tp.lake, lagoslakeid = Group.1, tp.count = x)


#sort data 
s2 <- data.frame(sort(secchi.lake$secchi.count))
s2$sum <- cumsum(s2$sort.secchi.lake.secchi.count.)
s2$fraction = (s2$sum/708172)
s2$lake.number <- c(1:12377)
s2$pct <- s2$lake.number/12377

chl2 <- data.frame(sort(chl.lake$chl.count))
chl2$sum <-cumsum(chl2$sort.chl.lake.chl.count.)
chl2$fraction = (chl2$sum/197868)
chl2$lake.number <- c(1:8525)
chl2$pct <- chl2$lake.number/8525

p2 <- data.frame(sort(tp.lake$tp.count))
p2$sum <- cumsum(p2$sort.tp.lake.tp.count.)
p2$fraction <- (p2$sum/148759)
p2$lake.number <- c(1:10490)
p2$pct <- p2$lake.number/10490

n2 <- data.frame(sort(tn.lake$tn.count))
n2$sum <- cumsum(n2$sort.tn.lake.tn.count.)
n2$fraction <- (n2$sum/37961)
n2$lake.number <- c(1:2772)
n2$pct <- n2$lake.number/2772

nh4.2 <- data.frame(sort(nh4.lake$nh4.count))
nh4.2$sum <- cumsum(nh4.2$sort.nh4.lake.nh4.count.)
nh4.2$fraction <- (nh4.2$sum/47525)
nh4.2$lake.number <- c(1:6502)
nh4.2$pct <- nh4.2$lake.number/6502

no3.2 <- data.frame(sort(no2no3.lake$no2no3.count))
no3.2$sum <- cumsum(no3.2$sort.no2no3.lake.no2no3.count.)
no3.2$fraction <- (no3.2$sum/68476)
no3.2$lake.number <- c(1:8173)
no3.2$pct <- no3.2$lake.number/8173

doc2 <- data.frame(sort(doc.lake$doc.count))
doc2$sum <- cumsum(doc2$sort.doc.lake.doc.count.)
doc2$fraction <- (doc2$sum/29287)
doc2$lake.number <- c(1:4997)
doc2$pct <- doc2$lake.number/4997

color2 <- data.frame(sort(color.lake$color.count))
color2$sum <- cumsum(color2$sort.color.lake.color.count.)
color2$fraction <- (color2$sum/43542)
color2$lake.number <- c(1:5636)
color2$pct <- color2$lake.number/5636



#Fig. 3- Grey Scale one panel
tiff(filename = "SoL_graphics/FinalFigures/cum_freq.tiff",
     width = 3.5,
     height = 3.5,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

#Change mfrow for multi-panel plots but don't change margins
par(mfrow = c(1,1),
    oma = c(0,0,0,0),
    mar=c(2.4,3.5,.5,.5),
    pty="s")
plot.new( )
plot.window( xlim=c(0,1), ylim=c(0,1) )
grid()
box(lwd=1)
#c_chl = rgb(43, 140, 190, max=255, alpha=255)
lines(chl2$pct, chl2$fraction,
      xlab="",ylab="",
      xaxt="n",yaxt="n",col="darkgrey",
      xlim=c(0,1),ylim=c(0,1),
      type="l",lwd=2.5,lty=1)
## add axis ticks
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
## add the labels
axis(1, line = -0.7, lwd = 0, cex.axis = 0.7)
axis(2, line = -.4, lwd =0, cex.axis = 0.7, las=1)
mtext(side=1,"Fraction of lakes", line=1.2)
#adjust the line value to move the y axis label in and out
#based on how many digits are in the y ticks
mtext(side=2, "Fraction of data", line = 1.9)
#c_tn = rgb(247,104, 161, max=255, alpha=255)
lines(n2$pct, n2$fraction,col="darkgrey",lwd=2.5,lty=2)
#c_doc = rgb(140, 81, 10, max=255, alpha=255)
lines(doc2$pct, doc2$fraction,col="darkgrey",lwd=2.5,lty=3)
legend("topleft", legend = c("Chl", "TN", "DOC"), 
       col = c("darkgrey", "darkgrey", "darkgrey"),bty="n",lwd=2,lty=c(1,2,3))
dev.off()


#Fig.S2- Color 3 panel
tiff(filename = "SoL_graphics/FinalFigures/cum_freq_3panel.tiff",
     width = 6.5,
     height = 2,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")
par(mfrow = c(1,3),
    oma = c(0,0,0,0),
    mar=c(2.4,3.5,.5,.5),
    pty="s")
plot.new( )
plot.window( xlim=c(0,1), ylim=c(0,1) )
grid()
box(lwd=1)
lines(s2$pct, s2$fraction,
      xlab="",ylab="",
      xaxt="n",yaxt="n",col="darkblue",
      xlim=c(0,1),ylim=c(0,1),
      type="l",lwd=2.5,lty=1)
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
axis(1, line = -0.7, lwd = 0, cex.axis = 0.7)
axis(2, line = -.4, lwd =0, cex.axis = 0.7, las=1)
mtext(side=2, "Fraction of data", line = 1.9,cex=.8)
lines(chl2$pct, chl2$fraction,col="dodgerblue2",lwd=2.5,lty=1)
lines(p2$pct, p2$fraction,col="lightskyblue1",lwd=2.5,lty=1)
legend("topleft", legend = c("Secchi", "Chl", "TP"), 
       col = c("darkblue", "dodgerblue2", "lightskyblue1"),bty="n",lwd=2,lty=1)

plot.new( )
plot.window( xlim=c(0,1), ylim=c(0,1) )
grid()
box(lwd=1)
lines(n2$pct, n2$fraction,
      xlab="",ylab="",
      xaxt="n",yaxt="n",col="darkorchid4",
      xlim=c(0,1),ylim=c(0,1),
      type="l",lwd=2.5,lty=1)
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
axis(1, line = -0.7, lwd = 0, cex.axis = 0.7)
axis(2, line = -.4, lwd =0, cex.axis = 0.7, las=1)
mtext(side=1,"Fraction of lakes", line=1.2,cex=.8)
lines(nh4.2$pct, nh4.2$fraction,col="darkorchid2",lwd=2.5,lty=1)
lines(no3.2$pct, no3.2$fraction,col="orchid1",lwd=2.5,lty=1)
legend("topleft", legend = c("TN", "NH4", "NO3"), 
       col = c("darkorchid4", "darkorchid2", "orchid1"),bty="n",lwd=2,lty=1)

plot.new( )
plot.window( xlim=c(0,1), ylim=c(0,1) )
grid()
box(lwd=1)
lines(color2$pct, color2$fraction,
      xlab="",ylab="",
      xaxt="n",yaxt="n",col="tan4",
      xlim=c(0,1),ylim=c(0,1),
      type="l",lwd=2.5,lty=1)
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
axis(1, line = -0.7, lwd = 0, cex.axis = 0.7)
axis(2, line = -.4, lwd =0, cex.axis = 0.7, las=1)
lines(doc2$pct, doc2$fraction,col="tan2",lwd=2.5,lty=1)
legend("topleft", legend = c("Color", "DOC"), 
       col = c("tan4", "tan2"),bty="n",lwd=2,lty=1)
dev.off()











plot(chl2$pct, chl2$fraction, cex.lab = 1.5, xlab = "Fraction of lakes", ylab = "Fraction of data", 
     pch = 15, cex= 0.5, col = "green")
par(new = TRUE)
plot(n2$pct, n2$fraction, axes = FALSE, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "black")
par(new = TRUE)
plot(doc2$pct, doc2$fraction, axes = FALSE, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "brown")
legend("topleft", legend = c("Chl", "TN", "DOC"),
       cex = 1.1, lty=c(1,1), col = c("green", "black", "brown"))

par(mfrow=c(1,2))
plot(s2$pct, s2$fraction, xlab = "Fraction of lakes", ylab = "Fraction of data", 
     pch = 15, cex= 0.5, col = "red")
par(new = TRUE)
plot(chl2$pct, chl2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "green")
par(new = TRUE)
plot(p2$pct, p2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "blue")
par(new = TRUE)
plot(n2$pct, n2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "black")
par(new = TRUE)
plot(doc2$pct, doc2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "brown")
par(new = TRUE)
legend("topleft", legend = c("Secchi", "Chl a", "TP", "TN", "DOC"),
       cex = 0.8, lty=c(1,1), col = c("red", "green", "blue","black", "brown"))

plot(n2$pct, n2$fraction, xlab = "Fraction of lakes", ylab = "Fraction of data", 
     pch = 15, cex= 0.5, col = "black")
par(new = TRUE)
plot(nh4.2$pct, nh4.2$fraction, xlab = "", ylab = "", xaxt='n',  yaxt='n', pch = 15, cex= 0.5, col = "orange")
par(new = TRUE)
plot(no3.2$pct, no3.2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "cyan")
par(new = TRUE)

legend("topleft", legend = c("TN", "NH4", "NO2NO3"),
       cex = 0.8, lty=c(1,1), col = c("black", "orange", "cyan"))

dev.off()

plot(s2$pct, s2$fraction, xlab = "", ylab = "", 
     pch = 15, cex= 0.5, col = "red")
par(new = TRUE)
plot(chl2$pct, chl2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "green")
par(new = TRUE)
plot(p2$pct, p2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "blue")
legend("topleft", legend = c("Secchi", "Chl a", "TP"),
       cex = 0.8, lty=c(1,1), col = c("red", "green", "blue"))


plot(n2$pct, n2$fraction, xlab = "", ylab = "Fraction of data", pch = 15, cex= 0.5, col = "black")
par(new = TRUE)
plot(nh4.2$pct, nh4.2$fraction, xlab = "", ylab = "", xaxt='n',  yaxt='n', pch = 15, cex= 0.5, col = "cyan")
par(new = TRUE)
plot(no3.2$pct, no3.2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "dodgerblue")
legend("topleft", legend = c("TN", "NH4", "NO2NO3"),
       cex = 0.8, lty=c(1,1), col = c("black", "cyan", "dodgerblue"))


plot(color2$pct, color2$fraction, xlab = "Fraction of lakes", ylab = "", pch = 15, cex= 0.5, col = "orange")
par(new = TRUE)
plot(doc2$pct, doc2$fraction, xlab = "", ylab = "", xaxt='n', yaxt='n', pch = 15, cex= 0.5, col = "brown")
legend("topleft", legend = c("Color", "DOC"),
       cex = 0.8, lty=c(1,1), col = c("orange", "brown"))

