#generating some graphs to display contribution of individual lakes to all observations

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

#organizing data to get info on # lakes into bins based on # of observations/lake
chl_hist <- hist(chl.lake$chl.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
color_hist <- hist(color.lake$color.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
doc_hist <- hist(doc.lake$doc.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
nh4_hist <- hist(nh4.lake$nh4.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
no2no3_hist <- hist(no2no3.lake$no2no3.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
secchi_hist<- hist(secchi.lake$secchi.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
tn_hist <- hist(tn.lake$tn.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
tp_hist <- hist(tp.lake$tp.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))

#counts- categories of # of observations/lake
counts <- data.frame(c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000))
counts <- rename(counts, counts=c.1..2..5..10..20..50..100..200..500..1000..2000.)
chl.count <-chl_hist$counts
color.count <- color_hist$counts
doc.count <- doc_hist$counts
nh4.count <- nh4_hist$counts
no2no3.count <-no2no3_hist$counts
secchi.count <- secchi_hist$counts
tn.count <- tn_hist$counts
tp.count <- tp_hist$counts

counts <-cbind(counts, chl.count, color.count, doc.count, nh4.count,
               no2no3.count, secchi.count, tn.count, tp.count)

plot(log10(counts$counts), counts$secchi.count, type= "l", ylim = c(0, 5000), col= "red",
     xlab = "Log10(Observations per Lake)",
     ylab= "Number of lakes")
par(new=TRUE)
plot(log10(counts$counts), counts$chl.count, type = "l", ylim = c(0, 5000), col = "green",
     xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$tp.count, type = "l", ylim = c(0, 5000), col = "blue",  xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$tn.count, type = "l", ylim = c(0, 5000), col = "salmon",  xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$doc.count, type = "l", ylim = c(0, 5000), col = "brown",  xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$color.count, type = "l", ylim = c(0,5000), col = "orange",  xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$nh4.count, type = "l", ylim = c(0, 5000), col = "black",  xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$no2no3.count, type = "l", ylim = c(0, 5000), col = "cyan",  xlab = "", ylab = "")
par(new=TRUE)
plot(log10(counts$counts), counts$srp.count, type = "l", ylim = c(0, 5000), col = "darkgoldenrod",  xlab = "", ylab = "")
legend("topright", legend = c("Secchi", "Chl a", "TP", "TN", "DOC", "Color", "NH4", "NO2NO3", "SRP"),
       cex = 0.8, lty=c(1,1), col = c("red", "green", "blue","salmon", "brown", "orange",
                                      "black", "cyan", "darkgoldenrod"))

#Next- try a cumulative frequency graph as another way to illustrate that a few lakes 
#contribute a lot of data, many lakes have just 1 observation and contribute little to 
#the total number of observations/variable
# plot rank on value of y


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

