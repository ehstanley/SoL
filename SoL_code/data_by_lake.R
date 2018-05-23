

library(dplyr)

secchi <- SoL3[!is.na(SoL3$secchi),]
secchi.lake = aggregate(secchi$secchi, by = list(secchi$lagoslakeid), FUN = length)
secchi.lake <- rename(secchi.lake, secchi.count = data.count)
secchi.lake$log_secchi <- log10(secchi.lake$secchi.count)

chl <- SoL3[!is.na(SoL3$chla),]
chl.lake = aggregate(chl$chla, by = list(chl$lagoslakeid), FUN = length)
chl.lake <- rename(chl.lake, lagoslakeid = Group.1, chl.count = x)
chl.lake$log_chl <- log10(chl.lake$chl.count)

color <- SoL3[!is.na(SoL3$colort),]
color.lake = aggregate(color$colort, by = list(color$lagoslakeid), FUN = length)
color.lake <- rename(color.lake, lagoslakeid = Group.1, color.count = x)
color.lake$log_color <- log10(color.lake$color.count)

doc<- SoL3[!is.na(SoL3$doc),]
doc.lake = aggregate(doc$doc, by = list(doc$lagoslakeid), FUN = length)
doc.lake <- rename(doc.lake, lagoslakeid = Group.1, doc.count = x)
doc.lake$log_doc <- log10(doc.lake$doc.count)

nh4<- SoL3[!is.na(SoL3$nh4),]
nh4.lake = aggregate(nh4$nh4, by = list(nh4$lagoslakeid), FUN = length)
nh4.lake <- rename(nh4.lake, lagoslakeid = Group.1, nh4.count = x)
nh4.lake$log_nh4 <- log10(nh4.lake$nh4.count)

no2no3<- SoL3[!is.na(SoL3$no2no3),]
no2no3.lake = aggregate(no2no3$no2no3, by = list(no2no3$lagoslakeid), FUN = length)
no2no3.lake <- rename(no2no3.lake, lagoslakeid = Group.1, no2no3.count = x)
no2no3.lake$log_no2no3 <- log10(no2no3.lake$no2no3.count)

srp<- SoL3[!is.na(SoL3$srp),]
srp.lake = aggregate(srp$srp, by = list(srp$lagoslakeid), FUN = length)
srp.lake <- rename(srp.lake, lagoslakeid = Group.1, srp.count = x)
srp.lake$log_srp <- log10(srp.lake$srp.count)

tkn<- SoL3[!is.na(SoL3$tkn),]
tkn.lake = aggregate(tkn$tkn, by = list(tkn$lagoslakeid), FUN = length)
tkn.lake <- rename(tkn.lake, lagoslakeid = Group.1, tkn.count = x)
tkn.lake$log_tkn <-log10(tkn.lake$tkn.count)

tn<- SoL3[!is.na(SoL3$tn),]
tn.lake = aggregate(tn$tn, by = list(tn$lagoslakeid), FUN = length)
tn.lake <- rename(tn.lake, lagoslakeid = Group.1, tn.count = x)
tn.lake$log_tn <- log10(tn.lake$tn.count)

tp<- SoL3[!is.na(SoL3$tp),]
tp.lake = aggregate(tp$tp, by = list(tp$lagoslakeid), FUN = length)
tp.lake <- rename(tp.lake, lagoslakeid = Group.1, tp.count = x)
tp.lake$log_tp <- log10(tp.lake$tp.count)

hist(secchi.lake$secchi.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))     
hist(secchi.lake$log_secchi, breaks = 50)
secchi_hist<- hist(secchi.lake$secchi.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))

hist(chl.lake$log_chl)
hist(chl.lake$chl.count)
chl_hist <- hist(chl.lake$chl.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))

color_hist <- hist(color.lake$color.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
doc_hist <- hist(doc.lake$doc.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
nh4_hist <- hist(nh4.lake$nh4.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
no2no3_hist <- hist(no2no3.lake$no2no3.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
srp_hist <- hist(srp.lake$srp.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
tkn_hist <- hist(tkn.lake$tkn.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
tn_hist <- hist(tn.lake$tn.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))
tp_hist <- hist(tp.lake$tp.count, breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 3500))

counts <- data.frame(c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000))
counts <- rename(counts, counts=c.1..2..5..10..20..50..100..200..500..1000..2000.)
secchi.count <- secchi_hist$counts
chl.count <-chl_hist$counts
color.count <- color_hist$counts
doc.count <- doc_hist$counts
nh4.count <- nh4_hist$counts
no2no3.count <-no2no3_hist$counts
srp.count <- srp_hist$counts
tkn.count <-tkn_hist$counts
tn.count <- tn_hist$counts
tp.count <- tp_hist$counts

counts <-cbind(counts, secchi.count, chl.count, color.count, doc.count, nh4.count,
               no2no3.count, srp.count, tkn.count, tn.count, tp.count)
plot(counts$counts, counts$secchi.count, type= "l", ylim = c(0, 5000), col= "red",
     xlab = "Observations per lake",
     ylab= "Number of lakes")
par(new=TRUE)
plot(counts$counts, counts$chl.count, type = "l", ylim = c(0, 5000), col = "green",
     xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$tp.count, type = "l", ylim = c(0, 5000), col = "blue",  xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$tn.count, type = "l", ylim = c(0, 5000), col = "salmon",  xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$doc.count, type = "l", ylim = c(0, 5000), col = "brown",  xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$color.count, type = "l", ylim = c(0,5000), col = "orange",  xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$nh4.count, type = "l", ylim = c(0, 5000), col = "black",  xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$no2no3.count, type = "l", ylim = c(0, 5000), col = "cyan",  xlab = "", ylab = "")
par(new=TRUE)
plot(counts$counts, counts$srp.count, type = "l", ylim = c(0, 5000), col = "darkgoldenrod",  xlab = "", ylab = "")
legend("topright", legend = c("Secchi", "Chl a", "TP", "TN", "DOC", "Color", "NH4", "NO2NO3", "SRP"),
       cex = 0.8, lty=c(1,1), col = c("red", "green", "blue","salmon", "brown", "orange",
                                      "black", "cyan", "darkgoldenrod"))

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
