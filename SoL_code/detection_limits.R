##Dot plot of detection limits. 

###NOTE####
##EHS made corrections to some of the original detection limit data in LAGOSNE epi_nutr file
##that are now incorporated into the master SoL_data file
##These changes were reported in "Issues identified with current LAGOS-NE LIMNO module"
##So they could be updated in next LAGOS-NE release
##Changes were mostly failure to convert detection limit information to the LAGOS-appropriate units

###
#Detection limits graph
SoL3 <- readRDS("SoL_data.rds")
detect <- SoL3[, c(21:29)]
chla_detect <- unique(detect$chla_detectionlimit)
colort_detect <-unique(detect$colort_detectionlimit)
doc_detect <- unique(detect$doc_detectionlimit)
nh4_detect <- unique(detect$nh4_detectionlimit)
no3_detect <- unique(detect$no2no3_detectionlimit)
srp_detect <- unique(detect$srp_detectionlimit)
tkn_detect <- unique(detect$tkn_detectionlimit)
tn_detect <- unique(detect$tn_detectionlimit)
tp_detect <- unique(detect$tp_detectionlimit)

#get rid of NAs from these vectors
chla_detect <- chla_detect[2:12]
colort_detect <-colort_detect[2:5]
doc_detect <- doc_detect[2:30]
nh4_detect <-sort(nh4_detect)
no3_detect <- sort(no3_detect)
srp_detect <- sort(srp_detect)
tkn_detect <- sort(tkn_detect)
tn_detect <- sort(tn_detect)
tp_detect <- sort(tp_detect)

d1 <- data.frame(chla_detect, 1)
d2 <- data.frame(colort_detect, 2)
d3 <- data.frame(doc_detect, 3)
d4 <- data.frame(nh4_detect, 4)
d5 <- data.frame(no3_detect, 5)
d6 <- data.frame(srp_detect, 6)
d7 <- data.frame(tkn_detect, 7)
d8 <- data.frame(tn_detect, 8)
d9 <- data.frame(tp_detect, 9)

par(mar=c(5.1,5.1,2.1,2.1))
plot(log10(d1$chla_detect),  d1$X1, xlab = "log10(detections limits)", ylab = "",
     xlim = c(-1.5, 3), ylim = c(0, 10), pch = 16, yaxt="n")
ytick<-c("Chl a", "True color", "DOC", "NH4", "NO3", "SRP", "TKN", "TN", "TP")
axis(side=2, at=seq(1, 9, by=1), labels = ytick, las = 2)

par(new = TRUE)
plot(log10(d2$colort_detect),  d2$X2, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d3$doc_detect),  d3$X3, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d4$nh4_detect),  d4$X4, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d5$no3_detect),  d5$X5, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d6$srp_detect),  d6$X6, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d7$tkn_detect),  d7$X7, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d8$tn_detect),  d8$X8, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")

par(new = TRUE)
plot(log10(d9$tp_detect),  d9$X9, xlab = "", ylab = "",  xlim = c(-1.5, 3), ylim = c(0, 10),
     pch = 16, yaxt="n")


