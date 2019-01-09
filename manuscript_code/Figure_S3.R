###Code for Fig. S3- long and clunky.

library(dplyr)

sol <-readRDS("SoL_data/SoL_data.rds")
lakes = readRDS("SoL_data/lakes.rds")

secchi <- sol[!is.na(sol$secchi),]
secchi <-secchi %>% group_by(lagoslakeid) %>% summarise(avg = mean(secchi))
secchi <- left_join(secchi, lakes, by = "lagoslakeid")


chl <- sol[!is.na(sol$chla),]
chl <-chl %>% group_by(lagoslakeid) %>% summarise(avg = mean(chla))
chl <-left_join(chl, lakes, by = "lagoslakeid")

tp <- sol[!is.na(sol$tp),]
tp <-tp %>% group_by(lagoslakeid) %>% summarise(avg = mean(tp))
tp <- left_join(tp, lakes, by = "lagoslakeid")

tn <- sol[!is.na(sol$tn),]
tn <- tn %>% group_by(lagoslakeid) %>% summarise(avg = mean(tn))
tn <- left_join(tn, lakes, by = "lagoslakeid")

nh4 <- sol[!is.na(sol$nh4),]
nh4 <- nh4 %>% group_by(lagoslakeid) %>% summarise(avg = mean(nh4))
nh4 <- left_join(nh4, lakes, by = "lagoslakeid")

no3 <- sol[!is.na(sol$no2no3),]
no3 <- no3 %>% group_by(lagoslakeid) %>% summarise(avg = mean(no2no3))
no3 <- left_join(no3, lakes, by = "lagoslakeid")

color <- sol[!is.na(sol$colort),]
color <-color %>% group_by(lagoslakeid) %>% summarise(avg = mean(colort))
color <- left_join(color, lakes, by = "lagoslakeid")

doc <- sol[!is.na(sol$doc),]
doc <- doc %>% group_by(lagoslakeid) %>% summarise(avg = mean(doc))
doc <- left_join(doc, lakes, by = "lagoslakeid")

#divide all lakes up into surface area size bins
lakes_1 <- lakes[which(lakes$lake_area_ha < 4),]
lakes_2 <- lakes[which(lakes$lake_area_ha >=4 & lakes$lake_area_ha <10),]
lakes_3 <- lakes[which(lakes$lake_area_ha >=10 & lakes$lake_area_ha <20),]
lakes_4 <- lakes[which(lakes$lake_area_ha >=20 & lakes$lake_area_ha <50),]
lakes_5 <- lakes[which(lakes$lake_area_ha >=50 & lakes$lake_area_ha <100),]
lakes_6 <- lakes[which(lakes$lake_area_ha >=100 & lakes$lake_area_ha <200),]
lakes_7 <- lakes[which(lakes$lake_area_ha >=200 & lakes$lake_area_ha <500),]
lakes_8 <- lakes[which(lakes$lake_area_ha >=500 & lakes$lake_area_ha <1000),]
lakes_9 <- lakes[which(lakes$lake_area_ha >=1000 & lakes$lake_area_ha <10000),]
lakes_10 <- lakes[which(lakes$lake_area_ha >=10000),]

count <- data.frame(c(length(lakes_1$lake_area_ha), length(lakes_2$lake_area_ha), 
                      length(lakes_3$lake_area_ha), length(lakes_4$lake_area_ha), 
                      length(lakes_5$lake_area_ha), length(lakes_6$lake_area_ha), 
                      length(lakes_7$lake_area_ha), length(lakes_8$lake_area_ha), 
                      length(lakes_9$lake_area_ha), length(lakes_10$lake_area_ha)))
names(count) <-c("census")

secchi_1 <- secchi[which(secchi$lake_area_ha < 4),]
secchi_2 <- secchi[which(secchi$lake_area_ha >=4 & secchi$lake_area_ha <10),]
secchi_3 <- secchi[which(secchi$lake_area_ha >=10 & secchi$lake_area_ha <20),]
secchi_4 <- secchi[which(secchi$lake_area_ha >=20 & secchi$lake_area_ha <50),]
secchi_5 <- secchi[which(secchi$lake_area_ha >=50 & secchi$lake_area_ha <100),]
secchi_6 <- secchi[which(secchi$lake_area_ha >=100 & secchi$lake_area_ha <200),]
secchi_7 <- secchi[which(secchi$lake_area_ha >=200 & secchi$lake_area_ha <500),]
secchi_8 <- secchi[which(secchi$lake_area_ha >=500 & secchi$lake_area_ha <1000),]
secchi_9 <- secchi[which(secchi$lake_area_ha >=1000 & secchi$lake_area_ha <10000),]
secchi_10 <- secchi[which(secchi$lake_area_ha >=10000),]

secchi_count <- (c(length(secchi_1$avg), length(secchi_2$avg), length(secchi_3$avg), length(secchi_4$avg), 
                length(secchi_5$avg), length(secchi_6$avg), length(secchi_7$avg), length(secchi_8$avg), 
                length(secchi_9$avg), length(secchi_10$avg)))

chl_1 <- chl[which(chl$lake_area_ha < 4),]
chl_2 <- chl[which(chl$lake_area_ha >=4 & chl$lake_area_ha <10),]
chl_3 <- chl[which(chl$lake_area_ha >=10 & chl$lake_area_ha <20),]
chl_4 <- chl[which(chl$lake_area_ha >=20 & chl$lake_area_ha <50),]
chl_5 <- chl[which(chl$lake_area_ha >=50 & chl$lake_area_ha <100),]
chl_6 <- chl[which(chl$lake_area_ha >=100 & chl$lake_area_ha <200),]
chl_7 <- chl[which(chl$lake_area_ha >=200 & chl$lake_area_ha <500),]
chl_8 <- chl[which(chl$lake_area_ha >=500 & chl$lake_area_ha <1000),]
chl_9 <- chl[which(chl$lake_area_ha >=1000 & chl$lake_area_ha <10000),]
chl_10 <- chl[which(chl$lake_area_ha >=10000),]

chl_count <- (c(length(chl_1$avg), length(chl_2$avg), length(chl_3$avg), length(chl_4$avg), 
                   length(chl_5$avg), length(chl_6$avg), length(chl_7$avg), length(chl_8$avg), 
                   length(chl_9$avg), length(chl_10$avg)))

tp_1 <- tp[which(tp$lake_area_ha < 4),]
tp_2 <- tp[which(tp$lake_area_ha >=4 & tp$lake_area_ha <10),]
tp_3 <- tp[which(tp$lake_area_ha >=10 & tp$lake_area_ha <20),]
tp_4 <- tp[which(tp$lake_area_ha >=20 & tp$lake_area_ha <50),]
tp_5 <- tp[which(tp$lake_area_ha >=50 & tp$lake_area_ha <100),]
tp_6 <- tp[which(tp$lake_area_ha >=100 & tp$lake_area_ha <200),]
tp_7 <- tp[which(tp$lake_area_ha >=200 & tp$lake_area_ha <500),]
tp_8 <- tp[which(tp$lake_area_ha >=500 & tp$lake_area_ha <1000),]
tp_9 <- tp[which(tp$lake_area_ha >=1000 & tp$lake_area_ha <10000),]
tp_10 <- tp[which(tp$lake_area_ha >=10000),]

tp_count <- (c(length(tp_1$avg), length(tp_2$avg), length(tp_3$avg), length(tp_4$avg), 
                length(tp_5$avg), length(tp_6$avg), length(tp_7$avg), length(tp_8$avg), 
                length(tp_9$avg), length(tp_10$avg)))

tn_1 <- tn[which(tn$lake_area_ha <4),]
tn_2 <- tn[which(tn$lake_area_ha >=4 & tn$lake_area_ha <10),]
tn_3 <- tn[which(tn$lake_area_ha >=10 & tn$lake_area_ha <20),]
tn_4 <- tn[which(tn$lake_area_ha >=20 & tn$lake_area_ha <50),]
tn_5 <- tn[which(tn$lake_area_ha >=50 & tn$lake_area_ha <100),]
tn_6 <- tn[which(tn$lake_area_ha >=100 & tn$lake_area_ha <200),]
tn_7 <- tn[which(tn$lake_area_ha >=200 & tn$lake_area_ha <500),]
tn_8 <- tn[which(tn$lake_area_ha >=500 & tn$lake_area_ha <1000),]
tn_9 <- tn[which(tn$lake_area_ha >=1000 & tn$lake_area_ha <10000),]
tn_10 <- tn[which(tn$lake_area_ha >=10000),]

tn_count <- (c(length(tn_1$avg), length(tn_2$avg), length(tn_3$avg), length(tn_4$avg), 
                length(tn_5$avg), length(tn_6$avg), length(tn_7$avg), length(tn_8$avg), 
                length(tn_9$avg), length(tn_10$avg)))

nh4_1 <- nh4[which(nh4$lake_area_ha <4),]
nh4_2 <- nh4[which(nh4$lake_area_ha >=4 & nh4$lake_area_ha <10),]
nh4_3 <- nh4[which(nh4$lake_area_ha >=10 & nh4$lake_area_ha <20),]
nh4_4 <- nh4[which(nh4$lake_area_ha >=20 & nh4$lake_area_ha <50),]
nh4_5 <- nh4[which(nh4$lake_area_ha >=50 & nh4$lake_area_ha <100),]
nh4_6 <- nh4[which(nh4$lake_area_ha >=100 & nh4$lake_area_ha <200),]
nh4_7 <- nh4[which(nh4$lake_area_ha >=200 & nh4$lake_area_ha <500),]
nh4_8 <- nh4[which(nh4$lake_area_ha >=500 & nh4$lake_area_ha <1000),]
nh4_9 <- nh4[which(nh4$lake_area_ha >=1000 & nh4$lake_area_ha <10000),]
nh4_10 <- nh4[which(nh4$lake_area_ha >=10000),]

nh4_count <- (c(length(nh4_1$avg), length(nh4_2$avg), length(nh4_3$avg), length(nh4_4$avg), 
                length(nh4_5$avg), length(nh4_6$avg), length(nh4_7$avg), length(nh4_8$avg), 
                length(nh4_9$avg), length(nh4_10$avg)))

no3_1 <- no3[which(no3$lake_area_ha <4),]
no3_2 <- no3[which(no3$lake_area_ha >=4 & no3$lake_area_ha <10),]
no3_3 <- no3[which(no3$lake_area_ha >=10 & no3$lake_area_ha <20),]
no3_4 <- no3[which(no3$lake_area_ha >=20 & no3$lake_area_ha <50),]
no3_5 <- no3[which(no3$lake_area_ha >=50 & no3$lake_area_ha <100),]
no3_6 <- no3[which(no3$lake_area_ha >=100 & no3$lake_area_ha <200),]
no3_7 <- no3[which(no3$lake_area_ha >=200 & no3$lake_area_ha <500),]
no3_8 <- no3[which(no3$lake_area_ha >=500 & no3$lake_area_ha <1000),]
no3_9 <- no3[which(no3$lake_area_ha >=1000 & no3$lake_area_ha <10000),]
no3_10 <- no3[which(no3$lake_area_ha >=10000),]

no3_count <- (c(length(no3_1$avg), length(no3_2$avg), length(no3_3$avg), length(no3_4$avg), 
                length(no3_5$avg), length(no3_6$avg), length(no3_7$avg), length(no3_8$avg), 
                length(no3_9$avg), length(no3_10$avg)))

color_1 <- color[which(color$lake_area_ha <4),]
color_2 <- color[which(color$lake_area_ha >=4 & color$lake_area_ha <10),]
color_3 <- color[which(color$lake_area_ha >=10 & color$lake_area_ha <20),]
color_4 <- color[which(color$lake_area_ha >=20 & color$lake_area_ha <50),]
color_5 <- color[which(color$lake_area_ha >=50 & color$lake_area_ha <100),]
color_6 <- color[which(color$lake_area_ha >=100 & color$lake_area_ha <200),]
color_7 <- color[which(color$lake_area_ha >=200 & color$lake_area_ha <500),]
color_8 <- color[which(color$lake_area_ha >=500 & color$lake_area_ha <1000),]
color_9 <- color[which(color$lake_area_ha >=1000 & color$lake_area_ha <10000),]
color_10 <- color[which(color$lake_area_ha >=10000),]

color_count <- (c(length(color_1$avg), length(color_2$avg), length(color_3$avg), length(color_4$avg), 
                length(color_5$avg), length(color_6$avg), length(color_7$avg), length(color_8$avg), 
                length(color_9$avg), length(color_10$avg)))

doc_1 <- doc[which(doc$lake_area_ha <4),]
doc_2 <- doc[which(doc$lake_area_ha >=4 & doc$lake_area_ha <10),]
doc_3 <- doc[which(doc$lake_area_ha >=10 & doc$lake_area_ha <20),]
doc_4 <- doc[which(doc$lake_area_ha >=20 & doc$lake_area_ha <50),]
doc_5 <- doc[which(doc$lake_area_ha >=50 & doc$lake_area_ha <100),]
doc_6 <- doc[which(doc$lake_area_ha >=100 & doc$lake_area_ha <200),]
doc_7 <- doc[which(doc$lake_area_ha >=200 & doc$lake_area_ha <500),]
doc_8 <- doc[which(doc$lake_area_ha >=500 & doc$lake_area_ha <1000),]
doc_9 <- doc[which(doc$lake_area_ha >=1000 & doc$lake_area_ha <10000),]
doc_10 <- doc[which(doc$lake_area_ha >=10000),]

doc_count <- (c(length(doc_1$avg), length(doc_2$avg), length(doc_3$avg), length(doc_4$avg), 
                length(doc_5$avg), length(doc_6$avg), length(doc_7$avg), length(doc_8$avg), 
                length(doc_9$avg), length(doc_10$avg)))


count<- cbind(count, secchi_count, chl_count, tp_count, tn_count, nh4_count, no3_count,
      color_count, doc_count)

count$secchi_pct <- (count$secchi_count/count$census)*100
count$chl_pct <- (count$chl_count/count$census)*100
count$tp_pct <- (count$tp_count/count$census)*100
count$tn_pct <- (count$tn_count/count$census)*100
count$nh4_pct <- (count$nh4_count/count$census)*100
count$no3_pct <- (count$no3_count/count$census)*100
count$color_pct <- (count$color_count/count$census)*100
count$doc_pct <- (count$doc_count/count$census)*100

count$bin <- c(1:10)

#Fig. S3- plot of % lakes that have been sampled for each size bin 
tiff(filename = "SoL_graphics/fig_S2_v2.tiff",
     width = 3.5,
     height = 2,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

#Change mfrow for multi-panel plots but don't change margins
par(mfrow = c(1,1),
    oma = c(0,0,0,0),
    mar=c(2.4,3.5,.5,.5))
plot(count$bin, count$secchi_pct, xlab = "", ylab = "",
     xaxt = "n", yaxt = "n", type = "o", pch = 16, col = "darkblue", ylim  = c(0, 100))
par(new=T)
plot(count$bin, count$chl_pct, type = "o", pch = 17, col = "dodgerblue2", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new=T)
plot(count$bin, count$tp_pct, type = "o", pch = 18, col = "lightskyblue1", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new = T)
plot(count$bin, count$tn_pct, type = "o", pch = 16, col = "darkorchid4", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new = T)
plot(count$bin, count$nh4_pct, type = "o", pch = 17, col = "darkorchid2", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new = T)
plot(count$bin, count$no3_pct, type = "o", pch = 18, col = "orchid1", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new = T)
plot(count$bin, count$color_pct, type = "o", pch = 16, col = "tan4", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new = T)
plot(count$bin, count$doc_pct, type = "o", pch = 17, col = "tan2", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
legend("topleft", legend = c("Secchi", "Chl", "TP", "TN", "NH4", "NO3", "Color", "DOC"), cex = 0.6,
       lty = c(1,1), pch = c(16, 17, 18), col = c("darkblue", "dodgerblue2", "lightskyblue1", "darkorchid4", 
                             "darkorchid2", "orchid1", "tan4", "tan2"),bty = "n")

## add axis ticks
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
## add the labels
axis(1, line = -0.7, lwd = 0, cex.axis = 0.7)
axis(2, line = -.4, lwd = 0, cex.axis = 0.7, las=1)
mtext(side=1,"Size bin", line=1.2)
#adjust the line value to move the y axis label in and out
#based on how many digits are in the y ticks
mtext(side=2, "Percent of lakes with data", line = 2.3)
dev.off()



##another version, separating variables into groups (1 plot/var group)
#this version had been considered as an alternative version of Fig. S3, but was ultimately was not used.
#however, keeping the code just in case.
tiff(filename = "SoL_graphics/fig_S2_v3.tiff",
     width = 3.5,
     height = 7,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

#Change mfrow for multi-panel plots but don't change margins
par(mfrow = c(3,1),
    oma = c(0,0,0,0),
    mar=c(3.5,4.5,.5,.5))

#trophic variables
plot(count$bin, count$secchi_pct, xlab = "", ylab = "", cex.axis = 1.2, las = 2,
     xaxt = "n", type = "o", pch = 16, col = "darkblue", ylim  = c(0, 100))
par(new=T)
plot(count$bin, count$chl_pct, type = "o", pch = 17, col = "dodgerblue2", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new=T)
plot(count$bin, count$tp_pct, type = "o", pch = 18, col = "lightskyblue1", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
legend("topleft", legend = c("Secchi", "Chl", "TP"), cex = 1.25,
       lty = c(1,1), pch = c(16, 17, 18), col = c("darkblue", "dodgerblue2", "lightskyblue1"),bty = "n")
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)

#N variables 
plot(count$bin, count$tn_pct, type = "o", pch = 16, col = "darkorchid4", ylim = c(0, 100), 
     xaxt = "n", xlab = "", ylab = "", cex.axis = 1.2, las = 2)
par(new = T)
plot(count$bin, count$nh4_pct, type = "o", pch = 17, col = "darkorchid2", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
par(new = T)
plot(count$bin, count$no3_pct, type = "o", pch = 18, col = "orchid1", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
legend("topleft", legend = c("TN", "NH4", "NO3"), cex = 1.25,
       lty = c(1,1), pch = c(16, 17, 18), col = c("darkorchid4","darkorchid2", "orchid1"),bty = "n")
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
mtext(side=2, "Percent of lakes with data", line = 3.0)

#C variables
plot(count$bin, count$color_pct, type = "o", pch = 16, col = "tan4", ylim = c(0, 100), 
     xaxt = "n", xlab = "", ylab = "", cex.axis = 1.2, las = 2)
axis(1, label = FALSE, tck = -0.015)
axis(1, lwd = 0, cex.axis = 1.2)
par(new = T)
plot(count$bin, count$doc_pct, type = "o", pch = 17, col = "tan2", ylim = c(0, 100), 
     axes = FALSE, xlab = "", ylab = "")
legend("topleft", legend = c("Color", "DOC"), cex = 1.25,
       lty = c(1,1), pch = c(16, 17), col = c("tan4", "tan2"),bty = "n")

mtext(side=1,"Size bin", line=2.5)
### adjust the line value to move the y axis label in and out
### based on how many digits are in the y ticks

dev.off()







