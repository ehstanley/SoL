
library(dplyr)
sol <-readRDS("SoL_data/SoL_data.rds")
lakes = readRDS("SoL_data/lakes.rds")

##Fig. S4- comparing % of lakes in each hydrologic category in the census population vs sampled lakes

#marking lagoslakeid's that have data (dat column = 1 in sol- df that contains the chem data)
sol$dat = 1
s1 = sol[, c("lagoslakeid", "dat")]
s1 = s1[!duplicated(s1$lagoslakeid),]

lakes$RT = lakes$iws_ha/lakes$lake_area_ha
L2 = left_join(lakes, s1, by = "lagoslakeid")
L2$status = ifelse(is.na(L2$dat == TRUE), 0, L2$dat)

type.all = table(L2$lakeconnection)
type.sol = table(L2$lakeconnection[L2$status==1])

type.all2 = c(12361, 42792, 21205, 64907)
type.sol2 = c(3973, 4737, 2416, 3105)
types = data.frame(cbind(type.all2, type.sol2))
types$allpct = (types$type.all2/141265)*100
types$solpct = (types$type.sol2/14231)*100



tiff(filename = "SoL_graphics/fig_S3.tiff",
     width = 3.5,
     height = 2,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

par(mfrow = c(1,2),
    oma = c(0,0,0,0),
    mar=c(2.4,3.5,.5,.5))

barplot(types$allpct, axis.lty=1, col= "grey", xlab= "", ylab = "", xaxt="n", yaxt="n", 
        ylim = c(0, 50))
axis(1, label = FALSE, at = c(0.7, 1.9, 3.1, 4.3),tck = -0.015)
labs <- c("DR-LS", "DR-S", "HW", "ISO")
text(c(0.7, 1.9, 3.1, 4.3), par("usr")[3] - 0.8, labels = labs, cex = 0.6, srt = 45, pos = 1, xpd = TRUE)    
axis(2, label = FALSE, tck = -0.015)
axis(2, line = -.4, lwd = 0, cex.axis = 0.9, las=1)
mtext(side=2, "Percent of lakes", cex= 1.2, line = 2.3)

barplot(types$solpct, axis.lty=1, col = "grey", xlab= "", ylab = "", xaxt="n", yaxt="n",
        ylim = c(0,50))
axis(1, label =   FALSE, at = c(0.7, 1.9, 3.1, 4.3), 
     cex.axis = 0.6, tck = -0.015)
text(c(0.7, 1.9, 3.1, 4.3), par("usr")[3] - 0.8, labels = labs, cex = 0.6, srt = 45, pos = 1, xpd = TRUE)
axis(2, label = FALSE, tck = -0.015)
axis(2, line = -.4, lwd = 0, cex.axis = 0.9, las=1)
labs <- c("DR-LS", "DR-S", "HW", "ISO")
dev.off()





