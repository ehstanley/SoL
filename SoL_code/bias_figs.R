
library(dplyr)
sol <-readRDS("SoL_data/SoL_data.rds")
lakes = readRDS("SoL_data/lakes.rds")

#marking lagoslakeid's that have data (dat column = 1 in sol- df that contains the chem data)
sol$dat = 1
s1 = sol[, c("lagoslakeid", "dat")]
s1 = s1[!duplicated(s1$lagoslakeid),]

lakes$RT = lakes$iws_ha/lakes$lake_area_ha
L2 = left_join(lakes, s1, by = "lagoslakeid")
L2$status = ifelse(is.na(L2$dat == TRUE), 0, L2$dat)

type.all = table(L2$lakeconnection)
type.sol = table(L2$lakeconnection[L2$status==1])

barplot(type.all, xlab= "Lake Connectivity Type")
barplot(type.sol)

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

#par(mfrow=c(1,2))
#par(mar = c(5, 4, 2, 1))
barplot(types$allpct, axis.lty=1, col= "grey", xlab= "", ylab = "", xaxt="n", yaxt="n", ylim = c(0, 50))
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
axis(side = 1, at = c(1:4), labels = FALSE)
labs <- c("DR-LS", "DR-S", "HW", "ISO")
text(seq(1, 4, by=1), par("usr")[3] - 0.8, labels = labs, cex = 0.6, srt = 45, pos = 1, xpd = TRUE)
#axis(1, line = -0.7, lwd = 0, cex.axis = 0.9)
axis(2, line = -.4, lwd = 0, cex.axis = 0.9, las=1)
mtext(side=2, "Percent of lakes", cex= 1.2, line = 2.3)
#axis(side = 1, at = c(0:5), labels = c("", "DR_LS", "DR_S", "HW", "ISO", ""), cex.axis= 0.75)

barplot(types$solpct, axis.lty=1, col = "grey", xlab= "", ylab = "", xaxt="n", yaxt="n",
        ylim = c(0,50))
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
axis(side = 1, at = c(1:4), labels = FALSE)
labs <- c("DR-LS", "DR-S", "HW", "ISO")
text(seq(1, 4, by=1), par("usr")[3] - 0.8, labels = labs, cex = 0.6, srt = 45, pos = 1, xpd = TRUE)
axis(2, line = -.4, lwd = 0, cex.axis = 0.9, las=1)
#mtext(side=1,"Hydrologic categorgy", line=1.2)
dev.off()


## add the labels
#axis(1, line = -0.7, lwd = 0, cex.axis = 0.9)
#axis(2, line = -.4, lwd = 0, cex.axis = 0.9, las=1)
#mtext(side=1,"Hydrologic categorgy", line=1.2)
#adjust the line value to move the y axis label in and out
#based on how many digits are in the y ticks
#mtext(side=2, "", line = 2.3)



#area = aggregate(L2$lake_area_ha, by = list(L2$lakeconnection), FUN = mean)
#area <- rename(area, Group = Group.1, Area = x)
#barplot(area$Area, ylab= "Hectares", main = "Avg Lake Area")
#axis(side = 1, at = c(1:4), labels = c("DR_LS", "DR_S", "HW", "ISO"), cex.axis= 0.75)


par(mfrow= c(1,3))
hist(log10(L2$lake_area_ha), prob=TRUE, breaks = 20, col=rgb(1,0,0,0.5),xlim=c(0, 5), 
     main= "", xlab="Log10(Area)")
hist(log10(L2$lake_area_ha[L2$status==1]), prob=TRUE, breaks = 20, col=rgb(0,0,1,0.5), add=T)
box()

hist(log10(L2$maxdepth), prob=TRUE, breaks = 20, col=rgb(1,0,0,0.5),xlim=c(-1.0, 2.5), 
     main= "All lakes and lakes with data", xlab=" (Log10(Zmax)")
hist(log10(L2$maxdepth[L2$status==1]), prob=TRUE, breaks = 20, col=rgb(0,0,1,0.5), add=T)
box()

hist(log10(L2$RT), prob=TRUE, breaks = 20, col=rgb(1,0,0,0.5),xlim=c(-2.0, 6), 
     main= "", xlab=" (Log10(WA:LA)")
hist(log10(L2$RT[L2$status==1]), prob=TRUE, breaks = 20, col=rgb(0,0,1,0.5), add=T)
box()


