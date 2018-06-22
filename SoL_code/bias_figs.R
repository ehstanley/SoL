setwd("C:/Users/Nobody/Documents/LocalR/SoL/SoL_data")
library(dplyr)
lakes = readRDS("lakes.rds")
sol = readRDS("SoL_data.rds")

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
types$allpct = types$type.all2/141265
types$solpct = types$type.sol2/14231

par(mfrow=c(1,3))
par(mar = c(5, 4, 2, 1))
barplot(types$allpct, axis.lty=1, col= "darkblue", ylab = "Percent of lakes", cex.names = 0.5, 
        ylim = c(0, 0.5), main = "All lakes")
axis(side = 1, at = c(0:5), labels = c("", "DR_LS", "DR_S", "HW", "ISO", ""), cex.axis= 0.75)
barplot(types$solpct, axis.lty=1, col = "lightblue", cex.names = 0.5,
        ylim = c(0,0.5), main = "Lakes with data")
axis(side = 1, at = c(1:4), labels = c("DR_LS", "DR_S", "HW", "ISO"), cex.axis= 0.75)

area = aggregate(L2$lake_area_ha, by = list(L2$lakeconnection), FUN = mean)
area <- rename(area, Group = Group.1, Area = x)
barplot(area$Area, ylab= "Hectares", main = "Avg Lake Area")
axis(side = 1, at = c(1:4), labels = c("DR_LS", "DR_S", "HW", "ISO"), cex.axis= 0.75)


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


