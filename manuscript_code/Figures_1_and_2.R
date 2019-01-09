###Code for Figs. 1, 2 and Tables 1, 2, S1
###May also contain some exploratory analyses not included in the Stanley et al. paper


library(dplyr)
library(tidyverse)
library(gridExtra)

sol <-readRDS("SoL_data/SoL_data.rds")


## Find stats of a variable by lake for each year, + plot of # of lakes sampled/yr
#secchi
SoLsecchi <- sol[!is.na(sol$secchi),]
lake.average.secchi = aggregate(SoLsecchi$secchi, SoLsecchi[,c("lagoslakeid", "year")], 
                                FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.secchi = data.frame(lagoslakeid = lake.average.secchi$lagoslakeid, year = lake.average.secchi$year, mean = lake.average.secchi$x[,1], sd = lake.average.secchi$x[,2], coef.var = lake.average.secchi$x[,3], med = lake.average.secchi$x[,4], nobs = lake.average.secchi$x[,5])


#chla
SoLchl <- sol[!is.na(sol$chla), ]
lake.average.chla = aggregate(SoLchl$chla, SoLchl[,c("lagoslakeid", "year")], 
                              FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.chla = data.frame(lagoslakeid = lake.average.chla$lagoslakeid, year = lake.average.chla$year, mean = lake.average.chla$x[,1], sd = lake.average.chla$x[,2], coef.var = lake.average.chla$x[,3], med = lake.average.chla$x[,4], nobs = lake.average.chla$x[,5])


#tp
SoLtp <- sol[!is.na(sol$tp), ]
lake.average.tp = aggregate(SoLtp$tp, SoLtp[,c("lagoslakeid", "year")], 
                            FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.tp = data.frame(lagoslakeid = lake.average.tp$lagoslakeid, year = lake.average.tp$year, mean = lake.average.tp$x[,1], sd = lake.average.tp$x[,2], coef.var = lake.average.tp$x[,3], med = lake.average.tp$x[,4], nobs = lake.average.tp$x[,5])


#NH4-N
SoLnh4 <- sol[!is.na(sol$nh4), ]
lake.average.nh4 = aggregate(SoLnh4$nh4, SoLnh4[,c("lagoslakeid", "year")], 
                             FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.nh4 = data.frame(lagoslakeid = lake.average.nh4$lagoslakeid, year = lake.average.nh4$year, mean = lake.average.nh4$x[,1], sd = lake.average.nh4$x[,2], coef.var = lake.average.nh4$x[,3], med = lake.average.nh4$x[,4], nobs = lake.average.nh4$x[,5])


#NO3-N
SoLno2no3 <- sol[!is.na(sol$no2no3), ]
lake.average.no2no3 = aggregate(SoLno2no3$no2no3, SoLno2no3[,c("lagoslakeid", "year")], 
                                FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.no2no3 = data.frame(lagoslakeid = lake.average.no2no3$lagoslakeid, year = lake.average.no2no3$year, mean = lake.average.no2no3$x[,1], sd = lake.average.no2no3$x[,2], coef.var = lake.average.no2no3$x[,3], med = lake.average.no2no3$x[,4], nobs = lake.average.no2no3$x[,5])


#TN
SoLtn <- sol[!is.na(sol$tn_combined), ]
lake.average.tn = aggregate(SoLtn$tn_combined, SoLtn[,c("lagoslakeid", "year")], 
                            FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.tn = data.frame(lagoslakeid = lake.average.tn$lagoslakeid, year = lake.average.tn$year, mean = lake.average.tn$x[,1], sd = lake.average.tn$x[,2], coef.var = lake.average.tn$x[,3], med = lake.average.tn$x[,4], nobs = lake.average.tn$x[,5])


#DOC
SoLdoc <- sol[!is.na(sol$doc), ]
lake.average.doc = aggregate(SoLdoc$doc, SoLdoc[,c("lagoslakeid", "year")], 
                             FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.doc = data.frame(lagoslakeid = lake.average.doc$lagoslakeid, year = lake.average.doc$year, mean = lake.average.doc$x[,1], sd = lake.average.doc$x[,2], coef.var = lake.average.doc$x[,3], med = lake.average.doc$x[,4], nobs = lake.average.doc$x[,5])


#color
SoLcolor <- sol[!is.na(sol$colort), ]
lake.average.color = aggregate(SoLcolor$colort, SoLcolor[,c("lagoslakeid", "year")], 
                               FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.color = data.frame(lagoslakeid = lake.average.color$lagoslakeid, year = lake.average.color$year, mean = lake.average.color$x[,1], sd = lake.average.color$x[,2], coef.var = lake.average.color$x[,3], med = lake.average.color$x[,4], nobs = lake.average.color$x[,5])

#total number of observations/variable-For Table 1
chl.N = sum(lake.average.chla$nobs)
color.N = sum(lake.average.color$nobs)
doc.N = sum(lake.average.doc$nobs)
nh4.N = sum(lake.average.nh4$nobs)
no2no3.N = sum(lake.average.no2no3$nobs)
secchi.N = sum(lake.average.secchi$nobs)
tn.N = sum(lake.average.tn$nobs)
tp.N = sum(lake.average.tp$nobs)


#get lakes that were only sampled one time EVER- For Table 2
chl_1 <-SoLchl %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
chl.once<- chl_1[which(chl_1$n_obs ==1),]

color_1 <-SoLcolor %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
color.once<- color_1[which(color_1$n_obs ==1),]

doc_1 <-SoLdoc %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
doc.once<- doc_1[which(doc_1$n_obs ==1),]

nh4_1 <-SoLnh4 %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
nh4.once<- nh4_1[which(nh4_1$n_obs ==1),]

no2no3_1 <-SoLno2no3 %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
no2no3.once<- no2no3_1[which(no2no3_1$n_obs ==1),]

secchi_1 <-SoLsecchi %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
secchi.once<- secchi_1[which(secchi_1$n_obs ==1),]

tn_1 <-SoLtn %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
tn.once<- tn_1[which(tn_1$n_obs ==1),]

tp_1 <-SoLtp %>% group_by(lagoslakeid) %>% summarise(n_obs = n_distinct(sampledate))
tp.once<- tp_1[which(tp_1$n_obs ==1),]

###calculate a lake's grand median from yearly medians so all years are equal
#lake grand medians calculated for each lake that was sampled for variables of interest during the study period

Grand.med.secchi <-aggregate(lake.average.secchi$med, list(lake.average.secchi$lagoslakeid), median)
names(Grand.med.secchi) <-c("lagoslakeid", "Median_secchi")

Grand.med.chl <-aggregate(lake.average.chla$med, list(lake.average.chla$lagoslakeid), median)
names(Grand.med.chl) <-c("lagoslakeid", "Median_chl")

Grand.med.color <-aggregate(lake.average.color$med, list(lake.average.color$lagoslakeid), median)
names(Grand.med.color) <-c("lagoslakeid", "Median_color")

Grand.med.doc <-aggregate(lake.average.doc$med, list(lake.average.doc$lagoslakeid), median)
names(Grand.med.doc) <-c("lagoslakeid", "Median_doc")

Grand.med.nh4 <-aggregate(lake.average.nh4$med, list(lake.average.nh4$lagoslakeid), median)
names(Grand.med.nh4) <-c("lagoslakeid", "Median_nh4")

Grand.med.no2no3 <-aggregate(lake.average.no2no3$med, list(lake.average.no2no3$lagoslakeid), median)
names(Grand.med.no2no3) <-c("lagoslakeid", "Median_no2no3")

Grand.med.tn <-aggregate(lake.average.tn$med, list(lake.average.tn$lagoslakeid), median)
names(Grand.med.tn) <-c("lagoslakeid", "Median_tn")

Grand.med.tp <-aggregate(lake.average.tp$med, list(lake.average.tp$lagoslakeid), median)
names(Grand.med.tp) <-c("lagoslakeid", "Median_tp")


### merge averages and add in lake data on lat, long,lake area, state
Med1 <- full_join(Grand.med.secchi, Grand.med.chl, by = NULL, copy=FALSE)
Med2 <- full_join(Med1, Grand.med.color, by = NULL, copy=FALSE)
Med3 <- full_join(Med2, Grand.med.doc, by = NULL, copy=FALSE)
Med4 <- full_join(Med3, Grand.med.nh4, by = NULL, copy=FALSE)
Med5 <- full_join(Med4, Grand.med.no2no3, by = NULL, copy=FALSE)
Med6 <- full_join(Med5, Grand.med.tn, by = NULL, copy=FALSE)
Med10 <- full_join(Med6, Grand.med.tp, by = NULL, copy=FALSE)


# add in state names and lake attributes
lakes = readRDS("SoL_data/lakes.rds")

SoL.Med <- merge(Med10, lakes, by = "lagoslakeid")

#Getting a count of data diversity- # of lakes with any data for any of the 8 variables
#i.e., do they have any info on 1 variable, 2 variables...all 8 variables?
chem.Med <- SoL.Med[, c(1:9)]
chem.Med$diversity <- rowSums(!is.na(chem.Med))
chem.Med$diversity = chem.Med$diversity - 1

#removing 1 lake which had no data for any of the 8 variables. 
#it probably has data for a variable that got excluded
chem.Med <- chem.Med[which(chem.Med$diversity != 0),]    

#quick table summarizing #, % of lakes by variable count
var_div <-data.frame(table(chem.Med$diversity))
var_div <- rename(var_div, Num.Variables = Var1)
var_div$pct <- (var_div$Freq/14228) *100
barplot(var_div$pct)


#calcs for Table 1-2, summarizing # total lakes sampled, # lakes sampled 1X, their percentages
Variable = c("Secchi", "chla", "color", "doc", "nh4", "no2no3", "tn", "tp")
nlakes = c(length(Grand.med.secchi$lagoslakeid), length(Grand.med.chl$lagoslakeid), 
           length(Grand.med.color$lagoslakeid), length(Grand.med.doc$lagoslakeid), 
           length(Grand.med.nh4$lagoslakeid), length(Grand.med.no2no3$lagoslakeid), 
           length(Grand.med.tn$lagoslakeid), length(Grand.med.tp$lagoslakeid))

table2 <- data.frame(cbind(Variable, nlakes))
table2$nlakes <- as.numeric(levels(table2$nlakes))[table2$nlakes]
table2$pct_all = (table2$nlakes/141265)*100
once = c(length(secchi.once$lagoslakeid), length(chl.once$lagoslakeid), 
         length(color.once$lagoslakeid), length(doc.once$lagoslakeid),
         length(nh4.once$lagoslakeid), length(no2no3.once$lagoslakeid), 
         length(tn.once$lagoslakeid), length(tp.once$lagoslakeid))
table2$once = cbind(once)
table2$pct_once <- (table2$once/table2$nlakes)*100

N.data = c(secchi.N, chl.N, color.N, doc.N, nh4.N, no2no3.N, tn.N, tp.N)
table2$N_data = cbind(N.data)
table2$pct_data = (table2$N_data/(sum(N.data)))*100

# Number of lakes sampled/year by variable
chl.nlakes.yr <- count(lake.average.chla, year)
chl.nlakes.yr <- rename(chl.nlakes.yr, chl = n)
color.nlakes.yr <- count(lake.average.color, year)
color.nlakes.yr <- rename(color.nlakes.yr, color = n)
doc.nlakes.yr <- count(lake.average.doc, year)
doc.nlakes.yr <- rename(doc.nlakes.yr, doc = n)
nh4.nlakes.yr <- count(lake.average.nh4, year)
nh4.nlakes.yr <- rename(nh4.nlakes.yr, nh4 = n)
no2no3.nlakes.yr <- count(lake.average.no2no3, year)
no2no3.nlakes.yr <- rename(no2no3.nlakes.yr, no2no3 = n)
secchi.nlakes.yr <- count(lake.average.secchi, year)
secchi.nlakes.yr <- rename(secchi.nlakes.yr, secchi = n)
tn.nlakes.yr <- count(lake.average.tn, year)
tn.nlakes.yr <- rename(tn.nlakes.yr, tn = n)
tp.nlakes.yr <- count(lake.average.tp, year)
tp.nlakes.yr <- rename(tp.nlakes.yr, tp = n)


#putting timeline of lakes sampled each year together into 1 data.frame 
YR <- data.frame(c(1933:2010))
YR <-rename(YR, year = c.1933.2010.)
YR1 <-left_join(YR, chl.nlakes.yr, by = "year") 
YR1 <-left_join(YR1, color.nlakes.yr, by = "year")
YR1 <-left_join(YR1, doc.nlakes.yr, by = "year")
YR1 <-left_join(YR1, nh4.nlakes.yr, by = "year")
YR1 <-left_join(YR1, no2no3.nlakes.yr, by = "year")
YR1 <-left_join(YR1, secchi.nlakes.yr, by = "year")
YR1 <-left_join(YR1, tp.nlakes.yr, by = "year")
YR1 <-left_join(YR1, tn.nlakes.yr, by = "year")
#missing data issue- replace NAs with zero
YR1 <- data.frame(replace(YR1, is.na(YR1), 0))
YR1$total <- YR1$chl+YR1$color+YR1$doc+YR1$nh4+YR1$no2no3+YR1$secchi+YR1$tn+YR1$tp+YR1$tn
#plot(YR1$year, YR$total)


N_lakes <- summarise_all(YR1, funs(sum))
N_lakes <- N_lakes[, c("chl", "color", "doc", "nh4", "no2no3", "secchi", "tp", "tn")]
N <- as.numeric(N_lakes[1,])
var <- names(N_lakes) 
L.by.var <- data.frame(N, var)
L2 <- L.by.var[order(-N),]
#a first version of sampling effort of number of lakes sampled/year by variable graphic
barplot(L2$N, col = "blue", ylab = "Sum of lakes/yr")
xtick<-c("Secchi", "TP", "Chla", "NO3", "TN", "NH4", "Color", "DOC")
axis(side=1, at=seq(1, 8, by=1), labels = xtick, las = 2)

N.totals = c(chl.N, color.N, doc.N, nh4.N, no2no3.N, secchi.N, tn.N, tp.N)
N.totals = sort(N.totals, decreasing = TRUE)

#a first version of sampling effort by variable graphic
barplot(N.totals, ylab = "Total data count", ylim = c(0, 750000),col = "darkseagreen")
xtick<-c("Secchi", "Chl", "TP", "NO3", "TN","NH4", "Color", "DOC")
axis(side=1, at=seq(1, 8, by=1), labels = xtick, las = 2)


#Fig. 2- lakes visited/year by variable over time
tiff(filename = "SoL_graphics/fig2v2.tiff",
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

plot(YR1$year, YR1$secchi, xlim = c(1950, 2010), ylim = c(0, 4500), xlab = "", 
     ylab = "", xaxt = "n", yaxt = "n", type = "l", lwd = 1.5, col = "darkblue")
par(new=T)
plot(YR1$year, YR1$chl, xlim = c(1950, 2010), ylim = c(0,4500), xaxt='n', xlab = "",
     yaxt='n', ylab= "", type = "l", lwd = 1.5, col= "dodgerblue2")
par(new=TRUE)
plot(YR1$year, YR1$tp, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     yaxt='n', ylab = "", type = "l", lwd = 1.5, col= "lightskyblue1")
par(new=T)
plot(YR1$year, YR1$tn, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     yaxt='n', ylab = "", type = "l", lwd = 1.5, col = "darkorchid4")
par(new=T)
plot(YR1$year, YR1$no2no3,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "l", lwd = 1.5, col= "darkorchid2")
par(new=T)
plot(YR1$year, YR1$nh4,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n',xlab = "", 
     yaxt='n', ylab = "", type = "l", lwd = 1.5, col= "orchid1") 
par(new=T)
plot(YR1$year, YR1$color,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n',xlab = "", 
     yaxt='n', ylab = "", type = "l", lwd = 1.5, col="tan4")
par(new=T)
plot(YR1$year, YR1$doc, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "l", lwd = 1.5, col = "tan2")
legend("topleft", legend = c("Secchi", "Chl", "TP", "TN", "NO3", "NH4", "Color", "DOC"), cex = 0.6,
       lty = c(1,1), col = c("darkblue", "dodgerblue2", "lightskyblue1", "darkorchid4", 
                             "darkorchid2", "orchid1", "tan4", "tan2"), bty = "n")

## add axis ticks
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
## add the labels
axis(1, line = -0.7, lwd = 0, cex.axis = 0.7)
axis(2, line = -.4, lwd = 0, cex.axis = 0.7, las=1)
mtext(side=1,"Year", line=1.2)
#adjust the line value to move the y axis label in and out
#based on how many digits are in the y ticks
mtext(side=2, "Number of lakes", line = 2.3)
dev.off()


####A 3rd version following reviewer request to separate the plot out into the 3 variable groups
##did not include this in the revised ms version
tiff(filename = "SoL_graphics/fig2v3.tiff",
     width = 3.5,
     height = 7,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

#Change mfrow for multi-panel plots but don't change margins
par(mfrow = c(3,1),
    oma = c(0,0,0,0),
    mar=c(3.5,6,.25,.5))
    
#trophic variables
plot(YR1$year, YR1$secchi, xlim = c(1950, 2010), ylim = c(0, 4500), xlab = "", 
     ylab = "", cex.axis = 1.2, las = 2, xaxt = "n", type = "l", lwd = 1.5, col = "darkblue")
par(new=T)
plot(YR1$year, YR1$chl, xlim = c(1950, 2010), ylim = c(0,4500), axes = FALSE, 
     type = "l", lwd = 1.5, col= "dodgerblue2", xlab = "", ylab = "")
par(new=TRUE)
plot(YR1$year, YR1$tp, xlim = c(1950, 2010), ylim = c(0, 4500), axes = FALSE, xlab = "", ylab = "",
    type = "l", lwd = 1.5, col= "lightskyblue1")
legend("topleft", legend = c("Secchi", "Chl", "TP"), cex = 1.25,
       lty = c(1,1), col = c("darkblue", "dodgerblue2", "lightskyblue1"), bty = "n")
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)


#N variables
plot(YR1$year, YR1$tn, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     ylab = "", cex.axis = 1.2, las = 2, type = "l", lwd = 1.5, col = "darkorchid4")
par(new=T)
plot(YR1$year, YR1$no2no3,  xlim = c(1950, 2010), ylim = c(0, 4500), axes = FALSE, xlab = "", ylab = "",
     type = "l", lwd = 1.5, col= "darkorchid2")
par(new=T)
plot(YR1$year, YR1$nh4,  xlim = c(1950, 2010), ylim = c(0, 4500), axes = FALSE, xlab = "", ylab = "",
     type = "l", lwd = 1.5, col= "orchid1") 
legend("topleft", legend = c("TN", "NO3", "NH4"), cex = 1.25,
       lty = c(1,1), col = c("darkorchid4", "darkorchid2", "orchid1"), bty = "n")
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
mtext(side=2, "Number of lakes", line = 4.0, cex = 1.2)


#C variables
plot(YR1$year, YR1$color,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n',xlab = "",  
     ylab = "", cex.axis = 1.2, las = 2, type = "l", lwd = 1.5, col="tan4")
par(new=T)
plot(YR1$year, YR1$doc, xlim = c(1950, 2010), ylim = c(0, 4500),axes = FALSE, xlab = "", ylab = "",  
     type = "l", lwd = 1.5, col = "tan2")
legend("topleft", legend = c("Color", "DOC"), cex = 1.25,
       lty = c(1,1), col = c("tan4", "tan2"), bty = "n")
axis(1, label = FALSE, tck = -0.015)
axis(1, lwd = 0, cex.axis = 1.2)
#axis(2, label = FALSE, tck = -0.015)
#axis(2, lwd = 0, cex.axis = 1.2)
mtext(side=1,"Year", line=2.5, cex = 1.2)

dev.off()




###Fig. 1 code
#sampling effort by day of year- how many data points are there for each day of year?
secchi.day <- as.data.frame(table(SoLsecchi$day))
names(secchi.day) <- c("Yr.day", "secchi_f")
secchi.day$Yr.day <- as.integer(secchi.day$Yr.day)

chl.day <- as.data.frame(table(SoLchl$day))
names(chl.day) <- c("Yr.day", "chl_f")
chl.day$Yr.day <- as.integer(chl.day$Yr.day)

color.day <- as.data.frame(table(SoLcolor$day))
names(color.day) <- c("Yr.day", "color_f")
color.day$Yr.day <- as.integer(color.day$Yr.day)

doc.day <- as.data.frame(table(SoLdoc$day))
names(doc.day) <- c("Yr.day", "doc_f")
doc.day$Yr.day <- as.integer(doc.day$Yr.day)

nh4.day <- as.data.frame(table(SoLnh4$day))
names(nh4.day) <- c("Yr.day", "nh4_f")
nh4.day$Yr.day <- as.integer(nh4.day$Yr.day)

no2no3.day <- as.data.frame(table(SoLno2no3$day))
names(no2no3.day) <- c("Yr.day", "no2no3_f")
no2no3.day$Yr.day <- as.integer(no2no3.day$Yr.day)

tn.day <- as.data.frame(table(SoLtn$day))
names(tn.day) <- c("Yr.day", "tn_f")
tn.day$Yr.day <- as.integer(tn.day$Yr.day)

tp.day <- as.data.frame(table(SoLtp$day))
names(tp.day) <- c("Yr.day", "tp_f")
tp.day$Yr.day <- as.integer(tp.day$Yr.day)


#now combining sampling effort/variable by day for all variables
#and a couple of group  plots
Jdays <- data.frame(c(1:365))
names(Jdays) <-c("Yr.day")
Jday2 <-left_join(Jdays, secchi.day, by = "Yr.day")
Jday2 <- left_join(Jday2, chl.day, by = "Yr.day")
Jday2 <- left_join(Jday2, color.day, by = "Yr.day")
Jday2 <- left_join(Jday2, doc.day, by = "Yr.day")
Jday2 <- left_join(Jday2, nh4.day, by = "Yr.day")
Jday2 <- left_join(Jday2, no2no3.day, by = "Yr.day")
Jday2 <- left_join(Jday2, tn.day, by = "Yr.day")
Jday2 <- left_join(Jday2, tp.day, by = "Yr.day")


## ggplot version stacked vertically
jday_long <- tidyr::gather(Jday2, key = 'variable', value = 'nobs', -Yr.day) %>%
  mutate(variable = gsub('_f', '', variable)) %>%
  mutate(variable_type = case_when(
    variable %in% c('secchi', 'chl', 'tp') ~ 'Trophic',
    variable %in% c('color', 'doc') ~ 'Carbon',
    variable %in% c('tn', 'nh4', 'no2no3') ~ 'Nitrogen'))
head(jday_long)

jday_long$variable_type <- factor(jday_long$variable_type, levels = c('Trophic', 'Nitrogen', 'Carbon'))
jday_long$variable <- factor(jday_long$variable, levels = c('secchi', 'chl', 'tp', 'tn', 'nh4', 'no2no3', 'color', 'doc'))
jday_long <- mutate(jday_long, variable = recode(variable, secchi = 'Secchi',
                                                 chl = 'Chl a', 
                                                 tp = 'TP', 
                                                 tn = 'TN',
                                                 nh4 = 'NH4',
                                                 no2no3 = 'NO3',
                                                 color = 'Color', 
                                                 doc = 'DOC'))
jday_long <- jday_long %>% mutate(y_max=NA) %>% 
  mutate(y_max = replace(y_max,variable_type=="Trophic",values = 6000)) %>% 
  mutate(y_max = replace(y_max,variable_type=="Nitrogen",values = 600)) %>%
  mutate(y_max = replace(y_max,variable_type=="Carbon",values = 400))

dat.split <- split(jday_long,f=jday_long$variable_type)
p1 <- ggplot(dat.split$Trophic, aes(x = Yr.day, y = nobs)) +
  geom_line(aes(color = variable, group = variable)) +
  facet_wrap(~variable_type, ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  labs(x = '', y = '', color = '') +
  scale_color_manual(values =  c("darkblue", 
                                 "dodgerblue2", 
                                 "lightskyblue1", 
                                 "darkorchid4", 
                                 "orchid1", 
                                 "darkorchid2", 
                                 "tan4", 
                                 "tan2")) +
  geom_vline(xintercept = c(166, 258), linetype = 3) +
  geom_blank(aes(y = y_max)) + 
  theme(text=element_text(size=10,  family="sans")) + 
  theme(legend.position = c(0.12, 0.8),
        legend.background=element_blank(),
        legend.key=element_blank()) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.margin = unit(c(0, .2, .2, .2), "cm"))
p1
p2 <- ggplot(dat.split$Nitrogen, aes(x = Yr.day, y = nobs)) +
  geom_line(aes(color = variable, group = variable)) +
  facet_wrap(~variable_type, ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  labs(x = '', y = 'Number of data points', color = '') +
  scale_color_manual(values =  c("darkorchid4", 
                                 "orchid1", 
                                 "darkorchid2", 
                                 "tan4", 
                                 "tan2")) +
  geom_vline(xintercept = c(166, 258), linetype = 3) +
  geom_blank(aes(y = y_max)) + 
  theme(text=element_text(size=10,  family="sans")) + 
  theme(legend.position = c(0.12, 0.8),
        legend.background=element_blank(),
        legend.key=element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(plot.margin = unit(c(0, .2, .2, .2), "cm"))
p2
p3 <- ggplot(dat.split$Carbon, aes(x = Yr.day, y = nobs)) +
  geom_line(aes(color = variable, group = variable)) +
  facet_wrap(~variable_type, ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  labs(x = 'Day of the year', y = '', color = '') +
  scale_color_manual(values =  c("tan4", 
                                 "tan2")) +
  geom_vline(xintercept = c(166, 258), linetype = 3) +
  geom_blank(aes(y = y_max)) + 
  theme(text=element_text(size=10,  family="sans")) + 
  theme(legend.position = c(0.12, 0.89),
        legend.background=element_blank(),
        legend.key=element_blank()) +
  theme(plot.margin = unit(c(0, .2, .1, .2), "cm"))
p3

# p.out <- grid.arrange(p1,p2,p3,heights=c(1,1,1),ncol=1)
p.out <- plot_grid(p1,p2,p3,ncol=1,align = "hv")
p.out <- ggarrange(p1,p2,p3,ncol=1) #egg package
ggsave(filename = "SoL_graphics/doy_vertical.tiff",
       plot = p.out,
       device = "tiff",
       width = 3.5,
       height = 5,
       dpi = 300,
       units = "in",
       compression="lzw")


#Table 2 code- length of records and # of records with 20+ years
chl_yr <-SoLchl %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
chl_20 <- chl_yr[which(chl_yr$n_yrs > 19),]
chl_20 <- left_join(chl_20, lakes, by = "lagoslakeid")

color_yr <-SoLcolor %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
color_20 <- color_yr[which(color_yr$n_yrs > 19),]
color_20  <- left_join(color_20, lakes, by = "lagoslakeid")

doc_yr <-SoLdoc %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
doc_20 <- doc_yr[which(doc_yr$n_yrs > 19),]
doc_20 <- left_join(doc_20, lakes, by = "lagoslakeid")

nh4_yr <-SoLnh4 %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
nh4_20 <- nh4_yr[which(nh4_yr$n_yrs > 19),]
nh4_20 <- left_join(nh4_20, lakes, by = "lagoslakeid")

no2no3_yr <-SoLno2no3 %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
no2no3_20 <- no2no3_yr[which(no2no3_yr$n_yrs > 19),]
no2no3_20 <- left_join(no2no3_20, lakes, by = "lagoslakeid")

secchi_yr <-SoLsecchi %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
secchi_20 <- secchi_yr[which(secchi_yr$n_yrs > 19),]
secchi_20 <- left_join(secchi_20, lakes, by = "lagoslakeid")

tn_yr <-SoLtn %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
tn_20 <- tn_yr[which(tn_yr$n_yrs > 19),]
tn_20 <- left_join(tn_20, lakes, by = "lagoslakeid")

tp_yr <-SoLtp %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
tp_20 <- tp_yr[which(tp_yr$n_yrs > 19),]
tp_20 <- left_join(tp_20, lakes, by = "lagoslakeid")

#For Table S1- 20-year records for each LAGOS state
chl_x <- table(chl_20$state_name)
color_x <-table(color_20$state_name)
doc_x <- table(doc_20$state_name)
nh4_x <- table(nh4_20$state_name)
no2no3_x <- table(no2no3_20$state_name)
secchi_x <- table(secchi_20$state_name)
tn_x <- table(tn_20$state_name)
tp_x <- table(tp_20$state_name)


