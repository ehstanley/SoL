rm(list=ls())
cat("\14")

library(dplyr)
library(maps)

sol <-readRDS("SoL_data/SoL_data.rds")  #desktop


## find stats of a variable by lake for each year, + plot of # of lakes sampled/yr
##Code borrowed from SKO
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


chl.N = sum(lake.average.chla$nobs)
color.N = sum(lake.average.color$nobs)
doc.N = sum(lake.average.doc$nobs)
nh4.N = sum(lake.average.nh4$nobs)
no2no3.N = sum(lake.average.no2no3$nobs)
secchi.N = sum(lake.average.secchi$nobs)
tn.N = sum(lake.average.tn$nobs)
tp.N = sum(lake.average.tp$nobs)


#get lakes that were only sampled one time EVER
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

#save file 
#saveRDS(SoL.Med, file = ("SoL_data/SoL_medians.rds"))

#Getting a count of data diversity- # of lakes with any data for any of the 8 variables
#i.e., do they have any info on 1 variable, 2 variables...all 8 variables?
chem.Med <- SoL.Med[, c(1:9)]
chem.Med$diversity <- rowSums(!is.na(chem.Med))
chem.Med$diversity = chem.Med$diversity - 1

#removing 1 lake which had no data for any of the 8 variables. 
#it probably has data for one of the excluded variables.
chem.Med <- chem.Med[which(chem.Med$diversity != 0),]    

#quick little  table summarizing #, % of lakes by variable count
var_div <-data.frame(table(chem.Med$diversity))
var_div <- rename(var_div, Num.Variables = Var1)
var_div$pct <- (var_div$Freq/14228) *100
barplot(var_div$pct)


#calcs for potential Table 1-2, summarizing # total lakes sampled, # lakes sampled 1X, their percentages
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
#srp.nlakes.yr <- count(lake.average.srp, year)
#srp.nlakes.yr <- rename(srp.nlakes.yr, srp = n)
#tkn.nlakes.yr <- count(lake.average.tkn, year)
#tkn.nlakes.yr <-rename(tkn.nlakes.yr, tkn = n)
tn.nlakes.yr <- count(lake.average.tn, year)
tn.nlakes.yr <- rename(tn.nlakes.yr, tn = n)
tp.nlakes.yr <- count(lake.average.tp, year)
tp.nlakes.yr <- rename(tp.nlakes.yr, tp = n)
#tnmeasured.nlakes.yr <- count(lake.average.tn_measured, year)
#tnmeasured.nlakes.yr <- rename(tnmeasured.nlakes.yr, tnmeasured = n)

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
barplot(L2$N, col = "blue", ylab = "Sum of lakes/yr")
xtick<-c("Secchi", "TP", "Chla", "NO3", "TN", "NH4", "Color", "DOC")
axis(side=1, at=seq(1, 8, by=1), labels = xtick, las = 2)

N.totals = c(chl.N, color.N, doc.N, nh4.N, no2no3.N, secchi.N, tn.N, tp.N)
N.totals = sort(N.totals, decreasing = TRUE)

barplot(N.totals, ylab = "Total data count", ylim = c(0, 750000),col = "darkseagreen")
xtick<-c("Secchi", "Chl", "TP", "NO3", "TN","NH4", "Color", "DOC")
axis(side=1, at=seq(1, 8, by=1), labels = xtick, las = 2)



#plot of lakes visited/year by variable over time
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


plot(Jday2$Yr.day, Jday2$secchi_f,  ylim = c(0, 7000), xlab = "Day of the Year",
     ylab= "Data Count", cex.lab = 1.5, type = "l", col= "red")
par(new=TRUE)
plot(Jday2$Yr.day, Jday2$chl_f,  ylim = c(0, 7000), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col= "green")
par(new=T)
plot(Jday2$Yr.day, Jday2$tp_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", axes = FALSE, type = "l", col= "blue")
legend("topleft", legend = c("Secchi", "Chl a", "TP"), 
       lty=c(1,1), col = c("red", "green", "blue"))
abline(v = 152, lty = 5, col = "black")
abline(v = 273, lty = 5, col = "black")


plot(Jday2$Yr.day, Jday2$tn_f,  ylim = c(0, 600), xlab = "Day of the Year", cex.lab = 1.5,
     ylab = "", type = "l", col= "black")
par(new=T)
plot(Jday2$Yr.day, Jday2$nh4_f,  ylim = c(0, 600), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col= "grey")
par(new=T)
plot(Jday2$Yr.day, Jday2$no2no3_f,  ylim = c(0, 600), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col= "cyan")
legend("topleft", legend = c("TN", "NH4", "NO3"), lty=c(1,1), col = c("black", "grey", "cyan"))
abline(v = 152, lty = 2, col = "black")
abline(v = 273, lty = 2, col = "black")


plot(Jday2$Yr.day, Jday2$color_f,  ylim = c(0, 600), axes = FALSE, xlab = "", 
     ylab = "", type = "l", col= "orange")
par(new=T)
plot(Jday2$Yr.day, Jday2$doc_f,  ylim = c(0, 600), xlab = "Day of the Year", cex.lab = 1.5,
     ylab = "", type = "l", col= "brown")
legend("topleft", legend = c("Color", "DOC"),
       lty=c(1,1), col = c("orange", "brown"))
abline(v = 152, lty = 5, col = "black")
abline(v = 273, lty = 5, col = "black")





#length of records and # of records with 20+ years
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

chl_x <- table(chl_20$state_name)
color_x <-table(color_20$state_name)
doc_x <- table(doc_20$state_name)
nh4_x <- table(nh4_20$state_name)
no2no3_x <- table(no2no3_20$state_name)
secchi_x <- table(secchi_20$state_name)
tn_x <- table(tn_20$state_name)
tp_x <- table(tp_20$state_name)


######some old mapping efforts#####
#yes, there's a much easier way to do this, but making some data frames for mapping
#maps of all lakes that have data for variable X
chl.sites <- SoL.Med[!is.na(SoL.Med$Median_chl),]
chl.sites <- chl.sites[, 10:11]
TP.sites <- SoL.Med[!is.na(SoL.Med$Median_tp),]
TP.sites <- TP.sites[, 10:11]
secchi.sites <- SoL.Med[!is.na(SoL.Med$Median_secchi),]
secchi.sites <- secchi.sites[, 10:11]
TN.sites <- SoL.Med[!is.na(SoL.Med$Median_tn),]
TN.sites <- TN.sites[, 10:11]
DOC.sites <- SoL.Med[!is.na(SoL.Med$Median_doc),]
DOC.sites <- DOC.sites[, 10:11]
color.sites <-SoL.Med[!is.na(SoL.Med$Median_color),]
color.sites <- color.sites[, 10:11]
no3.sites <-SoL.Med[!is.na(SoL.Med$Median_no2no3),]
no3.sites <- no3.sites[, 10:11]
nh4.sites <-SoL.Med[!is.na(SoL.Med$Median_nh4),]
nh4.sites <- nh4.sites[, 10:11]
srp.sites <-SoL.Med[!is.na(SoL.Med$Median_srp),]
srp.sites <- srp.sites[, 10:11]
tkn.sites <-SoL.Med[!is.na(SoL.Med$Median_tkn),]
tkn.sites <- tkn.sites[, 10:11]
tnmeasured.sites <-SoL.Med[!is.na(SoL.Med$Median_tn_measured),]
tnmeasured.sites <- tnmeasured.sites[, 10:11]


## create a map of where data come from values 
#and yes, I should write a function....
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(chl.sites$nhd_long, chl.sites$nhd_lat, cex = 0.1, pch=20, col ="darkgreen")
title("Chl a (n= 8525 lakes)")

points(TP.sites$nhd_long, TP.sites$nhd_lat, cex = .1, pch=20, col = "blue")
title("TP (n = 10490 lakes)")

points(secchi.sites$nhd_long, secchi.sites$nhd_lat, cex = .1, pch=20, col= "red")
title("Secchi Depth (n = 12377 lakes)")

points(TN.sites$nhd_long, TN.sites$nhd_lat, cex = .1, pch=20, col = "darkgoldenrod4")
title("TN (n = 6553 lakes)")

points(DOC.sites$nhd_long, DOC.sites$nhd_lat, cex = .1, pch=20, col = "brown")
title("DOC (n = 4997 lakes)")

points(color.sites$nhd_long, color.sites$nhd_lat, cex = .1, pch=20, col = "orange")
title("True Color (n = 5636 lakes)")

points(nh4.sites$nhd_long, nh4.sites$nhd_lat, cex = .1, pch=20, col = "darkgray")
title("NH4-N (n = 6502 lakes)")

points(no3.sites$nhd_long, no3.sites$nhd_lat, cex = .1, pch=20, col = "cyan")
title("NO2-NO3-N (n = 8173 lakes)")

points(tnmeasured.sites$nhd_long, tnmeasured.sites$nhd_lat, cex = .1, pch=20, col = "salmon")
title("TN measured (n = 2772 lakes)")


all.sites <-rbind(secchi.sites, chl.sites, color.sites, DOC.sites, nh4.sites, no3.sites, srp.sites, TN.sites, TP.sites)
all.sites2 <-unique(all.sites)
points(all.sites2$nhd_long, all.sites2$nhd_lat, cex = .1, pch=20, col = "black")
title("All lakes with any data (n = 14229)")

troph.sites <-rbind(secchi.sites, chl.sites, TP.sites)
tr.sites <-unique(troph.sites)
points(tr.sites$nhd_long, tr.sites$nhd_lat, cex = .1, pch=20, col = "limegreen")
title("All lakes with any Secchi, chl, TP data (n = 13954)")

carbon.sites <-rbind(color.sites, DOC.sites)
c.sites <-unique(carbon.sites)
points(c.sites$nhd_long, c.sites$nhd_lat, cex = .1, pch=20, col = "orange")
title("All lakes with any color or DOC data (n = 6641)")

nitrogen.sites <-rbind(nh4.sites, no3.sites, tkn.sites, TN.sites)
n.sites <-unique(nitrogen.sites)
points(n.sites$nhd_long, n.sites$nhd_lat, cex = .1, pch=20, col = "darkgoldenrod")
title("All lakes with any NH4, NO3, TKN, TN data (n = 10622)")


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
p <- ggplot(jday_long, aes(x = Yr.day, y = nobs)) +
  geom_line(aes(color = variable, group = variable)) +
  facet_wrap(~variable_type, ncol = 1, scales = 'free_y') +
  theme_bw() +
  theme(panel.grid = element_blank(), strip.background = element_blank()) +
  labs(x = 'Day of the Year', y = 'Data Count', color = 'Parameter') +
  scale_color_manual(values =  c("red", "green", "blue", "black", "gray", "cyan", "orange", "brown")) +
  geom_vline(xintercept = c(166, 258), linetype = 3)

ggsave('SoL_graphics/doy_vertical.png', height = 6, width = 5)  
  
