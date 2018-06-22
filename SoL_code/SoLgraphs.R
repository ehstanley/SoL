rm(list=ls())
cat("\14")


setwd("C:/Users/Nobody/Documents/LocalR/SoL/SoL_data")
#setwd("D:/ehstanley/Dropbox/CSI local folder_UW/SoL")

SoL3 <- readRDS("SoL_data.rds")


library(dplyr)
library(maps)


## find stats of a variable by lake for each year, + plot of # of lakes sampled/yr
##Code borrowed from SKO
#secchi
SoLsecchi <- SoL3[!is.na(SoL3$secchi),]
lake.average.secchi = aggregate(SoLsecchi$secchi, SoLsecchi[,c("lagoslakeid", "year")], 
                                FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))

lake.average.secchi = data.frame(lagoslakeid = lake.average.secchi$lagoslakeid, year = lake.average.secchi$year, mean = lake.average.secchi$x[,1], sd = lake.average.secchi$x[,2], coef.var = lake.average.secchi$x[,3], med = lake.average.secchi$x[,4], nobs = lake.average.secchi$x[,5])

boxplot(lake.average.secchi$med~lake.average.secchi$year, ylab="secchi depth)")
abline(h=1, col="red", lwd=2)
title(main = "Secchi Depth")

boxplot(log(lake.average.secchi)$med~lake.average.secchi$year, ylab="secchi depth)")
abline(h=0, col="red", lwd=2)
title(main = "Secchi Depth")

#chla
SoLchl <- SoL3[!is.na(SoL3$chla), ]
lake.average.chla = aggregate(SoLchl$chla, SoLchl[,c("lagoslakeid", "year")], 
                              FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))

lake.average.chla = data.frame(lagoslakeid = lake.average.chla$lagoslakeid, year = lake.average.chla$year, mean = lake.average.chla$x[,1], sd = lake.average.chla$x[,2], coef.var = lake.average.chla$x[,3], med = lake.average.chla$x[,4], nobs = lake.average.chla$x[,5])

boxplot(log(lake.average.chla$med)~lake.average.chla$year, ylab="log(chla)")
abline(h=2.70805, col="green", lwd=2)
title(main = "Boxplot of median chlorophyll by lake")

#tp
SoLtp <- SoL3[!is.na(SoL3$tp), ]
lake.average.tp = aggregate(SoLtp$tp, SoLtp[,c("lagoslakeid", "year")], 
                            FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.tp = data.frame(lagoslakeid = lake.average.tp$lagoslakeid, year = lake.average.tp$year, mean = lake.average.tp$x[,1], sd = lake.average.tp$x[,2], coef.var = lake.average.tp$x[,3], med = lake.average.tp$x[,4], nobs = lake.average.tp$x[,5])

boxplot(log(lake.average.tp$med)~lake.average.tp$year, ylab="log(TP)")
abline(h=2.995732, col="blue", lwd=2)
title(main = "Boxplot of median TP by lake")

#NH4-N
SoLnh4 <- SoL3[!is.na(SoL3$nh4), ]
lake.average.nh4 = aggregate(SoLnh4$nh4, SoLnh4[,c("lagoslakeid", "year")], 
                             FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.nh4 = data.frame(lagoslakeid = lake.average.nh4$lagoslakeid, year = lake.average.nh4$year, mean = lake.average.nh4$x[,1], sd = lake.average.nh4$x[,2], coef.var = lake.average.nh4$x[,3], med = lake.average.nh4$x[,4], nobs = lake.average.nh4$x[,5])

boxplot(lake.average.nh4$med~lake.average.nh4$year, ylab="NH4-N (ug/L)")
boxplot(log(lake.average.nh4$med)~lake.average.nh4$year, ylab="NH4-N (ug/L)")

#NO3-N
SoLno2no3 <- SoL3[!is.na(SoL3$no2no3), ]
lake.average.no2no3 = aggregate(SoLno2no3$no2no3, SoLno2no3[,c("lagoslakeid", "year")], 
                                FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.no2no3 = data.frame(lagoslakeid = lake.average.no2no3$lagoslakeid, year = lake.average.no2no3$year, mean = lake.average.no2no3$x[,1], sd = lake.average.no2no3$x[,2], coef.var = lake.average.no2no3$x[,3], med = lake.average.no2no3$x[,4], nobs = lake.average.no2no3$x[,5])

boxplot(lake.average.no2no3$med~lake.average.no2no3$year, ylab="NO3-N (ug/L)")
boxplot(log(lake.average.no2no3$med)~lake.average.no2no3$year, ylab="NO3-N (ug/L)")

#SRP
SoLsrp <- SoL3[!is.na(SoL3$srp), ]
lake.average.srp = aggregate(SoLsrp$srp, SoLsrp[,c("lagoslakeid", "year")], 
                             FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.srp = data.frame(lagoslakeid = lake.average.srp$lagoslakeid, year = lake.average.srp$year, mean = lake.average.srp$x[,1], sd = lake.average.srp$x[,2], coef.var = lake.average.srp$x[,3], med = lake.average.srp$x[,4], nobs = lake.average.srp$x[,5])

boxplot(lake.average.srp$med~lake.average.srp$year, ylab="SRP (ug/L)")
boxplot(log(lake.average.srp$med)~lake.average.srp$year, ylab="SRP (ug/L)")

#TN
SoLtn <- SoL3[!is.na(SoL3$tn_combined), ]
lake.average.tn = aggregate(SoLtn$tn_combined, SoLtn[,c("lagoslakeid", "year")], 
                            FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.tn = data.frame(lagoslakeid = lake.average.tn$lagoslakeid, year = lake.average.tn$year, mean = lake.average.tn$x[,1], sd = lake.average.tn$x[,2], coef.var = lake.average.tn$x[,3], med = lake.average.tn$x[,4], nobs = lake.average.tn$x[,5])

boxplot(lake.average.tn$med~lake.average.tn$year, ylab="TN (ug/L)",main = "TN combined")
abline(h=600, col ="azure4", lwd=2)

boxplot(log(lake.average.tn$med)~lake.average.tn$year, ylab="log(TN)", main = "TN combined")
abline(h=6.39693, col ="azure4", lwd=2)

#TKN
SoLtkn <- SoL3[!is.na(SoL3$tkn), ]
lake.average.tkn = aggregate(SoLtkn$tkn, SoLtkn[,c("lagoslakeid", "year")], 
                             FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.tkn = data.frame(lagoslakeid = lake.average.tkn$lagoslakeid, year = lake.average.tkn$year, mean = lake.average.tkn$x[,1], sd = lake.average.tkn$x[,2], coef.var = lake.average.tkn$x[,3], med = lake.average.tkn$x[,4], nobs = lake.average.tkn$x[,5])

#boxplot(lake.average.din$med~lake.average.din$year, ylab="DIN (ug/L)")
#boxplot(log(lake.average.din$med)~lake.average.din$year, ylab="DIN (ug/L)")

#DOC
SoLdoc <- SoL3[!is.na(SoL3$doc), ]
lake.average.doc = aggregate(SoLdoc$doc, SoLdoc[,c("lagoslakeid", "year")], 
                             FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.doc = data.frame(lagoslakeid = lake.average.doc$lagoslakeid, year = lake.average.doc$year, mean = lake.average.doc$x[,1], sd = lake.average.doc$x[,2], coef.var = lake.average.doc$x[,3], med = lake.average.doc$x[,4], nobs = lake.average.doc$x[,5])

boxplot(lake.average.doc$med~lake.average.doc$year, ylab="DOC (mg/L)")
boxplot(log(lake.average.doc)$med~lake.average.doc$year, ylab="DOC (mg/L)")

#color
SoLcolor <- SoL3[!is.na(SoL3$colort), ]
lake.average.color = aggregate(SoLcolor$colort, SoLcolor[,c("lagoslakeid", "year")], 
                               FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.color = data.frame(lagoslakeid = lake.average.color$lagoslakeid, year = lake.average.color$year, mean = lake.average.color$x[,1], sd = lake.average.color$x[,2], coef.var = lake.average.color$x[,3], med = lake.average.color$x[,4], nobs = lake.average.color$x[,5])

boxplot(lake.average.color$med~lake.average.color$year, ylab="True color (PCU)")
boxplot(log(lake.average.color)$med~lake.average.color$year, ylab="True Color (PCU)")

#TN measured directly
SoLtn_measured <- SoL3[!is.na(SoL3$tn), ]
lake.average.tn_measured = aggregate(SoLtn_measured$tn, SoLtn_measured[,c("lagoslakeid", "year")], 
                                     FUN=function(x) c(mean=mean(x),sd=sd(x), covar=sd(x)/mean(x), median=median(x), nobs=length(x)))
lake.average.tn_measured = data.frame(lagoslakeid = lake.average.tn_measured$lagoslakeid, year = lake.average.tn_measured$year, mean = lake.average.tn_measured$x[,1], sd = lake.average.tn_measured$x[,2], coef.var = lake.average.tn_measured$x[,3], med = lake.average.tn_measured$x[,4], nobs = lake.average.tn_measured$x[,5])

chl.N = sum(lake.average.chla$nobs)
color.N = sum(lake.average.color$nobs)
doc.N = sum(lake.average.doc$nobs)
nh4.N = sum(lake.average.nh4$nobs)
no2no3.N = sum(lake.average.no2no3$nobs)
secchi.N = sum(lake.average.secchi$nobs)
tn.N = sum(lake.average.tn$nobs)
tp.N = sum(lake.average.tp$nobs)


###Assessing long-term eutrophication status for lakes- from ASLO 2017 talk
######updated 17Mar2018 with updated dataset that includes fixed NH
#median for each unique lake, averaged across the 10-year period [2001-2011]- 
#this is a grand average and may be biased because some years are heavily sampled, others only a couple
#of times, so 'heavy' year would be emphasized more in the grand average.
#lake_Avgchl<-na.omit(aggregate(SoL$chla, list(SoL$lagoslakeid), median))
#names(lake_Avgchl) <-c("lagoslakeid", "Median_chl")
#lake_Avgchl <- merge(lake_Avgchl, lakes, by = "lagoslakeid", all.x=TRUE)

###alternative strategy: calculate a lake's grand median from yearly medians so all years are equal
#lake grand medians calculated for each lake that was sampled for variables of interest during the study period

Grand.med.chl <-aggregate(lake.average.chla$med, list(lake.average.chla$lagoslakeid), median)
names(Grand.med.chl) <-c("lagoslakeid", "Median_chl")

Grand.med.tp <-aggregate(lake.average.tp$med, list(lake.average.tp$lagoslakeid), median)
names(Grand.med.tp) <-c("lagoslakeid", "Median_tp")

Grand.med.secchi <-aggregate(lake.average.secchi$med, list(lake.average.secchi$lagoslakeid), median)
names(Grand.med.secchi) <-c("lagoslakeid", "Median_secchi")

Grand.med.color <-aggregate(lake.average.color$med, list(lake.average.color$lagoslakeid), median)
names(Grand.med.color) <-c("lagoslakeid", "Median_color")

Grand.med.doc <-aggregate(lake.average.doc$med, list(lake.average.doc$lagoslakeid), median)
names(Grand.med.doc) <-c("lagoslakeid", "Median_doc")

Grand.med.nh4 <-aggregate(lake.average.nh4$med, list(lake.average.nh4$lagoslakeid), median)
names(Grand.med.nh4) <-c("lagoslakeid", "Median_nh4")

Grand.med.no2no3 <-aggregate(lake.average.no2no3$med, list(lake.average.no2no3$lagoslakeid), median)
names(Grand.med.no2no3) <-c("lagoslakeid", "Median_no2no3")

Grand.med.srp <-aggregate(lake.average.srp$med, list(lake.average.srp$lagoslakeid), median)
names(Grand.med.srp) <-c("lagoslakeid", "Median_srp")

Grand.med.tn <-aggregate(lake.average.tn$med, list(lake.average.tn$lagoslakeid), median)
names(Grand.med.tn) <-c("lagoslakeid", "Median_tn")

Grand.med.tkn <-aggregate(lake.average.tkn$med, list(lake.average.tkn$lagoslakeid), median)
names(Grand.med.tkn) <-c("lagoslakeid", "Median_tkn")

Grand.med.tn_measured <-aggregate(lake.average.tn_measured$med, list(lake.average.tn_measured$lagoslakeid), median)
names(Grand.med.tn_measured) <-c("lagoslakeid", "Median_tn_measured")


### merge averages and add in lake data on lat, long,lake area, state
Med1 <- full_join(Grand.med.chl, Grand.med.tp, by = NULL, copy=FALSE)
Med2 <- full_join(Med1, Grand.med.secchi, by = NULL, copy=FALSE)
Med3 <- full_join(Med2, Grand.med.color, by = NULL, copy=FALSE)
Med4 <- full_join(Med3, Grand.med.doc, by = NULL, copy=FALSE)
Med5 <- full_join(Med4, Grand.med.nh4, by = NULL, copy=FALSE)
Med6 <- full_join(Med5, Grand.med.no2no3, by = NULL, copy=FALSE)
Med7 <- full_join(Med6, Grand.med.srp, by = NULL, copy=FALSE)
Med8 <- full_join(Med7, Grand.med.tn, by = NULL, copy=FALSE)
Med9 <- full_join(Med8, Grand.med.tkn, by = NULL, copy=FALSE)
Med10 <- full_join(Med9, Grand.med.tn_measured, by = NULL, copy=FALSE)

# add in state names and lake attributes
lakes = readRDS("lakes.rds")
SoL.Med <- merge(Med10, lakes, by = "lagoslakeid")

#save file name depending on year duration
#write.csv(SoL.Med, file = ("SoL_LakeMedians.csv"), row.names = F) 
#write.csv(SoL.Med, file = ("SoL_LakeMedians2010.csv"), row.names = F) 


#probably a much easier way to do this, but making some data frames for mapping
#maps of all lakes that have data for variable X
chl.sites <- SoL.Med[!is.na(SoL.Med$Median_chl),]
chl.sites <- chl.sites[, 13:14]
TP.sites <- SoL.Med[!is.na(SoL.Med$Median_tp),]
TP.sites <- TP.sites[, 13:14]
secchi.sites <- SoL.Med[!is.na(SoL.Med$Median_secchi),]
secchi.sites <- secchi.sites[, 13:14]
TN.sites <- SoL.Med[!is.na(SoL.Med$Median_tn),]
TN.sites <- TN.sites[, 13:14]
DOC.sites <- SoL.Med[!is.na(SoL.Med$Median_doc),]
DOC.sites <- DOC.sites[, 13:14]
color.sites <-SoL.Med[!is.na(SoL.Med$Median_color),]
color.sites <- color.sites[, 13:14]
no3.sites <-SoL.Med[!is.na(SoL.Med$Median_no2no3),]
no3.sites <- no3.sites[, 13:14]
nh4.sites <-SoL.Med[!is.na(SoL.Med$Median_nh4),]
nh4.sites <- nh4.sites[, 13:14]
srp.sites <-SoL.Med[!is.na(SoL.Med$Median_srp),]
srp.sites <- srp.sites[, 13:14]
tkn.sites <-SoL.Med[!is.na(SoL.Med$Median_tkn),]
tkn.sites <- tkn.sites[, 13:14]
tnmeasured.sites <-SoL.Med[!is.na(SoL.Med$Median_tn_measured),]
tnmeasured.sites <- tnmeasured.sites[, 13:14]

chl_tp.sites <- SoL.Med[!is.na(SoL.Med$Median_chl),]
chl_tp.sites <- chl_tp.sites[!is.na(chl_tp.sites$Median_tp), ]
Basic2.sites <- chl_tp.sites[, 12:13]

chl_tp_secchi.sites <- chl_tp.sites[!is.na(chl_tp.sites$Median_secchi),]
Big3.sites <- chl_tp_secchi.sites[, 13:14]

All4 <- chl_tp_secchi.sites[!is.na(chl_tp_secchi.sites$Median_tn), ]
All4.sites <- All4[, 13:14]



## create a map of where data come from values 
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(chl.sites$nhd_long, chl.sites$nhd_lat, cex = 0.01, pch=20, col ="darkgreen")
#points(chl_eu.sites$nhd_long, chl_eu.sites$nhd_lat, cex = 0.1, pch =20, col= "green")
title("Chl a (n= 8525 lakes)")
#title("Chlorophyll a")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(TP.sites$nhd_long, TP.sites$nhd_lat, cex = .1, pch=20, col = "blue")
#points(TP_eu.sites$nhd_long, TP_eu.sites$nhd_lat, cex = 0.1, pch =20, col= "blue")
#title("Lakes with TP noting eutrophic lakes")
title("TP (n = 10490 lakes)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(secchi.sites$nhd_long, secchi.sites$nhd_lat, cex = .1, pch=20, col= "red")
title("Secchi Depth (n = 12377 lakes)")
#points(secchi_eu.sites$nhd_long, secchi_eu.sites$nhd_lat, cex = 0.1, pch =20, col= "red")
#title("Lakes with Secchi noting eutrophic lakes")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(TN.sites$nhd_long, TN.sites$nhd_lat, cex = .1, pch=20, col = "darkgoldenrod4")
title("TN (n = 6553 lakes)")
#points(TN_eu.sites$nhd_long, TN_eu.sites$nhd_lat, cex = 0.1, pch =20, col= "goldenrod3")


map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(DOC.sites$nhd_long, DOC.sites$nhd_lat, cex = .1, pch=20, col = "brown")
title("DOC (n = 4997 lakes)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(color.sites$nhd_long, color.sites$nhd_lat, cex = .1, pch=20, col = "orange")
title("True Color (n = 5636 lakes)")


map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(nh4.sites$nhd_long, nh4.sites$nhd_lat, cex = .1, pch=20, col = "darkgray")
title("NH4-N (n = 6502 lakes)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(no3.sites$nhd_long, no3.sites$nhd_lat, cex = .1, pch=20, col = "cyan")
title("NO2-NO3-N (n = 8173 lakes)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(srp.sites$nhd_long, srp.sites$nhd_lat, cex = .1, pch=20, col = "darkorchid2")
title("SRP (n = 1566 lakes)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(tkn.sites$nhd_long, tkn.sites$nhd_lat, cex = .1, pch=20, col = "burlywood")
title("TKN (n = 5625 lakes)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(tnmeasured.sites$nhd_long, tnmeasured.sites$nhd_lat, cex = .1, pch=20, col = "salmon")
title("TN measured (n = 2772 lakes)")


all.sites <-rbind(secchi.sites, chl.sites, color.sites, DOC.sites, nh4.sites, no3.sites, srp.sites, TN.sites, TP.sites)
all.sites2 <-unique(all.sites)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(all.sites2$nhd_long, all.sites2$nhd_lat, cex = .1, pch=20, col = "black")
title("All lakes with any data (n = 14229)")

troph.sites <-rbind(secchi.sites, chl.sites, TP.sites)
tr.sites <-unique(troph.sites)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(tr.sites$nhd_long, tr.sites$nhd_lat, cex = .1, pch=20, col = "limegreen")
title("All lakes with any Secchi, chl, TP data (n = 13954)")

carbon.sites <-rbind(color.sites, DOC.sites)
c.sites <-unique(carbon.sites)
nitrogen.sites <-rbind(nh4.sites, no3.sites, tkn.sites, TN.sites)
n.sites <-unique(nitrogen.sites)

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(c.sites$nhd_long, c.sites$nhd_lat, cex = .1, pch=20, col = "orange")
title("All lakes with any color or DOC data (n = 6641)")

map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(n.sites$nhd_long, n.sites$nhd_lat, cex = .1, pch=20, col = "darkgoldenrod")
title("All lakes with any NH4, NO3, TKN, TN data (n = 10622)")




#map with data for all 4 indicators used to assess trophic state
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(All4.sites$nhd_long, All4.sites$nhd_lat, cex = .1, pch=20, col = "blue")
title("Lakes with data for all 4 trophic state indicators (n = 5096)")

#######need to update this- non-mod and general check; somethings off########
##a first effort to map lakes with NO3 data relative to detection limit
##this needs more thought before going prime time
no3_NC1 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "NC1"),]
no3_NC2 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "NC2"),]
no3_NC3 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "NC3"),]
no3_NC4 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "NC4"),]
no3_NA <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "NA"),]
no3_above <-rbind(no3_NC1, no3_NC2, no3_NC3, no3_NC4, no3_NA)
no3_LE1 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "LE1"),]
no3_LE2 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "LE2"),]
no3_LE3 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "LE3"),]
no3_LE4 <-SoLno2no3[which(SoLno2no3$no2no3_censorcode == "LE4"),]
no3_below <-rbind(no3_LE1, no3_LE2, no3_LE3, no3_LE4)
above1<- no3_above[duplicated(no3_above[1]),]
above2 <-above1[, 33:34]
below<- no3_below[duplicated(no3_below[1]),]
below2 <- below[, 33:34]
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = FALSE)
points(above2$nhd_long, above2$nhd_lat, cex = 1, pch=20, col = "gray")
par(new=TRUE)
points(below2$nhd_long, below2$nhd_lat, cex = 0.1, pch=20, col = "cyan")
title("Nitrate above DL or NA (gray; n= 47909) and below DL (cyan; n=19643)")

##########

# Number of lakes sampled/year by variable
chl.nlakes.yr <- count(lake.average.chla, year)
chl.nlakes.yr <- rename(chl.nlakes.yr, chl = n)
color.nlakes.yr <- count(lake.average.color, year)
color.nlakes.yr <- rename(color.nlakes.yr, color = n)
tkn.nlakes.yr <- count(lake.average.tkn, year)
tkn.nlakes.yr <- rename(tkn.nlakes.yr, din = n)
doc.nlakes.yr <- count(lake.average.doc, year)
doc.nlakes.yr <- rename(doc.nlakes.yr, doc = n)
nh4.nlakes.yr <- count(lake.average.nh4, year)
nh4.nlakes.yr <- rename(nh4.nlakes.yr, nh4 = n)
no2no3.nlakes.yr <- count(lake.average.no2no3, year)
no2no3.nlakes.yr <- rename(no2no3.nlakes.yr, no2no3 = n)
secchi.nlakes.yr <- count(lake.average.secchi, year)
secchi.nlakes.yr <- rename(secchi.nlakes.yr, secchi = n)
srp.nlakes.yr <- count(lake.average.srp, year)
srp.nlakes.yr <- rename(srp.nlakes.yr, srp = n)
tkn.nlakes.yr <- count(lake.average.tkn, year)
tkn.nlakes.yr <-rename(tkn.nlakes.yr, tkn = n)
tn.nlakes.yr <- count(lake.average.tn, year)
tn.nlakes.yr <- rename(tn.nlakes.yr, tn = n)
tp.nlakes.yr <- count(lake.average.tp, year)
tp.nlakes.yr <- rename(tp.nlakes.yr, tp = n)
tnmeasured.nlakes.yr <- count(lake.average.tn_measured, year)
tnmeasured.nlakes.yr <- rename(tnmeasured.nlakes.yr, tnmeasured = n)

#putting timeline of lakes sampled each year together into 1 data.frame 
YR <- data.frame(c(1933:2010))
YR <-rename(YR, year = c.1933.2010.)
YR1 <-left_join(YR, chl.nlakes.yr, by = "year") 
YR1 <-left_join(YR1, color.nlakes.yr, by = "year")
YR1 <-left_join(YR1, tkn.nlakes.yr, by = "year")
YR1 <-left_join(YR1, doc.nlakes.yr, by = "year")
YR1 <-left_join(YR1, nh4.nlakes.yr, by = "year")
YR1 <-left_join(YR1, no2no3.nlakes.yr, by = "year")
YR1 <-left_join(YR1, secchi.nlakes.yr, by = "year")
YR1 <-left_join(YR1, tp.nlakes.yr, by = "year")
YR1 <-left_join(YR1, tn.nlakes.yr, by = "year")
#missing data issue- replace NAs with zero
YR1 <- data.frame(replace(YR1, is.na(YR1), 0))
YR1$total <- YR1$chl+YR1$color+YR1$tkn+YR1$doc+YR1$nh4+YR1$no2no3+YR1$secchi+YR1$tn+YR1$tp+YR1$tn
#plot(YR1$year, YR1$total)

#write.csv(YR1, file = ("Lake_by_year.csv"), row.names = F)
#YR1 <- read.csv("Lake_by_year.csv")

N_lakes <- summarise_all(YR1, funs(sum))
N_lakes <- N_lakes[, c("chl", "color", "doc", "nh4", "no2no3", "secchi", "tp", "tn")]
N <- as.numeric(N_lakes[1,])
var <- names(N_lakes) 
L.by.var <- data.frame(N, var)
L2 <- L.by.var[order(-N),]
barplot(L2$N, col = "blue", ylab = "Sum of lakes/yr")
xtick<-c("Secchi", "TP", "Chla", "NO3", "NH4", "TKN", "Color", "DOC", "TN")
axis(side=1, at=seq(1, 9, by=1), labels = xtick, las = 2)

N.totals = c(chl.N, color.N, doc.N, nh4.N, no2no3.N, secchi.N, tn.N, tp.N)
N.totals = sort(N.totals, decreasing = TRUE)

barplot(N.totals, ylab = "Total data count", ylim = c(0, 750000),col = "darkseagreen")
xtick<-c("Secchi", "Chl", "TP", "NO3", "TN","NH4", "Color", "DOC")
axis(side=1, at=seq(1, 8, by=1), labels = xtick, las = 2)


#plot of lakes visited/year by variable over time
plot(YR1$year, YR1$secchi, xlim = c(1950, 2010), ylim = c(0, 4500), xlab = "Year", 
     ylab= "Number of lakes", type = "o", pch = 16, col = "red")
par(new=T)
plot(YR1$year, YR1$chl, xlim = c(1950, 2010), ylim = c(0,4500), xaxt='n', xlab = "",
     yaxt='n', ylab = "", type = "o", pch = 16, col= "green")
par(new=TRUE)
plot(YR1$year, YR1$tp, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col= "blue")
par(new=T)
plot(YR1$year, YR1$tn, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col = "burlywood")
par(new=T)
plot(YR1$year, YR1$no2no3,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col= "cyan")
par(new=T)
plot(YR1$year, YR1$nh4,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col= "gray")
par(new=T)
plot(YR1$year, YR1$doc, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col = "brown")
legend("topleft", legend = c("secchi", "chla", "tp", "tn", "no2no3", "nh4", "doc"), cex = 0.8,
       lty = c(1,1), col = c("red", "green", "blue", "burlywood", "cyan", "gray", "brown"))


plot(YR1$year, YR1$secchi, xlim = c(1950, 2010), ylim = c(0, 4500), xlab = "Year", 
     ylab = "Number of lakes", type = "l", col = "red")
par(new=T)
plot(YR1$year, YR1$chl, xlim = c(1950, 2010), ylim = c(0,4500), xaxt='n', xlab = "",
     yaxt='n', ylab= "", type = "l", col= "green")
par(new=TRUE)
plot(YR1$year, YR1$tp, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     yaxt='n', ylab = "", type = "l", col= "blue")
par(new=T)
plot(YR1$year, YR1$tn, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab= "", 
     yaxt='n', ylab = "", type = "l", col = "burlywood")
par(new=T)
plot(YR1$year, YR1$no2no3,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "l", col= "cyan")
par(new=T)
plot(YR1$year, YR1$nh4,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n',xlab = "", 
     yaxt='n', ylab = "", type = "l", col= "gray")
par(new=T)
plot(YR1$year, YR1$doc, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "l", col = "brown")
legend("topleft", legend = c("Secchi", "Chl", "TP", "TN", "NO3", "NH4", "DOC"), cex = 0.8,
       lty = c(1,1), col = c("red", "green", "blue", "salmon", "burlywood", "cyan", "gray", "brown"))



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

tn.day <- as.data.frame(table(SoLtn_measured$day))
names(tn.day) <- c("Yr.day", "tn_f")
tn.day$Yr.day <- as.integer(tn.day$Yr.day)

tkn.day <- as.data.frame(table(SoLtkn$day))
names(tkn.day) <- c("Yr.day", "tkn_f")
tkn.day$Yr.day <- as.integer(tkn.day$Yr.day)

tp.day <- as.data.frame(table(SoLtp$day))
names(tp.day) <- c("Yr.day", "tp_f")
tp.day$Yr.day <- as.integer(tp.day$Yr.day)

srp.day <- as.data.frame(table(SoLsrp$day))
names(srp.day) <- c("Yr.day", "srp_f")
srp.day$Yr.day <- as.integer(srp.day$Yr.day)

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
Jday2 <- left_join(Jday2, tkn.day, by = "Yr.day")
Jday2 <- left_join(Jday2, srp.day, by = "Yr.day")
Jday2 <- left_join(Jday2, tn.day, by = "Yr.day")
Jday2 <- left_join(Jday2, tp.day, by = "Yr.day")


plot(Jday2$Yr.day, Jday2$secchi_f,  ylim = c(0, 7000), xlab = "Day of the Year",
     ylab= "Data Count", type = "l", col= "red")
par(new=TRUE)
plot(Jday2$Yr.day, Jday2$chl_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "green")
par(new=T)
plot(Jday2$Yr.day, Jday2$tp_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "blue")
par(new=T)
plot(Jday2$Yr.day, Jday2$tn_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "salmon")
par(new=T)
plot(Jday2$Yr.day, Jday2$tkn_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "burlywood")
par(new=T)
plot(Jday2$Yr.day, Jday2$no2no3_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "cyan")
par(new=T)
plot(Jday2$Yr.day, Jday2$doc_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "brown")
par(new=T)
plot(Jday2$Yr.day, Jday2$color_f,  ylim = c(0, 7000), xlab = "", 
     ylab = "", type = "l", col= "orange")
legend("topleft", legend = c("Secchi", "Chl a", "TP", "TN", "TKN", "NO3", "DOC", "Color"), cex = 0.8,
       lty=c(1,1), col = c("red", "green", "blue", "darkgoldenrod", "salmon", "cyan", "brown", "orange"))
abline(v = 166, lty = 3, col = "black")
abline(v = 258, lty = 3, col = "black")

#Same as above but without secchi
plot(Jday2$Yr.day, Jday2$chl_f,  ylim = c(0, 2200), xlab = "Day of the Year",
     ylab= "Data Count", type = "l", col= "green")
par(new=T)
plot(Jday2$Yr.day, Jday2$tp_f,  ylim = c(0, 2200), xlab = "", 
     ylab = "", type = "l", col= "blue")
par(new=T)
plot(Jday2$Yr.day, Jday2$tn_f,  ylim = c(0, 2200), xlab = "", 
     ylab = "", type = "l", col= "salmon")
par(new=T)
plot(Jday2$Yr.day, Jday2$tkn_f,  ylim = c(0, 2200), xlab = "", 
     ylab = "", type = "l", col= "burlywood")
par(new=T)
plot(Jday2$Yr.day, Jday2$no2no3_f,  ylim = c(0, 2200), xlab = "", 
     ylab = "", type = "l", col= "cyan")
par(new=T)
plot(Jday2$Yr.day, Jday2$doc_f,  ylim = c(0, 2200), xlab = "", 
     ylab = "", type = "l", col= "brown")
par(new=T)
plot(Jday2$Yr.day, Jday2$color_f,  ylim = c(0, 2200), xlab = "", 
     ylab = "", type = "l", col= "orange")
legend("topleft", legend = c("Chl a", "TP", "TN", "TKN", "NO3", "DOC", "Color"), cex = 0.8,
       lty=c(1,1), col = c("green", "blue", "darkgoldenrod", "salmon", "cyan", "brown", "orange"))
abline(v = 166, lty = 3, col = "black")
abline(v = 258, lty = 3, col = "black")


#sampling effort by month- similar to above (jday vs.# samples), but with 1 more step
#of expressing sampling effort as a fraction of all data points/variable
#note- the initial line to remove NAs may be redundant- was done at the start of the script
#as 1st step for calculating lakes/year
SoLsecchi <- SoL3[!is.na(SoL3$secchi),]
secchi.month <- as.data.frame(table(SoLsecchi$month))
names(secchi.month) <- c("month", "n.secchi")
secchi.month$month <- as.integer(secchi.month$month)

SoLchl <- SoL3[!is.na(SoL3$chla),]
chl.month <- as.data.frame(table(SoLchl$month))
names(chl.month) <- c("month", "n.chla")
chl.month$month <- as.integer(chl.month$month)

SoLcolor <- SoL3[!is.na(SoL3$colort),]
color.month <- as.data.frame(table(SoLcolor$month))
names(color.month) <- c("month", "n.color")
color.month$month <- as.integer(color.month$month)

SoLdoc <- SoL3[!is.na(SoL3$doc),]
doc.month <- as.data.frame(table(SoLdoc$month))
names(doc.month) <- c("month", "n.doc")
doc.month$month <- as.integer(doc.month$month)

SoLnh4 <- SoL3[!is.na(SoL3$nh4),]
nh4.month <- as.data.frame(table(SoLnh4$month))
names(nh4.month) <- c("month", "n.nh4")
nh4.month$month <- as.integer(nh4.month$month)

SoLno2no3 <- SoL3[!is.na(SoL3$no2no3),]
no2no3.month <- as.data.frame(table(SoLno2no3$month))
names(no2no3.month) <- c("month", "n.no2no3")
no2no3.month$month <- as.integer(no2no3.month$month)

SoLtn_measured <- SoL3[!is.na(SoL3$tn),]
tn.month <- as.data.frame(table(SoLtn_measured$month))
names(tn.month) <- c("month", "n.tn")
tn.month$month <- as.integer(tn.month$month)

SoLtkn <- SoL3[!is.na(SoL3$tkn),]
tkn.month <- as.data.frame(table(SoLtkn$month))
names(tkn.month) <- c("month", "n.tkn")
tkn.month$month <- as.integer(tkn.month$month)

SoLtp <- SoL3[!is.na(SoL3$tp),]
tp.month <- as.data.frame(table(SoLtp$month))
names(tp.month) <- c("month", "n.tp")
tp.month$month <- as.integer(tp.month$month)

SoLsrp <- SoL3[!is.na(SoL3$srp),]
srp.month <- as.data.frame(table(SoLsrp$month))
names(srp.month) <- c("month", "n.srp")
srp.month$month <- as.integer(srp.month$month)

#now combining sampling effort/variable by month for all variables
#and a couple of group  plots
Month <-left_join(secchi.month, chl.month, by = "month")
Month <- left_join(Month, color.month, by = "month")
Month <- left_join(Month, doc.month, by = "month")
Month <- left_join(Month, nh4.month, by = "month")
Month <- left_join(Month, no2no3.month, by = "month")
Month <- left_join(Month, srp.month, by = "month")
Month <- left_join(Month, tkn.month, by = "month")
Month <- left_join(Month, tn.month, by = "month")
Month <- left_join(Month, tp.month, by = "month")

month.sum <- summarise_all(Month, funs(sum))
month.sum <- month.sum[2:11]
Month$f.secchi <- Month$n.secchi/month.sum$n.secchi
Month$f.chl <- Month$n.chla/month.sum$n.chla
Month$f.color <-Month$n.color/month.sum$n.color
Month$f.doc <- Month$n.doc/month.sum$n.doc
Month$f.nh4 <- Month$n.nh4/month.sum$n.nh4
Month$f.no2no3 <- Month$n.no2no3/month.sum$n.no2no3
Month$f.srp <- Month$n.srp/month.sum$n.srp
Month$f.tkn <- Month$n.tkn/month.sum$n.tkn
Month$f.tn <- Month$n.tn/month.sum$n.tn
Month$f.tp <- Month$n.tp/month.sum$n.tp

plot(Month$month, Month$f.secchi,  ylim = c(0, 0.25), xlab = "Month",
     ylab= "Fraction of all data", type = "l", col= "red")
par(new=TRUE)
plot(Month$month, Month$f.chl,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "green")
par(new=T)
plot(Month$month, Month$f.tp,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "blue")
par(new=T)
plot(Month$month, Month$f.tn,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "salmon")
par(new=T)
plot(Month$month, Month$f.tkn,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "burlywood")
par(new=T)
plot(Month$month, Month$f.no2no3,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "cyan")
par(new=T)
plot(Month$month, Month$f.doc,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "brown")
par(new=T)
plot(Month$month, Month$f.color,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "orange")
legend("topleft", legend = c("Secchi", "Chl a", "TP", "TN", "TKN", "NO3", "DOC", "Color"), cex = 0.8,
       lty=c(1,1), col = c("red", "green", "blue", "darkgoldenrod", "salmon", "cyan", "brown", "orange"))
abline(v = 6.5, lty = 3, col = "black")
abline(v = 9.5, lty = 3, col = "black")

plot(Month$month, Month$f.secchi,  ylim = c(0, 0.25), xlab = "Month",
     ylab= "Fraction of all data", type = "l", col= "red")
par(new=TRUE)
plot(Month$month, Month$f.tp,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "blue")
par(new=T)
plot(Month$month, Month$f.tkn,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "burlywood")
par(new=T)
plot(Month$month, Month$f.doc,  ylim = c(0, 0.25), xlab = "", 
     ylab = "", xaxt ="n", yaxt="n", type = "l", col= "brown")
legend("topleft", legend = c("Secchi", "TP", "TKN", "DOC"), cex = 0.8,
       lty=c(1,1), col = c("red", "blue", "darkgoldenrod", "brown"))
abline(v = 6.5, lty = 3, col = "black")
abline(v = 9.5, lty = 3, col = "black")

