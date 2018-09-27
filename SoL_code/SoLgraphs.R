rm(list=ls())
cat("\14")


setwd("C:/Users/Nobody/Documents/LocalR/SoL/SoL_data")
#setwd("D:/ehstanley/Dropbox/CSI local folder_UW/SoL")

sol <- readRDS("SoL_data.rds")


library(dplyr)
library(maps)


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

#count the # of times a lake shows up on the above list- i.e. # of different years sampled
#1st line isn't needed- just did it to have a look at the data!
avgchl_sort <- lake.average.chla[order(lake.average.chla$lagoslakeid),]
chlmed<- count(avgchl_sort, lagoslakeid)
#lakes that were sampled in only 1 year
chl.once = chlmed[which(chlmed$n==1),]
#lakes that were sampled once within any given year, but might have been sampled 1, 2, or more years
chl_1 = avgchl_sort[which(avgchl_sort$nobs==1),]
#counting number of yrs a lake with 1 obs/yr was sampled
x <- chl_1 %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
#lakes that were just sampled one time EVER
x1 = x[which(x$n_yrs == 1),]

#shorten just to get lakes that were only sampled one time EVER
chl_1x = lake.average.chla[which(lake.average.chla$nobs == 1),]
chl1 <- chl_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
chl.once = chl1[which(chl1$n_yrs == 1),]

color1x = lake.average.color[which(lake.average.color$nobs == 1),]
color1 <- color1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
color.once = color1[which(color1$n_yrs == 1),]

doc_1x = lake.average.doc[which(lake.average.doc$nobs == 1),]
doc1 <- doc_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
doc.once = doc1[which(doc1$n_yrs == 1),]

nh4_1x = lake.average.nh4[which(lake.average.nh4$nobs == 1),]
nh41 <- nh4_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
nh4.once = nh41[which(nh41$n_yrs == 1),]

no2no3_1x = lake.average.no2no3[which(lake.average.no2no3$nobs == 1),]
no2no31 <- no2no3_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
no2no3.once = no2no31[which(no2no31$n_yrs == 1),]

secchi_1x = lake.average.secchi[which(lake.average.secchi$nobs == 1),]
secchi1 <- secchi_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
secchi.once = secchi1[which(secchi1$n_yrs == 1),]

tn_1x = lake.average.tn[which(lake.average.tn$nobs == 1),]
tn1 <- tn_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
tn.once = tn1[which(tn1$n_yrs == 1),]

tp_1x = lake.average.tp[which(lake.average.tp$nobs == 1),]
tp1 <- tp_1x %>% group_by(lagoslakeid) %>% summarise(n_yrs = n_distinct(year))
tp.once = tp1[which(tp1$n_yrs == 1),]



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
Med2 <- full_join(Med2, Grand.med.color, by = NULL, copy=FALSE)
Med3 <- full_join(Med3, Grand.med.doc, by = NULL, copy=FALSE)
Med4 <- full_join(Med4, Grand.med.nh4, by = NULL, copy=FALSE)
Med5 <- full_join(Med5, Grand.med.no2no3, by = NULL, copy=FALSE)
Med6 <- full_join(Med7, Grand.med.tn, by = NULL, copy=FALSE)
Med10 <- full_join(Med8, Grand.med.tp, by = NULL, copy=FALSE)


# add in state names and lake attributes
lakes = readRDS("lakes.rds")
SoL.Med <- merge(Med10, lakes, by = "lagoslakeid")

#save file 
#saveRDS(SoL.Med, file = ("SoL_medians.rds"))

#Getting a count of data diversity- # of lakes with any data for any of the 8 variables
#i.e., do they have any info on 1 variable, 2 variables...all 8 variables?
chem.Med <- SoL.Med[, c(1, 2, 3, 4, 5, 6, 7, 8, 10)]
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



#calcs for potential Table 2, summarizing # total lakes sampled, # lakes sampled 1X, their percentages
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
table2$once <- as.numeric(levels(table2$once))[table2$once]
table2$pct_once <- (table2$once/table2$nlakes)*100

N.data = c(secchi.N, chl.N, color.N, doc.N, nh4.N, no2no3.N, tn.N, tp.N)
table2$N_data = cbind(N.data)
table2$pct_dta = (table2$N_data/(sum(N.data)))*100

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


####all sorts of map options follow#####
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
#######


######a first effort to map lakes with NO3 data relative to detection limit#####
#!!need to update this- non-mod and general check; it's off base
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
plot(YR1$year, YR1$color,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col= "orange")
par(new=T)
plot(YR1$year, YR1$doc, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "o", pch = 16, col = "brown")
legend("topleft", legend = c("secchi", "chla", "tp", "tn", "no2no3", "nh4", "color", "doc"), cex = 0.8,
       lty = c(1,1), col = c("red", "green", "blue", "burlywood", "cyan", "gray", "orange", "brown"))


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
plot(YR1$year, YR1$color,  xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n',xlab = "", 
     yaxt='n', ylab = "", type = "l", col= "orange")
par(new=T)
plot(YR1$year, YR1$doc, xlim = c(1950, 2010), ylim = c(0, 4500), xaxt='n', xlab = "", 
     yaxt='n', ylab = "", type = "l", col = "brown")
legend("topleft", legend = c("Secchi", "Chl", "TP", "TN", "NO3", "NH4", "Color", "DOC"), cex = 0.8,
       lty = c(1,1), col = c("red", "green", "blue", "burlywood", "cyan", "gray", "orange", "brown"))



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
  scale_color_manual(values =  c("red", "green", "blue", "black", "gray", "cyan", "brown", "orange")) +
  geom_vline(xintercept = c(166, 258), linetype = 3)

ggsave('SoL_graphics/doy_vertical.png', height = 6, width = 5)  
  
