
require(ggplot2)
library(RColorBrewer)
library(maps)
require(akima)
require(rgdal)
library(dplyr)

data<-readRDS(file="SoL_data/SoL_data.rds")

#only need response var, lagoslakeid, year from this

data.lake <- data[, c("lagoslakeid", "tp", "year")]

#make median response by lake, first median for each year, then median of those
lakeyrmed<- data.lake %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(tp, na.rm=T))
lakemed<- na.omit(lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n()))

cordsids<-unique(data[,c("lagoslakeid", "nhd_lat", "nhd_long")])

lakemed<-merge(lakemed, cordsids, by="lagoslakeid", all.x=T, all.y=F)

#only keep lakes with minimally 2 years of response values, calc cv of response by lake for lakes with 5 years
#define cv
oneyr<- filter(lakemed, count==1)
twofive<-filter(lakemed, count>1)
twofive<-filter(twofive, count<6)
sixten<-filter(lakemed, count>5)
sixten<-filter(sixten, count<11)
eleventwenty<-filter(lakemed, count>10)
eleventwenty<-filter(eleventwenty, count<21)
overtwenty<-filter(lakemed, count>20)


rgb(12,44,132,max=255, alpha=alpha)

pdf("SoL_graphics/TP_nobs_map.pdf", width=13, height=8)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(oneyr$nhd_long, oneyr$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(255,255,204, 150,max=255), cex=1)
points(twofive$nhd_long, twofive$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(161, 218, 180, 150,max=255), cex=1)
points(sixten$nhd_long, sixten$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(65, 182, 196, 150,max=255), cex=1)
points(eleventwenty$nhd_long, eleventwenty$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(44, 127, 184, 150,max=255), cex=1)

points(overtwenty$nhd_long, overtwenty$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(37, 52, 148, 150,max=255), cex=1)


dev.off()
