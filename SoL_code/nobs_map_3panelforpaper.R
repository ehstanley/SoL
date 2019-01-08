require(ggplot2)
library(RColorBrewer)
library(maps)
require(akima)
require(rgdal)
library(dplyr)

data<-readRDS(file="SoL_data/SoL_data.rds")
lakes<-readRDS(file="SoL_data/lakes.rds")
cordsids<-unique(lakes[,c("lagoslakeid", "nhd_lat", "nhd_long")])



pdf("SoL_graphics/FinalFigures/3pchltndoc.pdf", width=3.5, height=7)
par(mfrow=c(3,1), xpd=NA)
  
  data.var<- na.omit(data[,c("lagoslakeid", "chla", "year")])
  
  lakeyrmed<- data.var %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(chla, na.rm=T))
  lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n())
  
  lakemed<-merge(lakemed, cordsids, by="lagoslakeid", all.x=T, all.y=F)
  
  undertwenty<-filter(lakemed, count<20)
  twentyplus<-filter(lakemed, count>19)
  
  map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                    "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                    "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                    "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
  points(undertwenty$nhd_long, undertwenty$nhd_lat, pch=21, col="grey70", lwd=.5, bg=rgb(211,211,211, 200,max=255), cex=.8)
  points(twentyplus$nhd_long, twentyplus$nhd_lat, pch=19, lwd=.5, col="black", cex=.8)
 
  text(-97, 49.5, "a) Chlorophyll a", adj=c(0,0), cex=1.3)
  
  data.var<- na.omit(data[,c("lagoslakeid", "tn_combined", "year")])
  
  lakeyrmed<- data.var %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(tn_combined, na.rm=T))
  lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n())
  
  lakemed<-merge(lakemed, cordsids, by="lagoslakeid", all.x=T, all.y=F)
  
  undertwenty<-filter(lakemed, count<20)
  twentyplus<-filter(lakemed, count>19)
  
  map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                    "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                    "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                    "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
  points(undertwenty$nhd_long, undertwenty$nhd_lat, pch=21, col="grey70", lwd=.5, bg=rgb(211,211,211, 200,max=255), cex=.8)
  points(twentyplus$nhd_long, twentyplus$nhd_lat, pch=19, lwd=.5, col="black", cex=.8)
  text(-97, 49.5, "b) Total Nitrogen", adj=c(0,0), cex=1.3)
  
  #legend(-84, 51, legend=c("Lakes with data", "Lakes with 20+ years of data"),
      #   fill=c(rgb(211,211,211, 200,max=255), 
       #         rgb(34,94,168, 200,max=255)), cex=1, box.lty=0)
  
  data.var<- na.omit(data[,c("lagoslakeid", "doc", "year")])
  
  lakeyrmed<- data.var %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(doc, na.rm=T))
  lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n())
  
  lakemed<-merge(lakemed, cordsids, by="lagoslakeid", all.x=T, all.y=F)
  
  undertwenty<-filter(lakemed, count<20)
  twentyplus<-filter(lakemed, count>19)
  
  map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                    "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                    "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                    "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
  points(undertwenty$nhd_long, undertwenty$nhd_lat, pch=21, col="grey70", lwd=.5, bg=rgb(211,211,211, 200,max=255), cex=.8)
  points(twentyplus$nhd_long, twentyplus$nhd_lat, pch=19, lwd=.5, col="black", cex=.8)
  text(-97, 49.5, "c) Dissolved Organic Carbon", adj=c(0,0), cex=1.3)
  
  par(mar=c(0,0,0,0), xpd=NA)

  
  
  
  dev.off()








