
require(ggplot2)
library(RColorBrewer)
library(maps)
require(akima)
require(rgdal)
library(dplyr)

data<-readRDS(file="SoL_data/SoL_data.rds")
lakes<-readRDS(file="SoL_data/lakes.rds")
cordsids<-unique(lakes[,c("lagoslakeid", "nhd_lat", "nhd_long")])



mapnobs<- function(var, nut, filename) {
  
data.var<- na.omit(data[,c("lagoslakeid", var, "year")])
  
lakeyrmed<- data.var %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(get(nut), na.rm=T))
lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med, na.rm=T), count=n())

lakemed<-merge(lakemed, cordsids, by="lagoslakeid", all.x=T, all.y=F)

oneyr<- filter(lakemed, count==1)
twoeight<-filter(lakemed, count>1)
twoeight<-filter(twoeight, count<11)
eleventwenty<-filter(lakemed, count>10)
eleventwenty<-filter(eleventwenty, count<21)
overtwenty<-filter(lakemed, count>20)


pdf(filename, width=13, height=8)
map(database = "state", regions=c("Minnesota", "Wisconsin", "Iowa", "Illinois","Missouri",
                                  "Indiana","Michigan","Ohio", "Pennsylvania","New York",
                                  "New Jersey", "Connecticut","Rhode Island","Massachusetts",
                                  "Vermont", "New Hampshire","Maine"), fill = TRUE, col="white", fg="grey30", lwd=1,mar=c(0,0,1,0),oma=c(0,0,0,0))
points(oneyr$nhd_long, oneyr$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(255,255,204, 200,max=255), cex=1)
points(twoeight$nhd_long, twoeight$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(161,218,180, 200,max=255), cex=1)
points(eleventwenty$nhd_long, eleventwenty$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(65,182,196, 200,max=255), cex=1)
points(overtwenty$nhd_long, overtwenty$nhd_lat, pch=21, col="black", lwd=.5, bg=rgb(34,94,168, 200,max=255), cex=1)

dev.off()
}

mapnobs("tp", 'tp', "SoL_graphics/nobsmaps/TP_nobs_map.pdf")
mapnobs("secchi", 'secchi', "SoL_graphics/nobsmaps/secchi_nobs_map.pdf")
mapnobs("colort", 'colort', "SoL_graphics/nobsmaps/color_nobs_map.pdf")
mapnobs("doc", 'doc', "SoL_graphics/nobsmaps/doc_nobs_map.pdf")
mapnobs("tn_combined", 'tn_combined', "SoL_graphics/nobsmaps/TN_nobs_map.pdf")
mapnobs("chla", 'chla', "SoL_graphics/nobsmaps/chla_nobs_map.pdf")
mapnobs("no2no3", 'no2no3', "SOL_graphics/nobsmaps/no3_nobs_map.pdf")

#filename something like this "SoL_graphics/TP_nobs_map.pdf"

