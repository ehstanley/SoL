rm(list=ls())
##want to do a basic resampling exercise with lakes that have at least 20 years of data -- how many years of data are required for mean and variance to approach "true" values from actual 20 yr time series.  actual values for mean and variance are specific by lagoslakeid, so calculate the absolute value of the % difference betweena "true" average and average based on small/resampled data.

library(lubridate)
library(tidyverse)
library(vioplot)
library(brotools)

data = readRDS("SoL_data/SoL_data.rds")

#path of least resistance to painful 

#Choose which parameter to run
parameter.choice = "secchi"

variable.dat = na.omit(data[,c("lagoslakeid", "year", parameter.choice,"sampledate")])
names(variable.dat)[3] = "value"

#calculate actual median and variance for lakes with at least 20 years of data, save vector of lakeids for those lakes to limit sample to them.

cv <- function(x) {return(sd(x)/mean(x))}

lakeyrmed<- variable.dat %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(value, na.rm=T))
lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n())
yrs20<- filter(lakemed, count>=20)
lakekeeps<-unique(yrs20$lagoslakeid)

dat_20yr_initial <- variable.dat[variable.dat$lagoslakeid %in% lakekeeps, ] 

#deal with dates
dat_20yr_initial$sampledate = strptime(dat_20yr_initial$sampledate, "%m/%d/%Y")
dat_20yr_initial$sampledate = as.POSIXct(dat_20yr_initial$sampledate)
dat_20yr_initial <- dat_20yr_initial %>% mutate(sampledate = ymd(sampledate)) %>%  mutate(quarter = quarter(sampledate),week=week(sampledate),month=month(sampledate))


#create semi-uniformally sampled time series (3 sets of for loops, select random sample per week,
#select a random monthly sample from the weekly samples, and finally select random sample from
#monthly samples for quarterly samples. result is semi uniformily quarterly sampled data)

lakes = unique(dat_20yr_initial$lagoslakeid)
dat_20yr_weekly <- dat_20yr_initial[-(1:nrow(dat_20yr_initial)),]

#loop through all the lakes and randomly select a single observation by for each lake,year,week combination
for(i in 1:length(lakes)) {
  temp.lake = dat_20yr_initial[which(dat_20yr_initial$lagoslakeid==lakes[i]),] #subset to lake in lakes vector
  years = unique(temp.lake$year)
  for(j in 1:length(years)) {
    temp.year = temp.lake[which(temp.lake$year==years[j]),] #subset to year in years vector
    weeks = unique(temp.year$week)
    for(t in 1:length(weeks)) {
      temp.week = temp.year[which(temp.year$week==weeks[t]),] #subset to week within each lake year combination
      temp.dat = temp.week[sample(1:nrow(temp.week),1),] #randomly select one observation from each week
      dat_20yr_weekly = rbind(dat_20yr_weekly,temp.dat) #bind that observation to reduced datasets
    }
  }
}

#loop through all the lakes and randomly select a single observation by for each lake,year, month combination
lakes = unique(dat_20yr_weekly$lagoslakeid)
dat_20yr_monthly = dat_20yr_weekly[-(1:nrow(dat_20yr_weekly)),]

for(i in 1:length(lakes)) {
  temp.lake = dat_20yr_weekly[which(dat_20yr_weekly$lagoslakeid==lakes[i]),] #subset to lake in lakes vector
  years = unique(temp.lake$year)
  for(j in 1:length(years)) {
    temp.year = temp.lake[which(temp.lake$year==years[j]),] #subset to year in years vector
    months = unique(temp.year$month)
    for(t in 1:length(months)) {
      temp.month = temp.year[which(temp.year$month==months[t]),] #subset to week within each lake year combination
      temp.dat = temp.month[sample(1:nrow(temp.month),1),] #randomly select one observation from each week
      dat_20yr_monthly = rbind(dat_20yr_monthly,temp.dat) #bind that observation to reduced datasets
    }
  }
}

#loop through all the lakes and randomly select a single observation by for each lake,year, quarter combination
lakes = unique(dat_20yr_monthly$lagoslakeid)
dat_20yr_quarterly = dat_20yr_monthly[-(1:nrow(dat_20yr_monthly)),]

for(i in 1:length(lakes)) {
  temp.lake = dat_20yr_monthly[which(dat_20yr_monthly$lagoslakeid==lakes[i]),] #subset to lake in lakes vector
  years = unique(temp.lake$year)
  for(j in 1:length(years)) {
    temp.year = temp.lake[which(temp.lake$year==years[j]),] #subset to year in years vector
    quarters = unique(temp.year$quarter)
    for(t in 1:length(quarters)) {
      temp.quarter = temp.year[which(temp.year$quarter==quarters[t]),] #subset to week within each lake year combination
      temp.dat = temp.quarter[sample(1:nrow(temp.quarter),1),] #randomly select one observation from each week
     dat_20yr_quarterly = rbind(dat_20yr_quarterly,temp.dat) #bind that observation to reduced datasets
    }
  }
}

#identify lake years where 2 quarters of data exist (quarters 2 and 3) and retains 20+ yrs of data
lakeqrcount <- dat_20yr_quarterly %>%  
  filter(quarter == 2 | quarter ==3) %>% 
  group_by(lagoslakeid,year) %>% 
  summarise(ly.med=median(value, na.rm=T),count=n()) %>% filter(count==2)

lakemed<- lakeqrcount %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n()) %>% filter(count >= 20)

lakeyrkeeps <- lakeqrcount[lakeqrcount$lagoslakeid %in% lakemed$lagoslakeid,] %>% 
  select(lagoslakeid,year)

#filter the quarterly data to the above lake/year combinations

sec20unif <- lakeyrkeeps %>% left_join(dat_20yr_quarterly) %>%
  filter(quarter == 2 | quarter == 3)
newkeeps <- unique(sec20unif$lagoslakeid)

#datacheckes
obsperyear <- sec20unif %>%  group_by(lagoslakeid,year) %>% 
  summarise(ly.med=median(value, na.rm=T),count=n())
unique(obsperyear$count) # yep...everylake as at least 3 observations
numyears <- obsperyear %>% group_by(lagoslakeid) %>% 
  summarise(l.med=median(ly.med,na.rm=T),count=n())
range(numyears$count) # yep mininum number of years with 3+ quarterly observations is 20

#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(1,2,3,5,10,20,30,40)

#data frame to store medians for each lake and sample size combo
lake.ss<- data.frame(med1obs=numeric(),
                     med2obs=numeric(),
                     med3obs=numeric(),
                     med5obs=numeric(),
                     med10obs=numeric(),
                     med20obs=numeric(),
                     med30obs=numeric(),
                     med40obs=numeric())

for (k in 1:length(newkeeps)) {
  
  id<-newkeeps[k]
  dat.id<-sec20unif[sec20unif$lagoslakeid==id,]
  med.t<-median(dat.id$value)
  
  
  #vector to store data to populate each row of lake.ss
  lakemeds=numeric(8)

for (j in 1:length(n)) {
  
  #dummy vector to store median from inner loop
  sample.med=numeric(1000)
  
  for (i in 1:1000) {
    
    #sample n observations of 20 year secchi dataset and take the median and cv, repeat 1,000 times to fill the chla.median and chla.variance vectors
    sample.x = sample(dat.id$value, n[j], replace=F)
    sample.med[i]=median(sample.x)
  }
  
  lakemeds[j]= median(abs((sample.med-med.t)/med.t*100))
  		
}
  
  lake.ss[k,]=lakemeds
  
}
lake.ss <- lake.ss %>% drop_na()

#change to saving these as parameter.choice specific estimates to be able to make a 3 panel vio plot later - loop through for each individual parameter.choice then save the following objects for the plots after the appropriate parameter. (suboptimal but whatever)

vio1tp<-lake.ss$med1obs
vio2tp<-lake.ss$med2obs
vio3tp<-lake.ss$med3obs
vio5tp<-lake.ss$med5obs
vio10tp<-lake.ss$med10obs
vio20tp<-lake.ss$med20obs
vio30tp<-lake.ss$med30obs
vio40tp<-lake.ss$med40obs

vio1sec<-lake.ss$med1obs
vio2sec<-lake.ss$med2obs
vio3sec<-lake.ss$med3obs
vio5sec<-lake.ss$med5obs
vio10sec<-lake.ss$med10obs
vio20sec<-lake.ss$med20obs
vio30sec<-lake.ss$med30obs
vio40sec<-lake.ss$med40obs


vio1chl<-lake.ss$med1obs
vio2chl<-lake.ss$med2obs
vio3chl<-lake.ss$med3obs
vio5chl<-lake.ss$med5obs
vio10chl<-lake.ss$med10obs
vio20chl<-lake.ss$med20obs
vio30chl<-lake.ss$med30obs
vio40chl<-lake.ss$med40obs

png("SoL_graphics/ViolinResample3Panel.png", width=3.5, height=7, units='in', res=300)
par(mfrow=c(3,1), mar=c(0,0,0,0), oma=c(4,4,1,1))
vioplot(vio1sec,vio2sec, vio3sec, vio5sec, vio10sec, vio20sec, vio30sec, vio40sec,
        names=c("", "", "", "", "", "", "", ""),
        col="lightgrey", ylim=c(1,68))
#mtext("Percent Error", side=2, line=2)
text(7.8, 65, "a) Secchi", cex=1.2)

vioplot(vio1chl,vio2chl, vio3chl, vio5chl, vio10chl, vio20chl, vio30chl, vio40chl,
      names=c("", "", "", "", "", "", "", ""),
        col="lightgrey")
mtext("Percent Error", side=2, line=2)
text(7.4, 100, "b) Chlorophyll", cex=1.2)

vioplot(vio1tp,vio2tp, vio3tp, vio5tp, vio10tp, vio20tp, vio30tp, vio40tp,
        names=c("1","2","3","5", "10", "20", "30", "40"),
        col="lightgrey", ylim=c(1, 65))
#mtext("Percent Error", side=2, line=2)
mtext("Sample Size", side=1, line=2.5)
text(8.1, 60, "c) TP", cex=1.2)
dev.off()

# png(paste("SoL_graphics/",parameter.choice,"_TimeResampleViolin.png",sep=""), width=6, height=5, units='in', res=300)
# vioplot(vio1,vio2,vio3, vio5, vio10, vio20, vio30, vio40, 
#         names=c("1","2","3","5", "10", "20", "30", "40"), 
#         col="lightgrey")
# mtext("Percent Error", side=2, line=2)
# mtext("Sample Size", side=1, line=2)
# abline(h=0)
# dev.off()

(summary_table <- brotools::describe(lake.ss))
write_csv(x = summary_table,path = paste("SoL_data/",parameter.choice,"_TimeResamplestats.csv",sep=""))
##what about doing this with yearly medians to identify how many years of 20 are needed to capture
#year to year variation (not within year)?

#use lakeyrmed data for lakes in lakekeeps

# sec20ymed <- sec20unif %>%  group_by(lagoslakeid,year) %>% 
#   summarise(ly.med=median(secchi, na.rm=T),count=n())
# 
# 
# #set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
# n=c(1,2,3,5,7,10,15,20)
# 
# #data frame to store medians for each lake and sample size combo
# lake.ss.meds<- data.frame(med1obs=numeric(),
#                      med2obs=numeric(),
#                      med3obs=numeric(),
#                      med5obs=numeric(),
#                      med7obs=numeric(),
#                      med10obs=numeric(),
#                      med15obs=numeric(),
#                      med20obs=numeric())
# 
# for (k in 1:length(newkeeps)) {
#   
#   id<-newkeeps[k]
#   dat.id<-sec20ymed[sec20ymed$lagoslakeid==id,]
#   med.t<-median(dat.id$ly.med)
#   
#   
#   #vector to store data to populate each row of lake.ss
#   lakemeds=numeric(8)
#   
#   for (j in 1:length(n)) {
#     
#     #dummy vector to store median from inner loop
#     sample.med=numeric(1000)
#     
#     for (i in 1:1000) {
#       
#       #sample n observations of 20 year secchi dataset and take the median and cv, repeat 1,000 times to fill the chla.median and chla.variance vectors
#       sample.x = sample(dat.id$ly.med, n[j], replace=F)
#       sample.med[i]=median(sample.x)
#     }
#     
#     lakemeds[j]= median(abs((sample.med-med.t)/med.t*100))
#     
#   }
#   
#   lake.ss.meds[k,]=lakemeds
#   
# }
# 
# vio1m<-lake.ss.meds$med1obs
# vio2m<-lake.ss.meds$med2obs
# vio3m<-lake.ss.meds$med3obs
# vio5m<-lake.ss.meds$med5obs
# vio7m<-lake.ss.meds$med7obs
# vio10m<-lake.ss.meds$med10obs
# vio15m<-lake.ss.meds$med15obs
# vio20m<-lake.ss.meds$med20obs
# 
# vioplot(vio1m,vio2m, vio3m, vio5m, vio7m, vio10m, vio15m, vio20m, 
#         names=c("1","2", "3", "5", "7", "10", "15", "20"), 
#         col="lightgrey")
# 
# png("SoL_graphics/ViolinResample2Panel.png", width=6, height=10, units='in', res=300)
# par(mfrow=c(2,1), mar=c(2,0,0,0), oma=c(2,4,1,1))
# vioplot(vio1,vio2,vio3, vio5, vio10, vio20, vio30, vio60, 
#         names=c("1","2","3","5", "10", "20", "30", "60"), 
#         col="lightgrey")
# mtext("Percent Error", side=2, line=2)
# text(5.3, 43, "a) Within and across years")
# vioplot(vio1m,vio2m, vio3m, vio5m, vio7m, vio10m, vio15m, vio20m, 
#         names=c("1","2", "3", "5", "7", "10", "15", "20"), 
#         col="lightgrey")
# mtext("Percent Error", side=2, line=2)
# mtext("Sample Size", side=1, line=2)
# text(7.2, 64, "b) Across years only")
# dev.off()
# 
# png("SoL_graphics/lt_secchi_data%03d.png", width=8, height=10.5, units='in', res=300)
# par(mfrow=c(11,5), mar=c(0,0,0,0), oma=c(0,0,0,0))
# for(i in 1:length(newkeeps)){
#   dat.t = sec20unif[which(sec20unif$lagoslakeid==newkeeps[i]),]
#   plot(dat.t$sampledate,dat.t$secchi,pch=16,xaxt="n",yaxt="n",xlab="",ylab="",type="b")
#   mtext(side=3,dat.t$lagoslakeid,col=rgb(211,211,211,max=255,alpha = 100),line=-1.5)
# }
# dev.off()


