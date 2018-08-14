##want to do a basic resampling exercise with lakes that have at least 20 years of data -- how many years of data are required for mean and variance to approach "true" values from actual 20 yr time series.  actual values for mean and variance are specific by lagoslakeid, so calculate the absolute value of the % difference betweena "true" average and average based on small/resampled data.

library(dplyr)
library(lubridate)
library(tidyverse)
library(vioplot)

data = readRDS("SoL_data/SoL_data.rds")

#try this with secchi first b/c lots of LT data

sec.dat = na.omit(data[,c("lagoslakeid", "year", "secchi","sampledate")])

#calculate actual median and variance for lakes with at least 20 years of data, save vector of lakeids for those lakes to limit sample to them.

cv <- function(x) {return(sd(x)/mean(x))}

lakeyrmed<- sec.dat %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(secchi, na.rm=T))
lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n())
yrs20<- filter(lakemed, count>19)
lakekeeps<-unique(yrs20$lagoslakeid)

sec20 <- sec.dat[sec.dat$lagoslakeid %in% lakekeeps, ] 

#deal with dates
sec20$sampledate = strptime(sec20$sampledate, "%m/%d/%Y")
sec20$sampledate = as.POSIXct(sec20$sampledate)
sec20 <- sec20 %>% mutate(sampledate = ymd(sampledate)) %>%  mutate(quarter = quarter(sampledate),week=week(sampledate),month=month(sampledate))


#create semi-uniformally sampled time series (3 sets of for loops, select random sample per week,
#select a random monthly sample from the weekly samples, and finally select random sample from
#monthly samples for quarterly samples. result is semi uniformily quarterly sampled data)

lakes = unique(sec20$lagoslakeid)
sec20_reduced <- sec20[-(1:nrow(sec20)),]

#loop through all the lakes and randomly select a single observation by for each lake,year,week combination
for(i in 1:length(lakes)) {
  temp.lake = sec20[which(sec20$lagoslakeid==lakes[i]),] #subset to lake in lakes vector
  years = unique(temp.lake$year)
  for(j in 1:length(years)) {
    temp.year = temp.lake[which(temp.lake$year==years[j]),] #subset to year in years vector
    weeks = unique(temp.year$week)
    for(t in 1:length(weeks)) {
      temp.week = temp.year[which(temp.year$week==weeks[t]),] #subset to week within each lake year combination
      temp.dat = temp.week[sample(1:nrow(temp.week),1),] #randomly select one observation from each week
      sec20_reduced = rbind(sec20_reduced,temp.dat) #bind that observation to reduced datasets
    }
  }
}

#loop through all the lakes and randomly select a single observation by for each lake,year, month combination
lakes = unique(sec20_reduced$lagoslakeid)
sec20_monthly = sec20_reduced[-(1:nrow(sec20_reduced)),]

for(i in 1:length(lakes)) {
  temp.lake = sec20_reduced[which(sec20_reduced$lagoslakeid==lakes[i]),] #subset to lake in lakes vector
  years = unique(temp.lake$year)
  for(j in 1:length(years)) {
    temp.year = temp.lake[which(temp.lake$year==years[j]),] #subset to year in years vector
    months = unique(temp.year$month)
    for(t in 1:length(months)) {
      temp.month = temp.year[which(temp.year$month==months[t]),] #subset to week within each lake year combination
      temp.dat = temp.month[sample(1:nrow(temp.month),1),] #randomly select one observation from each week
      sec20_monthly = rbind(sec20_monthly,temp.dat) #bind that observation to reduced datasets
    }
  }
}

#loop through all the lakes and randomly select a single observation by for each lake,year, quarter combination
lakes = unique(sec20_monthly$lagoslakeid)
sec20_quarterly = sec20_monthly[-(1:nrow(sec20_monthly)),]

for(i in 1:length(lakes)) {
  temp.lake = sec20_monthly[which(sec20_monthly$lagoslakeid==lakes[i]),] #subset to lake in lakes vector
  years = unique(temp.lake$year)
  for(j in 1:length(years)) {
    temp.year = temp.lake[which(temp.lake$year==years[j]),] #subset to year in years vector
    quarters = unique(temp.year$quarter)
    for(t in 1:length(quarters)) {
      temp.quarter = temp.year[which(temp.year$quarter==quarters[t]),] #subset to week within each lake year combination
      temp.dat = temp.quarter[sample(1:nrow(temp.quarter),1),] #randomly select one observation from each week
      sec20_quarterly = rbind(sec20_quarterly,temp.dat) #bind that observation to reduced datasets
    }
  }
}

#identify lake years where 3 quarers of data exist and retains 20+ yrs of data
lakeqrcount <- sec20_quarterly %>%  group_by(lagoslakeid,year) %>% 
  summarise(ly.med=median(secchi, na.rm=T),count=n()) %>% filter(count>=3)
length(unique(lakeqrcount$lagoslakeid))
lakemed<- lakeqrcount %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n()) %>% filter(count >= 20)
#110 lakes meet the criteria
lakeyrkeeps <- lakeqrcount[lakeqrcount$lagoslakeid %in% lakemed$lagoslakeid,] %>% 
  select(lagoslakeid,year)

#filter the quarterly data to the above lake/year combinations

sec20unif <- lakeyrkeeps %>% left_join(sec20_quarterly)
newkeeps <- unique(sec20unif$lagoslakeid)

#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(1,2,3,5,10,20,30,60)

#data frame to store medians for each lake and sample size combo
lake.ss<- data.frame(med1obs=numeric(),
                     med2obs=numeric(),
                     med3obs=numeric(),
                     med5obs=numeric(),
                     med10obs=numeric(),
                     med20obs=numeric(),
                     med30obs=numeric(),
                     med60obs=numeric())

for (k in 1:length(newkeeps)) {
  
  id<-newkeeps[k]
  dat.id<-sec20unif[sec20unif$lagoslakeid==id,]
  med.t<-median(dat.id$secchi)
  
  
  #vector to store data to populate each row of lake.ss
  lakemeds=numeric(8)

for (j in 1:length(n)) {
  
  #dummy vector to store median from inner loop
  sample.med=numeric(1000)
  
  for (i in 1:1000) {
    
    #sample n observations of 20 year secchi dataset and take the median and cv, repeat 1,000 times to fill the chla.median and chla.variance vectors
    sample.x = sample(dat.id$secchi, n[j], replace=F)
    sample.med[i]=median(sample.x)
  }
  
  lakemeds[j]= median(abs((sample.med-med.t)/med.t*100))
  		
}
  
  lake.ss[k,]=lakemeds
  
}
  

library(vioplot)

vio1<-lake.ss$med1obs
vio2<-lake.ss$med2obs
vio3<-lake.ss$med3obs
vio5<-lake.ss$med5obs
vio10<-lake.ss$med10obs
vio20<-lake.ss$med20obs
vio30<-lake.ss$med30obs
vio60<-lake.ss$med60obs


png("SoL_graphics/TimeResampleViolin.png", width=6, height=5, units='in', res=300)
vioplot(vio1,vio2,vio3, vio5, vio10, vio20, vio30, vio60, 
        names=c("1","2","3","5", "10", "20", "30", "60"), 
        col="lightgrey")
mtext("% Difference in Median Secchi", side=2, line=2)
mtext("Sample Size", side=1, line=2)
abline(h=0)
dev.off()

##what about doing this with yearly medians to identify how many years of 20 are needed to capture
#year to year variation (not within year)?

#use lakeyrmed data for lakes in lakekeeps

sec20ymed <- sec20unif %>%  group_by(lagoslakeid,year) %>% 
  summarise(ly.med=median(secchi, na.rm=T),count=n())


#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(1,2,3,5,7,10,15,20)

#data frame to store medians for each lake and sample size combo
lake.ss.meds<- data.frame(med1obs=numeric(),
                     med2obs=numeric(),
                     med3obs=numeric(),
                     med5obs=numeric(),
                     med7obs=numeric(),
                     med10obs=numeric(),
                     med15obs=numeric(),
                     med20obs=numeric())

for (k in 1:length(newkeeps)) {
  
  id<-newkeeps[k]
  dat.id<-sec20ymed[sec20ymed$lagoslakeid==id,]
  med.t<-median(dat.id$ly.med)
  
  
  #vector to store data to populate each row of lake.ss
  lakemeds=numeric(8)
  
  for (j in 1:length(n)) {
    
    #dummy vector to store median from inner loop
    sample.med=numeric(1000)
    
    for (i in 1:1000) {
      
      #sample n observations of 20 year secchi dataset and take the median and cv, repeat 1,000 times to fill the chla.median and chla.variance vectors
      sample.x = sample(dat.id$ly.med, n[j], replace=F)
      sample.med[i]=median(sample.x)
    }
    
    lakemeds[j]= median(abs((sample.med-med.t)/med.t*100))
    
  }
  
  lake.ss.meds[k,]=lakemeds
  
}

vio1m<-lake.ss.meds$med1obs
vio2m<-lake.ss.meds$med2obs
vio3m<-lake.ss.meds$med3obs
vio5m<-lake.ss.meds$med5obs
vio7m<-lake.ss.meds$med7obs
vio10m<-lake.ss.meds$med10obs
vio15m<-lake.ss.meds$med15obs
vio20m<-lake.ss.meds$med20obs

vioplot(vio1m,vio2m, vio3m, vio5m, vio7m, vio10m, vio15m, vio20m, 
        names=c("1","2", "3", "5", "7", "10", "15", "20"), 
        col="lightgrey")

png("SoL_graphics/ViolinResample2Panel.png", width=6, height=10, units='in', res=300)
par(mfrow=c(2,1), mar=c(2,0,0,0), oma=c(2,4,1,1))
vioplot(vio1,vio2,vio3, vio5, vio10, vio20, vio30, vio60, 
        names=c("1","2","3","5", "10", "20", "30", "60"), 
        col="lightgrey")
mtext("Percent Error", side=2, line=2)
text(5.3, 43, "a) Within and across years")
vioplot(vio1m,vio2m, vio3m, vio5m, vio7m, vio10m, vio15m, vio20m, 
        names=c("1","2", "3", "5", "7", "10", "15", "20"), 
        col="lightgrey")
mtext("Percent Error", side=2, line=2)
mtext("Sample Size", side=1, line=2)
text(7.2, 64, "b) Across years only")
dev.off()

png("SoL_graphics/lt_secchi_data%03d.png", width=8, height=10.5, units='in', res=300)
par(mfrow=c(11,5), mar=c(0,0,0,0), oma=c(0,0,0,0))
for(i in 1:length(newkeeps)){
  dat.t = sec20unif[which(sec20unif$lagoslakeid==newkeeps[i]),]
  plot(dat.t$sampledate,dat.t$secchi,pch=16,xaxt="n",yaxt="n",xlab="",ylab="",type="b")
}
dev.off()


