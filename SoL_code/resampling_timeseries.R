##want to do a basic resampling exercise with lakes that have at least 20 years of data -- how many years of data are required for mean and variance to approach "true" values from actual 20 yr time series.  actual values for mean and variance are specific by lagoslakeid, so calculate the absolute value of the % difference betweena "true" average and average based on small/resampled data.

library(dplyr)

data = readRDS("SoL_data/SoL_data.rds")

#try this with secchi first b/c lots of LT data

sec.dat = na.omit(data[,c("lagoslakeid", "year", "secchi")])


#calculate actual median and variance for lakes with at least 20 years of data, save vector of lakeids for those lakes to limit sample to them.

cv <- function(x) {return(sd(x)/mean(x))}

lakeyrmed<- sec.dat %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(secchi, na.rm=T))
lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n())
yrs20<- filter(lakemed, count>19)
lakekeeps<-unique(yrs20$lagoslakeid)

sec20 <- sec.dat[sec.dat$lagoslakeid %in% lakekeeps, ]


#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(2, 5, 10, 15, 20, 40, 60, 100)

#data frame to store medians for each lake and sample size combo
lake.ss<- data.frame(med2obs=numeric(),
                     med5obs=numeric(),
                     med10obs=numeric(),
                     med15obs=numeric(),
                     med20obs=numeric(),
                     med40obs=numeric(),
                     med60obs=numeric(),
                     med100obs=numeric())

for (k in 1:length(lakekeeps)) {
  
  id<-lakekeeps[k]
  dat.id<-sec.dat[sec.dat$lagoslakeid==id,]
  med.t<-median(dat.id$secchi)
  
  
  #vector to store data to populate each row of lake.ss
  lakemeds=numeric(8)

for (j in 1:length(n)) {
  
  #dummy vector to store median from inner loop
  sample.med=numeric(1000)
  
  for (i in 1:1000) {
    
    #sample n observations of 20 year secchi dataset and take the median and cv, repeat 1,000 times to fill the chla.median and chla.variance vectors
    sample.x = sample(dat.id$secchi, n[j], replace=T)
    sample.med[i]=median(sample.x)
  }
  
  lakemeds[j]= (median(sample.med)-med.t)/med.t*100
  		
}
  
  lake.ss[k,]=lakemeds
  
}
  

library(vioplot)

vio2<-lake.ss$med2obs
vio5<-lake.ss$med5obs
vio10<-lake.ss$med10obs
vio15<-lake.ss$med15obs
vio20<-lake.ss$med20obs
vio40<-lake.ss$med40obs
vio60<-lake.ss$med60obs
vio100<-lake.ss$med100obs

vioplot(vio2, vio5, vio10, vio15, vio20, vio40, vio60, vio100, names=c("2", "5", "10", "15", "20", "40", "60", "100"), 
        col="lightgrey")
abline(h=0)

mat.ss<-as.matrix(lake.ss)
boxplot.matrix(mat.ss)

