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

sec20obs<- sec20 %>% group_by(lagoslakeid) %>% summarise(count=n())
hist(sec20obs$count)
##def don't want over 500, get rid of those to get a better window on hist
sec20under500<-filter(sec20obs, count<500)
hist(sec20under500$count)

##150-350 observations still includes a lot of lakes and is a nice relatively tight window.  try that for now.
sec.20.bottom<-filter(sec20obs, count>149)
sec.20.both<-filter(sec.20.bottom, count<351)

#this gets us 625 lakes - still solid!
newkeeps<-unique(sec.20.both$lagoslakeid)

sec20unif <- sec.dat[sec.dat$lagoslakeid %in% newkeeps, ]


#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(2, 5, 10, 20, 40, 80, 120)

#data frame to store medians for each lake and sample size combo
lake.ss<- data.frame(med2obs=numeric(),
                     med5obs=numeric(),
                     med10obs=numeric(),
                     med20obs=numeric(),
                     med40obs=numeric(),
                     med80obs=numeric(),
                     med120obs=numeric())

for (k in 1:length(newkeeps)) {
  
  id<-newkeeps[k]
  dat.id<-sec.dat[sec.dat$lagoslakeid==id,]
  med.t<-median(dat.id$secchi)
  
  
  #vector to store data to populate each row of lake.ss
  lakemeds=numeric(7)

for (j in 1:length(n)) {
  
  #dummy vector to store median from inner loop
  sample.med=numeric(1000)
  
  for (i in 1:1000) {
    
    #sample n observations of 20 year secchi dataset and take the median and cv, repeat 1,000 times to fill the chla.median and chla.variance vectors
    sample.x = sample(dat.id$secchi, n[j], replace=F)
    sample.med[i]=median(sample.x)
  }
  
  lakemeds[j]= median((sample.med-med.t)/med.t*100)
  		
}
  
  lake.ss[k,]=lakemeds
  
}
  

library(vioplot)

vio2<-lake.ss$med2obs
vio5<-lake.ss$med5obs
vio10<-lake.ss$med10obs
vio20<-lake.ss$med20obs
vio40<-lake.ss$med40obs
vio80<-lake.ss$med80obs
vio120<-lake.ss$med120obs

png("SoL_graphics/TimeResampleViolin.png", width=6, height=5, units='in', res=300)
vioplot(vio2, vio10, vio20, vio40, vio80, vio120, names=c("2", "10", "20", "40", "80", "120"), 
        col="lightgrey")
mtext("% Difference in Median Secchi", side=2, line=2)
mtext("Sample Size", side=1, line=2)
abline(h=0)
dev.off()

mat.ss<-as.matrix(lake.ss)
boxplot.matrix(mat.ss)

