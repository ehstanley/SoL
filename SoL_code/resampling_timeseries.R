##want to do a basic resampling exercise with lakes that have at least 20 years of data -- how many years of data are required for mean and variance to approach "true" values from actual 20 yr time series

library(dplyr)

data = readRDS("SoL_data/SoL_data.rds")

#try this with secchi first b/c lots of LT data

sec.dat = na.omit(data[,c("lagoslakeid", "sampledate", "secchi")])

#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(2, 5, 10, 15, 20)

#calculate actual median and variance for lakes with at least 20 years of data, save vector of lakeids for those lakes to limit sample to them.

ADD BASIC SUMMARIZE HERE

#set up dummy vectors to store the average median and average variance for each n, these will be as long as n becuase we'll return a single median and a single variance for each n that is the median of the 1000 iterations below
chla.median.median = numeric(length(n))
chla.variance.median = numeric(length(n))

for (j in 1:length(n)) {
  
  #set up dummy vectors to store median and variance estimates from the inner loop
  chla.median = numeric(1000)
  #chla.variance = numeric(1000)
  
  for (i in 1:1000) {
    
    #sample n elements of the chla dataset and take the median, repeat 1,000 times to fill the chla.median and chla.variance vectors
    sample.x = sample(lagoschla, n[j], replace=F)
    chla.median[i] = median(sample.x)
    #chla.variance[i] = sd(sample.x)
  }
  
  #take a median of the 1000 iterations for chla.median and a median of the 1000 iterations for chla.variance to summarize for each n				
  chla.median.median[j] = median(chla.median)
  #chla.variance.median[j] = median(chla.variance)
  chla.variance.median[j] = sd(chla.median)
}

#the actual mean and sd of all of the lagos chla data are 5.9 and 31.8 respectively. check out how the resampled data compared (vectors with 8 values for the 8 values of n)
chla.median.median
chla.variance.median

#plot the means and variance by sample size

par(mfrow=c(2,1))
barplot(chla.median.median, names=n, main="Median chla by sample size")
barplot(chla.variance.median, names=n, main="SD chla by sample size")

#medians are basically the same regardless of how much we resample
#variance increases from ~25 in low sample to 31 in higher samples.  This makes sense given how skewed the data are, our chances of having low variance in a sample of 25 is higher than 3200 becuase the 25 samples are unlikely to capture the shape of the chla data distribution very well but the 3200 are.
