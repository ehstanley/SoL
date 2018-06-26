##SMC June 2018
## want to sample from lakes with chem data distribution to create same distribution as the census
## i.e., un-bias the data to see if chemisty is different in our biased sample of lakes compared to all lakes

# one approach - bin data and sample to have same # in the bin as we'd expect for distribution
# seems cumbersome
# alternatively, try something like this:
# https://stackoverflow.com/questions/41495240/how-to-sample-data-based-off-the-distribution-of-another-dataset-in-r


#read latest dataset and dataset that has lake area and additional variables

c.data<-readRDS(file="SoL_data/SoL_data.rds")
l.data<-readRDS(file="SoL_data/lakes.rds")

l4.data<-l.data[l.data$lake_area_ha>=4,]

l.c.data<-l.data[l.data$lagoslakeid %in% c.data$lagoslakeid, ]
lc4.data<-l.c.data[l.c.data$lake_area_ha>=4,]

hist(log(l4.data$lake_area_ha))
hist(log(lc4.data$lake_area_ha))

#theywant to get model dist from obs dist
# Obs <- rnorm(1000, 5, 2.5)
# Model <- rnorm(10000, 8, 1.5)
# dens.obs <- density(Obs, n=length(Model))
# predict_density <- approxfun(dens.obs)
# SampleMod3 <- sample(Model, length(Obs), replace=FALSE, prob=predict_density(Model))


Biased<-round(log(l.c.data$lake_area_ha), digits=0)
Unbiased<-round(log(l.data$lake_area_ha), digits=0)
dens.bias <- density(Biased, adjust=0.8, n=16, from=min(Unbiased), to=max(Unbiased))
dens.unbias <- density(Unbiased, adjust=0.8, n=16, from=min(Unbiased), to=max(Unbiased))
#predict_density <-approxfun(dens.unbias)
#probvec<-dens.obs$y
newsample <- sample(dens.bias$x, 1000, replace=TRUE, prob=dens.unbias$y)


#this works but doesn't work b/c lagoslakeid needs to be affiliated with sample, so we need to pick a number of 
#bins and make a prob distribution so we can sample.  Use density with 2^n of a reasonable # (e.g. 16?)
#then convert whatever the hell dens$y is (doens't sum to 1) to a prob dist by dividing each by the sum of the vector

dens.unbias <- density(Unbiased, adjust=0.8, n=16, from=min(Unbiased), to=max(Unbiased))
probs<-dens.unbias$y/sum(dens.unbias$y)
bins<-dens.unbias$x

#stratified function can do this with our df of log lake area - probs is size - need to get this from bins still, group is bins, df is log lake area
#not sure what this returns?
                     