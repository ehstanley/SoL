##want to do a basic resampling exercise with lakes that have at least 20 years of data -- how many years of data are required for mean and variance to approach "true" values from actual 20 yr time series.  actual values for mean and variance are specific by lagoslakeid, so calculate the absolute value of the % difference betweena "true" average and average based on small/resampled data.

library(dplyr)
library(lubridate)
library(tidyverse)

data = readRDS("SoL_data/SoL_data.rds")

#try this with secchi first b/c lots of LT data

sec.dat = na.omit(data[,c("lagoslakeid", "year", "secchi","sampledate")])

#####Get Lottig 2016 data and select lakes from clusters 3:6
infile2  <- "https://pasta.lternet.edu/package/data/eml/edi/126/2/eeb6944d2f47c358ba9444d355e71535" 
infile2 <- sub("^https","http",infile2) 
dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "lagoslakeid",     
                 "nhd_latitude_degrees",     
                 "nhd_longitude_degrees",     
                 "huc8zoneid",     
                 "secchi_pattern_clusterID",     
                 "lake_area_ha",     
                 "maxdepth_meter",     
                 "secchi_median_meter",     
                 "lakeconnectivity",     
                 "watershed_area_ha",     
                 "drainagebasin_lakearea_ratio",     
                 "landscape_slope_100mbuffer_degrees",     
                 "landscape_slope_500mbuffer_degrees",     
                 "landscape_slope_watershed_degrees",     
                 "terrain_roughness_100mbuffer_meters",     
                 "terrain_roughness_500mbufferi_meters",     
                 "terrain_roughness_watershed_meters",     
                 "openwater_nlcd2011_100mbuffer_percent",     
                 "openwater_nlcd2011_500mbuffer_percent",     
                 "openwater_nlcd2011_watershed_percent",     
                 "developedopenspace_nlcd2011_100mbuffer_percent",     
                 "developedopenspace_nlcd2011_500mbuffer_percent",     
                 "developedopenspace_nlcd2011_watershed_percent",     
                 "developedlowintensity_nlcd2011_100mbuffer_percent",     
                 "developedlowintensity_nlcd2011_500mbuffer_percent",     
                 "developedlowintensity_nlcd2011_watershed_percent",     
                 "developedmediumintensity_nlcd2011_100mbuffer_percent",     
                 "developedmediumintensity_nlcd2011_500mbuffer_percent",     
                 "developedmediumintensity_nlcd2011_watershed_percent",     
                 "developedhighintensity_nlcd2011_100mbuffer_percent",     
                 "developedhighintensity_nlcd2011_500mbuffer_percent",     
                 "developedhighintensity_nlcd2011_watershed_percent",     
                 "barrenland_nlcd2011_100mbuffer_percent",     
                 "barrenland_nlcd2011_500mbuffer_percent",     
                 "barrenland_nlcd2011_watershed_percent",     
                 "deciduousforest_nlcd2011_100mbuffer_percent",     
                 "deciduousforest_nlcd2011_500mbuffer_percent",     
                 "deciduousforest_nlcd2011_watershed_percent",     
                 "evergreenforest_nlcd2011_100mbuffer_percent",     
                 "evergreenforest_nlcd2011_500mbuffer_percent",     
                 "evergreenforest_nlcd2011_watershed_percent",     
                 "mixedforest_nlcd2011_100mbuffer_percent",     
                 "mixedforest_nlcd2011_500mbuffer_percent",     
                 "mixedforest_nlcd2011_watershed_percent",     
                 "shrubscrub_nlcd2011_100mbuffer_percent",     
                 "shrubscrub_nlcd2011_500mbuffer_percent",     
                 "shrubscrub_nlcd2011_watershed_percent",     
                 "grassland_nlcd2011_100mbuffer_percent",     
                 "grassland_nlcd2011_500mbuffer_percent",     
                 "grassland_nlcd2011_watershed_percent",     
                 "pasturehay_nlcd2011_100mbuffer_percent",     
                 "pasturehay_nlcd2011_500mbuffer_percent",     
                 "pasturehay_nlcd2011_watershed_percent",     
                 "cultivatedcrops_nlcd2011_100mbuffer_percent",     
                 "cultivatedcrops_nlcd2011_500mbuffer_percent",     
                 "cultivatedcrops_nlcd2011_watershed_percent",     
                 "woodwetlands_nlcd2011_100mbuffer_percent",     
                 "woodywetlands_nlcd2011_500mbuffer_percent",     
                 "woodywetlands_nlcd2011_watershed_percent",     
                 "emergentwetlands_nlcd2011_100mbuffer_percent",     
                 "emergentwetlands_nlcd2011_500mbuffer_percent",     
                 "emergentwetlands_nlcd2011_watershed_percent",     
                 "precipitation_30yr_normal_annual_mean_mm",     
                 "temperature_30yr_normal_annual_mean_degreesc",     
                 "annual_cummulative_precip_clusterID",     
                 "runoff_mean_inyr",     
                 "baseflowindex_mean_percent"    ), check.names=TRUE)


# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$lagoslakeid)!="factor") dt2$lagoslakeid<- as.factor(dt2$lagoslakeid)
if (class(dt2$nhd_latitude_degrees)=="factor") dt2$nhd_latitude_degrees <-as.numeric(levels(dt2$nhd_latitude_degrees))[as.integer(dt2$nhd_latitude_degrees) ]
if (class(dt2$nhd_longitude_degrees)=="factor") dt2$nhd_longitude_degrees <-as.numeric(levels(dt2$nhd_longitude_degrees))[as.integer(dt2$nhd_longitude_degrees) ]
if (class(dt2$huc8zoneid)!="factor") dt2$huc8zoneid<- as.factor(dt2$huc8zoneid)
if (class(dt2$secchi_pattern_clusterID)!="factor") dt2$secchi_pattern_clusterID<- as.factor(dt2$secchi_pattern_clusterID)
if (class(dt2$lake_area_ha)=="factor") dt2$lake_area_ha <-as.numeric(levels(dt2$lake_area_ha))[as.integer(dt2$lake_area_ha) ]
if (class(dt2$maxdepth_meter)=="factor") dt2$maxdepth_meter <-as.numeric(levels(dt2$maxdepth_meter))[as.integer(dt2$maxdepth_meter) ]
if (class(dt2$secchi_median_meter)=="factor") dt2$secchi_median_meter <-as.numeric(levels(dt2$secchi_median_meter))[as.integer(dt2$secchi_median_meter) ]
if (class(dt2$lakeconnectivity)!="factor") dt2$lakeconnectivity<- as.factor(dt2$lakeconnectivity)
if (class(dt2$watershed_area_ha)=="factor") dt2$watershed_area_ha <-as.numeric(levels(dt2$watershed_area_ha))[as.integer(dt2$watershed_area_ha) ]
if (class(dt2$drainagebasin_lakearea_ratio)=="factor") dt2$drainagebasin_lakearea_ratio <-as.numeric(levels(dt2$drainagebasin_lakearea_ratio))[as.integer(dt2$drainagebasin_lakearea_ratio) ]
if (class(dt2$landscape_slope_100mbuffer_degrees)=="factor") dt2$landscape_slope_100mbuffer_degrees <-as.numeric(levels(dt2$landscape_slope_100mbuffer_degrees))[as.integer(dt2$landscape_slope_100mbuffer_degrees) ]
if (class(dt2$landscape_slope_500mbuffer_degrees)=="factor") dt2$landscape_slope_500mbuffer_degrees <-as.numeric(levels(dt2$landscape_slope_500mbuffer_degrees))[as.integer(dt2$landscape_slope_500mbuffer_degrees) ]
if (class(dt2$landscape_slope_watershed_degrees)=="factor") dt2$landscape_slope_watershed_degrees <-as.numeric(levels(dt2$landscape_slope_watershed_degrees))[as.integer(dt2$landscape_slope_watershed_degrees) ]
if (class(dt2$terrain_roughness_100mbuffer_meters)=="factor") dt2$terrain_roughness_100mbuffer_meters <-as.numeric(levels(dt2$terrain_roughness_100mbuffer_meters))[as.integer(dt2$terrain_roughness_100mbuffer_meters) ]
if (class(dt2$terrain_roughness_500mbufferi_meters)=="factor") dt2$terrain_roughness_500mbufferi_meters <-as.numeric(levels(dt2$terrain_roughness_500mbufferi_meters))[as.integer(dt2$terrain_roughness_500mbufferi_meters) ]
if (class(dt2$terrain_roughness_watershed_meters)=="factor") dt2$terrain_roughness_watershed_meters <-as.numeric(levels(dt2$terrain_roughness_watershed_meters))[as.integer(dt2$terrain_roughness_watershed_meters) ]
if (class(dt2$openwater_nlcd2011_100mbuffer_percent)=="factor") dt2$openwater_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$openwater_nlcd2011_100mbuffer_percent))[as.integer(dt2$openwater_nlcd2011_100mbuffer_percent) ]
if (class(dt2$openwater_nlcd2011_500mbuffer_percent)=="factor") dt2$openwater_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$openwater_nlcd2011_500mbuffer_percent))[as.integer(dt2$openwater_nlcd2011_500mbuffer_percent) ]
if (class(dt2$openwater_nlcd2011_watershed_percent)=="factor") dt2$openwater_nlcd2011_watershed_percent <-as.numeric(levels(dt2$openwater_nlcd2011_watershed_percent))[as.integer(dt2$openwater_nlcd2011_watershed_percent) ]
if (class(dt2$developedopenspace_nlcd2011_100mbuffer_percent)=="factor") dt2$developedopenspace_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$developedopenspace_nlcd2011_100mbuffer_percent))[as.integer(dt2$developedopenspace_nlcd2011_100mbuffer_percent) ]
if (class(dt2$developedopenspace_nlcd2011_500mbuffer_percent)=="factor") dt2$developedopenspace_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$developedopenspace_nlcd2011_500mbuffer_percent))[as.integer(dt2$developedopenspace_nlcd2011_500mbuffer_percent) ]
if (class(dt2$developedopenspace_nlcd2011_watershed_percent)=="factor") dt2$developedopenspace_nlcd2011_watershed_percent <-as.numeric(levels(dt2$developedopenspace_nlcd2011_watershed_percent))[as.integer(dt2$developedopenspace_nlcd2011_watershed_percent) ]
if (class(dt2$developedlowintensity_nlcd2011_100mbuffer_percent)=="factor") dt2$developedlowintensity_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$developedlowintensity_nlcd2011_100mbuffer_percent))[as.integer(dt2$developedlowintensity_nlcd2011_100mbuffer_percent) ]
if (class(dt2$developedlowintensity_nlcd2011_500mbuffer_percent)=="factor") dt2$developedlowintensity_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$developedlowintensity_nlcd2011_500mbuffer_percent))[as.integer(dt2$developedlowintensity_nlcd2011_500mbuffer_percent) ]
if (class(dt2$developedlowintensity_nlcd2011_watershed_percent)=="factor") dt2$developedlowintensity_nlcd2011_watershed_percent <-as.numeric(levels(dt2$developedlowintensity_nlcd2011_watershed_percent))[as.integer(dt2$developedlowintensity_nlcd2011_watershed_percent) ]
if (class(dt2$developedmediumintensity_nlcd2011_100mbuffer_percent)=="factor") dt2$developedmediumintensity_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$developedmediumintensity_nlcd2011_100mbuffer_percent))[as.integer(dt2$developedmediumintensity_nlcd2011_100mbuffer_percent) ]
if (class(dt2$developedmediumintensity_nlcd2011_500mbuffer_percent)=="factor") dt2$developedmediumintensity_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$developedmediumintensity_nlcd2011_500mbuffer_percent))[as.integer(dt2$developedmediumintensity_nlcd2011_500mbuffer_percent) ]
if (class(dt2$developedmediumintensity_nlcd2011_watershed_percent)=="factor") dt2$developedmediumintensity_nlcd2011_watershed_percent <-as.numeric(levels(dt2$developedmediumintensity_nlcd2011_watershed_percent))[as.integer(dt2$developedmediumintensity_nlcd2011_watershed_percent) ]
if (class(dt2$developedhighintensity_nlcd2011_100mbuffer_percent)=="factor") dt2$developedhighintensity_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$developedhighintensity_nlcd2011_100mbuffer_percent))[as.integer(dt2$developedhighintensity_nlcd2011_100mbuffer_percent) ]
if (class(dt2$developedhighintensity_nlcd2011_500mbuffer_percent)=="factor") dt2$developedhighintensity_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$developedhighintensity_nlcd2011_500mbuffer_percent))[as.integer(dt2$developedhighintensity_nlcd2011_500mbuffer_percent) ]
if (class(dt2$developedhighintensity_nlcd2011_watershed_percent)=="factor") dt2$developedhighintensity_nlcd2011_watershed_percent <-as.numeric(levels(dt2$developedhighintensity_nlcd2011_watershed_percent))[as.integer(dt2$developedhighintensity_nlcd2011_watershed_percent) ]
if (class(dt2$barrenland_nlcd2011_100mbuffer_percent)=="factor") dt2$barrenland_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$barrenland_nlcd2011_100mbuffer_percent))[as.integer(dt2$barrenland_nlcd2011_100mbuffer_percent) ]
if (class(dt2$barrenland_nlcd2011_500mbuffer_percent)=="factor") dt2$barrenland_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$barrenland_nlcd2011_500mbuffer_percent))[as.integer(dt2$barrenland_nlcd2011_500mbuffer_percent) ]
if (class(dt2$barrenland_nlcd2011_watershed_percent)=="factor") dt2$barrenland_nlcd2011_watershed_percent <-as.numeric(levels(dt2$barrenland_nlcd2011_watershed_percent))[as.integer(dt2$barrenland_nlcd2011_watershed_percent) ]
if (class(dt2$deciduousforest_nlcd2011_100mbuffer_percent)=="factor") dt2$deciduousforest_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$deciduousforest_nlcd2011_100mbuffer_percent))[as.integer(dt2$deciduousforest_nlcd2011_100mbuffer_percent) ]
if (class(dt2$deciduousforest_nlcd2011_500mbuffer_percent)=="factor") dt2$deciduousforest_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$deciduousforest_nlcd2011_500mbuffer_percent))[as.integer(dt2$deciduousforest_nlcd2011_500mbuffer_percent) ]
if (class(dt2$deciduousforest_nlcd2011_watershed_percent)=="factor") dt2$deciduousforest_nlcd2011_watershed_percent <-as.numeric(levels(dt2$deciduousforest_nlcd2011_watershed_percent))[as.integer(dt2$deciduousforest_nlcd2011_watershed_percent) ]
if (class(dt2$evergreenforest_nlcd2011_100mbuffer_percent)=="factor") dt2$evergreenforest_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$evergreenforest_nlcd2011_100mbuffer_percent))[as.integer(dt2$evergreenforest_nlcd2011_100mbuffer_percent) ]
if (class(dt2$evergreenforest_nlcd2011_500mbuffer_percent)=="factor") dt2$evergreenforest_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$evergreenforest_nlcd2011_500mbuffer_percent))[as.integer(dt2$evergreenforest_nlcd2011_500mbuffer_percent) ]
if (class(dt2$evergreenforest_nlcd2011_watershed_percent)=="factor") dt2$evergreenforest_nlcd2011_watershed_percent <-as.numeric(levels(dt2$evergreenforest_nlcd2011_watershed_percent))[as.integer(dt2$evergreenforest_nlcd2011_watershed_percent) ]
if (class(dt2$mixedforest_nlcd2011_100mbuffer_percent)=="factor") dt2$mixedforest_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$mixedforest_nlcd2011_100mbuffer_percent))[as.integer(dt2$mixedforest_nlcd2011_100mbuffer_percent) ]
if (class(dt2$mixedforest_nlcd2011_500mbuffer_percent)=="factor") dt2$mixedforest_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$mixedforest_nlcd2011_500mbuffer_percent))[as.integer(dt2$mixedforest_nlcd2011_500mbuffer_percent) ]
if (class(dt2$mixedforest_nlcd2011_watershed_percent)=="factor") dt2$mixedforest_nlcd2011_watershed_percent <-as.numeric(levels(dt2$mixedforest_nlcd2011_watershed_percent))[as.integer(dt2$mixedforest_nlcd2011_watershed_percent) ]
if (class(dt2$shrubscrub_nlcd2011_100mbuffer_percent)=="factor") dt2$shrubscrub_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$shrubscrub_nlcd2011_100mbuffer_percent))[as.integer(dt2$shrubscrub_nlcd2011_100mbuffer_percent) ]
if (class(dt2$shrubscrub_nlcd2011_500mbuffer_percent)=="factor") dt2$shrubscrub_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$shrubscrub_nlcd2011_500mbuffer_percent))[as.integer(dt2$shrubscrub_nlcd2011_500mbuffer_percent) ]
if (class(dt2$shrubscrub_nlcd2011_watershed_percent)=="factor") dt2$shrubscrub_nlcd2011_watershed_percent <-as.numeric(levels(dt2$shrubscrub_nlcd2011_watershed_percent))[as.integer(dt2$shrubscrub_nlcd2011_watershed_percent) ]
if (class(dt2$grassland_nlcd2011_100mbuffer_percent)=="factor") dt2$grassland_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$grassland_nlcd2011_100mbuffer_percent))[as.integer(dt2$grassland_nlcd2011_100mbuffer_percent) ]
if (class(dt2$grassland_nlcd2011_500mbuffer_percent)=="factor") dt2$grassland_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$grassland_nlcd2011_500mbuffer_percent))[as.integer(dt2$grassland_nlcd2011_500mbuffer_percent) ]
if (class(dt2$grassland_nlcd2011_watershed_percent)=="factor") dt2$grassland_nlcd2011_watershed_percent <-as.numeric(levels(dt2$grassland_nlcd2011_watershed_percent))[as.integer(dt2$grassland_nlcd2011_watershed_percent) ]
if (class(dt2$pasturehay_nlcd2011_100mbuffer_percent)=="factor") dt2$pasturehay_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$pasturehay_nlcd2011_100mbuffer_percent))[as.integer(dt2$pasturehay_nlcd2011_100mbuffer_percent) ]
if (class(dt2$pasturehay_nlcd2011_500mbuffer_percent)=="factor") dt2$pasturehay_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$pasturehay_nlcd2011_500mbuffer_percent))[as.integer(dt2$pasturehay_nlcd2011_500mbuffer_percent) ]
if (class(dt2$pasturehay_nlcd2011_watershed_percent)=="factor") dt2$pasturehay_nlcd2011_watershed_percent <-as.numeric(levels(dt2$pasturehay_nlcd2011_watershed_percent))[as.integer(dt2$pasturehay_nlcd2011_watershed_percent) ]
if (class(dt2$cultivatedcrops_nlcd2011_100mbuffer_percent)=="factor") dt2$cultivatedcrops_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$cultivatedcrops_nlcd2011_100mbuffer_percent))[as.integer(dt2$cultivatedcrops_nlcd2011_100mbuffer_percent) ]
if (class(dt2$cultivatedcrops_nlcd2011_500mbuffer_percent)=="factor") dt2$cultivatedcrops_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$cultivatedcrops_nlcd2011_500mbuffer_percent))[as.integer(dt2$cultivatedcrops_nlcd2011_500mbuffer_percent) ]
if (class(dt2$cultivatedcrops_nlcd2011_watershed_percent)=="factor") dt2$cultivatedcrops_nlcd2011_watershed_percent <-as.numeric(levels(dt2$cultivatedcrops_nlcd2011_watershed_percent))[as.integer(dt2$cultivatedcrops_nlcd2011_watershed_percent) ]
if (class(dt2$woodwetlands_nlcd2011_100mbuffer_percent)=="factor") dt2$woodwetlands_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$woodwetlands_nlcd2011_100mbuffer_percent))[as.integer(dt2$woodwetlands_nlcd2011_100mbuffer_percent) ]
if (class(dt2$woodywetlands_nlcd2011_500mbuffer_percent)=="factor") dt2$woodywetlands_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$woodywetlands_nlcd2011_500mbuffer_percent))[as.integer(dt2$woodywetlands_nlcd2011_500mbuffer_percent) ]
if (class(dt2$woodywetlands_nlcd2011_watershed_percent)=="factor") dt2$woodywetlands_nlcd2011_watershed_percent <-as.numeric(levels(dt2$woodywetlands_nlcd2011_watershed_percent))[as.integer(dt2$woodywetlands_nlcd2011_watershed_percent) ]
if (class(dt2$emergentwetlands_nlcd2011_100mbuffer_percent)=="factor") dt2$emergentwetlands_nlcd2011_100mbuffer_percent <-as.numeric(levels(dt2$emergentwetlands_nlcd2011_100mbuffer_percent))[as.integer(dt2$emergentwetlands_nlcd2011_100mbuffer_percent) ]
if (class(dt2$emergentwetlands_nlcd2011_500mbuffer_percent)=="factor") dt2$emergentwetlands_nlcd2011_500mbuffer_percent <-as.numeric(levels(dt2$emergentwetlands_nlcd2011_500mbuffer_percent))[as.integer(dt2$emergentwetlands_nlcd2011_500mbuffer_percent) ]
if (class(dt2$emergentwetlands_nlcd2011_watershed_percent)=="factor") dt2$emergentwetlands_nlcd2011_watershed_percent <-as.numeric(levels(dt2$emergentwetlands_nlcd2011_watershed_percent))[as.integer(dt2$emergentwetlands_nlcd2011_watershed_percent) ]
if (class(dt2$precipitation_30yr_normal_annual_mean_mm)=="factor") dt2$precipitation_30yr_normal_annual_mean_mm <-as.numeric(levels(dt2$precipitation_30yr_normal_annual_mean_mm))[as.integer(dt2$precipitation_30yr_normal_annual_mean_mm) ]
if (class(dt2$temperature_30yr_normal_annual_mean_degreesc)=="factor") dt2$temperature_30yr_normal_annual_mean_degreesc <-as.numeric(levels(dt2$temperature_30yr_normal_annual_mean_degreesc))[as.integer(dt2$temperature_30yr_normal_annual_mean_degreesc) ]
if (class(dt2$annual_cummulative_precip_clusterID)!="factor") dt2$annual_cummulative_precip_clusterID<- as.factor(dt2$annual_cummulative_precip_clusterID)
if (class(dt2$runoff_mean_inyr)=="factor") dt2$runoff_mean_inyr <-as.numeric(levels(dt2$runoff_mean_inyr))[as.integer(dt2$runoff_mean_inyr) ]
if (class(dt2$baseflowindex_mean_percent)=="factor") dt2$baseflowindex_mean_percent <-as.numeric(levels(dt2$baseflowindex_mean_percent))[as.integer(dt2$baseflowindex_mean_percent) ]
str(dt2)

cluster.choice = c(3:6)
dt2 <- dt2 %>% select(lagoslakeid,secchi_pattern_clusterID) %>% filter(secchi_pattern_clusterID %in% cluster.choice)


#calculate actual median and variance for lakes with at least 20 years of data, save vector of lakeids for those lakes to limit sample to them.

cv <- function(x) {return(sd(x)/mean(x))}




# lakeyrmed<- sec.dat %>% group_by(lagoslakeid, year) %>% summarise(ly.med=median(secchi, na.rm=T))
# lakemed<- lakeyrmed %>% group_by(lagoslakeid) %>% summarise(l.med=median(ly.med), l.cv=cv(ly.med), count=n())
# yrs20<- filter(lakemed, count>19)
# lakekeeps<-unique(yrs20$lagoslakeid)

sec20 <- sec.dat[sec.dat$lagoslakeid %in% dt2$lagoslakeid, ] 

#deal with dates
sec20$sampledate = strptime(sec20$sampledate, "%m/%d/%Y")
sec20$sampledate = as.POSIXct(sec20$sampledate)
sec20 <- sec20 %>% mutate(sampledate = ymd(sampledate)) %>%  mutate(quarter = quarter(sampledate),week=week(sampledate),month=month(sampledate))

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










sec20obs<- sec20_monthly %>% group_by(lagoslakeid,year) %>% summarise(count=n())
(table(sec20obs$count)/sum(table(sec20obs$count)))

hist(sec20_monthly$quarter,breaks = c(0:4))
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
  
  lakemeds[j]= median(abs((sample.med-med.t)/med.t*100))
  		
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
vioplot(vio2, vio5, vio10, vio20, vio40, vio80, vio120, names=c("2", "5", "10", "20", "40", "80", "120"), 
        col="lightgrey")
mtext("% Difference in Median Secchi", side=2, line=2)
mtext("Sample Size", side=1, line=2)
abline(h=0)
dev.off()

##what about doing this with yearly medians to identify how many years of 20 are needed to capture
#year to year variation (not within year)?

#use lakeyrmed data for lakes in lakekeeps

sec20ymed <- lakeyrmed[lakeyrmed$lagoslakeid %in% lakekeeps, ]


#set up a vector of different sample sizes that we want to use - # of years sampled from lakes that have at least 20 years of data
n=c(2, 3, 4, 5, 7, 10, 15, 20)

#data frame to store medians for each lake and sample size combo
lake.ss.meds<- data.frame(med2obs=numeric(),
                     med3obs=numeric(),
                     med4obs=numeric(),
                     med5obs=numeric(),
                     med7obs=numeric(),
                     med10obs=numeric(),
                     med15obs=numeric(),
                     med20obs=numeric())

for (k in 1:length(lakekeeps)) {
  
  id<-lakekeeps[k]
  dat.id<-lakeyrmed[lakeyrmed$lagoslakeid==id,]
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

vio2m<-lake.ss.meds$med2obs
vio3m<-lake.ss.meds$med3obs
vio4m<-lake.ss.meds$med4obs
vio5m<-lake.ss.meds$med5obs
vio7m<-lake.ss.meds$med7obs
vio10m<-lake.ss.meds$med10obs
vio15m<-lake.ss.meds$med15obs
vio20m<-lake.ss.meds$med20obs

vioplot(vio2m, vio3m, vio4m, vio5m, vio7m, vio10m, vio15m, vio20m, names=c("2", "3", "4", "5", "7", "10", "15", "20"), 
        col="lightgrey")

png("SoL_graphics/ViolinResample2Panel.png", width=6, height=10, units='in', res=300)
par(mfrow=c(2,1), mar=c(2,0,0,0), oma=c(2,4,1,1))
vioplot(vio2, vio10, vio20, vio40, vio80, vio120, names=c("2", "10", "20", "40", "80", "120"), 
        col="lightgrey")
mtext("% Difference between sample median and true median", side=2, line=2)
text(5.3, 43, "a) Within and across years")
vioplot(vio2m, vio3m, vio4m, vio5m, vio7m, vio10m, vio15m, vio20m, names=c("2", "3", "4", "5", "7", "10", "15", "20"), 
        col="lightgrey")
mtext("% Difference between sample median and true median", side=2, line=2)
mtext("Sample Size", side=1, line=2)
text(7.2, 64, "b) Across years only")
dev.off()


