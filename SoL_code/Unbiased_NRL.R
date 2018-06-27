library(LAGOSNE)
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(cowplot)

dat_full <-lagosne_load(version = "1.087.1")
dat_locus <- dat_full$locus %>% 
  select(lagoslakeid,lake_area_ha) %>% 
  mutate(log_area = log10(lake_area_ha)) %>%
  filter(lake_area_ha > 1)
hist(dat_locus$log_area)
dat_locus$group <- cut(dat_locus$log_area,breaks = 16,labels = c(1:16))     
hist(dat_locus$log_area,breaks = 16)

#assess data sampling bias by calcing sampling freq for each cluster
#full dataset (unbiased sample)
(full <- table(dat_locus$group)/sum(table(dat_locus$group)))
#observed dataset (what we sampled)
c.data<-readRDS(file="SoL_data/SoL_data.rds")
#Get summer medians
summer.dat <- c.data %>% filter(day>=166 & day<=258) %>% 
  select(lagoslakeid,
         year,
         chla,
         colort,
         doc,
         nh4,
         no2no3,
         srp,
         tkn,
         tn,
         tn_calculated,
         tn_combined,
         tp,
         secchi) %>% 
  group_by(lagoslakeid,year) %>% 
  summarise_all(funs(med = median(., na.rm = TRUE))) %>% 
  select(-year) %>% 
  group_by(lagoslakeid) %>% 
  summarise_all(funs(median(., na.rm = TRUE))) %>% 
  left_join(dat_locus)

#get variable specific dataframe (select which variable in var.dat)
var.dat <- summer.dat %>% select(lagoslakeid,colort_med,group) %>% na.omit()
names(var.dat)[2] <- "value"

#get sample frequency for observation dataset
obs <- table(var.dat$group)/sum(table(var.dat$group))
table(var.dat$group)
#plot biases
dat.overview = data.frame(cluster=1:16,full=as.vector(full),obs=as.vector(obs))
dat.overview <- dat.overview %>% gather(key = "key",value = "value",-cluster)
p1 <- ggplot(dat.overview, aes(fill=key, y=value, x=cluster)) +
  geom_bar(position="dodge", stat="identity") +labs(y="percent", 
x="size bin", title="ColorT Sampling Distribution") + 
  theme(plot.title = element_text(hjust = 0.5))
p1

#stratified sub-sample
num.samples = 1000  #how many total samples to get
pop.samples = round(as.vector(full)*num.samples,digits=0) #how many to sample from each cluster
names(pop.samples) = as.character(c(1:16)) #name vector
var.unbiased = stratified(indt = var.dat, #random stratification
                          group = "group",
                          size = pop.samples,
                          replace = TRUE)
dat = data.frame(value=c(sample(var.dat$value,size = 1000,replace = FALSE),var.unbiased$value), 
                 group=rep(c("Observed","Unbiased"), 
                           c(1000,
                             length (var.unbiased$value))))
p2 <- ggplot(dat, aes(value+.1, fill=group, colour=group)) +
  stat_ecdf(geom="step") +
  theme_bw() + 
  labs(y="ECDF", 
                  x="colort", 
       title="Comparison of Unbiased Distribution") + 
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_log10()
p2

p3 <- ggplot(dat, aes(x=group, y=value+.1, color=group)) + 
  geom_violin() + theme_bw() + 
  geom_boxplot(width=0.1) + scale_y_log10()
p3

plots <- plot_grid(p1,p2,p3,align="h",nrow = 3,ncol = 1)
save_plot("SoL_graphics/colort_stratified.png",
          plots,base_aspect_ratio = 1.3,nrow=3,ncol=1,
          base_width = 6)

#check the effect of resampling on median estimate
sample.median = rep(x = NA,100)
for(i in 1:1000){
  secchi.unbiased = stratified(indt = secchi, #random stratification
                               group = "group",
                               size = pop.samples,
                               replace = TRUE)
  sample.median[i] <- median(secchi.unbiased$secchi_med)
}
boxplot(sample.median)
range(sample.median)
