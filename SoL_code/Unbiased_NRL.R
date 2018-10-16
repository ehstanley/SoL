library(LAGOSNE)
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(gridExtra)
library(cowplot)

#create variable for outputing summary data
summary.output = data.frame(temp=NA)

dat_full <-lagosne_load(version = "1.087.1")
dat_locus <- dat_full$locus %>% 
  select(lagoslakeid,lake_area_ha) %>% 
  mutate(log_area = log10(lake_area_ha)) %>%
  filter(lake_area_ha >= 1)
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

dat.full <- data.frame(paremeter=NA,cluster=NA,key=NA,value=NA)
dat.full <- dat.full[-1,]
#get variable specific dataframe (select which variable in var.dat)
parameter = names(summer.dat)[c(2:6,8:13)]
for(i in 1:length(parameter)){
var.dat <- summer.dat %>% select(lagoslakeid,parameter[i],group) %>% na.omit()
names(var.dat)[2] <- "value"

#get sample frequency for observation dataset
obs <- table(var.dat$group)/sum(table(var.dat$group))
table(var.dat$group)
#data for area plot biases
param = parameter[i]
dat.overview = data.frame(cluster=1:16,full=as.vector(full),obs=as.vector(obs))
dat.overview <- dat.overview %>% 
  gather(key = "key",value = "value",-cluster) %>% 
  mutate(variable = as.character(param)) %>% 
  select(variable, cluster, key, value)

  dat.full <- rbind(dat.full,dat.overview)


#stratified sub-sample
num.samples = 1000  #how many total samples to get
pop.samples = round(as.vector(full)*num.samples,digits=0) #how many to sample from each cluster
pop.samples[which(pop.samples==0)] <- 1
names(pop.samples) = as.character(c(1:16)) #name vector
var.unbiased = stratified(indt = var.dat, #random stratification
                          group = "group",
                          size = pop.samples,
                          replace = TRUE)
dat = data.frame(value=c(sample(var.dat$value,size = num.samples,replace = FALSE),
                         var.unbiased$value), 
                 group=rep(c("Observed","Unbiased"), 
                           c(1000,
                             length (var.unbiased$value))))

assign(x = parameter[i],value = dat)
summary.stats <- tapply(dat$value, dat$group, summary)

summary.table <- data.frame(param = rep(parameter[i],2),
                            group=c("Observed_n1000","Unbiased_n1000"),
                            min = c(summary.stats$Observed[1],summary.stats$Unbiased[1]),
                            qua_1st = c(summary.stats$Observed[2],summary.stats$Unbiased[2]),
                            median = c(summary.stats$Observed[3],summary.stats$Unbiased[3]),
                            mean = c(summary.stats$Observed[4],summary.stats$Unbiased[4]),
                            qua_3rd = c(summary.stats$Observed[5],summary.stats$Unbiased[5]),
                            max = c(summary.stats$Observed[6],summary.stats$Unbiased[6])
                            )

if(nrow(summary.output)>=2) {
  summary.output = rbind(summary.output,summary.table)
} else summary.output = summary.table

}

write_csv(x = summary.output,"SoL_data/unbiased_stats.csv")

#########Unbiased Figure

p1 <- ggplot(data = chla_med,aes(value + 0.1, color=group,)) + stat_ecdf(geom="step") +
  theme_bw() + theme(legend.position = "none") +
  xlab(bquote(Chla~(mu*g~L^-1))) + ylab("ECDF") + scale_x_log10() +
  scale_color_manual(breaks=c("Observed","Unbiased"), values = c("grey","black")) +
  labs(color="Sample Population") +
  theme(text=element_text(size=10,  family="sans"))
p1
p2 <- ggplot(data = tn_calculated_med,aes(value + 0.1, color=group,)) + stat_ecdf(geom="step") +
  theme_bw() + theme(legend.position = "none") +
  xlab(bquote(Total~Nitrogen~(mu*g~L^-1))) + ylab("ECDF") + scale_x_log10() +
  scale_color_manual(breaks=c("Observed","Unbiased"), values = c("grey","black"))  +
  theme(text=element_text(size=10,  family="sans"))
p2
p3 <- ggplot(data = doc_med,aes(value + 0.1, color=group,)) + stat_ecdf(geom="step") +
  theme_bw() + theme(legend.position = "none") +
  xlab(bquote(DOC~(mg~L^-1))) + ylab("ECDF") + scale_x_log10() +
  scale_color_manual(breaks=c("Observed","Unbiased"), values = c("grey","black"))  +
  theme(text=element_text(size=10,  family="sans"))
p3
p4 <- ggplot(chla_med, aes(x=group, y=value+.1, color=group)) +
  geom_violin() + theme_bw() +
  geom_boxplot(width=0.1) + scale_y_log10() +
  theme_bw() + theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote(Chla~(mu*g~L^-1))) + xlab("") +
  scale_color_manual(breaks=c("Observed","Unbiased"), values = c("grey","black")) +
  theme(text=element_text(size=10,  family="sans"),axis.title=(element_text(size=10)))
p4
p5 <- ggplot(tn_calculated_med, aes(x=group, y=value+.1, color=group)) +
  geom_violin() + theme_bw() +
  geom_boxplot(width=0.1) + scale_y_log10() +
  theme_bw() + theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote(Total~Nitrogen~(mu*g~L^-1))) + xlab("") +
  scale_color_manual(breaks=c("Observed","Unbiased"), values = c("grey","black")) +
  theme(text=element_text(size=10,  family="sans"),axis.title=(element_text(size=10)))
p5
p6 <- ggplot(doc_med, aes(x=group, y=value+.1, color=group)) +
  geom_violin() + theme_bw() +
  geom_boxplot(width=0.1) + scale_y_log10() +
  theme_bw() + theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote(DOC~(mg~L^-1))) + xlab("") +
  scale_color_manual(breaks=c("Observed","Unbiased"), values = c("grey","black")) +
  theme(text=element_text(size=10,  family="sans"),axis.title=(element_text(size=10)))
p6

plots <- grid.arrange(p1,p2,p3,p4,p5,p6,ncol=3)
# plots <- plot_grid(p1,p2,p3,p4,p5,p6, align = "v",nrow = 2)
plots
ggsave(plot = plots,filename = "SoL_graphics/unbiased_distributions.tiff",
       device = "tiff",
       width = 7.5,
       height = 5,
       dpi = 300,
       units = "in",
       compression="lzw")
#Area Bias Figure
#
#limit to variables of interest
dat.full <- dat.full %>% filter(variable!= "tkn_med") %>%  
  filter(variable!="tn_calculated_med") %>% 
  filter(variable!= "tn_med")
dat.full$var2 <- factor(dat.full$variable, labels = c("Chla","Color","DOC","NH4","NO3","Secchi","TN","TP"))

p7 <- ggplot(dat.full, aes(fill=key, y=value, x=cluster)) +
  geom_bar(position="dodge", stat="identity") + facet_wrap(vars(var2)) +
  labs(y="Fraction of Lakes",x="Lake size class") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text=element_text(size=10,  family="sans"),
        axis.title=element_text(size=10)) +
  scale_fill_manual(breaks=c("obs","full"), values = c("grey","black"))
p7

ggsave(plot = p7,filename = "SoL_graphics/area_histograms.tiff",
       device = "tiff",
       width = 7.5,
       height = 5,
       dpi = 300,
       units = "in",
       compression="lzw")
