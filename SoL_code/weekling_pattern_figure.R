library(LAGOSNE)
library(tidyverse)
library(lubridate)
library(splitstackshape)
library(cowplot)

#observed dataset (what we sampled)
c.data<-readRDS(file="SoL_data/SoL_data.rds")
c.data$sampledate <- as.POSIXct(strptime(c.data$sampledate,"%m/%d/%Y"))
c.data$week <- week(c.data$sampledate)
#Get summer weekly values
summer.dat <- c.data %>% filter(week>=24 & week<=37) %>% 
  select(lagoslakeid,
         year,
         week,
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
  group_by(lagoslakeid,year,week) %>%
  summarise_all(funs(med = median(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  select(-year) %>% 
  group_by(lagoslakeid,week) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

var.dat <- summer.dat %>% select(lagoslakeid,week, colort_med) %>% na.omit()
names(var.dat)[3] <- "value"
min(table(var.dat$week))
strat.dat <- stratified(indt = var.dat, #random stratification
                        group = "week",
                        size = 339,
                        replace = FALSE) %>% 
  select(-lagoslakeid) %>% 
  group_by(week) %>% 
  summarise(q25=quantile(value, probs=0.25),
          q50=quantile(value, probs=0.5),
          q75=quantile(value, probs=0.75),
          avg=mean(value),
          n=n())

p <- ggplot(strat.dat) + geom_ribbon(aes(x=week, ymin=q25, ymax=q75, 
                             y=NULL, color=NULL),alpha=0.5) +
  geom_line(aes(x=week,y=q50)) + labs(y="True Color")

p
secchi <- p
chla <-p
tp <- p
doc <- p
no2no3 <-p
nh4 <- p
tn <- p
color <- p

plots <- plot_grid(secchi,
                   chla,
                   tp,
                   doc,
                   no2no3,
                   nh4,
                   tn,
                   color,
                   align="h",nrow = 2,ncol = 4)
save_plot("SoL_graphics/summer_patterns.png",
          plots,base_aspect_ratio = 1.3,nrow=2,ncol=4,
          base_width = 6)
