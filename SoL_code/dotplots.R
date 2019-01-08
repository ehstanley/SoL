# dotplots
library(dplyr)
library(rlang)
library(Hmisc)
library(RColorBrewer)

# read in data
sample <- readRDS('SoL_data/SoL_data.rds')
population <- readRDS('SoL_data/lakes.rds')

# add lake metadata to sample 
sample <- left_join(sample, select(population, -state_name), by = 'lagoslakeid')

# create "residence time" var by taking the iws_ha:lake_area_ha 
sample$res_time_proxy <- sample$iws_ha/sample$lake_area_ha
population$res_time_proxy <- population$iws_ha/population$lake_area_ha

# sample vs population statistics
# sample

# create state order
# for now, doing it from most to least number of lakes in the population

state_order <- filter(population, !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
  group_by(state_name) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  pull(state_name)

# define groups of variables
#sample <- mutate(trophic = sum(cis.na()))
variable_summ <- function(input_dat, variable) {
  
  variable_quo <- sym(variable)
  sample_stats <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    #filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name, lake_area_ha, res_time_proxy) %>%
    distinct() %>%
    group_by(state_name) %>%
    summarize_at(vars(lake_area_ha, res_time_proxy), funs(mean), na.rm = T) %>%
    rename()
  
  count_stats <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    #filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name) %>%
    distinct() %>%
    group_by(state_name) %>%
    summarise(count = n()) %>%
    mutate(prop_count = count/sum(count))
  
  iso_stats <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    #filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name, lakeconnection) %>%
    mutate(lakeconnection = as.factor(lakeconnection)) %>%
    distinct() %>%
    group_by(state_name) %>%
    summarise(count = n(),
              iso = summary(as.factor(lakeconnection))[4]) %>%
    mutate(prop_iso = iso/count) %>%
    select(state_name, prop_iso)
  
  sampled_n <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    #filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name) %>% 
    distinct() %>%
    group_by(state_name) %>%
    summarise(count_sample = n())
  
  pop_n <- filter(population, !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    #filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name) %>% 
    distinct() %>%
    group_by(state_name) %>%
    summarise(count_all = n())
  
  sampled_stats <- left_join(sampled_n, pop_n) %>%
    mutate(prop_sampled = count_sample/count_all) %>%
    select(state_name, prop_sampled)
  
  all_stats <- left_join(sample_stats, count_stats, by = 'state_name') %>%
    left_join(iso_stats, by = 'state_name') %>%
    left_join(sampled_stats, by = 'state_name') %>%
    arrange(match(state_name, state_order), state_name)
  

  return(all_stats)
  
  
}

tp_summ <- variable_summ(sample, 'tp')
tn_summ <- variable_summ(sample, 'tn_combined')
chl_summ <- variable_summ(sample, 'chla')
doc_summ <- variable_summ(sample, 'doc')
secchi_summ <- variable_summ(sample, 'secchi')
no3_summ <- variable_summ(sample, 'no2no3')
nh4_summ <- variable_summ(sample, 'nh4')
color_summ <- variable_summ(sample,'colort')
pop_summ <- variable_summ(population, 'lagoslakeid')

# dotplot

# set colors for variables
#Secchi- darkblue
#Chl- dodgerblue2 (boo dodgers!)
#TP- lightskyblue1

#TN- darkorchid4
#NO3- darkorchid2
#NH4- orchid1

#Color- tan4
#DOC- tan2
my.cols <- c("darkblue","dodgerblue2","lightskyblue1",
             "darkorchid4","darkorchid2","orchid1",
             "tan2","tan4")
add.alpha <- function(col, alpha=0.8){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
my.cols <- add.alpha(my.cols)
dot_size <-2.5

###### morphology ######
{tiff(filename = "SoL_graphics/dotplots.tiff",
     width = 5.2,
     height = 3.5,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")
par(mfrow=c(1,3), oma = c(1.5,5.5,2,.5),mar=c(2,5,0,0),cex.axis=1)

# n lakes in a state divided by n lakes in sample or population
# e.g., for TP, n lakes in MN with TP divided by n lakes with TP
#Trophic Variables
dotchart2(pop_summ$prop_count, labels = pop_summ$state_name,dotsize = dot_size, xlab = "", 
          xlim = c(-0.02, 0.4), bty= "L", width.factor = .2, col = add.alpha('black', 0.7),
          cex.labels = 0.7, pch = 8)
dotchart2(secchi_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[1])
dotchart2(tp_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[2])
dotchart2(chl_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[3])
box(lwd=1)

#Nitrogen Variables
dotchart2(pop_summ$prop_count, labels = "",
          dotsize = dot_size, 
          xlab = "", 
          xlim = c(-.02, 0.4), 
          bty= "L", width.factor = .2, 
          col = add.alpha('black', 0.7), pch = 8)
dotchart2(tn_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[4])
dotchart2(no3_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[5])
dotchart2(nh4_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[6])
box(lwd=1)
mtext(side=1,text = "Proportion of lakes in sample or population",line=2.2)

#carbon
dotchart2(pop_summ$prop_count, labels = "",
          dotsize = dot_size, 
          xlab = "", 
          xlim = c(-.02, 0.4), 
          bty= "L", width.factor = .2, 
          col = add.alpha('black', 0.7), pch = 8)
dotchart2(doc_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[7])
dotchart2(color_summ$prop_count,  dotsize = dot_size, add = TRUE, col = my.cols[8])
box(lwd=1)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(-0.65,1.1, legend = c("Census", "Secchi", "TP","Chl", 
                            "TN", "NO3", "NH4",
                            "DOC", "Color"), 
       text.width = c(0, 0.16, 0.16, 0.13, 0.12, 0.11, 0.11, 0.11, 0.11),
       pch = c(8, rep(16, 8)), pt.cex = 1.5, bty = "n", col = c("black", my.cols), cex =1, horiz = TRUE)

dev.off()}
# lake area

# minor ticks for log scale function, edited version of 
# function found here:  https://stackoverflow.com/questions/6955440/displaying-minor-logarithmic-ticks-in-x-axis-in-r

minor.ticks.axis <- function(ax,n,t.ratio=0.5,mn,mx,...){
  
  lims <- par("usr")
  if(ax %in%c(1,3)) lims <- lims[1:2] else lims[3:4]
  
  major.ticks <- mn:mx
  if(missing(mn)) mn <- min(major.ticks)
  if(missing(mx)) mx <- max(major.ticks)
  
  major.ticks <- major.ticks[major.ticks >= mn & major.ticks <= mx]
  
  labels <- sapply(major.ticks,function(i)
    as.expression(bquote(10^ .(i)))
  )
  axis(ax,at=major.ticks,labels=FALSE,...)
  
  n <- n+2
  minors <- log10(pretty(10^major.ticks[1:2],n))-major.ticks[1]
  minors <- minors[-c(1,n)]
  
  minor.ticks = c(outer(minors,major.ticks,`+`))
  minor.ticks <- minor.ticks[minor.ticks > mn & minor.ticks < mx]
  
  
  axis(ax,at=minor.ticks,tcl=par("tcl")*t.ratio,labels=FALSE)
}

x <- 10^(0:8)
y <- 1:9
plot(log10(x),y,xaxt="n",xlab="x",xlim=c(0,9))
minor.ticks.axis(1,9,mn=0,mx=8)

tiff(filename = "SoL_graphics/dotplots_area.tiff",
     width = 3.5,
     height = 3.5,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")
par(mfrow=c(1,1), oma = c(1.5,5.5,2,.5),mar=c(2,5,0,0),cex.axis=1)
dotchart2(log10(pop_summ$lake_area_ha), labels = pop_summ$state_name,dotsize = 1.5, xlab = "", 
          xlim = c(0.9, 4.1), bty= "L", width.factor = .2, col = add.alpha('black', 0.7),
          cex.labels = 0.7,axisat = c(0:4), axislabels = c('1', '10', '100', '1000', '10000'),
          pch = 8)
minor.ticks.axis(1,9,mn=1,mx=4)

dotchart2(log10(chl_summ$lake_area_ha),  dotsize = 1.5, add = TRUE, col = my.cols[2])
dotchart2(log10(tn_summ$lake_area_ha),  dotsize = 1.5, add = TRUE, col = my.cols[4])
dotchart2(log10(doc_summ$lake_area_ha),  dotsize = 1.5, add = TRUE, col = my.cols[7])
box(lwd=1)
mtext(side=1,text = "Area (ha)",line=2.2)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(-0.36,1.1, legend = c("Census", "Chl", "TN", "DOC"), 
       text.width = c(0, 0.28, 0.2, 0.17),
       pch = c(8,rep(16,3)), pt.cex = 1.3, bty = "n", col = c("black", my.cols[c(2,4,7)]), cex =1, horiz = TRUE)
dev.off()


# residence time proxy or WSA/LA
#not appropriate for SoL since we used lakes <4ha - WSA data are considered unreliable for these lakes.
dotchart2(pop_summ$res_time_proxy, labels = "", xlab = "WSA:LA", dotsize = 2, xlim = c(0, 150), 
          bty= "L", width.factor = .2, col = add.alpha('black', 0.7))
dotchart2(tn_summ$res_time_proxy,  dotsize = 2, add = TRUE, col = my.cols[1])
dotchart2(tp_summ$res_time_proxy,  dotsize = 2, add = TRUE, col = my.cols[2])
dotchart2(chl_summ$res_time_proxy,  dotsize = 2, add = TRUE, col = my.cols[3])
dotchart2(doc_summ$res_time_proxy,  dotsize = 2, add = TRUE, col = my.cols[4])

# proportion of sampled or population lakes in each state that are isolated
dotchart2(pop_summ$prop_iso, labels = "", xlab = "Proportion isolated lakes", dotsize = 2, xlim = c(-0.02, 0.5), 
          bty= "L", width.factor = .2, col = add.alpha('black', 0.7))
dotchart2(tn_summ$prop_iso,  dotsize = 2, add = TRUE, col = my.cols[1])
dotchart2(tp_summ$prop_iso,  dotsize = 2, add = TRUE, col = my.cols[2])
dotchart2(chl_summ$prop_iso,  dotsize = 2, add = TRUE, col = my.cols[3])
dotchart2(doc_summ$prop_iso,  dotsize = 2, add = TRUE, col = my.cols[4])

# add legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(-.9,1.05, legend = c("Census", "TN Sample", "TP Sample", "Chl Sample", "DOC Sample"), 
       pch = 16, pt.cex = 2, bty = "n", col = c("black", my.cols), cex =1, horiz = TRUE)
dev.off()

###### lakes sampled #####
# just proportion of lakes sampled
png("SoL_graphics/proportion_sampled_state_dotplot.png", height = 600, width = 400)
#par(mfrow=c(1,4), cex = 1, oma = c(0,5,3,3))
par(mar=c(5,7,1,1), oma = c(0, 5, 4, 0))

# dotplot of proportion of lakes sampled
dotchart2(tn_summ$prop_sampled, labels = pop_summ$state_name,  dotsize = 2, xlab = "Proportion of lakes sampled", 
          xlim = c(0, 0.7), bty= "L", width.factor = .2, col = my.cols[1])
dotchart2(tp_summ$prop_sampled,  dotsize = 2, add = TRUE, col = my.cols[2])
dotchart2(chl_summ$prop_sampled,  dotsize = 2, add = TRUE, col = my.cols[3])
dotchart2(doc_summ$prop_sampled,  dotsize = 2, add = TRUE, col = my.cols[4])


# add legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(-0.6,1, legend = c("TN Sample", "TP Sample", "Chl Sample", "DOC Sample"), 
       pch = 16, pt.cex = 2, bty = "n", col = my.cols, cex =1,ncol = 2)
dev.off()

##### lake area solo fig #########
png("SoL_graphics/lakearea_repvars_dotplot.png", height = 600, width = 450)
par(cex = 1, oma = c(0,5,3,3))
par(mar=c(5,0,0,0))
dotchart2(log10(pop_summ$lake_area_ha), labels = pop_summ$state_name,dotsize = 2, xlab = "Area (ha)", 
          bty= "L", width.factor = .2, col = add.alpha('black', 0.7), xlim = c(0.9, 4.1),
          axisat = c(0:4), axislabels = c('1', '10', '100', '1000', '10000'))
dotchart2(log10(tn_summ$lake_area_ha),  dotsize = 2, add = TRUE, col = my.cols[1])
#dotchart2(log10(tp_summ$lake_area_ha),  dotsize = 2, add = TRUE, col = my.cols[2])
dotchart2(log10(chl_summ$lake_area_ha),  dotsize = 2, add = TRUE, col = my.cols[3])
dotchart2(log10(doc_summ$lake_area_ha),  dotsize = 2, add = TRUE, col = my.cols[4])

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend(0.37,.85, legend = c("Population", "TN", "Chl", "DOC"), 
       pch = 16, pt.cex = 1.8, bty = "o", col = c(add.alpha('black', 0.7), my.cols[c(1,3,4)]), 
       cex =1, horiz = F)
dev.off()


