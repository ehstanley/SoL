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

state_order <- filter(population, lake_area_ha >= 4, !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
  group_by(state_name) %>%
  summarise(count = n()) %>%
  arrange(-count) %>%
  pull(state_name)

variable_summ <- function(input_dat, variable_quo) {
  
  variable_quo <- sym(variable_quo)
  sample_stats <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name, lake_area_ha, res_time_proxy) %>%
    distinct() %>%
    group_by(state_name) %>%
    summarize_at(vars(lake_area_ha, res_time_proxy), funs(mean), na.rm = T) %>%
    rename()
  
  count_stats <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name) %>%
    distinct() %>%
    group_by(state_name) %>%
    summarise(count = n()) %>%
    mutate(prop_count = count/sum(count))
  
  iso_stats <- filter(input_dat, !is.na(!!variable_quo), !(state_name %in% "OUT_OF_COUNTY_STATE")) %>%
    filter(lake_area_ha >= 4) %>%
    select(lagoslakeid, state_name, lakeconnection) %>%
    mutate(lakeconnection = as.factor(lakeconnection)) %>%
    distinct() %>%
    group_by(state_name) %>%
    summarise(count = n(),
              iso = summary(as.factor(lakeconnection))[4]) %>%
    mutate(prop_iso = iso/count) %>%
    select(state_name, prop_iso)
  
  all_stats <- left_join(sample_stats, count_stats, by = 'state_name') %>%
    left_join(iso_stats, by = 'state_name') %>%
    arrange(match(state_name, state_order), state_name)
  

  return(all_stats)
  
  
}

tp_summ <- variable_summ(sample, 'tp')
tn_summ <- variable_summ(sample, 'tn_combined')
chl_summ <- variable_summ(sample, 'chla')
doc_summ <- variable_summ(sample, 'doc')
pop_summ <- variable_summ(population, 'lagoslakeid')

# dotplot

# set colors for variables
my.cols <- brewer.pal(4, "Set1")
add.alpha <- function(col, alpha=0.8){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
my.cols <- add.alpha(my.cols)

png("SoL_graphics/Morphology_state_dotplot.png", height = 600, width = 1400)
par(mfrow=c(1,4), cex = 1, oma = c(0,5,3,3))
par(mar=c(5,0,0,0))

# n lakes in a state divided by n lakes in sample or population
# e.g., for TP, n lakes in MN with TP divided by n lakes with TP
dotchart2(pop_summ$prop_count, labels = pop_summ$state_name,dotsize = 2, xlab = "Proportion of lakes in sample or population", 
          xlim = c(0, 0.3), bty= "L", width.factor = .2, col = add.alpha('black', 0.7))
dotchart2(tn_summ$prop_count,  dotsize = 2, add = TRUE, col = my.cols[1])
dotchart2(tp_summ$prop_count,  dotsize = 2, add = TRUE, col = my.cols[2])
dotchart2(chl_summ$prop_count,  dotsize = 2, add = TRUE, col = my.cols[3])
dotchart2(doc_summ$prop_count,  dotsize = 2, add = TRUE, col = my.cols[4])

# lake area
dotchart2(pop_summ$lake_area_ha, labels = "",dotsize = 2, xlab = "Area (ha)", xlim = c(0, 600), 
          bty= "L", width.factor = .2, col = add.alpha('black', 0.7))
dotchart2(tn_summ$lake_area_ha,  dotsize = 2, add = TRUE, col = my.cols[1])
dotchart2(tp_summ$lake_area_ha,  dotsize = 2, add = TRUE, col = my.cols[2])
dotchart2(chl_summ$lake_area_ha,  dotsize = 2, add = TRUE, col = my.cols[3])
dotchart2(doc_summ$lake_area_ha,  dotsize = 2, add = TRUE, col = my.cols[4])

# residence time proxy or WSA/LA
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
legend(-.9,1.05, legend = c("Population", "TN Sample", "TP Sample", "Chl Sample", "DOC Sample"), 
       pch = 16, pt.cex = 2, bty = "n", col = c("black", my.cols), cex =1, horiz = TRUE)
dev.off()




