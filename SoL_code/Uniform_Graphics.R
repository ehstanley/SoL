library(ggplot2)
x = rnorm(n = 10,mean = 1,sd = 5)
y = rnorm(n = 10, mean = 1000, sd= 5)

####PlosOne figure dimensions
####Text width = 5.2 inches
####Page width = 7.5 inches
####Max height = 8.75 inches (no space below for caption)
####proposed narrow width = 3.5 inches


#___________BASE___________#
tiff(filename = "base_plot.tiff",
     width = 3.5,
     height = 2,
     units = "in",res = 300,
     pointsize = 10,
     family="sans",
     compression = "lzw")

#Change mfrow for multi-panel plots but don't change margins
par(mfrow = c(1,1),
    oma = c(0,0,0,0),
    mar=c(2.4,3.5,.5,.5))

plot(x,y,xlab="",ylab="",xaxt="n",yaxt="n",pch=16)
## add axis ticks
axis(1, label = FALSE, tck = -0.015)
axis(2, label = FALSE, tck = -0.015)
## add the labels
axis(1, line = -0.7, lwd = 0, cex.axis = 0.9)
axis(2, line = -.4, lwd = 0, cex.axis = 0.9, las=1)
mtext(side=1,"X axis label", line=1.2)
#adjust the line value to move the y axis label in and out
#based on how many digits are in the y ticks
mtext(side=2, "Y axis label", line = 2.3)
dev.off()

#____________ggplot_____________#
ggplot() + geom_point(aes(x=x,y=y)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(text=element_text(size=10,  family="sans")) +
  xlab("X axis label") + ylab("Y axis label")

ggsave("ggplot.tiff",
       device = "tiff",
       width = 3.5,
       height = 2,
       dpi = 300,
       units = "in",
       compression="lzw")
