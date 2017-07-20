#library(dplyr)
#See https://github.com/kchezik/River-Network-Flow-Trends for original data.
load("03_Data_Annual.RData")
library(tidyverse)
Y.Data = Y.Data %>%
	group_by(Station.ID) %>%
	mutate(Med.sc = as.vector(scale(Median.F, center = F)),
				 Min.sc = as.vector(scale(Min.F, center = F)),
				 Max.sc = as.vector(scale(Max.F, center = F)))

Flow.Time = function(time, response, weight, group, alpha = 0.25, seq.inc = 0.083, Te.cex = 0.7, xlab= "time", ylab ="response"){
	#vector scaled 0-1 for function 
	col = ((weight-min(weight))/(diff(range(weight))))^0.3
	
	#color ramp function
	library(RColorBrewer)
	Blues = brewer.pal(9, "Blues")#; RdYlBu = RdYlBu[-c(5,6,7)]
	FUN = colorRamp(Blues[c(2:7)], bias=1)
	
	#apply function
	cols = FUN(col)
	cols = rgb(cols, maxColorValue=256)
	cols = paste(cols, "99", sep = "")
	
	#Determination of point/line size and legend point data.
	Area.width = ((((weight-min(weight))/(diff(range(weight))))^0.3)+0.01)*12 #Line width.
	Area.size = (((weight-min(weight))/(diff(range(weight))))^0.3)*3 #Point size.
	
	par(cex.axis = Te.cex, mgp = c(0,0,0), oma = c(0,0,0,0), mar=c(0,0,0,0), family = 'serif', fg = NA, las = 1, mex = 1, mfrow = c(1,1))
	plot(response~jitter(time,factor = 2), col = cols, axes = F, xaxt = 'n', yaxt = 'n', pch = 16, cex = Area.size, xlim = c(1980,2000), ylim = c(0.27,2))
	#points(time, response, bg = cols, pch = 21, cex = Area.size)
	
	#Add loess lines
	df = data.frame(group, time, response, weight, cols, Area.width, stringsAsFactors = F)
	plyr::d_ply(df, "group", function(x){
		m = loess(response~time, model = T, span = alpha, data = x)
		lines(predict(m, data.frame(time = seq(min(x$time), max(x$time),seq.inc)))~seq(min(x$time), max(x$time),seq.inc), col = x$cols, lwd = x$Area.width)
	})
}

setwd("~/Documents/CV_Transcripts_Pubs/Web_Site/pictures")
jpeg(filename = "Flow-Time_Banner.jpg", width = 1350, height = 425, pointsize = 23, bg = "#F7F7F7")

Flow.Time(time = Y.Data$Year, response = Y.Data$Med.sc, weight = Y.Data$Area, group = Y.Data$Station.ID, xlab = "Year", ylab = expression("Scaled Median Flow (m"^3%.%"sec"^-1*")"%.%"year"^-1))

dev.off()
