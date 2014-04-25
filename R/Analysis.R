rm(list=ls(all=TRUE))

library(devtools)
library(HIEv)
library(plyr)
library(reshape)
library(packrat)
library(ggplot2)
library(gmodels)
library(car)
library(lubridate)

source("R/functions.R")

##################################
# download and process HIEv date #
##################################
# download up to date files from HIEv
setToken("wSoiD4s3UDMzxwdNhzmG ")
allsoils <- downloadTOA5("WTC.._Table1",cachefile="Data/hievdata/tmp.RData",
                         topath="Data/hievdata/row_data", maxnfiles = 300)
pr_allsoils <- PrcsDat(allsoils)

summary(pr_allsoils)

# there are some weirdvalues, let's identify
# dataset is too huge to plot everything so make daily summary
daySoil <- ddply(pr_allsoils, .(Date, Chamber), function(x) {
  colMeans(x[, c("SoilVW_5_25", "SoilVW_30_50", "SoilVW_HL", "Temp5", 
                 "Temp10", "Temp20", "Temp30", "Temp50", "Temp100",
                 "SoilTemp10_1", "SoilTemp10_2")], na.rm = TRUE)
}
)


pltVar <- function(variable){
  p <- ggplot(daySoil, aes_string(x = "Date", y = variable, col = "Chamber"))
  p + geom_point() +
    scale_color_manual(values = palette())
}

mainlab <- names(daySoil)[-1:-2]
theme_set(theme_bw()) 
plst <- lapply(mainlab, pltVar)
names(plst[[1]]$plot_env)
pdf(file = "Output//Figs/AllPlot.pdf", width = 6, height = 5)
l_ply(plst, print)
dev.off()

print(plst[[2]])$plot


?ggsave






# mean of SoilTemp10
soilRowMean <- cbind(pr_allsoils[ ,-grep("SoilTemp10", names(pr_allsoils))], 
                     'SoilTemp10' = rowMeans(pr_allsoils[ ,grep("SoilTemp10", names(pr_allsoils))], na.rm = TRUE))

# Daily mean
DayChmMean <- ddply(soilRowMean, .(Date, Chamber, temp), colSmryDF)
summary(DayChmMean)

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))



?legend

save(DayChmMean, file = "Output/Data/WTC_soilMoistTemp_Chamber_DailyMean.RData")

DayTrtMean <- ddply(DayChmMean, .(Date, temp), colMeanDF)
save(DayTrtMean, file = "Output/Data/WTC_soilMoistTemp_Trt_DailyMean.RData")
