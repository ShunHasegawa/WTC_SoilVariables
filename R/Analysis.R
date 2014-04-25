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

# mean of SoilTemp10
soilRowMean <- cbind(pr_allsoils[ ,-grep("SoilTemp10", names(pr_allsoils))], 
                     'SoilTemp10' = rowMeans(pr_allsoils[ ,grep("SoilTemp10", names(pr_allsoils))], na.rm = TRUE))

head(soilRowMean)

soilMlt <- melt(soilRowMean, id = c("DateTime", "Date", "RECORD", "Date", "Source", "Chamber", "temp"))

soilMlt <- melt(te, id = c("DateTime", "Date", "RECORD", "Date", "Source", "Chamber", "temp"))

# Daily mean
DayChmMean <- ddply(soilRowMean, .(Date, Chamber, temp), colSmryDF)
save(DayChmMean, file = "Output/Data/WTC_soilMoistTemp_Chamber_DailyMean.RData")

DayTrtMean <- ddply(DayChmMean, .(Date, temp), colMeanDF)
save(DayTrtMean, file = "Output/Data/WTC_soilMoistTemp_Trt_DailyMean.RData")
