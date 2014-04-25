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

soilMlt <- melt(soilRowMean, id = c("DateTime", "Date", "RECORD", "Source", "Chamber", "temp"))

# remove NA in value
soilMlt <- soilMlt[complete.cases(soilMlt$value), ]
soilMlt <- droplevels(soilMlt) 

# Daily mean
DayChmMean <- ddply(soilMlt, .(Date, Chamber, temp, variable), summarise, 
                    Mean = mean(value, na.rm = TRUE),
                    Min = min(value, na.rm = TRUE),
                    Max = max(value, na.rm = TRUE), .progress = "text")
summary(DayChmMean)
?ddply

head(DayChmMean)
save(DayChmMean, file = "Output/Data/WTC_soilMoistTemp_Chamber_DailyMean.RData")

DayTrtMean <- ddply(DayChmMean, .(Date, temp), colMeanDF)
save(DayTrtMean, file = "Output/Data/WTC_soilMoistTemp_Trt_DailyMean.RData")
