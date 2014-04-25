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
# there are some weird valued especially in Temp10, Temp100
pr_allsoils$Temp10[which(pr_allsoils$Temp10 < 2 | pr_allsoils$Temp10 > 40)] <- NA
pr_allsoils$Temp100[which(pr_allsoils$Temp100 < 2 | pr_allsoils$Temp100 > 40)] <- NA
pr_allsoils$SoilTemp10_1[which(pr_allsoils$SoilTemp10_1 < 5)] <- NA

# mean of SoilTemp10
soilRowMean <- cbind(pr_allsoils[ ,-grep("SoilTemp10", names(pr_allsoils))], 
                     'SoilTemp10' = rowMeans(pr_allsoils[ ,grep("SoilTemp10", names(pr_allsoils))], na.rm = TRUE))

# Daily mean
DayChmMean <- ddply(soilRowMean, .(Date, Chamber, temp), colSmryDF)
summary(DayChmMean)

save(DayChmMean, file = "Output/Data/WTC_soilMoistTemp_Chamber_DailyMean.RData")

DayTrtMean <- ddply(DayChmMean, .(Date, temp), colMeanDF)
save(DayTrtMean, file = "Output/Data/WTC_soilMoistTemp_Trt_DailyMean.RData")
