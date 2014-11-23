rm(list=ls(all=TRUE))

library(HIEv)
library(plyr)
library(reshape)
library(ggplot2)
library(gmodels)
library(car)
library(lubridate)
library(dplyr)

source("R/functions.R")

##################################
# download and process HIEv date #
##################################
source("R//processData.R")

########################### 
#Daily Chamber & Trt mean #
###########################
# mean of SoilTemp10
soilRowMean <- cbind(CorrectSoil[ ,-grep("SoilTemp10", names(CorrectSoil))], 
                     'SoilTemp10' = rowMeans(CorrectSoil[ ,grep("SoilTemp10", names(CorrectSoil))], na.rm = TRUE))
soilmlt <- melt(soilRowMean, id = c("DateTime", "Date", "RECORD", "Source", "Chamber", "temp"), na.rm = TRUE)

summary(soilmlt)

soilChmSmry <- soilmlt %.% 
  group_by(Date, Chamber, temp, variable) %.%
  summarise(Mean = mean(value, na.rm = TRUE),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE))
save(soilChmSmry, file = "Output/Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")

soilTrtSmry <- ddply(soilChmSmry, .(Date, temp, variable), function(x) colMeans(x[,c("Mean", "Min", "Max")], na.rm = TRUE))
save(soilTrtSmry, file = "Output/Data/WTC_soilMoistTemp_TempTrt_DailySummary.RData")

########
# Figs #
########
source("R//Figs.R")
