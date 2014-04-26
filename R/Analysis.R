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
<<<<<<< HEAD
# surface moisture & temperature
unique(soilTrtSmry$variable)
theme_set(theme_bw())

p <- ggplot(soilTrtSmry[soilTrtSmry$variable %in% c("SoilVW_5_25", "SoilTemp10"), ],
            aes(x = Date, y = Mean, col = temp, fill = temp))
p + geom_line()+
  geom_ribbon(aes(x = Date, ymin = Min, ymax = Max), col = NA, alpha = 0.3) + # col=NA removes the border lines
  facet_grid(variable ~., scales = "free_y") + 
  scale_fill_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) +
  scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp"))





?facet_grid
a <- 
levels(a$variable)


# stratified moisture
# stratified temperature

source("R//Figs.R")

#trying to combien with developped branch
