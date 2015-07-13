rm(list=ls(all=TRUE))

source("R/packages.R")
source("R/functions.R")

##################################
# download and process HIEv date #
##################################
# source("R//processData.R")

########################### 
#Daily Chamber & Trt mean #
###########################
# mean of SoilTemp10
soilRowMean <- cbind(CorrectSoil[ ,-grep("SoilTemp10", names(CorrectSoil))], 
                     'SoilTemp10' = rowMeans(CorrectSoil[ ,grep("SoilTemp10", names(CorrectSoil))], na.rm = TRUE))
soilmlt <- melt(soilRowMean, id = c("DateTime", "Date", "RECORD", "Source", "Chamber", "temp"), na.rm = TRUE)

summary(soilmlt)

soilChmSmry <- soilmlt %>% 
  group_by(Date, Chamber, temp, variable) %>%
  summarise(Mean = mean(value, na.rm = TRUE),
            Min = min(value, na.rm = TRUE),
            Max = max(value, na.rm = TRUE))
save(soilChmSmry, file = "Output/Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")

soilTrtSmry <- soilChmSmry %>%
  group_by(Date, temp, variable) %>%
  select(Mean, Min, Max) %>%
  summarise(lci = ci(Mean, na.rm = TRUE)[2],
            uci = ci(Mean, na.rm = TRUE)[3],
            N = sum(!is.na(Mean)),
            Mean = mean(Mean, na.rm = TRUE),
            Min = mean(Min, na.rm = TRUE),
            Max = mean(Max, na.rm = TRUE))
soilTrtSmry
save(soilTrtSmry, file = "Output/Data/WTC_soilMoistTemp_TempTrt_DailySummary.RData")

load("Output/Data/WTC_soilMoistTemp_Chamber_DailySummary.RData")
load("Output/Data/WTC_soilMoistTemp_TempTrt_DailySummary.RData")
########
# Figs #
########
source("R//Figs.R")
