################
# process data #
################
PrcsDat <- function(data){
  #remvoe duplicate
  data <- data[!duplicated(data),]
  #add ring
  data$Chamber <- factor(substr(data$Source, 4, 5))
  #add temp
  data$temp <- factor(ifelse(as.numeric(data$Chamber) %in% seq(2, 12, 2), "elev", "amb"))
  # change column names more informative
  names(data)[c(3: 13)] <- c("SoilVW_5_25", "SoilVW_30_50", "SoilVW_HL", "Temp5", "Temp10", "Temp20",
                             "Temp30", "Temp50", "Temp100", "SoilTemp10_1", "SoilTemp10_2")
  
  return(data)
}

######################################
# Column mean for required variables #
######################################
colMeanDF <- function(data){
  a <- data[, c("SoilVW_5_25", "SoilVW_30_50", "SoilVW_HL", 
                "Temp5", "Temp10", "Temp20", "Temp30", "Temp50", "Temp100", 
                "SoilTemp10")]
  colMeans(a, na.rm = TRUE)
}
