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

#################################################################
# Column mean and minimum & maximum temp for required variables #
#################################################################
colSmryDF <- function(data){
  a <- data[, c("SoilVW_5_25", "SoilVW_30_50", "SoilVW_HL", 
                "Temp5", "Temp10", "Temp20", "Temp30", "Temp50", "Temp100", 
                "SoilTemp10")]
  means <- colMeans(a, na.rm = TRUE)
  
  TmpMin <- apply(a[, grep("Temp", names(a))], 2, min, na.rm = TRUE)
  names(TmpMin) <- paste("min", names(TmpMin), sep = "_")
  
  TmpMax <- apply(a[, grep("Temp", names(a))], 2, max, na.rm = TRUE)
  names(TmpMax) <- paste("max", names(TmpMin), sep = "_")
  c(means, TmpMin, TmpMax)
}


