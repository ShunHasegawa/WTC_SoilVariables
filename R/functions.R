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
VarColMean <- function(data){
  colMeans(data[, c("SoilVW_5_25", "SoilVW_30_50", "SoilVW_HL", "Temp5", 
                    "Temp10", "Temp20", "Temp30", "Temp50", "Temp100",
                    "SoilTemp10_1", "SoilTemp10_2")], na.rm = TRUE)
}

#########################################
# Column summary for required variables #
#########################################
colSmryDF <- function(data){
  a <- data[, c("SoilVW_5_25", "SoilVW_30_50", "SoilVW_HL", 
                "Temp5", "Temp10", "Temp20", "Temp30", "Temp50", "Temp100", 
                "SoilTemp10")]
  means <- colMeans(a, na.rm = TRUE)
  
  TmpMin <- apply(a[, grep("Temp", names(a))], 2, min, na.rm = TRUE)
  names(TmpMin) <- paste("min", names(TmpMin), sep = "_")
  
  TmpMax <- apply(a[, grep("Temp", names(a))], 2, max, na.rm = TRUE)
  names(TmpMax) <- paste("max", names(TmpMax), sep = "_")
  c(means, TmpMin, TmpMax)
}

######################
# Plot all variables #
######################
# create a graph
pltVar <- function(data, variable){
  theme_set(theme_bw()) 
  p <- ggplot(data, aes_string(x = "Date", y = variable, col = "Chamber"))
  p + geom_point() +
    scale_color_manual(values = palette())
}

# plot all and save
PltAllSoil <- function(data, filename){
  mainlab <- names(data)[-1:-2]
  plst <- lapply(mainlab, function(x) pltVar(data, x))
  pdf(file = filename, width = 6, height = 5)
  l_ply(plst, print)
  dev.off()
}

##########################################
# produce ylabs for ggplot with faceting #
##########################################
ylab_label <- function(variable, value){
  return(ylabs[value])
}


