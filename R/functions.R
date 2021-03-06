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

######################
# plot soil moisture #
######################
PltMoist <- function(data, ylab, vals, legtitle, leglabel, colvar, ...){
  data <- droplevels(data)
  data[, "Mean"] <- data[, "Mean"] * 100
  p <- ggplot(data, aes_string(x = "Date", y = "Mean", col = colvar))
  p + geom_line(...)+
    scale_color_manual(values = vals, name = legtitle, labels = leglabel) +
    scale_x_date(breaks= date_breaks("1 month"), 
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2013-2-1", "2014-2-15"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1, size = 6)) +
    labs(x = "Month", y = ylab)
}

#########################
# plot soil temperature #
#########################
PltTemp <- function(data, ylab, ...){
  data <- droplevels(data)
  p <- ggplot(data, aes(x = Date, y = Mean, col = temp))
  p + geom_line(...)+
    scale_color_manual(values = c("blue", "red"), "Temp trt", labels = c("Ambient", "eTemp")) +
    scale_x_date(breaks= date_breaks("1 month"), 
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2013-2-1", "2014-2-15"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1)) +
    labs(x = "Month", y= ylab)
}

##########################################
# plot soil temperature at chamber level #
##########################################
PltChTemp <- function(data, FaceLab = NULL){
  data <- droplevels(data)
  if(!is.null(FaceLab)) data$variable <- factor(data$variable, labels = FaceLab)
  data$temp <- factor(data$temp, labels = c("Ambient", "eTemp"))
  p <- ggplot(data, aes(x = Date, y = Mean, col = Chamber))
  p + geom_line(size = 0.3) +
    scale_color_manual(values = palette(), "Chamber", labels = paste("Ch", c(1:12), sep = "_")) +
    labs(x = "Time", y = expression(Soil~temperature~at~10~cm~(~degree~C)))
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

#########################
# droplevels and subset #
#########################
subsetD <- function(...) droplevels(subset(...))

#####################
# Create summary df #
#####################
SmmryDF <- function(data, variable){
  yval <- data[, variable]
  Mean <- mean(yval, na.rm = TRUE)
  SE <- ci(yval, na.rm = TRUE)[4]
  N <- sum(!is.na(yval))
  data.frame(Mean, SE, N)
}