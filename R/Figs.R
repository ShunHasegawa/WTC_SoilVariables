# Plot theme
science_theme <- theme(panel.border = element_rect(color = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.ticks.margin = unit(.5, "lines"),
                       legend.title = element_blank(),
                       legend.key.width = unit(2.5, "lines"),
                       legend.key = element_blank(),
                       legend.background = element_rect(fill = "transparent", colour = NA))

unique(soilTrtSmry$variable)

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))

theme_set(theme_bw())

############
# Moisture #
############
# surface moisture
TempTopMoist <- PltMoist(data = subset(soilTrtSmry, variable == "SoilVW_5_25"), 
                     ylab = "Soil moisture at 5-25 cm\n(% of volumetric water content)",
                     vals = c("blue", "red"), 
                     legtitle = "Temp trt", 
                     leglabel = c("Ambient", "eTemp"), 
                     colvar = "temp")


ChamTopMoist <- PltMoist(data = subset(data.frame(soilChmSmry), variable == "SoilVW_5_25"), 
                         ylab = "Soil moisture at 5-25 cm\n(% of volumetric water content)",
                         vals = palette(), 
                         legtitle = "Chamber", 
                         leglabel = paste("Ch", c(1:12), sep = "_"), 
                         colvar = "Chamber", 
                         size = 0.3,
                         aes_string(linetype = "Chamber")) +
  guides(color = guide_legend(keyheight = 0.8)) +
  scale_linetype_manual("Chamber", values = rep(c("solid", "dashed"), 6), 
                        labels = paste("Ch", c(1:12), sep = "_"))
  # in order to use "guides", the argument (i.e color this time) needs
  # to bedefined in ggplot. e.g. size or fill or shape can't be used for this plot
  
ggsavePP(filename= "Output//Figs/WTC_Trt_SoilMoisture5_25cm", plot = TempTopMoist, width = 6, height = 3)
ggsavePP(filename= "Output//Figs/WTC_Chamber_SoilMoisture5_25cm", plot = ChamTopMoist, width = 6, height = 3)

# moisture at different depths
ylabs <- list(
  'SoilVW_5_25' = "5-25 cm",
  'SoilVW_30_50' = "30-50 cm",
  'SoilVW_HL' = "HL",
  'amb' = "Ambient",
  'elev' = "eTemp"
)
unique(soilChmSmry$temp)
## Temp trt ##
MoistDifDep <- PltMoist(data = soilTrtSmry[grep("SoilVW", soilTrtSmry$variable), ],
                        ylab = "Soil moisture\n(% of volumetric water content)",
                        vals = c("blue", "red"),
                        legtitle = "Temp trt",
                        leglabel = c("Ambient", "eTemp"), 
                        colvar = "temp") + 
  facet_grid(variable ~., labeller= ylab_label)

ggsavePP(filename= "Output//Figs/WTC_Trt_SoilMoistureDepths", plot = MoistDifDep, width = 6, height = 6)

## Chamber ##
ChMoistDifDep <- PltMoist(data = soilChmSmry[grep("SoilVW", soilChmSmry$variable), ],
                        ylab = "Soil moisture\n(% of volumetric water content)",
                        vals = palette(),
                        legtitle = "Chamber",
                        leglabel = paste("Ch", c(1:12), sep = "_"), 
                        colvar = "Chamber",
                        aes_string(linetype = "Chamber")) +                        
  facet_grid(variable ~., labeller= ylab_label) +
  scale_linetype_manual("Chamber", values = rep(c("solid", "dashed"), 6), labels = paste("Ch", c(1:12), sep = "_"))
  
ggsavePP(filename= "Output//Figs/WTC_Chamber_SoilMoistureDepths", plot = ChMoistDifDep, width = 6, height = 6)



###############
# Temperature #
###############
# surface temperature
## Temp trt ##
ToTemp <- PltTemp(data = subset(soilTrtSmry, variable == "SoilTemp10"),
                  ylab = expression(Soil~temperature~at~10~cm~(~degree~C)))
ggsavePP(filename= "Output//Figs/WTC_Trt_SoilTemp10cm", plot = ToTemp, width = 6, height = 3)

########################
# Fig for presentation #
########################
poster_theme <- theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      legend.position = "non")

## line graph
ToTemp <- PltTemp(data = subset(soilTrtSmry, variable == "SoilTemp10"),
                  ylab = expression((~degree~C))) +
  ggtitle(expression(Soil~temperature~at~10~cm))+
  poster_theme +
  theme(plot.title = element_text(size = 25))
ggsavePP(filename= "Output//Figs/BES_Presentation/WTC_Trt_SoilTemp10cm", 
         plot = ToTemp, width = 5, height = 3)

## bargarph
# data to compute annual mean
df <- subset(soilChmSmry, variable == "SoilTemp10" & 
                Date >= as.Date("2013-2-1") &
                Date <= as.Date("2014-2-15"))
# annual chamber mean
AnnChMean <- ddply(df, .(temp, Chamber), function(x) SmmryDF(x, variable = "Mean"))

# annual temp mean
AnnTempMean <- ddply(AnnChMean, .(temp), function(x) SmmryDF(x, variable = "Mean"))

# create a fig
p <- ggplot(data = AnnTempMean, aes(x = temp, y = Mean, fill = temp))
p2 <- p + geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                width = .5) + 
  scale_fill_manual(values = c("blue", "red")) +
  poster_theme +
  scale_y_continuous(limits=c(16, 22), oob = rescale_none) +
  labs(x = NULL, y = NULL) +
  geom_text(aes(x = temp, y = Mean + SE, label = round(Mean, 1)), vjust = -.5) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + 
  ggtitle("Annual mean")
ggsavePP(filename= "Output//Figs/BES_Presentation/WTC_Trt_SoilTemp10cm_AnnualMean", 
         plot = p2, width = 2, height = 2)

##########################
# stratified temperature #
##########################
TempDifDep <- PltTemp(data = soilTrtSmry[grep("^Temp", soilTrtSmry$variable), ],
                      ylab = expression(Soil~temperature~(~degree~C)), size = 0.1) +
  facet_wrap(~variable, ncol = 2) +
  theme(axis.text.x = element_text(size = 6))
ggsavePP(filename= "Output//Figs/WTC_Trt_SoilTmpDepths", plot = TempDifDep, width = 6, height = 6)  

## Chamber ##
# top layer
ChTopTemp <-  PltChTemp(subset(soilChmSmry, variable == "SoilTemp10")) +
  facet_grid( .~temp, ) +
  guides(color = guide_legend(keyheight = 0.8, override.aes = list(size = 1))) +
  theme(axis.text.x = element_text(size = 6))

ggsavePP(filename= "Output//Figs/WTC_Chamber_SoilTemp10cm", plot = ChTopTemp, width = 6, height = 3)

# different layers
ChDifDepTemp <- PltChTemp(data = soilChmSmry[grep("^Temp", soilChmSmry$variable), ],
                          FaceLab = paste(c(5, 10, 20, 30, 50, 100), "cm")) +
  facet_grid(variable ~ temp) +
  theme(axis.text.x = element_text(size = 6)) +
  guides(color = guide_legend(override.aes = list(size = 1)))
ggsavePP(filename= "Output//Figs/WTC_Chamber_SoilTempDepths", plot = ChDifDepTemp, width = 6, height = 8)

######################
# Fig for manuscript #
######################
dd <- filter(soilTrtSmry, variable %in% c("SoilTemp10", "SoilVW_5_25") & Date <= as.Date("2014-2-15"))
dd[dd$variable ==  "SoilVW_5_25", c("Mean", "lci", "uci")] <- 
  apply(dd[dd$variable ==  "SoilVW_5_25", c("Mean", "lci", "uci")], 2,
        function(x) x * 100)

dd$variable <- factor(dd$variable, levels = c("SoilVW_5_25", "SoilTemp10"),
                      labels = c("Soil~moisture*~('%')", "Soil~temperature~(degree*C)"))
p <- ggplot(dd, aes(x = Date, y = Mean, col = temp))
p2 <- p + 
  geom_line() +
  geom_ribbon(aes(x = Date, ymin = lci, ymax = uci, fill = temp), col = NA, alpha = .4) +
  facet_grid(variable ~ ., scales = "free_y", labeller = label_parsed)+
  scale_color_manual(values = c("blue", "red"), labels = c("Ambient", "eTemp")) +
  scale_fill_manual(values = c("blue", "red"), labels = c("Ambient", "eTemp")) +
  scale_x_date(breaks= date_breaks("3 month"), 
               labels = date_format("%b-%y"),
               limits = as.Date(c("2013-2-1", "2014-2-15"))) +
  science_theme +
  theme(legend.position = c(.15, .9)) +
  labs(x = NULL, y = NULL)
p2
ggsavePP(plot = p2, filename = "Output/Figs/Manuscript/WTC_SoilVariable", width = 5, height = 4)
