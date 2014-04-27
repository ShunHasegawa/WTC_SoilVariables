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


ChamTopMoist <- PltMoist(data = subset(soilChmSmry, variable == "SoilVW_5_25"), 
                         ylab = "Soil moisture at 5-25 cm\n(% of volumetric water content)",
                         vals = palette(), 
                         legtitle = "Chamber", 
                         leglabel = paste("Ch", c(1:12), sep = "_"), 
                         colvar = "Chamber", 
                         size = 0.3,
                         aes_string(linetype = "Chamber")) +
  guides(color = guide_legend(keyheight = 0.8)) +
  scale_linetype_manual("Chamber", values = rep(c("solid", "dashed"), 6), labels = paste("Ch", c(1:12), sep = "_"))
  # in order to use "guides", the argument (i.e color this time) needs
  # to bedefined in ggplot. e.g. size or fill or shape can't be used for this plot
  
ggsave(filename= "Output//Figs/WTC_Trt_SoilMoisture5_25cm.pdf", plot = TempTopMoist, width = 6, height = 3)
ggsave(filename= "Output//Figs/WTC_Chamber_SoilMoisture5_25cm.pdf", plot = ChamTopMoist, width = 6, height = 3)

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

ggsave(filename= "Output//Figs/WTC_Trt_SoilMoistureDepths.pdf", plot = MoistDifDep, width = 6, height = 6)

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
  
ggsave(filename= "Output//Figs/WTC_Chamber_SoilMoistureDepths.pdf", plot = ChMoistDifDep, width = 6, height = 6)



###############
# Temperature #
###############
# surface temperature
## Temp trt ##
ToTemp <- PltTemp(data = subset(soilTrtSmry, variable == "SoilTemp10"),
                  ylab = expression(Soil~temperature~at~10~cm~(~degree~C)))
ggsave(filename= "Output//Figs/WTC_Trt_SoilTemp10cm.pdf", plot = ToTemp, width = 6, height = 3)

# stratified temperature
TempDifDep <- PltTemp(data = soilTrtSmry[grep("^Temp", soilTrtSmry$variable), ],
                      ylab = expression(Soil~temperature~(~degree~C)), size = 0.1) +
  facet_wrap(~variable, ncol = 2) +
  theme(axis.text.x = element_text(size = 6))
ggsave(filename= "Output//Figs/WTC_SoilTmpDepths.pdf", plot = TempDifDep, width = 6, height = 6)  

## Chamber ##
# top layer
ChTopTemp <-  PltChTemp(subset(soilChmSmry, variable == "SoilTemp10")) +
  facet_grid( .~temp, ) +
  guides(color = guide_legend(keyheight = 0.8, override.aes = list(size = 1))) +
  theme(axis.text.x = element_text(size = 6))

ggsave(filename= "Output//Figs/WTC_Chamber_SoilTemp10cm.pdf", plot = ChTopTemp, width = 6, height = 3)

# different layers
ChDifDepTemp <- PltChTemp(data = soilChmSmry[grep("^Temp", soilChmSmry$variable), ],
                          FaceLab = paste(c(5, 10, 20, 30, 50, 100), "cm")) +
  facet_grid(variable ~ temp) +
  theme(axis.text.x = element_text(size = 6)) +
  guides(color = guide_legend(override.aes = list(size = 1)))
ggsave(filename= "Output//Figs/WTC_Chamber_SoilTempDepths.pdf", plot = ChDifDepTemp, width = 6, height = 8)
?geom_line
?aes
