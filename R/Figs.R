unique(soilTrtSmry$variable)

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))
PltAllSoil(data = daySoil, filename= "Output/Figs/AllSoilVar.pdf")

theme_set(theme_bw())

############
# Moisture #
############
# surface moisture
PltMoist <- function(data, ylab, vals, legtitle, leglabel, colvar, legend.key.size, ...){
  data <- droplevels(data)
  p <- ggplot(data, aes_string(x = "Date", y = "Mean", col = colvar))
  p + geom_line(...)+
    scale_color_manual(values = vals, name = legtitle, labels = leglabel) +
    labs(x = "Time", y = ylab) +
}

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
                         size = 0.3)

ggsave(filename= "Output//Figs/WTC_Trt_SoilMoisture5_25cm.pdf", plot = TempTopMoist, width = 6, height = 3)
ggsave(filename= "Output//Figs/WTC_Chamber_SoilMoisture5_25cm.pdf", plot = ChamTopMoist, width = 6, height = 4)

# moisture at different depths
ylabs <- list(
  'SoilVW_5_25' = "5-25 cm",
  'SoilVW_30_50' = "30-50 cm",
  'SoilVW_HL' = "HL"
)
MoistDifDep <- PltMoist(data = soilTrtSmry[grep("SoilVW", soilTrtSmry$variable), ],
                        ylab = "Soil moisture\n(% of volumetric water content)") + 
  facet_grid(variable ~., labeller= ylab_label)

ggsave(filename= "Output//Figs/WTC_SoilMoistureDepths.pdf", plot = MoistDifDep, width = 6, height = 6)


###############
# Temperature #
###############
# surface temperature
ToTemp <- PltTemp(data = subset(soilTrtSmry, variable == "SoilTemp10"),
                  ylab = expression(Soil~temperature~at~10~cm~(~degree~C)))
ggsave(filename= "Output//Figs/WTC_SoilTemp10cm.pdf", plot = ToTemp, width = 6, height = 3)

# stratified temperature
TempDifDep <- PltTemp(data = soilTrtSmry[grep("^Temp", soilTrtSmry$variable), ],
                      ylab = expression(Soil~temperature~(~degree~C)), size = 0.1) +
  facet_wrap(~variable, ncol = 2) +
  theme(axis.text.x = element_text(size = 6))
ggsave(filename= "Output//Figs/WTC_SoilTmpDepths.pdf", plot = TempDifDep, width = 6, height = 6)  
