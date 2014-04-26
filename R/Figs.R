unique(soilTrtSmry$variable)
theme_set(theme_bw())

############
# Moisture #
############
# surface moisture
TopMoist <- PltMoist(data = subset(soilTrtSmry, variable == "SoilVW_5_25"),
                     ylab = "Soil moisture at 5-25 cm\n(% of volumetric water content)")
ggsave(filename= "Output//Figs/WTC_SoilMoisture5_25cm.pdf", plot = TopMoist, width = 6, height = 3)

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
  facet_wrap(~variable, ncol = 2)   
ggsave(filename= "Output//Figs/WTC_SoilTmpDepths.pdf", plot = TempDifDep, width = 6, height = 8)  