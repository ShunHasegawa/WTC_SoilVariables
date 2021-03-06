# download up to date files from HIEv
setToken(tokenfile = "Data/token.txt")
allsoils <- downloadTOA5("WTC.._Table1",cachefile="Data/hievdata/tmp.RData",
                         topath="Data/hievdata/row_data", maxnfiles = 300)
pr_allsoils <- PrcsDat(allsoils)

summary(pr_allsoils)

#########################
# Identify weird values #
#########################
# there are some weirdvalues, let's identify
# dataset is too huge to plot everything so make daily summary
daySoil <- ddply(pr_allsoils, .(Date, Chamber), VarColMean)

palette(c("blue2", "goldenrod1", "firebrick2", "chartreuse4", "deepskyblue1", "darkorange1", 
          "darkorchid3", "darkgrey", "mediumpurple1", "orangered2", "chocolate", "burlywood3"))
PltAllSoil(data = daySoil, filename= "Output/Figs/AllSoilVar.pdf")

#######################
# remove weird values #
#######################
# weird values are
# Temp10 in Ch08 & 11
CorrectSoil <- pr_allsoils
CorrectSoil$Temp10[which(CorrectSoil$Chamber %in% c("08", "11"))] <- NA

# Temp100 in Ch09
CorrectSoil$Temp100[which(CorrectSoil$Chamber == "09")] <- NA

# Temp100 in Ch10 after March2014
plot(Temp100 ~ Date, data = subset(pr_allsoils, Chamber == "10"))
plot(Temp100 ~ Date, data = subset(pr_allsoils, Chamber == "10" & 
                                     Date > as.Date("2014-02-01") & Date < as.Date("2014-02-10")))
# since 8th Feb 2014 this probe hasn't been working properly
CorrectSoil$Temp100[which(CorrectSoil$Chamber == "10" & CorrectSoil$Date >= as.Date("2014-02-08"))] <- NA

# SoilTemp10_1 till Feg2013
plot(SoilTemp10_1 ~ Date, data = subset(pr_allsoils, Chamber == "05" & Date < as.Date("2013-02-01")))
plot(SoilTemp10_1 ~ Date, data = subset(pr_allsoils, Chamber == "05" & Date < as.Date("2013-01-14")))
abline(v = seq(as.Date("2013-01-07"), as.Date("2013-01-10"), by = "days"))
# this probe doesn't seem to have worked till 10th Jan 2013
CorrectSoil$SoilTemp10_1[which(CorrectSoil$Chamber == "05" & CorrectSoil$Date <= as.Date("2013-01-10"))] <- NA

# plot all variable with the new dataset
DayCorrectSoil <- ddply(CorrectSoil, .(Date, Chamber), VarColMean)

PltAllSoil(data = DayCorrectSoil, filename= "Output/Figs/AllSoilVar_updated.pdf")
# this looks fine so carry on with this dataset
save(CorrectSoil, file = "Output//Data/WTC_soilMoistTemp_RowData.RData")
