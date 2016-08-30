# Other-Works
# Anomaly Detection Test
options(scipen = 999)
anmyl <- read.csv(file = "D:/Users/26000215/Desktop/Test/Anodot.csv")

anmyl <- cbind(anmyl, date = as.POSIXct(anmyl$LogTimeOnAppliance/1000, origin = "1970-01-01", tz = "GMT"))

anmyl <- cbind(anmyl, Time1 = substr(anmyl$date, 1, 10), Time2 = substr(anmyl$date,12 ,19))
anmyl<- anmyl[,c(-2)]



str(anmyl)

anmyl <- cbind(anmyl, Time_Stamp = as.Date(substr(anmyl$Time1,1,10)))

library(tseries)

dene <- tseries::read.ts(anmyl, )

library("mplot") 

m1 <- mPlot(
  CabinAirMeanTemp_H.value ~ date,
  data = anmyl,
  type = "Line"
  )
m1$set(pointSize = 0)
m1$set(hideHover = "auto")
m1$print("chart2")

library(openxlsx)
write.xlsx(x = anmyl, file = "Dene.xlsx")
