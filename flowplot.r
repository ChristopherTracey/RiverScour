library(dataRetrieval)
library(lubridate)
library(plotly)
library(zoo)

# https://waterdata.usgs.gov/usa/nwis/uv?03081500

siteNo <- "03081500"
pCode <- "00060"
start.date <- "2002-10-01"
end.date <- "2019-06-30"

FlowData <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

names(FlowData)

FlowData <- renameNWISColumns(FlowData)

head(FlowData)


FlowData$year <- year(FlowData$dateTime)
FlowData$month <- month(FlowData$dateTime)
FlowData$day <- day(FlowData$dateTime)
FlowData$doy <- yday(FlowData$dateTime)

FlowData$flowsmo <- na.approx(FlowData$Flow_Inst)
FlowData$flowlog <- log(FlowData$flowsmo)

library(reshape2)
# FlowData1 <- FlowData[c("year","doy","Flow_Inst")]
FlowData1 <- FlowData[c("year","doy","flowlog")]
FlowData1 <- acast(FlowData1, year~doy, fun.aggregate=mean, margins = TRUE)

# FlowData1[is.nan(FlowData1)] <- 0
#FlowData1[is.nan(FlowData1)] = 0
#plot_ly(data=FlowData, x=~doy ,y=~year, z=~Flow_Inst, type="surface")
#FlowData1s <- matrix(smooth(FlowData1), nrow=16, ncol=367)
# library(oce)
# FlowData1s <- matrixSmooth(FlowData1)

plot_ly(z=FlowData1, type="surface")

#mean by day of year
FlowData$period <- ifelse(FlowData$year>2010, "recent","baseline")  

library(dplyr)
se <- function(x) sqrt(var(x)/length(x))
FlowData_meandaily <- FlowData %>% group_by(doy, period) %>% summarise_at(vars(Flow_Inst), funs(mean, max, sd, se))

plot(FlowData_meandaily$mean, type="l", col="blue")
arrows(FlowData_meandaily$doy,FlowData_meandaily$mean, FlowData_meandaily$doy,FlowData_meandaily$mean+FlowData_meandaily$se, length=.025, angle=90)
arrows(FlowData_meandaily$doy,FlowData_meandaily$mean, FlowData_meandaily$doy,FlowData_meandaily$mean-FlowData_meandaily$se, length=.025, angle=90)

df <- FlowData

FlowData_meandaily$datenew <- as.Date(FlowData_meandaily$doy, origin = "2016-01-01")
FlowData_meandaily$month <- months(FlowData_meandaily$date)

#FlowData_meandaily$period <- if(FlowData_meandaily$)

library(tidyverse)
ggplot(FlowData_meandaily, aes(x=datenew, y=mean, group=period)) + # 
  geom_line(size=1, aes(color=period)) + #shape = 21, 
  geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,fill=period),  alpha=0.2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "mean daily flow - USGS 03081500 FlowDataiogheny River at Ohiopyle, PA",
     y="Mean Daily Flow (cfs)",
     x="Date") + theme_bw(base_size = 8) +
  coord_cartesian(ylim = c(0, max(FlowData_meandaily$mean))) 





