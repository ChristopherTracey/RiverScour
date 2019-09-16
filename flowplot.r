library(dataRetrieval)
library(lubridate)
library(plotly)
library(zoo)
library(tidyverse)


# https://waterdata.usgs.gov/usa/nwis/uv?03081500
# CheatRiver at Albright, WV  https://waterdata.usgs.gov/monitoring-location/03070260/

# parameters
siteNumbers <- c("03070260","03081500")  
siteNumbers <- c("03081500")
pCode <- "00060"

## getting the site info
siteINFO <- readNWISsite(siteNumbers)

dailyDataAvailable <- whatNWISdata(siteNumber = siteNumbers, service="dv") # whatNWISdata(siteNumbers) #, service="dv", statCd="00003"



a <- readNWISstat(siteNumbers, parameterCd=pCode, statReportType="monthly", startDate="2010", convertType=TRUE)

a$tt <- as.yearmon(paste(a$year_nu, a$month_nu, sep = "-"))
a$date <- with(a, ymd(sprintf('%04d%02d%s', year_nu, month_nu, "01")))

ggplot(a, aes(x=date, y=mean_va, group=site_no)) + # 
  geom_line(size=1, aes(color=site_no)) + #shape = 21, 
  #geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,fill=period),  alpha=0.2) +
  scale_x_date(breaks="1 year", minor_breaks="1 month", date_labels="%Y") +
  scale_y_continuous(breaks=scales::pretty_breaks(n=10)) +
  labs(title="Comparison of Mean Monthly Flow of Two Rivers",  #USGS 03081500 Youghiogheny River at Ohiopyle, PA paste("mean monthly flow - ",siteINFO$station_nm, sep="")
       y="Mean Monthly Flow (cfs)",
       x="Date") + theme_bw(base_size = 8) +
  coord_cartesian(ylim = c(0, max(a$mean_va))) 


######################################################################################
# OLD STUFF

start.date <-   "1996-11-22"
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

# FlowData$flowsmo <- na.approx(FlowData$Flow_Inst)
# FlowData$flowlog <- log(FlowData$flowsmo)


#mean by day of year
FlowData$period <- ifelse(FlowData$year>1998, "recent","baseline")  

library(dplyr)
se <- function(x) sqrt(var(x)/length(x))
FlowData_meandaily <- FlowData %>% group_by(doy, period) %>% summarise_at(vars(Flow_Inst), funs(mean, max, sd, se))

# plot(FlowData_meandaily$mean, type="l", col="blue")
# arrows(FlowData_meandaily$doy,FlowData_meandaily$mean, FlowData_meandaily$doy,FlowData_meandaily$mean+FlowData_meandaily$se, length=.025, angle=90)
# arrows(FlowData_meandaily$doy,FlowData_meandaily$mean, FlowData_meandaily$doy,FlowData_meandaily$mean-FlowData_meandaily$se, length=.025, angle=90)

df <- FlowData

FlowData_meandaily$datenew <- as.Date(FlowData_meandaily$doy, origin = "2016-01-01")
FlowData_meandaily$month <- months(FlowData_meandaily$datenew)

#FlowData_meandaily$period <- if(FlowData_meandaily$)


ggplot(FlowData_meandaily, aes(x=datenew, y=mean, group=period)) + # 
  geom_line(size=1, aes(color=period)) + #shape = 21, 
  geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,fill=period),  alpha=0.2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "mean daily flow - USGS 03070260 CHEAT RIVER AT ALBRIGHT, WV",  #USGS 03081500 Youghiogheny River at Ohiopyle, PA
     y="Mean Daily Flow (cfs)",
     x="Date") + theme_bw(base_size = 8) +
  coord_cartesian(ylim = c(0, max(FlowData_meandaily$mean))) 





