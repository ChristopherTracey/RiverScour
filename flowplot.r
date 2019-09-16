library(dataRetrieval)
library(lubridate)
library(plotly)
library(zoo)

# https://waterdata.usgs.gov/usa/nwis/uv?03081500

siteNo <- "03081500"
pCode <- "00060"
start.date <- "2002-10-01"
end.date <- "2019-06-30"

yough <- readNWISuv(siteNumbers = siteNo,
                     parameterCd = pCode,
                     startDate = start.date,
                     endDate = end.date)

names(yough)

yough <- renameNWISColumns(yough)

head(yough)


yough$year <- year(yough$dateTime)
yough$month <- month(yough$dateTime)
yough$day <- day(yough$dateTime)
yough$doy <- yday(yough$dateTime)

yough$flowsmo <- na.approx(yough$Flow_Inst)
yough$flowlog <- log(yough$flowsmo)

library(reshape2)
# yough1 <- yough[c("year","doy","Flow_Inst")]
yough1 <- yough[c("year","doy","flowlog")]
yough1 <- acast(yough1, year~doy, fun.aggregate=mean, margins = TRUE)

# yough1[is.nan(yough1)] <- 0
#yough1[is.nan(yough1)] = 0
#plot_ly(data=yough, x=~doy ,y=~year, z=~Flow_Inst, type="surface")
#yough1s <- matrix(smooth(yough1), nrow=16, ncol=367)
# library(oce)
# yough1s <- matrixSmooth(yough1)

plot_ly(z=yough1, type="surface")

#mean by day of year
yough$period <- ifelse(yough$year>2010, "recent","baseline")  

library(dplyr)
se <- function(x) sqrt(var(x)/length(x))
yough_meandaily <- yough %>% group_by(doy, period) %>% summarise_at(vars(Flow_Inst), funs(mean, max, sd, se))

plot(yough_meandaily$mean, type="l", col="blue")
arrows(yough_meandaily$doy,yough_meandaily$mean, yough_meandaily$doy,yough_meandaily$mean+yough_meandaily$se, length=.025, angle=90)
arrows(yough_meandaily$doy,yough_meandaily$mean, yough_meandaily$doy,yough_meandaily$mean-yough_meandaily$se, length=.025, angle=90)

df <- yough

yough_meandaily$datenew <- as.Date(yough_meandaily$doy, origin = "2016-01-01")
yough_meandaily$month <- months(yough_meandaily$date)

#yough_meandaily$period <- if(yough_meandaily$)

library(tidyverse)
ggplot(yough_meandaily, aes(x=datenew, y=mean, group=period)) + # 
  geom_line(size=1, aes(color=period)) + #shape = 21, 
  geom_ribbon(aes(ymin=mean-sd, ymax=mean+sd,fill=period),  alpha=0.2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "mean daily flow - USGS 03081500 Youghiogheny River at Ohiopyle, PA",
     y="Mean Daily Flow (cfs)",
     x="Date") + theme_bw(base_size = 8) +
  coord_cartesian(ylim = c(0, max(yough_meandaily$mean))) 





