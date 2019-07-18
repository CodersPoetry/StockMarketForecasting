install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("gridExtra")
install.packages("ggthemes")

library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)
library(dplyr)

df <- read.csv('C:/Users/Ashis.Mohanty/Documents/Forecasting - Prophet/^NSEI.csv', stringsAsFactors = FALSE)
df$Date <- as.Date(df$Date)
data.class(df$Date)

attach(df)

min(df$Close)

DailyPlot <- ggplot(df, aes(Date, Close)) +
  geom_point(na.rm=TRUE, color="blue", size=1) +
  ggtitle("Stock Market in last 5 years") +
  xlab("Date") + ylab("Closing Points")

DailyPlot_6mo <- DailyPlot + 
  (scale_x_date(breaks=date_breaks("6 months"),
                labels=date_format("%b %y")))

DailyPlot_6mo

DailyPlot_1yr <- DailyPlot + 
  (scale_x_date(breaks=date_breaks("1 year"),
                labels=date_format("%b %y")))

DailyPlot_1yr


