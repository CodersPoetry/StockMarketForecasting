install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("prophet")

library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggthemes)
library(dplyr)
library(magrittr)
library(prophet)

#Import the dataset. Mention stringAsFactors = FALSE so that the strings are not imported as Factors
df <- read.csv('C:/Users/Ashis.Mohanty/Documents/Forecasting - Prophet/^NSEI.csv', stringsAsFactors = FALSE)

#convert the date as from character to Date Format
df$Date <- as.Date(df$Date)

#check the class of the date
data.class(df$Date)

#convert the char to numeric type for rest of the columns
df$Open <- as.numeric(df$Open)
df$High <- as.numeric(df$High)
df$Low <- as.numeric(df$Low)
df$Close <- as.numeric(df$Close)
df$Adj.Close <- as.numeric(df$Adj.Close)
df$Volume <- as.numeric(df$Volume)


attach(df)

#check the number of na values for the column 'Close'
sum(is.na(df$Close))

#check the dimension of the dataframe before removing na values
dim(df)

#omit all the na values in the dataframe
df <- na.omit(df)

#check the dimension after removing the na values
dim(df)

#Plot for the Daily Close value of Nifty 50
DailyPlot <- ggplot(df, aes(Date, Close)) +
  geom_line(na.rm=TRUE, color="blue") +
  ggtitle("Stock Market in last 5 years") +
  xlab("Date") + ylab("Closing Points")

DailyPlot

#Plot with Date breaks of 6 months year
DailyPlot_6mo <- DailyPlot + 
  (scale_x_date(breaks=date_breaks("6 months"),
                labels=date_format("%b %y"))) +
  xlab("6 months interval") + ylab("Closing Points")

DailyPlot_6mo

#Plot with Date breaks of 1 year
DailyPlot_1yr <- DailyPlot + 
  (scale_x_date(breaks=date_breaks("1 year"),
                labels=date_format("%b %y"))) +
  xlab("1 year interval") + ylab("Closing Points")

DailyPlot_1yr

#Plot the Trend Line
DailyPlot_trend <- DailyPlot + stat_smooth(colour="green") +
  xlab("1 year interval") + ylab("Closing Points")

DailyPlot_trend

summary(df)


DailyPlot_all <- ggplot(df, aes(Date,Close)) +
  geom_line(aes(y=df$High),na.rm=TRUE, color="green") + ####green
  geom_line(aes(y=df$Close),na.rm=TRUE, color="blue") + ###Blue
  geom_line(aes(y=df$Low),na.rm=TRUE, color="red") + ####Red
  ggtitle("Plot of High vs Low") +
  xlab("Year") + ylab("High vs Low vs Closing") +
  theme(legend.position = "right")
  #scale_color_manual(name ="Legends", values = c("green", "blue", "red"))


DailyPlot_all



#last 1 year plots

min <- as.Date("2018-7-16")
max <- NA

Plot_Last1yr <- DailyPlot_all + scale_x_date(limits = c(min, max))+ ylim(9500,12500) + stat_smooth(color = 'green', method = "loess")
Plot_Last1yr

min <- as.Date("2017-7-16")
max <- NA

Plot_Last2yr <- DailyPlot_all + scale_x_date(limits = c(min, max))+ ylim(9500,12500) + stat_smooth(color = 'green', method = "loess")
Plot_Last2yr


summary(Volume)

#Plot for the Volumes of Nifty 50
DailyPlot_Volume <- ggplot(df, aes(Date, Volume), size =1) +
  geom_line(na.rm=TRUE, color="blue") +
  ylim(150000,750000) +
  ggtitle("Stock Market Volume in last 5 years") +
  xlab("Date") + ylab("Volume")

DailyPlot_Volume


min <- as.Date("2018-7-16")
max <- NA

Plot_Last1yrVolume <- DailyPlot_Volume + 
  scale_x_date(limits = c(min, max))+ ylim(150000,750000) + stat_smooth(color = 'green', method = "loess")
Plot_Last1yrVolume

min <- as.Date("2017-7-16")
max <- NA

Plot_Last2yrVolume <- DailyPlot_Volume + scale_x_date(limits = c(min, max))+ ylim(150000,750000) + stat_smooth(color = 'green', method = "loess")
Plot_Last2yrVolume


########################
#MODELLING WITH PROPHET
########################


#change column name --- Date as ds, Close as y
ds <- df$Date
y <- df$Close

#create a new dataframe with ds and y
new_df <- data.frame(ds,y)
new_df

#chack the class of ds(date) and y(numeric)
data.class(ds)
data.class(y)

dim(new_df)

#taken holidays after july 17th 2014(start date) into account till Dec 2019
holiday2014 <- data.frame(
  holiday = 'nseholiday2014',
  ds = as.Date(c('2014-07-29', '2014-08-15', '2014-08-29',
                 '2014-10-02', '2014-10-03', '2014-10-06',
                 '2014-10-23', '2014-10-24', '2014-11-04',
                 '2014-11-06', '2014-12-25')))


holiday2015 <- data.frame(
  holiday = 'nseholiday2015',
  ds = as.Date(c('2015-01-26', '2015-02-17', '2015-03-06',
                 '2015-04-02', '2015-04-03', '2015-04-14',
                 '2015-05-01', '2015-09-17', '2015-09-25',
                 '2015-10-02', '2015-10-22', '2015-11-12',
                 '2015-11-12', '2015-11-25', '2015-12-25')))

holiday2016 <- data.frame(
  holiday = 'nseholiday2016',
  ds = as.Date(c('2016-01-26', '2016-03-07', '2016-03-24',
                 '2016-03-25', '2016-04-14', '2016-04-15',
                 '2016-04-19', '2016-07-06', '2016-08-15',
                 '2016-09-05', '2016-09-13', '2016-10-11',
                 '2016-10-12', '2016-10-31', '2016-11-14')))


holiday2017 <- data.frame(
  holiday = 'nseholiday2017',
  ds = as.Date(c('2017-01-26', '2017-02-24', '2017-03-13',
                 '2017-04-04', '2017-04-14', '2017-05-01',
                 '2017-06-26', '2017-08-15', '2017-08-25',
                 '2017-10-02', '2017-10-20', '2017-12-25')))


holiday2018 <- data.frame(
  holiday = 'nseholiday2018',
  ds = as.Date(c('2018-01-26', '2018-02-13', '2018-03-02',
                 '2018-03-29', '2018-03-30', '2018-05-01',
                 '2018-08-15', '2018-08-22', '2018-09-13',
                 '2018-09-20', '2018-10-02', '2018-10-18',
                 '2018-11-08', '2018-11-23', '2018-12-25')))


holiday2019 <- data.frame(
  holiday = 'nseholiday2019',
  ds = as.Date(c('2019-03-04', '2019-03-21', '2019-03-17',
                 '2019-04-19', '2019-04-29', '2019-05-01',
                 '2019-06-05', '2019-08-12', '2019-08-15',
                 '2019-09-02', '2019-09-10', '2019-10-02',
                 '2019-10-08', '2019-10-28', '2019-11-12',
                 '2019-12-25')))

#create a dataframe from the list of holidays
holidays <- bind_rows(holiday2014,holiday2015,holiday2016,holiday2017,holiday2018,holiday2019)


#prophet model is stored in m
m <- prophet(new_df,changepoint.prior.scale = 0.05, holidays=holidays) 

#annual seasonality
prophet:::plot_yearly(m)

#create a data frame to predict for next 365 days
future <- make_future_dataframe(m, periods = 365)

#tail of future dataframe should show dates of 2020
tail(future)


#plot the components - trend, weekly, yearly
prophet_plot_components(m, forecast)





#predict and store the forecasts in forecast df
forecast <- predict(m,future)
tail(forecast)

tail(forecast[c('ds','yhat','yhat_lower', 'yhat_upper')])

#plot the forecasts
plot(m,forecast)


plot(m, forecast) + add_changepoints_to_plot(m, threshold = 0.01, cp_color = "red", cp_linetype = "dashed", trend = TRUE)

m$changepoints


# cross-validation.Here we do cross-validation to assess prediction performance on a 
# horizon of 365 days, starting with 1095 days of training data in the first cutoff and 
# then making predictions every 180 days. On this 5 year time series, this corresponds to 11 total forecasts.

df.cv <- cross_validation(m, initial = 1460, period = 180, horizon = 365, units = 'days')
head(df.cv)
df.cv


#A plot with actual and predicted, use the slider to zoom and 
#hover the mouse pointer on a point to get actual vs predicted
dyplot.prophet(m,forecast)



df.p <- performance_metrics(df.cv, NULL, rolling_window = 0.1)
head(df.p)



