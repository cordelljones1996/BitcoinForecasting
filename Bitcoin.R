# Libraries
library(prophet)
library(lubridate)
library(ggplot2)

# Bitcoin Data
data <- read.csv("BTC.csv")
# or use this if you have another cryto
# data <- read.csv(file.choose(), header = T)
str(data)
head(data)
data$Date <- dmy(data$Date)

# Plot
qplot(Date, Close, data = data,
      main = 'Bitcoin Closing Prices 2014-2020')

# Log transformation
ds <- data$Date
y <- log(data$Close)
df <- data.frame(ds, y)

qplot(ds, y, data = df,
      main = 'Bitcoin Closing Prices in Log Scale')

# Forecasting with Facebook's Prophet Package
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
tail(future)
forecast <- predict(m, future)

# Plot forecast
plot(m, forecast)
dyplot.prophet(m, forecast)
prophet_plot_components(m, forecast)

# Model Performance
str(forecast)
pred <- forecast$yhat[1:2121]
actual <- m$history$y
plot(actual, pred)
abline(lm(pred~actual), col = 'red')
summary(lm(pred~actual))

x <- cross_validation(m, 365, units = 'days')
performance_metrics(x, rolling_window = 0.1)
plot_cross_validation_metric(x,
                             metric = 'rmse',
                             rolling_window = 0.1)
