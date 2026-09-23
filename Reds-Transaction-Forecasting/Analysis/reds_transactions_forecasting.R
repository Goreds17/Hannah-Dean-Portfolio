library(forecast)
library(dplyr)
library(tseries)

library(readr)
mydata <- read_csv("C:/Users/hanna/OneDrive/Baseball Backgrounds/Desktop/STA 362/Time Series Data/Project 2 Data set - Sheet1.csv")

mydata$`# of transactions` = replace(mydata$`# of transactions`, c(53,54), 0)

mydata <- Project_2_Data_set_Sheet1
mydata$`# of transactions` = replace(mydata$`# of transactions`, c(53,54), 0)
# Example: Simulated time series data
time_series_data <- ts(mydata$`# of transactions`, frequency = 12, # monthly = 12, quarterly = 4, ... 
                       start = c(2016, 1))

mydata <- Project_2_Data_set_Sheet1
#ts() - time Series
# Plotting the time series
plot(time_series_data, 
     main = "Simulated Time Series Data", 
     ylab = "Value", 
     xlab = "Time")
          
# decomposing
plot(decompose(time_series_data))

Acf(time_series_data)
adf.test(time_series_data)

ses_model <- ses(time_series_data, h= 12)
holt_model <- holt(time_series_data, h = 12)
hw_model <- hw(time_series_data, h = 12)

plot(forecast(ses_model))
forecast(ses_model, h = 12)


plot(forecast(holt_model))
forecast(holt_model, h = 12)

plot(forecast(hw_model))
forecast(hw_model, h = 12)

#################################################################################

#ARIMA Modeling

library(forecast)
is.ts(time_series_data)

auto.arima(time_series_data)

#AR Regression
ar.out <- ar(time_series_data)
ar.forecast <- time_series_data - ar.out$resid

# Forecasting h = 12
ts.plot(time_series_data)
points(ar.forecast, type = "l", lty = 2, col = "red")
pred <- predict(ar.out, n.ahead = 12)
pred = pred$pred
se <- pred$se

#draw a picture w/ CI
ts.plot(time_series_data)
points(ar.forecast, type = "l", lty = 2, col = "red")
points(pred, type = "l", col = "blue")
points(pred - 2*se, type ="l", lty = 3, col = "blue")
points(pred + 2*se, type ="l", lty = 3, col = "blue")
