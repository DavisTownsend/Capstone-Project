#library for time series models
library(tseries)

data(AirPassengers)
#tells you that this data is in time series format
class(AirPassengers)
#find what year data starts in
start(AirPassengers)
#and when it ends
end(AirPassengers)
#find the cycle of the time series to be 12 months via:
frequency(AirPassengers)
#get summary stats for data
summary(AirPassengers)

#plot data and fit line to it
plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)))

#prints the cycle across years
cycle(AirPassengers)
#aggregates cycles and displays year on year trend
plot(aggregate(AirPassengers,FUN=mean))
#boxplot over years gives sense of seasonal affect
boxplot(AirPassengers~cycle(AirPassengers))

#take log of data to remove unequal variance, and we take difference to address the trend component (growth of passengers over time)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)
#look at p-value, see that series is stationary enough

#plot acf
acf(log(AirPassengers))
#acf plot
acf(diff(log(AirPassengers)))
#pacf plot
pacf(diff(log(AirPassengers)))
#fit the model, add in seasonal aspect
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
#make predictions with fitted model
pred <- predict(fit, n.ahead = 10*12)
#plot predictions
ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))