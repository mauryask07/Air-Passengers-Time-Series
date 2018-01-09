# Load the data
data("AirPassengers")
AirPassengers
class(AirPassengers)
#This tell us that tha data series is in a time series format
frequency(AirPassengers)
#This cycle of the time series is 12 months in a year
plot(AirPassengers)
summary(AirPassengers)
#Summary of the data
abline(reg = lm(AirPassengers~time(AirPassengers)))
#This will fit in a line
boxplot(AirPassengers~cycle(AirPassengers))
#Box plot across months will give us a sense on seasonal effect
plot(diff(log(AirPassengers)))
#We see that the series is stationary enough to do any kind of time series modelling.
library(tseries)

# Ar I MA

acf(diff(log(AirPassengers))) 
# Determines the value of q
pacf(diff(log(AirPassengers))) 
# Determines the value of p

#Let's fit an ARIMA model and predict the future 5 year
fit<-arima(log(AirPassengers),c(0,1,1),seasonal = (order=c(0,1,1)))
pred<-predict(fit,n.ahead = 5*12)
pred1<-2.718^pred$pred
#To convert log value to decimal value using "e" value
ts.plot(AirPassengers,2.718^pred$pred,log="y", lty=c(1,3))

#Testing our model
model1<-ts(AirPassengers,frequency = 12, start = c(1949,1),end=c(1959,12))
fit<-arima(log(model1),c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
pred<-predict(fit,n.ahead = 10*12)
pred1<-2.718^pred$pred
data1<-head(pred1,12)
predict_1960<-round(data1,digits =0)
original_1960<-tail(AirPassengers,12)
ts.plot(AirPassengers,2.718^pred$pred,log = "y", lty=c(1,3))

