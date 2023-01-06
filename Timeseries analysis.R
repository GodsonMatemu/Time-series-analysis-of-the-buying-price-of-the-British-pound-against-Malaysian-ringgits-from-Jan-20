# Import data from csv file
library(readr)
library(forecast)
library(tseries)


exchange_rates_4_ <- read_csv("C:/Users/user/Downloads/exchange-rates (4).csv")
rates_GBP <- exchange_rates_4_ 

# Transform data into time series
rates_GBP <- ts(rates_GBP)
plot(rates_GBP, main = "Buying Price GBP/Ringit from January to October 2022", 
     ylab = "Price ringit", xlab = "days")

#Man kendall test
mk.test(rates_GBP)

# ACF plot require forecast package
acf(rates_GBP,ylim=c(-1,1))

#stationary test require tseries package
adf.test(rates_GBP)

# ADF-test shows quakes data is not stationary, we take first difference to the series
diff_rates_GBP <- diff(rates_GBP,1)
adf.test(diff_rates_GBP)
plot(diff_rates_GBP)


#Forecasting
plot(rates_GBP,main="Exchange rate(GBP)",)
rates_ma <- SMA(rates_GBP,n=3)
lines(rates_ma,col=2,lwd=2)
rates_ses <- ses(rates_GBP,initial="simple")
lines(fitted(rates_ses),col=3,lwd=2)
rates_holt <- holt(rates_GBP)
lines(fitted(rates_holt),col=4,lwd=2)
legend("topright",c("Actual","3-SMA","SES","Holt"),col=1:4,lwd=2,cex=0.6)

# Split series into training and test data
training_rates_GBP <- rates_GBP[1:(0.8*length(rates_GBP)),]
testing_rates_GBP <- rates_GBP[(0.8*length(rates_GBP)+1):(length(rates_GBP) + 1),]


#Simple Exponential Smoothing
train_ses <- HoltWinters(training_rates_GBP, beta = FALSE, gamma = FALSE)
final_ses <- forecast(train_ses, h = length(testing_rates_GBP))
plot(final_ses,lwd = 2, title(main = 'Forecast from simple exponential smoothing'))
lines(fitted(train_ses)[,1], col='red', lwd = 2)
legend("bottomleft",legend = c("Historical data","Train data","Test data"),
       col = c("black","red","blue"), lty = 1, cex = 0.6 )

plot(final_ses, lwd = 2, col = 'red')
lines(rates_GBP,lwd = 1, col = 'black')
accuracy(final_ses,testing_rates_GBP)

#Holt forecasting method
train_holt <- HoltWinters(training_rates_GBP, gamma = FALSE)
final_holt <- forecast(train_holt, h = length(testing_rates_GBP))
plot(final_holt,lwd = 2)
summary(train_holt)
summary(train_ses)
lines(fitted(train_ses)[,1], col='yellow', lwd = 2)
legend("bottomleft",legend = c("Historical data","Train data","Test data"),
       col = c("black","yellow","blue"), lty = 1, cex = 0.6 )

plot(final_holt, lwd = 2, col = 'red')
lines(rates_GBP,lwd = 1, col = 'black')


accuracy(final_ses,testing_rates_GBP)
accuracy(ft_lm, testing_rates_GBP)
accuracy(final_holt, testing_rates_GBP)


holt_training_rates <- holt(training_rates_GBP,h=length(testing_rates_GBP))
plot(ft_lm, lwd = 2)
lines(fitted(fit), col='green', lwd = 2)
accuracy(holt_training_rates,testing_rates_GBP)

#Linear Trend model
fit <- tslm(Train_rates_GBP~trend)
ft_lm <- forecast(fit, h =length(testing_rates_GBP))
plot(ft_lm, lwd = 2)
lines(fitted(fit), col='green', lwd = 2)
legend("bottomleft",legend = c("Historical data","Train data","Test data"),
       col = c("black","green","blue"), lty = 1, cex = 0.6 )

accuracy(ft_lm, testing_rates_GBP)


plot(lmF, lwd = 2)
lines(fitted(lmM), col='green', lwd = 2)

#Question 2
rates_GBP
tsdisplay(rates_GBP)
ndiffs(rates_GBP)
tsdisplay(diff(rates_GBP,1))
auto.arima(rates_GBP, trace = TRUE)

#arima models
rates_arima_010 <- arima(rates_GBP,order = c(0,1,0))
rates_arima_111 <- arima(rates_GBP,order = c(1,1,1))
rates_arima_011 <- arima(rates_GBP,order = c(0,1,1))
rates_arima_110 <- arima(rates_GBP,order = c(1,1,0))

#Summary of the ARIMA MODELS
summary(rates_arima_010)
summary(rates_arima_111)


#testing coefficients
coeftest(rates_arima_111)
checkresiduals(rates_arima_010)


#forecasting
forecast_rates <- forecast(rates_arima_010, h= 20)
plot(forecast_rates, lwd = 2)
lines(rates_GBP, col='green', lwd = 1)
legend("bottomleft",legend = c("Historical data","Arima","forecast"),
       col = c("green","black", "blue"), lty = 1, cex = 0.6 )

accuracy(rates_arima_010)
accuracy(rates_arima_111)
