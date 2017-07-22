#Installing required packages
install.packages(c("RCurl","TSPred","forecast","tseries"))
library(RCurl)
touristdata<-read.delim("C:\\Users\\Dhruvesh\\Documents\\Kaggle\\ARIMA\\touristdata2012.txt",header = F)

#Binding all the columns to one
data = c(touristdata$V1,    touristdata$V2,    touristdata$V3,  touristdata$V4,  touristdata$V5,  touristdata$V6,  touristdata$V7,  touristdata$V8,  touristdata$V9,  touristdata$V10,  touristdata$V11,  touristdata$V12,	touristdata$V13,	touristdata$V14,	touristdata$V15,	touristdata$V16,	touristdata$V17,	touristdata$V18,	touristdata$V19,	touristdata$V20, touristdata$V21, touristdata$V22)
View(touristdata)
View(data)
data=log(data)

#Assign it as a time series
ts.data<-ts(data,frequency = 12,start = c(1990,1))
View(ts.data)
plot(ts.data)
dim(as.matrix(ts.data))

#Splitting into training and test data 
data.train<-window(ts.data, start=c(1990,1),end=c(2009,12))
View(data.train)
dim(as.matrix(data.train))
plot(data.train)
data.test<-window(ts.data,start=c(2010,1))
View(data.test)
dim(as.matrix(data.test))
plot(data.test)

#Developing arima model and analysis of model

library(forecast)
arima1<-auto.arima(data.train,trace = TRUE,test = "kpss",ic="bic")
summary(arima1)
confint(arima1)

#Residual Diagnostic 
plot.ts(arima1$residuals)
Box.test(arima1$residuals,lag = 20,type = "Ljung-Box")
acf(arima1$residuals,lag.max = 24,main="ACF of the model")
Box.test(arima1$residuals^2, lag=20,type="Ljung-Box")

#Jarque-Bera test

library(tseries)
jarque.bera.test(arima1$residuals)
library(forecast)
library(TSPred)
library(RCurl)
arima1.forecast<-forecast(arima1,h=24)
arima1.forecast
plot(arima1.forecast)

# Plot actual and the model values and compare them
plotarimapred(data.test,arima1,xlim = c(2009,2012),range=0.05)
accuracy(arima1.forecast,data.test) #Accuracy comes out to be 63%
