library(forecast)
library(TSA)
library(tseries)

#import data
data1= c(1620199,1014512,1106300,1141624,1659633,753574,1274714,1219915,
         1736212,810279,1372530,989557,1194588.60,799181,2259667,656379,
         1358081,1134604,1110714,1520301)
data1
#time series
data1.ts=ts(data1,frequency = 4, start = c(2018,1))
data1.ts

#plot
plot(data1.ts, main="Produksi Pisang Lampung Selatan 2018-2022")
Acf(data1.ts, main="Produksi Pisang Lampung Selatan 2018-2022")
Pacf(data1.ts, main="Produksi Pisang Lampung Selatan 2018-2022")
adf.test(data1)
kpss.test(data1)

BoxCox.lambda(data1.ts)
BoxCox.ar(data1.ts)

#Fitting the AR Model
AR <- Arima(data1.ts, order = c(1,0,0))
AR


#Fitting the MA model
MA<- Arima(data1.ts, order = c(0,0,1))
MA




#Fitting the MA model
ARMA = Arima(data1.ts, order = c(1,0,1))
print(ARMA)

#plot time series dengan fitted values
ts.plot(data1.ts)
ARMA_fit <- data1.ts - residuals(ARMA)
ARMA_fit
points(ARMA_fit, type = "l", col = 2, lty = 2)


#Meramal 8 periode kedepan
predict(ARMA, n.ahead = 8)

#plot Produksi Pisang plus ramalan
ts.plot(data1.ts, xlim = c(2018, 2022))
MA_forecast <- predict(MA, n.ahead = 8)$pred
MA_forecast_se <- predict(MA, n.ahead = 8)$se
points(MA_forecast, type = "l", col = 2)
points(MA_forecast - MA_forecast_se, type = "l", col = 2, lty = 2)
points(MA_forecast + MA_forecast_se, type = "l", col = 2, lty = 2)

MAD_1=mean(abs(data1.ts-ARMA$fitted))
MAD_1
MAPE_1=mean(abs(data1.ts-ARMA$fitted)/data1.ts,na.rm=TRUE)
MAPE_1
MSE_1=mean(abs(data1.ts-ARMA$fitted)^2)
MSE_1
perbandingan_1=c(MAD_1,MAPE_1,MSE_1)
perbandingan_1

MAD_2=mean(abs(data1.ts-AR$fitted))
MAD_2
MAPE_2=mean(abs(data1.ts-AR$fitted)/data1.ts,na.rm=TRUE)
MAPE_2
MSE_2=mean(abs(data1.ts-AR$fitted)^2)
MSE_2
perbandingan_2=c(MAD_2,MAPE_2,MSE_2)
perbandingan_2

MAD_3=mean(abs(data1.ts-MA$fitted))
MAD_3
MAPE_3=mean(abs(data1.ts-MA$fitted)/data1.ts,na.rm=TRUE)
MAPE_3
MSE_3=mean(abs(data1.ts-MA$fitted)^2)
MSE_3
perbandingan_3=c(MAD_3,MAPE_3,MSE_3)
perbandingan_3

perbandingantotal=rbind(perbandingan_1,perbandingan_2,perbandingan_3)
perbandingantotal
AIC(ARMA,AR,MA)

#Menguji Residual Independen (tidak ada korelasi)
#Uji Box-Ljung
Box.test(ARMA$residuals,type = "Ljung")
#Normalitas
ks.test(ARMA$residuals,"pnorm",mean(ARMA$residuals),sd(ARMA$residuals))

#peramalan
ramalan_1=forecast::forecast(ARMA,h=6)
ramalan_1
plot(ramalan_1)


plot.ts(data1, main="Produksi Pisang Di Lampung Selatan 2018-2022",col="blue")
plot(data1.ts,xlab='TAHUN',ylab='Produksi Pisang',main="Produksi Pisang Di Lampung Selatan 2018-2022",col="blue")
lines(fitted(ARMA),col="red")


