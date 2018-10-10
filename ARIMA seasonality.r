#This Model demonstrates the steps for an ARIMA modelling process. Model forecasts the prices of Bitcoin (or any other financial security) for a give interval.

#SUMMARY:
# 1. DOWNLOAD DATA/ READ CSV
# 2. PLOT TIME SERIES data and observe if there are any trend or seasonality present
# 3. DATA TRANSFORMATION/ CHECK VOLATIALITY of the data and apply Logarithimic or Box-Cox transformation to smoothen out the variance
# 4. DETECT SEASONALITY - This can be done by viewing a seasonal plot or STL method. However the best way to detect multiple seasonalies is to apply a FOURIER TRANSFORM(FFT). Learn more on how to apply Fourier Transform from this video  :https://www.youtube.com/watch?v=spUNpyF58BY&index=3&list=PL2rmKmUwkG-NE6cNUjb_aK9aIL4zlwIb3&t=0s
# 5. CHECK STATIONARITY - Use either ADF or KPSS statistical tests to check stationarity.
# 6. DIFFERENTIATE THE DATA - If Unit root is present, then differentiate the data. Do not differentiate beyond second order derivative. Otherwise you will lose critical data. After differentiating data , we need to view the plot again to ensure that there is no trend visible in the series.
# 7. IDENTIFY MODEL - using either of the following:
#		a). Using ACF and PACF Functions - By plotting Correlograms
#		b). Using Minimum Information Criteria Matrix (Recommended) 
# 8. MODEL ESTIMATION   - Either use a manual or an Automatic Selection Algorithm (auto.arima - with appropriate parameters can be used). In both cases, we need to choose the most parsimonious model. Also, compare models by using Min AIC/BIC criteria. Most coefficients should be significant
# 9. MODEL DIAGNOSTICS and RESUDUAL ANALYSIS - For Model diagnostics, we need to do the residual analysis - Use checkresiduals method
#		1. Residuals should be independent. ( No AutoCorrelation should be present)
#		2. Residuals should be Homoscedastic (const variance). ( No Hetroscedasticity should be present)
#		3. Error terms should be normally distributed.
#		4. Most Coefficients in the model equation should be significant
# 10. MODEL FORECAST - Finally, forecast method will provide the forecast for h intervals



# Download the latest data from https://www.blockchain.com/charts/market-price
#mydata <- read.csv("D:\\python\\Data Sciences-Udacity\\datasets\\BTC_Close_Data_2010_Aug2018 - R.csv")
#mydata <- read.csv("D:\\python\\Data Sciences-Udacity\\datasets\\BTC_Close_Data_2010_Aug2018 -1yr.csv")
mydata <- read.csv("D:\\python\\Data Sciences-Udacity\\Atul\\ARIMA - R\\Data_Historical_CSV_13Sep2018.csv")
#mydata <- read.csv("D:\\python\\Data Sciences-Udacity\\Atul\\ARIMA - R\\bitcoin_data.csv")

library(forecast)
library(fpp)
library(tseries)

#mydata <- read.csv("D:\\python\\Data Sciences-Udacity\\datasets\\AirPassengers.csv")

# Plot time series data
tsdisplay(mydata)


#Check volatiality
lambda = BoxCox.lambda(mydata$Close)
View(lambda)
tsdata2 = BoxCox(mydata$Close, lambda=lambda)
tsdisplay(tsdata2)

#Detect Seasonality

count_Close = ts(na.omit(tsdata2), frequency=365)
decomp = stl(count_Close, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)
tsdata2_seasonal_adj = seasadj(tsdata2)

#Alternative Seasonality detect
monthplot(mydata$Close)
seasonplot(mydata$Close,year.labels=TRUE)
# Seasonality can be checked by decomposing the series into seasonal data by decompose method
decomposed_ts = decompose(tsdata2)
# decomposed_ts$seasonal
plot(decomposed_ts)

#Make seasonal adjustment by subtracting the estimated seasonal component from the original time series
seasonally_adjusted_ts = tsdata2 - decomposed_ts$seasonal
plot(seasonally_adjusted_ts)

#Detect seasonality using Fourier Transform
#install.packages("TSA")
library(TSA)
p = periodogram(tsdata2)
dd = data.frame(freq= p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top5 = head(order, 5)
top5
#Convert Frequency to time
time = 1/top5
time
# output - [1] 1944.0  648.0  972.0  486.0  388.8  -Main seasonality detected is 1944 days in the series. Secondary seasonality is 648 days


#Check Stationarity
adf = adf.test(deseasonal_cnt)
#adf = adf.test(tsdata2) # Ho = NON-stationary (unit root present). Alternative - Stationary. p-value greater than 0.05 indicates non-stationarity, and  p-values less than 0.05 suggest stationarity.
kpss = kpss.test(tsdata2) # Ho= Stationary. In this case, p-value less than 0.05 indicates non-stationary series and p-value greater than 0.05 indicates stationary series.
adf
kpss

# Treat Non-stationary data by ndiffs(),diff() methods
ndiffs(tsdata2)

#Based on ndiffs value - enter the number of differences
tsdata3 = diff(tsdata2, differences = 1)
adf = adf.test(tsdata3) # Ho = NON-stationary. So p-value greater than 0.05 indicates non-stationarity, and  p-values less than 0.05 suggest stationarity.
kpss = kpss.test(tsdata3) # Ho= Stationary. In this case, p-value less than 0.05 indicates non-stationary series and p-value greater than 0.05 indicates stationary series.
plot.ts(tsdata3)



# Identify Model using either of the following:
#1 . Using ACF and PACF Functions
#2.  Using Minimum Information Criteria Matrix (Recommended)

#1. Using ACF
acf(tsdata3, lag.max = 20)
pacf(tsdata3, lag.max = 20)

#Automatic Selection Algorithm - Fast
finalmodel = auto.arima(tsdata3, trace= TRUE, ic ="aicc", approximation = FALSE)
#Auto Algorithm - Slow but more accurate
finalmodel = auto.arima(tsdata2, trace= TRUE, ic ="aicc", approximation = FALSE, stepwise = FALSE)
summary(finalmodel)

finalmodel = auto.arima(tsdata2, trace= TRUE, ic ="aicc", approximation = FALSE, stepwise = FALSE,xreg = tsdata2[,"nasdaq_composite"])

# Final Model
finalmodel = arima(tsdata2, order = c(2, 1, 2), seasonal = list(order = c(0,0,0), period = 365))
summary(finalmodel)



#Compare Models
AIC(arima(tsdata2, order = c(2, 1, 2), seasonal = list(order = c(0,0,0), period = 365)),
    arima(tsdata2, order = c(2, 1, 1), seasonal = list(order = c(0,0,0), period = 365)),
    arima(tsdata2, order = c(1, 1, 2), seasonal = list(order = c(0,0,0), period = 365)),
	arima(tsdata2, order = c(5, 1, 5), seasonal = list(order = c(0,0,0), period = 365)))

	
#Compare Models
AIC(arima(tsdata2, order = c(2,1,2), seasonal = list(order = c(0,0,0), period = 365)),
	arima(tsdata2, order = c(0,1,5), seasonal = list(order = c(0,0,0), period = 365)),
	arima(tsdata2, order = c(5,1,5), seasonal = list(order = c(0,0,0), period = 365)),
	arima(tsdata2, order = c(3,1,2), seasonal = list(order = c(0,0,0), period = 365)),
    arima(tsdata2, order = c(5,1,0), seasonal = list(order = c(0,0,0), period = 365)))


res_id = residuals(finalmodel)
View(res_id)
	
	
# Check Model accuracy/ performance by Error diagnostics
accuracy(forecast,h=30)
	
#Find the Residuals and do diagnostics. If Residuals are IID (no Autocorrelation) then model is a good fit
checkresiduals(finalmodel) # This will plot the residual graphs
checkresiduals(remodel_Arima)

#Plot Forecast Errors - check whether forecast errors are normally distributed
qqnorm(finalmodel$residuals)
qqline(finalmodel$residuals)


#If there are any patterns in the residuals and its not white noise then remodel with different parameters
tsdisplay(residuals(finalmodel), lag.max=45, main='(2,1,2) Model Residuals')
remodel_Arima = arima(tsdata2, order = c(5, 1, 5), seasonal = list(order = c(0,0,0), period = 365))
summary(remodel_Arima )

#Check AutoCorrelation from Box-Ljung test. 
# Ho = No Auto/Serial Correlation. 
# Small p Values indicates autocorrelation is present. If p-value < 5% (.05) then Reject Ho - AutoCorrelation present
# If p-value > 5% (.05) then do not Reject Ho - No AutoCorrelation present
# Lag selection - length of series/5, Length of series/ 
Box.test(res_id, lag = 5, type = "Ljung-Box")
Box.test(res_id, lag = 5, type = "Box-Pierce") #Same Ho as Ljung-Box test

# Forecast values for h intervals and plot forecast graph
forecast = forecast(finalmodel,h=30)
plot(forecast)



