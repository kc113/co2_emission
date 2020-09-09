#load the packages
library(ggplot2)
library(dplyr)
library(urca)
library(forecast)
#read the data set
c_data = read.csv("data/carbondioxide.csv")
#plot the line diagram
ggplot(c_data, aes( x = 1:nrow(c_data), y = average)) +
  geom_line() +
  labs(x = "ID", y = "Monthly Mean Carbon Emission(in ppm)")
ggsave("output/co2plot.jpg")
#check the seasonality from the data
for(y in 1990:2020){
  year_data = subset(c_data, year == y)
  year_data %>%
    filter(average == max(average)) %>%
    print(row.names = F)
}
#transform the variable to time series
x_ts = ts(c_data[,3], start = c(1990), frequency = 12)
#decompose the time series
split_ts = decompose(x_ts, type = "additive")
#deseasonalize the time series
x_des_ts = split_ts$x - split_ts$seasonal
autoplot(x_des_ts)
ggsave("output/co2deseasonlized.jpg")
#perform the ADF test
c_adf = ur.df(x_des_ts, type = "trend", selectlags = c("AIC"))
summary(c_adf)
#create a new series by applying differencing
y_ts = diff(x_des_ts)
#perform the ADF test
c1_adf = ur.df(y_ts, type = "trend", selectlags = c("AIC"))
summary(c1_adf)
#check for the deterministic trend
l = lm(y_ts ~ time(y_ts))
summary(l)
#calculate the sample acf
c_acf = acf(y_ts, plot = FALSE)
#prepare the table
t1 = data.frame(k = (c_acf$lag[-1] * 12), sample_acf = c_acf$acf[-1])
Qstat = c()
pvalue = c()
conc = c()
for(i in 1:25){
  b = Box.test(y_ts, lag = i, type = "Ljung")
  Qstat[i] = b$statistic
  pvalue[i] = b$p.value
  if (pvalue[i] < 0.05){
    conc[i] = "reject"
  }else{
    conc[i] = "accept"
  }
}
t1 = cbind(t1, Q_stat = Qstat, p_value = pvalue, conclusion = conc)
View(t1)
#calculate the sample acf
c_pacf = pacf(y_ts, plot = FALSE)
#prepare the table
t2 = data.frame(k = (c_pacf$lag * 12), sample_pacf = c_pacf$acf)
test_stat = sqrt(366) * c_pacf$acf
conc1 = c()
for(i in 1:25){
if (abs(test_stat[i]) > 1.96){
  conc1[i] = "reject"
}else{
  conc1[i] = "accept"
}
}
t2 = cbind(t2, test_statistic = test_stat, conclusion = conc1)
View(t2)
#fit the AR model
fit = ar(y_ts, order.max = 3, method = "mle")
#check the residuals are white noise or not
resid_series = fit$resid
c_acf_res = acf(na.omit(resid_series), plot = FALSE)
t3 = data.frame(k = c_acf_res$lag[-1] * 12 , sample_acf = c_acf_res$acf[-1])
Qstat1 = c()
pvalue1 = c()
conc1 = c()
for(i in 1:25){
  b = Box.test(na.omit(resid_series), lag = i, type = "Ljung")
  Qstat1[i] = b$statistic
  pvalue1[i] = b$p.value
  if (pvalue1[i] < 0.05){
    conc1[i] = "reject"
  }else{
    conc1[i] = "accept"
  }
}
t3 = cbind(t3, Q_stat = Qstat1, p_value = pvalue1, conclusion = conc1)
View(t3)
#forecast the value for next 5 years
fit1 = Arima(x_ts, order = c(3,1,0), seasonal = c(0,1,1))
autoplot(forecast(fit1, h = 60))
ggsave("output/co2predict.jpg")




  