library(urca)
library(forecast)
library(strucchange)
library(corrgram)
library(xts)

#Ряд с обменным курсом:
#загнали данные в дата фрейм
exrate_frame <- data.frame(read.csv('C:/EXUSEU.csv'))

#проверили
head(exrate_frame)

dates_rate <- as.Date(exrate_frame$DATE)
#создаем ряд (объект xts)
exrate_series <- xts(x = exrate_frame$EXUSEU, order.by = dates_rate)
head(exrate_series)
plot(exrate_series)

acf(exrate_series)
pacf(exrate_series)

#взяли в первую разность ряда
dif_exrate <- diff(exrate_series)
plot(dif_exrate)

#посмотрели на ACF и PACF (со второго элемента, т.к. при взятии разности первый NA) 
acf(dif_exrate[2:length(dif_exrate)])
pacf(dif_exrate[2:length(dif_exrate)])

#проверяем на брейки Бай-Перроном
bp.exrate <- breakpoints(exrate_series~1, h = 0.15)
bp.exrate
#смотрим даты по полученным номерам наблюдений 
exrate_series[c(52,97,136,192)]

#проверям на стационарность (сначала в уровне, потом в первой разности)
gls_result_level <- ur.ers(exrate_series, type = 'DF-GLS', model = 'trend')
summary(df_result)

gls_result_diff <- ur.ers(dif_exrate, type = 'DF-GLS', model = 'trend')
summary(df_result_diff)

auto_arima <- auto.arima(exrate_series[1:(length(exrate_series)-1)], test = 'kpss')
forecast_exrate <- forecast(auto_arima, h = 1)
forecast_exrate
#смотрим out-of-sample критерии по прогнозу на 1 шаг
accuracy(forecast_exrate, tail(dif_exrate, 1), D = 0, d = 1)

#Ряд с инфляцией:-------------------------------------------------------------------------

infl_frame <- data.frame(read.csv('C:/CPIAUCSL.csv'))

head(infl_frame)
series_inf = infl_frame$CPIAUCSL
dates_inf <- as.Date(infl_frame$DATE)

infl_series <- xts(x = series_inf, order.by = dates_inf)
plot(infl_series)

acf(infl_series)
pacf(infl_series)

dif_infl = diff(infl_series)
plot(dif_infl)

acf(dif_infl[2:length(dif_infl)])
pacf(dif_infl[2:length(dif_infl)])

bp.rts <- breakpoints(logrts~1, h = 0.15)
bp.rts
plot.ts(logrts)
abline(v = 2020.175)
lines(bp.rts)
bpdate.rts <- breakdates(bp.rts)
bpdate.rts

auto.arima(infl_series, test = 'adf')
