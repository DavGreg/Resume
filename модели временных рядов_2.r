library(urca)
library(forecast)
library(strucchange)
library(corrgram)
library(xts)

#��� � �������� ������:
#������� ������ � ���� �����
exrate_frame <- data.frame(read.csv('C:/EXUSEU.csv'))

#���������
head(exrate_frame)

dates_rate <- as.Date(exrate_frame$DATE)
#������� ��� (������ xts)
exrate_series <- xts(x = exrate_frame$EXUSEU, order.by = dates_rate)
head(exrate_series)
plot(exrate_series)

acf(exrate_series)
pacf(exrate_series)

#����� � ������ �������� ����
dif_exrate <- diff(exrate_series)
plot(dif_exrate)

#���������� �� ACF � PACF (�� ������� ��������, �.�. ��� ������ �������� ������ NA) 
acf(dif_exrate[2:length(dif_exrate)])
pacf(dif_exrate[2:length(dif_exrate)])

#��������� �� ������ ���-��������
bp.exrate <- breakpoints(exrate_series~1, h = 0.15)
bp.exrate
#������� ���� �� ���������� ������� ���������� 
exrate_series[c(52,97,136,192)]

#�������� �� �������������� (������� � ������, ����� � ������ ��������)
gls_result_level <- ur.ers(exrate_series, type = 'DF-GLS', model = 'trend')
summary(df_result)

gls_result_diff <- ur.ers(dif_exrate, type = 'DF-GLS', model = 'trend')
summary(df_result_diff)

auto_arima <- auto.arima(exrate_series[1:(length(exrate_series)-1)], test = 'kpss')
forecast_exrate <- forecast(auto_arima, h = 1)
forecast_exrate
#������� out-of-sample �������� �� �������� �� 1 ���
accuracy(forecast_exrate, tail(dif_exrate, 1), D = 0, d = 1)

#��� � ���������:-------------------------------------------------------------------------

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
