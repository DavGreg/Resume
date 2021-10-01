library(urca)
library(forecast)
library(strucchange)
library(xts)
library(MTS)
library(dplyr)
library(vars)
library(tsDyn)
library(rmgarch)

#предварительная работа с данными
data <- data.frame(read.csv2('C:/Users/79670/Desktop/stata/ryady/dz/data_dz2.csv', header = TRUE, sep=';',dec = '.'))
dates <-as.Date(data$date, format = "%d.%m.%Y")
#логарифмруем данные
log_rts <- log(xts(x = data$rts, order.by = dates))
log_snp <- log(xts(x = data$snp, order.by = dates))
log_brent <- log(xts(x = data$brent, order.by = dates))
log_nikkei <- log(xts(x = data$nikkei, order.by = dates))
#графики логарифимрованных значений
plot(log_rts, main = 'РТС', ylab = 'логарифмированные значения')
plot(log_snp, main = 'S&P500', ylab = 'логарифмированные значения')
plot(log_brent, main = 'спот-цена Brent', ylab = 'логарифмированные значения')
plot(log_nikkei, main = 'Nikkei 225', ylab = 'логарифмированные значения')
#взяли первые разности рядов
dif_log_rts <- diff(log_rts)[2:length(log_rts)]
dif_log_snp <- diff(log_snp)[2:length(log_rts)]
dif_log_brent <- diff(log_brent)[2:length(log_rts)]
dif_log_nikkei <- diff(log_nikkei)[2:length(log_rts)]
dif <- data.frame(cbind(dif_log_rts,dif_log_snp,dif_log_brent,dif_log_nikkei))
dif_frame <- dif[,2:5]
#графики лог-доходностей
plot(dif_log_rts, main = 'РТС', ylab = 'лог-доходности')
plot(dif_log_snp, main = 'S&P500', ylab = 'лог-доходности')
plot(dif_log_brent, main = 'спот-цена Brent', ylab = 'лог-доходности')
plot(dif_log_nikkei, main = 'Nikkei 225', ylab = 'лог-доходности')

#проверяем на брейки Бай-Перроном
bp.log_rts <- breakpoints(log_rts~1, h = 0.15, breaks = 3)
bp.log_snp <- breakpoints(log_snp~1, h = 0.15, breaks = 3)
bp.log_brent <- breakpoints(log_brent~1, h = 0.15, breaks = 3)
bp.log_nikkei <- breakpoints(log_nikkei~1, h = 0.15, breaks = 3)

#смотрим даты по полученным номерам наблюдений 
data.frame(log_rts[bp.log_rts$breakpoints])
data.frame(log_snp[bp.log_snp$breakpoints])
data.frame(log_brent[bp.log_brent$breakpoints])
data.frame(log_nikkei[bp.log_nikkei$breakpoints])

# строим ADL для РТС
formula_rts <- dif_log_rts~dif_log_rts+dif_log_snp+dif_log_brent+dif_log_nikkei
frame_data <- log(data[,2:5])
colnames(frame_data) <- c('dif_log_rts', 'dif_log_snp', 'dif_log_brent', 'dif_log_nikkei')
adl_rts <- dlm(formula_rts, frame_data, q=4)
adl_rts_lags <- finiteDLMauto(formula=formula_rts, frame_data, q.min = 1, q.max = 10, k.order = NULL,
              model.type = "dlm", error.type = "AIC",
              trace = FALSE)
summary(adl_rts)

#тест ADF на стационарность
adf_rts_level <- ur.df(log_rts,  type = "drift", selectlags = "BIC")
adf_rts_level@teststat
summary(adf_rts_level)

adf_snp_level <- ur.df(log_snp, type = "drift", selectlags = "BIC")
adf_snp_level@teststat
summary(adf_snp_level)

adf_brent_level <- ur.df(log_brent, type = "drift", selectlags = "BIC")
adf_brent_level@teststat
summary(adf_brent_level)

adf_nikkei_level <- ur.df(log_nikkei, type = "drift", selectlags = "BIC")
adf_nikkei_level@teststat
summary(adf_nikkei_level)

# проверка на стационарность первых разностей рядов
adf_rts_dif <- ur.df(dif_log_rts,  type = "drift", selectlags = "BIC")
adf_rts_dif@teststat
summary(adf_rts_level)

adf_snp_dif <- ur.df(dif_log_snp, type = "drift", selectlags = "BIC")
adf_snp_dif@teststat
summary(adf_snp_level)

adf_brent_dif <- ur.df(dif_log_brent, type = "drift", selectlags = "BIC")
adf_brent_dif@teststat
summary(adf_brent_level)

adf_nikkei_dif <- ur.df(dif_log_nikkei, type = "drift", selectlags = "BIC")
adf_nikkei_dif@teststat
summary(adf_nikkei_level)

# для теста Йохансена на коинтеграцию сначала объеденим одномерные ряды в многомерный
multivar_ts <- merge(log_snp,log_brent, log_nikkei, log_rts)
# потом проведём тест
johansen_coint <- ca.jo(multivar_ts, type = 'trace', ecdet = "trend", K = 4,
                        spec="transitory")

summary(johansen_coint)

# так как коинтеграции нет, строим VARIMA(p,d,q)
# varma <- VARMA(data_matrix, p = 2, q = 1)
# сдаёмся и строим VAR в разностях (VARIMA(p,1,0))
dif_multivar_ts <- merge(dif_log_rts,dif_log_snp,dif_log_nikkei,dif_log_brent)

# оцениваем VARMA(1,1,0), выбирая количество лагов по AIC
VAR_hat <- VAR(y = dif_multivar_ts[1:(length(dif_log_rts)-1),], ic = "AIC")
VAR_hat$varresult
summary(VAR_hat)

# проверяем на отсутсвие автокорреляции остатков тестом Бройша-Годфри
serial.test(VAR_hat, lags.bg = 1, type = "BG")

# проеряем, все ли характеристические корни полинома VAR меньше единицы по модулю
var_roots <- roots(VAR_hat)

# проверили на наличие arch эффекта
arch1 <- arch.test(VAR_hat, lags.multi = 1, multivariate.only = TRUE)
arch.test(VAR_hat, lags.multi = 3, multivariate.only = TRUE)
# делаем прогноз на один шаг
var_pred <- predict(VAR_hat, n.ahead = 1, ci = 0.95)
var_pred_fcst <- c(0.0003444441, -9.747825e-06, -0.001009998, -0.0005650997) 
var_pred_fact <- c(0.002450962, 0.002941694, -0.002754394, -0.007790147)

accuracy(var_pred_fcst, var_pred_fact, D = 0, d = 1)

# оцениваем SVAR
# сначала зададим априорную матрицу ограничений A (назовём её amat)
amat <- diag(4)
amat[2:4,1] <- NA       # матрицу задаётся исходя из
amat[3:4,2] <- NA       # предположения, что S&P500 може влиять в момент t
amat[4,3] <- NA         # на все остальные перменные, Brent на все, кроме S&P500
                        # Nikkei 225 на РТС и РТС ни на одну из перменых

SVAR_hat <- SVAR(VAR_hat, estmethod = "direct", Amat = amat, Bmat = NULL, lrtest = TRUE)

# функции импульсных откликов
irf_all <- irf(SVAR_hat, n.ahead = 10, boot = FALSE)
plot(irf_all, ylab = "", main = "S&P500 shock to RRP")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_rts", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "лог-доходности РТС",   main = "реакция РТС на шоки РТС")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_snp", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "лог-доходности РТС", main = "реакция РТС на шоки S&P500")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_brent", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "лог-доходности РТС", main = "реакция РТС на шоки Brent")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_nikkei", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "лог-доходности РТС", main = "реакция РТС на шоки Nikkei225")

# декрмпозиция дисперсии прогноза
fevd1 <- fevd(SVAR_hat, n.ahead = 5,)
fevd1
plot(fevd1)


# GARCH
rts_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 1)))
rts_garch <- ugarchfit(spec = rts_spec,
                       data=dif_log_rts)


forecast_volatil <- ugarchforecast(rts_garch)


