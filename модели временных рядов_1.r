library(urca)
library(forecast)
library(strucchange)
library(xts)
library(MTS)
library(dplyr)
library(vars)
library(tsDyn)
library(rmgarch)

#ïðåäâàðèòåëüíàÿ ðàáîòà ñ äàííûìè
data <- data.frame(read.csv2('C:/Users/79670/Desktop/stata/ryady/dz/data_dz2.csv', header = TRUE, sep=';',dec = '.'))
dates <-as.Date(data$date, format = "%d.%m.%Y")
#ëîãàðèôìðóåì äàííûå
log_rts <- log(xts(x = data$rts, order.by = dates))
log_snp <- log(xts(x = data$snp, order.by = dates))
log_brent <- log(xts(x = data$brent, order.by = dates))
log_nikkei <- log(xts(x = data$nikkei, order.by = dates))
#ãðàôèêè ëîãàðèôèìðîâàííûõ çíà÷åíèé
plot(log_rts, main = 'ÐÒÑ', ylab = 'ëîãàðèôìèðîâàííûå çíà÷åíèÿ')
plot(log_snp, main = 'S&P500', ylab = 'ëîãàðèôìèðîâàííûå çíà÷åíèÿ')
plot(log_brent, main = 'ñïîò-öåíà Brent', ylab = 'ëîãàðèôìèðîâàííûå çíà÷åíèÿ')
plot(log_nikkei, main = 'Nikkei 225', ylab = 'ëîãàðèôìèðîâàííûå çíà÷åíèÿ')
#âçÿëè ïåðâûå ðàçíîñòè ðÿäîâ
dif_log_rts <- diff(log_rts)[2:length(log_rts)]
dif_log_snp <- diff(log_snp)[2:length(log_rts)]
dif_log_brent <- diff(log_brent)[2:length(log_rts)]
dif_log_nikkei <- diff(log_nikkei)[2:length(log_rts)]
dif <- data.frame(cbind(dif_log_rts,dif_log_snp,dif_log_brent,dif_log_nikkei))
dif_frame <- dif[,2:5]
#ãðàôèêè ëîã-äîõîäíîñòåé
plot(dif_log_rts, main = 'ÐÒÑ', ylab = 'ëîã-äîõîäíîñòè')
plot(dif_log_snp, main = 'S&P500', ylab = 'ëîã-äîõîäíîñòè')
plot(dif_log_brent, main = 'ñïîò-öåíà Brent', ylab = 'ëîã-äîõîäíîñòè')
plot(dif_log_nikkei, main = 'Nikkei 225', ylab = 'ëîã-äîõîäíîñòè') 

#ïðîâåðÿåì íà áðåéêè Áàé-Ïåððîíîì
bp.log_rts <- breakpoints(log_rts~1, h = 0.15, breaks = 3)
bp.log_snp <- breakpoints(log_snp~1, h = 0.15, breaks = 3)
bp.log_brent <- breakpoints(log_brent~1, h = 0.15, breaks = 3)
bp.log_nikkei <- breakpoints(log_nikkei~1, h = 0.15, breaks = 3)

#ñìîòðèì äàòû ïî ïîëó÷åííûì íîìåðàì íàáëþäåíèé 
data.frame(log_rts[bp.log_rts$breakpoints])
data.frame(log_snp[bp.log_snp$breakpoints])
data.frame(log_brent[bp.log_brent$breakpoints])
data.frame(log_nikkei[bp.log_nikkei$breakpoints])

# ñòðîèì ADL äëÿ ÐÒÑ
formula_rts <- dif_log_rts~dif_log_rts+dif_log_snp+dif_log_brent+dif_log_nikkei
frame_data <- log(data[,2:5])
colnames(frame_data) <- c('dif_log_rts', 'dif_log_snp', 'dif_log_brent', 'dif_log_nikkei')
adl_rts <- dlm(formula_rts, frame_data, q=4)
adl_rts_lags <- finiteDLMauto(formula=formula_rts, frame_data, q.min = 1, q.max = 10, k.order = NULL,
              model.type = "dlm", error.type = "AIC",
              trace = FALSE)
summary(adl_rts)

#òåñò ADF íà ñòàöèîíàðíîñòü
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

# ïðîâåðêà íà ñòàöèîíàðíîñòü ïåðâûõ ðàçíîñòåé ðÿäîâ
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

# äëÿ òåñòà Éîõàíñåíà íà êîèíòåãðàöèþ ñíà÷àëà îáúåäåíèì îäíîìåðíûå ðÿäû â ìíîãîìåðíûé
multivar_ts <- merge(log_snp,log_brent, log_nikkei, log_rts)
# ïîòîì ïðîâåä¸ì òåñò
johansen_coint <- ca.jo(multivar_ts, type = 'trace', ecdet = "trend", K = 4,
                        spec="transitory")

summary(johansen_coint)

# òàê êàê êîèíòåãðàöèè íåò, ñòðîèì VARIMA(p,d,q)
# varma <- VARMA(data_matrix, p = 2, q = 1)
# ñäà¸ìñÿ è ñòðîèì VAR â ðàçíîñòÿõ (VARIMA(p,1,0))
dif_multivar_ts <- merge(dif_log_rts,dif_log_snp,dif_log_nikkei,dif_log_brent)

# îöåíèâàåì VARMA(1,1,0), âûáèðàÿ êîëè÷åñòâî ëàãîâ ïî AIC
VAR_hat <- VAR(y = dif_multivar_ts[1:(length(dif_log_rts)-1),], ic = "AIC")
VAR_hat$varresult
summary(VAR_hat)

# ïðîâåðÿåì íà îòñóòñâèå àâòîêîððåëÿöèè îñòàòêîâ òåñòîì Áðîéøà-Ãîäôðè
serial.test(VAR_hat, lags.bg = 1, type = "BG")

# ïðîåðÿåì, âñå ëè õàðàêòåðèñòè÷åñêèå êîðíè ïîëèíîìà VAR ìåíüøå åäèíèöû ïî ìîäóëþ
var_roots <- roots(VAR_hat)

# ïðîâåðèëè íà íàëè÷èå arch ýôôåêòà
arch1 <- arch.test(VAR_hat, lags.multi = 1, multivariate.only = TRUE)
arch.test(VAR_hat, lags.multi = 3, multivariate.only = TRUE)
# äåëàåì ïðîãíîç íà îäèí øàã
var_pred <- predict(VAR_hat, n.ahead = 1, ci = 0.95)
var_pred_fcst <- c(0.0003444441, -9.747825e-06, -0.001009998, -0.0005650997) 
var_pred_fact <- c(0.002450962, 0.002941694, -0.002754394, -0.007790147)

accuracy(var_pred_fcst, var_pred_fact, D = 0, d = 1)

# îöåíèâàåì SVAR
# ñíà÷àëà çàäàäèì àïðèîðíóþ ìàòðèöó îãðàíè÷åíèé A (íàçîâ¸ì å¸ amat)
amat <- diag(4)
amat[2:4,1] <- NA       # ìàòðèöó çàäà¸òñÿ èñõîäÿ èç
amat[3:4,2] <- NA       # ïðåäïîëîæåíèÿ, ÷òî S&P500 ìîæå âëèÿòü â ìîìåíò t
amat[4,3] <- NA         # íà âñå îñòàëüíûå ïåðìåííûå, Brent íà âñå, êðîìå S&P500
                        # Nikkei 225 íà ÐÒÑ è ÐÒÑ íè íà îäíó èç ïåðìåíûõ

SVAR_hat <- SVAR(VAR_hat, estmethod = "direct", Amat = amat, Bmat = NULL, lrtest = TRUE)

# ôóíêöèè èìïóëüñíûõ îòêëèêîâ
irf_all <- irf(SVAR_hat, n.ahead = 10, boot = FALSE)
plot(irf_all, ylab = "", main = "S&P500 shock to RRP")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_rts", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "ëîã-äîõîäíîñòè ÐÒÑ",   main = "ðåàêöèÿ ÐÒÑ íà øîêè ÐÒÑ")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_snp", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "ëîã-äîõîäíîñòè ÐÒÑ", main = "ðåàêöèÿ ÐÒÑ íà øîêè S&P500")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_brent", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "ëîã-äîõîäíîñòè ÐÒÑ", main = "ðåàêöèÿ ÐÒÑ íà øîêè Brent")

snp_rts_irf <- irf(SVAR_hat, impulse = "dif_log_nikkei", response = "dif_log_rts", n.ahead = 10, boot = FALSE)
plot(snp_rts_irf, ylab = "ëîã-äîõîäíîñòè ÐÒÑ", main = "ðåàêöèÿ ÐÒÑ íà øîêè Nikkei225")

# äåêðìïîçèöèÿ äèñïåðñèè ïðîãíîçà
fevd1 <- fevd(SVAR_hat, n.ahead = 5,)
fevd1
plot(fevd1)


# GARCH
rts_spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 1)))
rts_garch <- ugarchfit(spec = rts_spec,
                       data=dif_log_rts)


forecast_volatil <- ugarchforecast(rts_garch)


