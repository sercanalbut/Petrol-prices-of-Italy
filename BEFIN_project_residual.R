################################################################################
# BUSINESS, ECONOMICS AND FINANCIAL DATA PROJECT - ALBUT ZIRALDO
################################################################################

ls()
rm(list = ls())
setwd("C:/Users/Utente/Desktop/Università/MAGISTRALE/2 ANNO/BUSINESS ECONOMIC AND FINANCIAL DATA/BEFIN_project")

# load data
library(readxl)
library(DIMORA)
library(MASS);
library(pROC);
library(dplyr);
library(xtable);
library(glmnet);
library(corrplot);
library(randomForest);
library(caret);
library(fpp2)
library(forecast)
library(lmtest)
library(ggplot2)
library(xts)
library(reshape2)
library(sm)
library(splines)


benz <- read.csv("prezzi_mensili_benzina_dal_1996_a_20220114.csv", header = TRUE)

################################################################################
# models with monthly data
tsgas_month <- ts(benz$PREZZO, start = 1996, frequency = 12)
plot(tsgas_month, xlim=c(1996,2022))
abline(v=seq(1996,2022), lty=3, col="gray")
acf(tsgas_month)

# BASS model -------------------------------------------------------------------
bass_simple_benz <- BASS.standard(tsgas_month, display = T, ous = 100)
summary(bass_simple_benz)
coef(bass_simple_benz)

plot(residuals(bass_simple_benz))
abline(h=0)

acf(residuals(bass_simple_benz))
pacf(residuals(bass_simple_benz))

# BASS model with exponential shock --------------------------------------------
bass_gen_benz = BASS.generalized(tsgas_month, shock = "exp", nshock = 1, 
                                 prelimestimates = c(8.700109e+05,9.286915e-04,5.402670e-03, 180, -0.1, 0.1),
                                 display = T, ous = 100)
summary(bass_gen_benz)
coef(bass_gen_benz)

plot(residuals(bass_gen_benz))
abline(h=0)

acf(residuals(bass_gen_benz))
pacf(residuals(bass_gen_benz))

# BASS model with rectangular shock --------------------------------------------
bass_gen_benz2 = BASS.generalized(tsgas_month, shock = "rett", nshock = 1, 
                                 prelimestimates = c(8.700109e+05,9.286915e-04,5.402670e-03, 180, 240, 0.1),
                                 display = T, ous = 100)
summary(bass_gen_benz2)
coef(bass_gen_benz2)

plot(residuals(bass_gen_benz2))
abline(h=0)

acf(residuals(bass_gen_benz2))
pacf(residuals(bass_gen_benz2))

# linear model -----------------------------------------------------------------
mod_lin_month <- tslm(tsgas_month ~ trend+season)
summary(mod_lin_month) 
fit <- fitted(mod_lin_month)
plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fit, col=2)

#residuals(mod_lin_month)
plot(residuals(mod_lin_month))
abline(h=0)
acf(residuals(mod_lin_month))
pacf(residuals(mod_lin_month))



# linear model + arima residuals -----------------------------------------------
arima_res <- auto.arima(residuals(mod_lin_month))
summary(arima_res)   # arima(0,1,1) is simple exponential smoothing

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(mod_lin_month)+fitted(arima_res), col=3)

plot(residuals(arima_res))
abline(h=0)

acf(residuals(arima_res))
pacf(residuals(arima_res))

# ARIMA model ------------------------------------------------------------------
# model determined by auto arima (it resulted an ARIMA(0,1,2))
arima_gas <- auto.arima(tsgas_month)
summary(arima_gas)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gas), col=2)

plot(residuals(arima_gas))

abline(h=0)

acf(residuals(arima_gas))
Pacf(residuals(arima_gas))

for1 <- forecast(arima_gas)
plot(for1)

# try different types of ARIMA models
# ARIMA(1,1,1)
arima_gas1 <- Arima(tsgas_month, order=c(1,1,1))
summary(arima_gas1)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gas1), col=2)

plot(residuals(arima_gas1))
abline(h=0)

Acf(residuals(arima_gas1))
pacf(residuals(arima_gas1))

for_1 <- forecast(arima_gas1)
plot(for_1)

# ARIMA(1,1,2)
arima_gas2 <- Arima(tsgas_month, order=c(1,1,2))
summary(arima_gas2)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gas2), col=2)

plot(residuals(arima_gas2))
abline(h=0)

Acf(residuals(arima_gas2))
pacf(residuals(arima_gas2))

for_2 <- forecast(arima_gas2)
plot(for_2)


# try a seasonal arima model
arima_gas_seas <- Arima(tsgas_month, order=c(0,1,2), seasonal = c(0,1,1))
summary(arima_gas_seas)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gas_seas), col=2)

plot(residuals(arima_gas_seas))
abline(h=0)

Acf(residuals(arima_gas_seas))
pacf(residuals(arima_gas_seas))

for_s <- forecast(arima_gas_seas)
plot(for_s)


# nonparametric regression -----------------------------------------------------
sm.regression(seq(length(tsgas_month)), tsgas_month, h=10, add=F)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=1, add=T, col=2)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=3, add=T, col=3)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=5, add=T, col=4)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=15, add=T, col=5)

# splines ----------------------------------------------------------------------
plot(benz$PREZZO)
time <- seq(length(tsgas_month))

# let's fit a cubic spline with 5 knots
splines_mod <- lm(tsgas_month ~ bs(time, df=17, degree = 3)) # internal knots=8
fit2 <- predict(splines_mod, newdata=data.frame(x=time))

lines(time, fit2, col=2)

# let's fit a cubic spline with our chosen knots
splines_mod <- lm(tsgas_month ~ bs(time, knots = c(145,285), degree = 3)) # internal knots=5
fit2 <- predict(splines_mod, newdata=data.frame(x=time))

plot(benz$PREZZO)
lines(time, fit2, col=2)

# GAM --------------------------------------------------------------------------



