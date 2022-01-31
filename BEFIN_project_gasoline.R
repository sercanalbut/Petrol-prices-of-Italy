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
library(sm)
library(splines)


gasol <- read.csv("prezzi_mensili_gasolio_auto_dal_1996_a_20220116.csv", header = TRUE)

################################################################################
# models with monthly data
tsgas_month <- ts(gasol$PREZZO, start = 1996, frequency = 12)
plot(tsgas_month, xlim=c(1996,2022))
abline(v=seq(1996,2022), lty=3, col="gray")
acf(tsgas_month)

# BASS model -------------------------------------------------------------------
bass_simple_gasol <- BASS.standard(tsgas_month, display = T, ous = 100)
summary(bass_simple_gasol)
coef(bass_simple_gasol)

plot(residuals(bass_simple_gasol))
abline(h=0)

acf(residuals(bass_simple_gasol))
pacf(residuals(bass_simple_gasol))

# BASS model with exponential shock --------------------------------------------
bass_gen_gasol = BASS.generalized(tsgas_month, shock = "exp", nshock = 1, 
                                 prelimestimates = c(6.550306e+05,9.206756e-04,7.125423e-03, 180, -0.1, 0.1),
                                 display = T, ous = 100)
summary(bass_gen_gasol)
coef(bass_gen_gasol)

plot(residuals(bass_gen_gasol))
abline(h=0)

acf(residuals(bass_gen_gasol))
pacf(residuals(bass_gen_gasol))

# BASS model with rectangular shock --------------------------------------------
bass_gen_gasol2 = BASS.generalized(tsgas_month, shock = "rett", nshock = 1, 
                                 prelimestimates = c(6.550306e+05,9.206756e-04,7.125423e-03, 180, 240, 0.1),
                                 display = T, ous = 100)
summary(bass_gen_gasol2)
coef(bass_gen_gasol2)

plot(residuals(bass_gen_gasol2))
abline(h=0)

acf(residuals(bass_gen_gasol2))
pacf(residuals(bass_gen_gasol2))

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
summary(arima_res)   # arima(0,0,2)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(mod_lin_month)+fitted(arima_res), col=3)

plot(residuals(arima_res))
abline(h=0)

Acf(residuals(arima_res))
pacf(residuals(arima_res))

# ARIMA model ------------------------------------------------------------------
# model determined by auto arima (it resulted an ARIMA(0,1,2))
arima_gasol <- auto.arima(tsgas_month)
summary(arima_gasol)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gasol), col=2)

plot(residuals(arima_gasol))

abline(h=0)

Acf(residuals(arima_gasol))
pacf(residuals(arima_gasol))

for1 <- forecast(arima_gasol)
plot(for1)

# try different types of ARIMA models
# ARIMA(1,1,1)
arima_gasol1 <- Arima(tsgas_month, order=c(1,1,1))
summary(arima_gasol1)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gasol1), col=2)

plot(residuals(arima_gasol1))
abline(h=0)

Acf(residuals(arima_gasol1))
pacf(residuals(arima_gasol1))

for_1 <- forecast(arima_gasol1)
plot(for_1)

# ARIMA(1,1,2)
arima_gasol2 <- Arima(tsgas_month, order=c(1,1,2))
summary(arima_gasol2)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gasol2), col=2)

plot(residuals(arima_gasol2))
abline(h=0)

Acf(residuals(arima_gasol2))
pacf(residuals(arima_gasol2))

for_2 <- forecast(arima_gasol2)
plot(for_2)


# try a seasonal arima model
arima_gasol_seas <- Arima(tsgas_month, order=c(0,1,2), seasonal = c(0,1,1))
summary(arima_gasol_seas)

plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fitted(arima_gasol_seas), col=2)

plot(residuals(arima_gasol_seas))
abline(h=0)

Acf(residuals(arima_gasol_seas))
pacf(residuals(arima_gasol_seas))

for_s <- forecast(arima_gasol_seas)
plot(for_s)


# nonparametric regression -----------------------------------------------------
sm.regression(seq(length(tsgas_month)), tsgas_month, h=10, add=F)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=1, add=T, col=2)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=3, add=T, col=3)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=5, add=T, col=4)
sm.regression(seq(length(tsgas_month)), tsgas_month, h=15, add=T, col=5)

# splines ----------------------------------------------------------------------
plot(gasol$PREZZO)
time <- seq(length(tsgas_month))

# let's fit a cubic spline with 5 knots
splines_mod <- lm(tsgas_month ~ bs(time, df=30, degree = 3)) # internal knots=8
fit2 <- predict(splines_mod, newdata=data.frame(x=time))

lines(time, fit2, col=2)

# let's fit a cubic spline with our chosen knots
splines_mod <- lm(tsgas_month ~ bs(time, knots = c(145,285), degree = 3)) # internal knots=5
fit2 <- predict(splines_mod, newdata=data.frame(x=time))

plot(gasol$PREZZO)
lines(time, fit2, col=2)

# GAM --------------------------------------------------------------------------



