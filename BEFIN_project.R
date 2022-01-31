################################################################################
# BUSINESS, ECONOMICS AND FINANCIAL DATA PROJECT - ALBUT ZIRALDO
################################################################################

ls()
rm(list = ls())
setwd("C:/Users/serca/OneDrive/Masaüstü/BEFD proje/BEFIN_project")

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

data=read.csv(file = "weekly_fuel_prices_all_data_from_2005_to_20210823.csv",header = TRUE);
attach(data);
data = data.frame(data);
data$SURVEY_DATE <- as.Date(data$SURVEY_DATE, "%Y-%m-%d")
ES95 <- data[data$PRODUCT_ID == "1",]
AGO <- data[data$PRODUCT_ID == "2",]
HGO <- data[data$PRODUCT_ID == "3",]
LPG <- data[data$PRODUCT_ID == "5",]
RFO <- data[data$PRODUCT_ID == "6",]
HFO <- data[data$PRODUCT_ID == "8",]
str(ES95)

plot(ES95$PRICE, type="l", xlab = "Time")
plot(ES95$PRICE[0:104], type="l", xlab = "Time")
#plot(AGO$PRICE, type="l", xlab = "Time")
#plot(HGO$PRICE, type="l", xlab = "Time")
#plot(LPG$PRICE, type="l", xlab = "Time")
acf(ES95$PRICE)
pacf(ES95$PRICE)
acf(ES95$PRICE, 156)

#################################################################################
# simple BASS model

BM_simple <- BASS.standard(ES95$PRICE[0:104], display = T, ous = 100)
summary(BM_simple)
coef(BM_simple)
residuals(BM_simple)
plot(residuals(BM_simple))

predict(BM_simple, newx = 1:100)
abline(h=0)

plot(BM_simple, mode = "i", oos = "y", xlim = c(1,100))



#################################################################################
# Linear regression for time series

tsgas <- ts(ES95$PRICE, start = 2005, frequency = 52)
plot(tsgas)

# test on all time period 2005-2021
mod_lin <- tslm(tsgas ~ trend+season)
summary(mod_lin)
fit <- fitted(mod_lin)
plot(tsgas)
abline(v=years, lty=3)
lines(fit, col=2)


# test from 2005 to 2008 (pre-crisis)
mod_lin1 <- tslm(window(tsgas, start=2005, end=2008) ~ trend+season)
summary(mod_lin1)
fit1 <- fitted(mod_lin1)
plot(tsgas, xlim=c(2005,2008))
lines(fit1, col=2)

residuals(mod_lin1)
plot(residuals(mod_lin1))
abline(h=0)
acf(residuals(mod_lin1))
pacf(residuals(mod_lin1))


################################################################################
# models with monthly data
tsgas_month <- ts(benz$PREZZO, start = 1996, frequency = 12)
plot(tsgas_month, xlim=c(1996,2022))
abline(v=seq(1996,2022), lty=3, col="gray")
acf(tsgas_month)

# linear model
mod_lin_month <- tslm(tsgas_month ~ trend+season)
summary(mod_lin_month)
fit <- fitted(mod_lin_month)
plot(tsgas_month)
abline(v=seq(1996,2022), lty=3, col="gray")
lines(fit, col=2)

residuals(mod_lin_month)
plot(residuals(mod_lin_month))
abline(h=0)
acf(residuals(mod_lin_month))
pacf(residuals(mod_lin_month))

# bass model
bass_month <- BASS.standard(tsgas_month, display = T, ous = 100)
summary(bass_month)
coef(bass_month)

plot(residuals(bass_month))
abline(h=0)

acf(residuals(bass_month))
pacf(residuals(bass_month))

# try exponential shock
# linear model + arima residuals 
# gam?
# splines


