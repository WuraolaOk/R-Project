## AUTHOR: 42235_WURAOLA_OKUWOBI ##

#1a. IMPORT DATA INTO R #
install.packages("readxl")
library(readxl)
monthly_stock_returns <- read_excel("Documents/4328 - Applied Financial Econometrics/Assignment/Assignment 1/HW1data/data_assignment1.xlsx", sheet = "stock_monthly")

#1b. TRANSFORM THE SIMPLE RETURNS INTO LOG RETURNS#
monthly_stock_returns$COCACOLA1 <- c(0, diff(log(1+monthly_stock_returns$COCACOLA), lag = 1))
monthly_stock_returns$GE1 <- c(0, diff(log(1+monthly_stock_returns$GE), lag = 1))
monthly_stock_returns$IBM1 <- c(0, diff(log(1+monthly_stock_returns$IBM), lag = 1))
monthly_stock_returns$VWRET1 <- c(0, diff(log(1+monthly_stock_returns$VWRET), lag = 1))
monthly_stock_returns$EWRET1 <- c(0, diff(log(1+monthly_stock_returns$EWRET), lag = 1))

#1c. PLOT THE SIMPLE AND LOG RETURNS FOR ALL THE SERIES#
plot(monthly_stock_returns$COCACOLA, monthly_stock_returns$COCACOLA1)
plot(monthly_stock_returns$GE, monthly_stock_returns$GE1)
plot(monthly_stock_returns$IBM, monthly_stock_returns$IBM1)
plot(monthly_stock_returns$VWRET, monthly_stock_returns$VWRET1)
plot(monthly_stock_returns$EWRET, monthly_stock_returns$EWRET1)

#1d. SCATTERPLOT OF ALL THE LOG RETURNS SERIES#
monthly_stock_returns_log <- monthly_stock_returns[,c(7:11)]
plot(monthly_stock_returns_log)

#1e. SAMPLE MEAN, VARIANCE, SKEWNESS, EXCESS KURTOSIS, MINIMUM AND MAXIMUM OF LOG RETURNS#
install.packages("moments")
library(moments)
COCACOLA_log <- c(mean(monthly_stock_returns$COCACOLA1), var(monthly_stock_returns$COCACOLA1), skewness(monthly_stock_returns$COCACOLA1), kurtosis(monthly_stock_returns$COCACOLA1, excess = TRUE), min(monthly_stock_returns$COCACOLA1), max(monthly_stock_returns$COCACOLA1))
GE_log <- c(mean(monthly_stock_returns$GE1), var(monthly_stock_returns$GE1), skewness(monthly_stock_returns$GE1), kurtosis(monthly_stock_returns$GE1, excess = TRUE), min(monthly_stock_returns$GE1), max(monthly_stock_returns$GE1))
IBM_log <- c(mean(monthly_stock_returns$IBM1), var(monthly_stock_returns$IBM1), skewness(monthly_stock_returns$IBM1), kurtosis(monthly_stock_returns$IBM1, excess = TRUE), min(monthly_stock_returns$IBM1), max(monthly_stock_returns$IBM1))
VWRET_log <- c(mean(monthly_stock_returns$VWRET1), var(monthly_stock_returns$VWRET1), skewness(monthly_stock_returns$VWRET1), kurtosis(monthly_stock_returns$VWRET1, excess = TRUE), min(monthly_stock_returns$VWRET1), max(monthly_stock_returns$VWRET1))
EWRET_log <- c(mean(monthly_stock_returns$EWRET1), var(monthly_stock_returns$EWRET1), skewness(monthly_stock_returns$EWRET1), kurtosis(monthly_stock_returns$EWRET1, excess = TRUE), min(monthly_stock_returns$EWRET1), max(monthly_stock_returns$EWRET1))

summary_df <- data.frame(COCACOLA_log, GE_log, IBM_log, VWRET_log, EWRET_log)
row.names(summary_df) <- c("MEAN", "VARIANCE", "SKEWNESS", "EXCESS KURTOSIS", "MINIMUM", "MAXIMUM")
summary_df

#1f. AT 5% SIGNIFICANCE LEVEL, ARE SAMPLE MEAN, SKEWNESS AND EXCESS KURTOSIS STATISTICALLY DIFFERENT FROM ZERO? #

#1g. HISTOGRAM OF RETURNS IN COMPARISON TO NORMAL AND STUDENT DISTRIBUTIONS #

hist(monthly_stock_returns$COCACOLA)
hist(monthly_stock_returns$GE)
hist(monthly_stock_returns$IBM)
hist(monthly_stock_returns$VWRET)
hist(monthly_stock_returns$EWRET)
hist(monthly_stock_returns$COCACOLA1)
hist(monthly_stock_returns$GE1)
hist(monthly_stock_returns$IBM1)
hist(monthly_stock_returns$VWRET1)
hist(monthly_stock_returns$EWRET1)

#JARQUE - BERA TEST AT THE 5% SIGNIFICANCE LEVEL #

install.packages("tseries")
library(tseries)
jarque.bera.test(monthly_stock_returns$COCACOLA)
jarque.bera.test(monthly_stock_returns$IBM)
jarque.bera.test(monthly_stock_returns$GE)
jarque.bera.test(monthly_stock_returns$EWRET)
jarque.bera.test(monthly_stock_returns$VWRET)

#1H. CAPM REGRESSION FOR COCACOLA, GE AND IBM. #
monthly_stock_returns$RISKFREE <- 0
monthly_stock_returns$RISKPREMIUM <- monthly_stock_returns$VWRET - monthly_stock_returns$RISKFREE
COCACOLA_FIT <- lm(COCACOLA ~ VWRET, data = monthly_stock_returns)
GE_FIT <- lm(GE ~ VWRET, data = monthly_stock_returns)
IBM_FIT <- lm(IBM ~ VWRET, data = monthly_stock_returns)
summary(COCACOLA_FIT)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
str(monthly_stock_returns)
monthly_stock_returns <- xts(monthly_stock_returns[,2:13], order.by = as.Date(as.character(monthly_stock_returns[,1]))

COCACOLA.BETA <- CAPM.beta(Ra = monthly_stock_returns$COCACOLA,Rb = monthly_stock_returns$VWRET)

