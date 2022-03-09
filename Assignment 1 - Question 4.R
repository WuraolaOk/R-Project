## AUTHOR: 42235_WURAOLA_OKUWOBI ##

# IMPORT DATA INTO R #
install.packages("plm")
yes
install.packages("Matrix")
install.packages("lfe")
install.packages("nlme")

library(haven)
library(sandwich)
library(foreign)
library(lmtest)
library(zoo)
library(dplyr)
library("plm")

local_returns_df <-read_dta("Documents/4328 - Applied Financial Econometrics/Assignment/Assignment 1/HW1data/local_returns.dta")

str(local_returns_df)

View(local_returns_df)

#4a. CONFIRM IF BALANCED OR UNBALANCED PANEL. PROVIDE AGGREGATE STATISTICS

local_returns_panel <- pdata.frame(local_returns_df, index = c("permno", "date"))
is.pbalanced(local_returns_panel)
aggregate(local_returns_panel, list(local_returns_df$year), summary)

#4b. REPORT AVERAGE RETURNS AND VOLATILITIES OF FIRMS IN THE 20 CITIES. CHECK FOR DIFFERENTIAL RETURNS AMONGST CITIES

aggregate(local_returns_panel, list(local_returns_df$city_returns), mean)

#4c. ESTIMATE PANEL REGRESSION: ret(it) = α + β(1)city_returns(it) + β(2)indret(it) + ε(it)

local_returns_estimation <- lm(ret ~ city_returns + indret, data = local_returns_panel)
summary(local_returns_estimation)

#4d. ESTIMATE PANEL REGRESSION USING POOLED OLS WITHOUT DUMMIES
local_returns_estimation_pooled <- plm(ret ~ city_returns + indret, data = local_returns_panel, model = "pooling")
summary(local_returns_estimation_pooled)

#4e. TEST FOR HETEROSKEDASTICITY IN THE RESIDUALS

#4f. ESTIMATE PANEL REGRESSION USING POOLED OLS WITH TIME FIXED EFFECTS (e.g. TIME DUMMIES)

library(Matrix)
library(lfe)
local_returns_estimation_pooled_time <- 

#4g. ESTIMATE PANEL REGRESSION USING POOLED OLS WITH TIME AND FIRM FIXED EFFECTS. COMPARE RESULTS.

local_returns_estimation_pooled_results_bothtimeandfirm <-felm(ret ~ city_returns + indret | permno + year | 0 | 0, local_returns_panel)
summary(local_returns_estimation_pooled_results_bothtimeandfirm)
  
#4h. ESTIMATE PANEL REGRESSION USING POOLED OLS WITH TIME AND FIRM FIXED EFFECTS, AND WITH DOUBLE CLUSTERING ON BOTH FIRM AND TIME DIMENSIONS

local_returns_estimation_pooled_results_bothtimeandfirm_clust <-felm(ret ~ city_returns + indret | permno + year | 0 | permno + year, data = local_returns_panel)
summary(local_returns_estimation_pooled_results_bothtimeandfirm_clust)
  
#4i. ESTIMATE PANEL REGRESSION USING FIXED EFFECT (FE) ESTIMATOR

local_returns_estimation_firmfe <-felm(ret ~ city_returns + indret | permno | 0 | 0, data=local_returns_df)
summary(local_returns_estimation_firmfe)

#4j. ESTIMATE PANEL REGRESSION USING FIRST DIFFERENCE (FD) ESTIMATOR

local_returns_estimation_firstdiff <- 
summary(local_returns_estimation_firstdiff)

#4k. COMPARE THE FE, FD AND POOLED OLS WITH TIME AND FIXED EFFECTS WITH DOUBLE CLUSTERING

#4l. RE-ESTIMATE MODEL WITH NEW INTERACTION TERM. CONFIRM IF COEFFICIENT γ1 IS STATISTICALLY DIFFERENT FROM ZERO AND INTERPRET IT

