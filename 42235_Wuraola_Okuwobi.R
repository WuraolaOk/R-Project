## AUTHOR: 42235_WURAOLA_OKUWOBI ##

## QUESTION 1 ##

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
plot(monthly_stock_returns)

#1e. SAMPLE MEAN, VARIANCE, SKEWNESS, EXCESS KURTOSIS, MINIMUM AND MAXIMUM OF LOG RETURNS#
install.packages("moments")
library(moments)
COCACOLA_log <- c(mean(monthly_stock_returns$COCACOLA1), var(monthly_stock_returns$COCACOLA1), skewness(monthly_stock_returns$COCACOLA1), kurtosis(monthly_stock_returns$COCACOLA1), min(monthly_stock_returns$COCACOLA1), max(monthly_stock_returns$COCACOLA1))
GE_log <- c(mean(monthly_stock_returns$GE1), var(monthly_stock_returns$GE1), skewness(monthly_stock_returns$GE1), kurtosis(monthly_stock_returns$GE1), min(monthly_stock_returns$GE1), max(monthly_stock_returns$GE1))
IBM_log <- c(mean(monthly_stock_returns$IBM1), var(monthly_stock_returns$IBM1), skewness(monthly_stock_returns$IBM1), kurtosis(monthly_stock_returns$IBM1), min(monthly_stock_returns$IBM1), max(monthly_stock_returns$IBM1))
VWRET_log <- c(mean(monthly_stock_returns$VWRET1), var(monthly_stock_returns$VWRET1), skewness(monthly_stock_returns$VWRET1), kurtosis(monthly_stock_returns$VWRET1), min(monthly_stock_returns$VWRET1), max(monthly_stock_returns$VWRET1))
EWRET_log <- c(mean(monthly_stock_returns$EWRET1), var(monthly_stock_returns$EWRET1), skewness(monthly_stock_returns$EWRET1), kurtosis(monthly_stock_returns$EWRET1), min(monthly_stock_returns$EWRET1), max(monthly_stock_returns$EWRET1))

summary_df <- data.frame(COCACOLA_log, GE_log, IBM_log, VWRET_log, EWRET_log)
row.names(summary_df) <- c("MEAN", "VARIANCE", "SKEWNESS", "EXCESS KURTOSIS", "MINIMUM", "MAXIMUM")
summary_df

#1f. AT 5% SIGNIFICANCE LEVEL, ARE SAMPLE MEAN, SKEWNESS AND EXCESS KURTOSIS OF LOG RETURNS STATISTICALLY DIFFERENT FROM ZERO? #

# SAMPLE MEAN TEST #

mean_COCACOLA1 <- t.test(monthly_stock_returns_log$COCACOLA1)
mean_GE1 <- t.test(monthly_stock_returns_log$GE1)
mean_IBM1 <- t.test(monthly_stock_returns_log$IBM1)
mean_VWRET1 <- t.test(monthly_stock_returns_log$VWRET1)
mean_EWRET1 <- t.test(monthly_stock_returns_log$EWRET1)

#Answer: Since the p-value > 5%, at 5% significance level we do not reject the null hypothesis that the sample means of the log returns are statistically equL zero. This means that the true means of the log returns of the variables (COCACOLA, IBM, GE, VWRET AND EWRET) are equal to zero.

# SKEWNESS TEST #

skew_COCACOLA1 <- skewness(monthly_stock_returns_log$COCACOLA1)/sqrt(6/588)
skew_GE1 <- skewness(monthly_stock_returns_log$GE1)/sqrt(6/588)
skew_IBM1 <- skewness(monthly_stock_returns_log$IBM1)/sqrt(6/588)
skew_VWRET1 <- skewness(monthly_stock_returns_log$VWRET1)/sqrt(6/588)
skew_EWRET1 <- skewness(monthly_stock_returns_log$EWRET1)/sqrt(6/588)

#Answer: Since the absolute value from the result of the skewness test > 1.96, at 5% significance level we reject the null hypothesis that the skewness of the log returns are equal to zero. This means that the log returns of the variables (COCACOLA, IBM, GE, VWRET AND EWRET) do not follow normal distribution.

# KURTOSIS TEST #

kurtosis_COCACOLA1 <- (kurtosis(monthly_stock_returns_log$COCACOLA1)-3)/sqrt(24/588)
kurtosis_GE1 <- (kurtosis(monthly_stock_returns_log$GE1)-3)/sqrt(24/588)
kurtosis_IBM1 <- (kurtosis(monthly_stock_returns_log$IBM1)-3)/sqrt(24/588)
kurtosis_VWRET1 <- (kurtosis(monthly_stock_returns_log$VWRET1)-3)/sqrt(24/588)
kurtosis_EWRET1 <- (kurtosis(monthly_stock_returns_log$EWRET1)-3)/sqrt(24/588)

#Answer: Since the value from the result of the kurtosis test > 1.96, at 5% significance level we reject the null hypothesis that the kurtosis of the log returns are equal to zero. This means that the log returns of the variables (COCACOLA, IBM, GE, VWRET AND EWRET) do not follow normal distribution.

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

plot(density(monthly_stock_returns$COCACOLA))
plot(density(monthly_stock_returns$GE))
plot(density(monthly_stock_returns$IBM))
plot(density(monthly_stock_returns$VWRET))
plot(density(monthly_stock_returns$EWRET))
plot(density(monthly_stock_returns$COCACOLA1))
plot(density(monthly_stock_returns$GE1))
plot(density(monthly_stock_returns$IBM1))
plot(density(monthly_stock_returns$VWRET1))
plot(density(monthly_stock_returns$EWRET1))

#JARQUE - BERA TEST AT THE 5% SIGNIFICANCE LEVEL #

install.packages("tseries")
library(tseries)
jarque.bera.test(monthly_stock_returns$COCACOLA)
jarque.bera.test(monthly_stock_returns$IBM)
jarque.bera.test(monthly_stock_returns$GE)
jarque.bera.test(monthly_stock_returns$EWRET)
jarque.bera.test(monthly_stock_returns$VWRET)
jarque.bera.test(monthly_stock_returns$COCACOLA1)
jarque.bera.test(monthly_stock_returns$IBM1)
jarque.bera.test(monthly_stock_returns$GE1)
jarque.bera.test(monthly_stock_returns$EWRET1)
jarque.bera.test(monthly_stock_returns$VWRET1)

# Answer: Since the p-values from the result of the jarque-bera tests for the returns and log returns of COCACOLA, GE, IBM, VWRET and EWRET are less than 5%, then we reject the null hypothesis and conclude that the returns and log returns do not follow a normal distribution.

#1H. CAPM REGRESSION FOR COCACOLA, GE AND IBM. #
COCACOLA_FIT <- lm(COCACOLA ~ VWRET, data = monthly_stock_returns)
GE_FIT <- lm(GE ~ VWRET, data = monthly_stock_returns)
IBM_FIT <- lm(IBM ~ VWRET, data = monthly_stock_returns)
summary(COCACOLA_FIT)
summary(GE_FIT)
summary(IBM_FIT)

# Answer: Please note that the VWRET was selected as the proxy for market returns because the market portfolio is a value-weighted portfolio of all securities traded in the market. The alphas α for COCACOLA, GE and IBM are 0.006496, 0.0004826 and 0.002762 respectively while the betas β for COCACOLA, GE and IBM are 0.716446, 1.1199868 and 0.879652 respectively. The p-value of the β are less than 1.96, hence at 5% significance levels w edo not reject the null hypothesis and conclude that are statistically significant. 

## QUESTION 2 ##

#2a. IMPORT DATA INTO R #
library("haven")

pension_df <-read_dta("Documents/4328 - Applied Financial Econometrics/Assignment/Assignment 1/HW1data/pension.dta")

str(pension_df)

#2b. HOW MANY SINGLE PERSON HOUSEHOLDS ARE THERE IN THE DATASET #
library("dplyr")

no_of_single_person_households <- length(which(pension_df$marr == 0))

no_of_single_person_households

#2c. OLS ESTIMATION OF THE MODEL TO REFLECT RESULTS OF SINGLE-PERSON HOUSEHOLDS: nettfa = β0 + β1inc + β2age + u #

OLS_estimatea <- lm(nettfa ~ inc + age, data = pension_df)

summary(OLS_estimatea)

OLS_estimate_single <- lm(nettfa ~ inc + age, data = subset(pension_df, marr == 0))

summary(OLS_estimate_single)

#2d. MEANING OF INTERCEPT FROM THE REGRESSION: nettfa = β0 + β1inc + β2age + u #

# Answer: The intercept shows that single people have negative net financial wealth of -43-97943 if income and age equals zero. However, this negative net financial wealth position is worse for married people at an intercept of -60-69654.

#2e. p-VALUE FOR THE TEST H0 :β2 = 1 AGAINST H1 :β2 < 1 AT 1% SIGNIFICANCE LEVEL #


#2f. p-VALUE FOR THE TEST H0 :β2 = 1 AGAINST H1 :β2 != 1 AT 1% SIGNIFICANCE LEVEL #


#2g. OLS ESTIMATION OF THE MODEL: nettfa = β0 + β1inc + u #

OLS_estimate1 <- lm(nettfa ~ inc, data = pension_df)

summary(OLS_estimate1)

#2h. OLS ESTIMATION OF THE MODEL: nettfa = β0 + β1inc + β2age + β3inc^2 + β4age^2 + β5fsize +  u#

OLS_estimate2 <- lm(nettfa ~ inc + age + incsq + agesq + fsize, data = pension_df)

summary(OLS_estimate2)

#2i. F-TEST OF THE RESTRICTION: H0 : β3 = β4 = 0 IN MODE L(OLS_estimate2) #

install.packages("car", dependencies=TRUE)
library(car)
nullhyp <- c("incsq", "agesq")
linearHypothesis(OLS_estimate2, nullhyp)

#Answer: The probability value of the f-test is less than 1%, hence we reject the null hypothesis at 1% significance level and conclude that the coefficients of incsq and agesq do not equal zero.

#2j. RE-ESTIMATION OF THE MODEL (OLS_estimate2) AFTER RESCALING inc BY DIVIDING IT BY 10#

pension_df$inc_adjusted <- pension_df$inc/10

OLS_estimate2_adj <- lm(nettfa ~ inc_adjusted + age + I(inc^2) + I(age^2) + fsize, data = pension_df)

summary(OLS_estimate2_adj)

#2k. TESTING THE MODEL (OLS_estimate) FOR HETEROSKEDASTICITY USING THE BREUSCH-PAGAN TEST #

library(lmtest)
bptest(OLS_estimatea)

#Answer: the p-value is less than the significance level at 1% or 5%, hence we reject the null hypothesis and conclude that heteroskedasticity is present #

#2l. ESTIMATING THE MODEL (OLS_estimate) WITH HETEROSKEDASTICITY-ROBUST (e.g. WHITE) STANDARD ERRORS #

library(lmtest)
library(sandwich)
coeftest(OLS_estimatea, vcov = sandwich)

#2m. RE-ESTIMATING THE MODEL (OLS_estimate) AFTER STANDARDISING ALL VARIABLES IN THE MODEL. INTERPRETE COEFFICIENTS ON inc AND age #

OLS_estimatea <- lm(nettfa ~ inc + age, data = pension_df)
pension_df$resi <- OLS_estimatea$residuals
varfunc.ols <- lm(log(resi^2) ~ log(inc) + log(age), data = pension_df)
pension_df$varfunc <- exp(varfunc.ols$fitted.values)
OLS_estimatea_gls <- lm(nettfa ~ inc + age, weights = 1/sqrt(varfunc), data = pension_df)

summary(OLS_estimatea)
summary(OLS_estimatea_gls)

## QUESTION 3 ##

# IMPORT DATA INTO R #

library("haven")
library("sandwich")
library(foreign)
library("lmtest")
library("zoo")
library(dplyr)

ceo_salary_df <-read_dta("Documents/4328 - Applied Financial Econometrics/Assignment/Assignment 1/HW1data/ceo_salary.dta")

str(ceo_salary_df)

#3a. Estimation of the model using OLS: lsalary = β0 + β1lsales + β2lmktval + β3ceoten + β4ceoten^2 + u

OLS_estimate_salary <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq, data = ceo_salary_df)

summary(OLS_estimate_salary)

#3b. COUNT, MEAN, STANDARD DEVIATION, MINIMUM AND MAXIMUM OF EXPLANATORY VARIABLES. EXPLAIN REASON FOR NATURAL LOG

lsales. <- c(length(ceo_salary_df$lsales), mean(ceo_salary_df$lsales), sd(ceo_salary_df$lsales), min(ceo_salary_df$lsales), max(ceo_salary_df$lsales))
lmktval. <- c(length(ceo_salary_df$lmktval), mean(ceo_salary_df$lmktval), sd(ceo_salary_df$lmktval), min(ceo_salary_df$lmktval), max(ceo_salary_df$lmktval))
ceoten. <- c(length(ceo_salary_df$ceoten), mean(ceo_salary_df$ceoten), sd(ceo_salary_df$ceoten), min(ceo_salary_df$ceoten), max(ceo_salary_df$ceoten))
ceotensq. <- c(length(ceo_salary_df$ceotensq), mean(ceo_salary_df$ceotensq), sd(ceo_salary_df$ceotensq), min(ceo_salary_df$ceotensq), max(ceo_salary_df$ceotensq))

salary_summary_df <- data.frame(lsales., lmktval., ceoten., ceotensq.)
row.names(salary_summary_df) <- c("COUNT", "MEAN", "STANDARD DEVIATION", "MINIMUM", "MAXIMUM")
salary_summary_df

#3c. RESTIMATION OF MODEL WITH WHITE STANDARD ERRORS AND THE T-STATS

coeftest(OLS_estimate_salary, vcov = sandwich)
coeftest(OLS_estimate_salary, vcov = vcovHC(OLS_estimate_salary, "HC0"))
coeftest(OLS_estimate_salary, vcov = vcovHC(OLS_estimate_salary)) 
summary(OLS_estimate_salary)

#3d. OBTAIN RESIDUALS AND STANDARDIZE

residuals <- residuals(OLS_estimate_salary)
residuals
standardised_residuals <- rstandard(OLS_estimate_salary)
standardised_residuals

#3e. RE-ESTIMATE MODEL BY ADDING DUMMY "COLLEGE" AND INTERACTION TERM "COLLEGE*LSALES". INTERPRETE COEFFICIENT OF INTERACTION TERM

OLS_reestimate_salary <- lm(lsalary ~ lsales + lmktval + ceoten + ceotensq + college + college*sales, data = ceo_salary_df)
summary(OLS_reestimate_salary)

#3f. RE-ESTIMATE MODEL WITH VARIABLE "LSALES_ADJUSTED" INSTEAD OF "LSALES". COMPARE COEFFICIENT ON "LSALES_ADJUSTED" WITH "LSALES"

ceo_salary_df$lsales_adjusted <- ceo_salary_df$lsales * 0.9
OLS_reestimate_salary2 <- lm(lsalary ~ lsales_adjusted + lmktval + ceoten + ceotensq + college + college*sales, data = ceo_salary_df)
summary(OLS_reestimate_salary2)

#The original coefficient of lsales  is less than the coefficient of lsales_adjusted.

#3g. RE-ESTIMATE MODEL BY DROPPING ALL OBSERVATIONS WHERE STANDARDISED RESIDUAL IN 3d. IS GREATER THAN 2


## QUESTION 4 ##

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

bptest(local_returns_estimation_pooled, data = local_returns_panel, studentize = F)

#Answer: The p-value is less than 5%. Hence, at 5% significance level we reject the null hypothesis and conclude that there is heteroskedasticity in the residuals.

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

local_returns_estimation_firstdiff <- plm(ret ~ city_returns + indret, data=local_returns_df, model = "fd")
  coef(summary(local_returns_estimation_firstdiff))
  
#4k. COMPARE THE FE, FD AND POOLED OLS WITH TIME AND FIXED EFFECTS WITH DOUBLE CLUSTERING

#4l. RE-ESTIMATE MODEL WITH NEW INTERACTION TERM. CONFIRM IF COEFFICIENT γ1 IS STATISTICALLY DIFFERENT FROM ZERO AND INTERPRET IT


