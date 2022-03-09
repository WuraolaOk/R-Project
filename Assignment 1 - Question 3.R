## AUTHOR: 42235_WURAOLA_OKUWOBI ##

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
summaryR(OLS_estimate_salary)

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

#3g. RE-ESTIMATE MODEL BY DROPPING ALL OBSERVATIONS WHERE STANDARDISED RESIDUAL IN 3d. IS GREATER THAN 2


