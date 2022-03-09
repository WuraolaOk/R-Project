## AUTHOR: 42235_WURAOLA_OKUWOBI ##

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

OLS_estimatei <- lm(nettfa ~ inc + age, data = pension_df, subset = marr == 0)

summary(OLS_estimatei)

#2d. MEANING OF INTERCEPT FROM THE REGRESSION: nettfa = β0 + β1inc + β2age + u #



#2e. p-VALUE FOR THE TEST H0 :β2 = 1 AGAINST H1 :β2 < 1 AT 1% SIGNIFICANCE LEVEL #

OLS_estimate$p

#2f. p-VALUE FOR THE TEST H0 :β2 = 1 AGAINST H1 :β2 != 1 AT 1% SIGNIFICANCE LEVEL #



#2g. OLS ESTIMATION OF THE MODEL: nettfa = β0 + β1inc + u #

OLS_estimate1 <- lm(nettfa ~ inc, data = pension_df)

summary(OLS_estimate1)

#2h. OLS ESTIMATION OF THE MODEL: nettfa = β0 + β1inc + β2age + β3inc^2 + β4age^2 + β5fsize +  u#

OLS_estimate2 <- lm(nettfa ~ inc + age + I(inc^2) + I(age^2) + fsize, data = pension_df)

summary(OLS_estimate2)

#2i. F-TEST OF THE RESTRICTION: H0 : β3 = β4 = 0 IN MODE L(OLS_estimate2) #


#2j. RE-ESTIMATION OF THE MODEL (OLS_estimate2) AFTER RESCALING inc BY DIVIDING IT BY 10#


#2k. TESTING THE MODEL (OLS_estimate) FOR HETEROSKEDASTICITY USING THE BREUSCH-PAGAN TEST #


#2l. ESTIMATING THE MODEL (OLS_estimate) WITH HETEROSKEDASTICITY-ROBUST (e.g. WHITE) STANDARD ERRORS #


#2m. RE-ESTIMATING THE MODEL (OLS_estimate) AFTER STANDARDISING ALL VARIABLES IN THE MODEL. INTERPRETE COEFFICIENTS ON inc AND age #


