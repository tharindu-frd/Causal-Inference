## 1. Card (1993) used the National Longitudinal Survey of Young Men to estimate the causal effect of
#   education on earnings. The dataset ‘card1995.csv’ contains 3010 men with ages between 14 and
#   24 in the year 1966, and Card (1993) leveraged the geographic variation in college proximity as an
#   IV for education. Here, the IV ‘nearc4’ is the indicator of growing up near a four-year college,
#   the treatment ‘educ’ measures the years of education, and the outcome ‘lwage’ is the log wage
#    in the year 1976, ranging from 4.6 to 7.8.
#    Among the available additional covariates consider only the covariates exper, expersq, black,
#    south, smsa, reg661, reg662, reg663, reg664, reg665, reg666, reg667, reg668 and smsa66 for
#   the analysis.




# (a) Fit a 2-stage least square (2SLS) regression and estimate the causal effect of ‘educ’ on
#‘lwage’.

# (b) Obtain the corrected residuals using the coefficients from stage 2.

# (c) Using the corrected residuals, find the standard error of the 2SLS estimator.


# (d) Find the 95% confidence interval for the 2SLS estimator.

library ("car")
## Card Data
card.data = read.csv("card1995.csv")
Y = card.data [, "lwage"]
A = card.data [, "educ"]
Z = card.data [, "nearc4"]
X = card.data [, c("exper", "expersq", "black", "south",
                   "smsa", "reg661", "reg662", "reg663",
                   "reg664", "reg665", "reg666",
                   "reg667", "reg668", "smsa66")]
X = as.matrix (X)

#Fit 2SLS models 
A_hat = lm(A ~ Z + X)$fitted.values 
Y_hat = lm(Y ~ A_hat + X) 

#2SLS estimate of causal effect
twoSLS_est <- coef(Y_hat)[2] 

#Obtain the corrected residuals
res.correct = Y - cbind(1, A, X)%*% coef(Y_hat)
Y_hat$residuals = as.vector (res.correct )

#Compute the standard error
SE<- sqrt(hccm(Y_hat, type = "hc0")[2,2])

#Estimate and 95% confidence interval
results<-c(twoSLS_est , twoSLS_est - 1.96 * SE , twoSLS_est + 1.96 * SE )
names(results)<-c("2SLS_est","95% CI Lower","95% CI Upper")
round(results,4)
