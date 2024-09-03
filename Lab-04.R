#  (1) Use the dataset homocyst in the package senstrat. The outcome is homocysteine, 
# the homocysteine level, and the treatment is z, where z = 1
#for a daily smoker and z = 0 for a never smoker.
#Covariates are female, age3, ed3, bmi3, pov2 with detailed explanations in the package, and
#st is a stratum indicator, defined by all the combinations of the discrete covariates.

library(senstrat)
data<-as.data.frame(senstrat::homocyst)




# (a) How many strata have only treated or control units?
library(dplyr)
no_tc<-data %>% group_by(stf) %>% summarise(no_t=sum(z==1),no_c=sum(z==0)) %>% 
  filter(no_t==0 | no_c==0)
strata<-c(no_tc$stf)



# (b) What is the proportion of the units in these strata?
sum(no_tc$no_t+no_tc$no_c)/nrow(data)




# (c) Drop these strata and perform a stratified analysis of the observational study. Report the
# point estimator, variance estimator, and 95% confidence interval for the average causal effect.
df<-data %>% filter(!(stf %in% strata))
stf.levels<-levels(droplevels(df$stf))
tau<-0
N<-nrow(df)
for (i in stf.levels) {
  y<-df[df$stf==i,"homocysteine"]
  a<-df[df$stf==i,"z"]
  pi<-length(y)/N
  n1<-sum(a==1)
  n0<-sum(a==0)
  tau<-tau+pi*(mean(y[a==1])-mean(y[a==0]))
}
tau

#Cannot compute variance estimate V_hat because there are strata of size 1
#One option is to compute bootstrap standard errors which we will dicuss later

#There are 19 strata with only one unit in treatment group
#Let's remove these as well and compute V_hat

one_tc<-df %>% group_by(stf) %>% summarise(no_t=sum(z==1),no_c=sum(z==0)) %>% 
  filter(no_t==1 | no_c==1)
strata2<-c(one_tc$stf)
df2<-df %>% filter(!(stf %in% strata2))
stf.levels2<-levels(droplevels(df2$stf))
tau2<-0
V<-0
N<-nrow(df2)
for (i in stf.levels2) {
  y<-df2[df2$stf==i,"homocysteine"]
  a<-df2[df2$stf==i,"z"]
  pi<-length(y)/N
  n1<-sum(a==1)
  n0<-sum(a==0)
  tau2<-tau2+pi*(mean(y[a==1])-mean(y[a==0]))
  V<-V+pi^2*(var(y[a==1])/n1+var(y[a==0])/n0)
}
tau2; V
#95% confidence interval
c(tau2-1.96*sqrt(V),tau2+1.96*sqrt(V))




#(d) Run the OLS of the outcome on the treatment indicator and covariates without interactions.
# Report the coefficient of the treatment and the standard error.
#OLS with all strata
fit1<-lm(homocysteine~1+z+female+age3+ed3+bmi3+pov2,data=data)
summary(fit1)
fit1$coefficients["z"] #1.353452
library(car)
sqrt(hccm(fit1,type = "hc2")[2,2]) #0.370545

#OLS with no treatment or control groups removed
fit2<-lm(homocysteine~1+z+female+age3+ed3+bmi3+pov2,data=df)
summary(fit2)
fit2$coefficients["z"] #1.377624
sqrt(hccm(fit2,type = "hc2")[2,2]) #0.3750971



#(e) If you do not drop the strata with only treated or control units, what will happen?
#Suppose we didn't remove strata with only treated or control units
stf.levels3<-levels(data$stf)
tau3<-0
N3<-nrow(data)
for (i in stf.levels3) {
  y<-data[data$stf==i,"homocysteine"]
  a<-data[data$stf==i,"z"]
  pi<-length(y)/N3
  n1<-sum(a==1)
  n0<-sum(a==0)
  tau3<-tau3+pi*(mean(y[a==1])-mean(y[a==0]))
}
tau3








