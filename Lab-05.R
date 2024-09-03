#  (1) ‘nhanes bmi.csv’ contains data from NHANES 2007–2008 survey to study whether participation
#  in school meal programs led to an increase in BMI for school children. The dataset has the following

# covariates:
#  age              Age
#  ChildSex         Sex (1: male, 0: female)
#  black Race       (1: black, 0: otherwise)
#  mexam Race       (1: Hispanic: 0 otherwise)
#  pir200 plus      Family above 200% of the federal poverty level
#  WIC              Participation in the special supplemental nutrition program
#  Food Stamp       Participation in food stamp program
#  fsdchbi          Childhood food security
#  AnyIns           Any insurance
#  RefSex           Sex of the adult respondent (1: male, 0: female)
#  RefAge           Age of the adult respondent




# (a) Identify the outcome and the treatment variables.
nhanes<-read.csv("nhanes_bmi.csv")[,-1]
a<-nhanes$School_meal
y<-nhanes$BMI
x<-as.matrix(nhanes[,-c(1,2)])
x<-scale(x)



# (b) Estimate the propensity scores using all the covariates.
pscore <- glm(a ~ x, family = binomial)$fitted.values



# (c) Discretize the propensity score by taking K = 5.
K<-5
q<- quantile(pscore,(1:(K-1))/K)
psq<-cut(pscore,breaks = c(0,q,1),labels = 1:K)




# (d) Based on propensity score stratification with K = 5, calculate the point estimate for average
# causal effect (τ ) and its standard error.

# (e) Construct an R function to estimate τ and its standard error based 
# on propensity score stratification for any number of K strata. 
#[Hint: Inputs for the function will be the outcom  variable, treatment variable 
#and the vector of stratification.]

#(d) (e)
strat.est<-function(a,y,ps,K){
  q<- quantile(ps,(1:(K-1))/K)
  psq<-cut(ps,breaks = c(0,q,1),labels = 1:K)
  l<-unique(psq)
  piK<-rep (0, K)
  tauK<-rep (0, K)
  vK<-rep (0, K)
  for (k in 1:K) {
    lk = l[k]
    ak = a[psq == lk]
    yk = y[psq == lk]
    piK [k] = length (ak)/ length (a)
    tauK [k] = mean (yk[ak ==1]) - mean (yk[ak ==0])
    vK [k] = var(yk[ak ==1]) / sum(ak) +
      var (yk[ak ==0]) /sum (1 -ak)
  }
  return (c(sum(piK* tauK), sqrt(sum( piK^2*vK ))))
}

strat.est(a,y,pscore,5)




# (f) Repeat part (d) for K = 10, 20, 50 and 80. Comment on your results.
ns=c(5,10,20,50,80)
results<-data.frame(K=numeric(0),est=numeric(0),se=numeric(0))
j<-1
for (i in ns) {
  est<-strat.est(a,y,pscore,i)[1]
  se<-strat.est(a,y,pscore,i)[2]
  results[j,]<-c(i,est,se)
  j<-j+1
}
results                                                                                                                                      
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       
# (g) Compute the difference in means estimator and outcome regression 
#    estimators with and without interaction terms together with their standard errors.

#Difference in means estimator
difmean<-mean(y[a==1])-mean(y[a==0]) #0.5339044
c(difmean,sqrt(var(y[a==1])/sum(a==1)+var(y[a==0])/sum(a==0))) #0.2253199



#We can obtain the same results by fitting a regression with no covariates
fit1<-lm(y~a)
c(fit1$coefficients["a"],sqrt(hccm(fit1,type = "hc2")[2,2]))

#Outcome regression without interactions
fit2<-lm(y~a+x)
c(fit2$coefficients["a"],sqrt(hccm(fit2,type = "hc2")[2,2]))
#0.06124785,0.22599593

#Outcome regression with interactions
fit3<-lm(y~a+x+a*x)
c(fit3$coefficients["a"],sqrt(hccm(fit3,type = "hc2")[2,2]))







# (h) Compute the Horvitz–Thompson (HT) estimator and the H´ajek estimator with truncations
#    at (0,1), (0.05,0.95) and (0.1,0.9). Find their bootstrap standard errors
tau_ht<-mean(a*y/pscore-(1-a)*y/(1-pscore))
tau_hk<-sum(a*y/pscore)/sum(a/pscore)-sum((1-a)*y/(1-pscore))/sum((1-a)/(1-pscore))

trnc<-c(0,1)
ipw<-function(a,y,x,trnc){
  pscore <- glm(a ~ x, family = binomial)$fitted.values
  pscore_trnc<-pmax( trnc[1], pmin(trnc[2], pscore ))
  tau_ht<- mean(a*y/pscore_trnc-(1-a)*y/(1-pscore_trnc))
  tau_hk<-sum(a*y/pscore_trnc)/sum(a/pscore_trnc)-sum((1-a)*y/(1-pscore_trnc))/sum((1-a)/(1-pscore_trnc))
  return(c(tau_ht=tau_ht,tau_hk=tau_hk))
}
ipw(a,y,x,trnc)

#Bootstrap standard errors
n.boot<-500
sd.boot<-function(a,y,x,trnc){
  boot.est<-data.frame()
  n<-length(a)
  for (i in 1:n.boot) {
    id <- sample (1:n,n,replace = TRUE )
    boot.est<-rbind(boot.est,c(ipw(a[id],y[id],x[id,],trnc)))
  }
  colnames(boot.est)<-c("tau_ht","tau_hk")
  res = c("sd.tau_ht"=sd(boot.est[,1]), "sd.tau_hk"=sd(boot.est[,2]))
  return(res)
}

sd.boot(a,y,x,trnc)

trunc.list = list ( trunc0 = c(0 ,1) ,
                    trunc.05 = c(0.05 , 0.95) ,
                    trunc.1 = c(0.1 , 0.9))
lapply(trunc.list, function(t){
  est=ipw(a,y,x,t)
  sd=sd.boot(a,y,x,t)
  cbind(est,sd)
})











