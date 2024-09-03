# Table 1 presents a subsample of observations from a completely randomized experiment to evaluate
# the effect of an educational television program on reading skills. The unit is a class of students.
# The outcome of interest is the average reading score in the class. Half of the classes were shown
# the program of interest, and the other half did not have access to the program. 

                                                                                 
                                                                                 
                                                                                 
#  Table 1: A subsample from Children’s television workshop experiment data
                                                                                 
                                                                                 
#      Unit   Treatment    Observed Outcome      Potential Outcomes
#                            (Y_i)                 Y_i(0)   Y_i(1)                                                                 
#       1       0             55.0
#       2       1             70.0
#       3       0             72.0
#       4       1             66.0
#       5       0             72.7
#       6       1             78.9


T<-c(0,1,0,1,0,1)
Y<-c(55,70,72,66,72.7,78.9)

# (a) Complete the two columns of potential outcomes.


# (b) State the null hypothesis to conduct the Fisher’s randomization test.



# (c) Fill in the missing values for potential outcomes under the null hypothesis.


# (d) Consider the difference-in-means as the test statistic and compute it for the observed data.
diffmean<-(sum(T*Y)-sum((1-T)*Y))/3 #5.067


# (e) How many different treatment assignments are possible?
M<-choose(6,3) #20


#  (f) Derive the randomization distribution of the test statistic, compute the exact p-value and
#     state your conclusions.


trtassignment<- function(n,n1) {
  M<-choose(n,n1)
  treat.index<-combn(n,n1)
  A<-matrix(0,n,M)
  for (i in 1:M) {
    treat<-treat.index[,i]
    A[treat,i]<-1
  }
  A
}





A<-data.frame(t(trtassignment(6,3)))
for (i in 1:M) {
  t<-A[i,1:6]
  A[i,"est"]<-(sum(t*Y)-sum((1-t)*Y))/3
}
p_FRT<-sum(A$est>=diffmean)/20 #0.3
#Since 0.3>0.05 we do not reject H0 and conclude that 
#there is no evidence to claim that the television program improves reading skills.

#Your conclusion should change depending on whether you choose a two-sided p-value or not
p_FRT_2<-sum(abs(A$est)>=diffmean)/20
#Since 0.6>0.05 we do not reject H0 and conclude that 
#there is no evidence to claim that the television program has any effect on reading skills.















# (2) LaLonde (1986) was interested in the causal effect of a job training program on earnings. Their
# experimental data are available in the Matching package in R. “treat” is the treatment indicating
#whether a unit was randomly assigned to the job training program or not and “re78” is the outcome
#representing a unit’s real earnings in 1978.
library (Matching)
data(lalonde)
a = lalonde$treat
y = lalonde$re78
n1<-sum(a==1)
n0<-sum(a==0)



#(a) Estimate the average treatment effect assuming a completely randomized design.
#Difference in means estimator
est.diff<-mean(y[a==1])-mean(y[a==0]) #1794.343



##(b) Consider the estimator you selected in part (a) as the test statistic. By randomly permuting
# the treatment vector, obtain the Monte Carlo approximation of the randomization distribution
#of this test statistic. (Consider 104 permutations)

set.seed(1234)
MC<- 10^4
Diffhat = rep (0, MC)
for(mc in 1: MC){
  aperm = sample (a)
  Diffhat[mc]<-mean(y[aperm==1])-mean(y[aperm==0])
}




#(c) Compute the exact p-value using Monte Carlo approximation.
exact.pv.diff<-mean(Diffhat>=est.diff) #0.0022



#(d) Repeat parts (b) and (c) using t-statistic with equal variance assumption as the test statistic.
t.stat<-t.test(y[a==1],y[a==0],var.equal = TRUE)$statistic
TStathat = rep (0, MC)
for(mc in 1: MC){
  aperm = sample (a)
  TStathat[mc]<-t.test(y[aperm==1],y[aperm==0],var.equal = TRUE)$statistic
}

exact.pv.tstat<-mean(TStathat>=t.stat) #0.0021




#e) Without using Monte Carlo method, compute the asymptotic p-value for part (d), assuming
#normal distributions for the two potential outcome distributions and equal variances.
asymp.pv<-t.test(y[a==1],y[a==0],var.equal = TRUE)$p.value #0.0048





#(f) What are the possible reasons for any differences that you have observed between the p-values
#in parts (c), (d) and (e)?
due to asymptotic approximations.
#the default choice for t.test is a two-sided test.
#for a fair comparison we should multiple p-values in (c) and (d) by 2.





#  (g) Assess the covariate balance with respect to all pre-treatment variables.
attach(lalonde)
#age
var.test(age[a==1],age[a==0])
t.test(age[a==1],age[a==0],var.equal = TRUE)
#educ
var.test(educ[a==1],educ[a==0])
t.test(educ[a==1],educ[a==0],var.equal = TRUE)
#black
prop.test(table(a,black))
#hisp
prop.test(table(a,hisp))
#married
prop.test(table(a,married))
#nodegr
prop.test(table(a,nodegr))
#re74
var.test(re74[a==1],re74[a==0])
t.test(re74[a==1],re74[a==0],var.equal = FALSE)
#re75
var.test(re75[a==1],re75[a==0])
t.test(re75[a==1],re75[a==0],var.equal = TRUE)
#u74
prop.test(table(a,u74))
#u75
prop.test(table(a,u75))




#(h) Estimate a linear regression model with all pre—treatment variables as controls 
#(no interactions), and report the estimate of the average treatment effect and its standard error.
fit<-lm(re78~.,data=lalonde)
summary(fit)
fit$coefficients["treat"]                                                                              
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 
                                                                                 