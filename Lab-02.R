################################################################################
################################################################################
##################        LAB WORKSHEET -02   ##################################
################################################################################
################################################################################

# 
#  (01) Consider a completely randomized experiment (CRE) with a binary 
#       treatment A where n = 5 and n1 = 3.


#   (a) How many different treatment assignment mechanisms are possible?
choose(5,3)

#   (b) Write an R function to generate all possible treatment assignment 
#      mechanisms for any given n and n1. (Hint: Use the functions choose and combn)
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


#   (c) Find all the possible treatment assignment mechanisms for a CRE with
#     n = 5 and n1 = 3.
trtassignment(5,3)
















# (02) Angrist et al. (2009) conducted an experiment to evaluate different 
# strategies to improve academic performance among college freshmen in a
# Canadian university. The data is available in the file
# “star.dta”. The outcome is the GPA at the end of the first year. 
# We will focus on two covariates:  gender (encoded by female) and 
# baseline GPA(encoded by gpa0).
  
# (a) Extract a subset of the dataset which only contains records for the 
#     control group (encoded by the variable control) and the treatment
#     group which was offered academic support services  and financial incentives 
#     for good grades (encoded by the variable sfsp).

library("foreign")
angrist   = read.dta("star.dta")
table(angrist$control,angrist$sfsp)
data<-subset(angrist,control == 1|sfsp == 1)
str(data)



# (b) Impute the missing outcomes with the observed average
y<-data$GPA_year1
#Find the mean of y without missing values
meany<-mean(y,na.rm = T)
#Impute the mean of y to missing values
y<-ifelse(is.na(y),meany,y)
mean(y)






# (c) Check the balance of the two covariates. 
#(Hint: For each covariate, conduct a  suitable twosample test to compare
# the covariate means or proportions in the control and treatment groups)
a<-data$sfsp
#Assessing balance involves assessing whether the distributions of covariates are similar
#between the treated and control groups.

#Check the balance w.r.t. gender
#Since the variable 'female' is binary we use prop.test to test for proportion difference
addmargins(table(data$female,a),c(1,2))
prop.test(x=c(574,82),n=c(1006,150),correct = F)
#There is no significant difference in proportions

#Check the balance w.r.t. baseline GPA
gpa0a0<-data$gpa0[a==0]
gpa0a1<-data$gpa0[a==1]
var.test(gpa0a1,gpa0a0)
t.test(gpa0a1,gpa0a0,var.equal = T)







# (d) Estimate the ATE without adjusting for covariates. 
#  (Hint: The unadjusted estimator is numerically identical to the 
# difference-in-means of the outcome)
#Unadjusted estimator
#Difference in means estimator
mean(y[a==1])-mean(y[a==0])
#OLS fit estimator
fit<-lm(y~a)
summary(fit)
ate_unadj<-coef(fit)[2] #0.0518



# (e) Estimate the ATE adjusted for the covariates.

#Covariate adjustment using regression
x<-data[,c("female","gpa0")]
x<-scale(x)
fitadj<-lm(y~a*x)
summary(fitadj)
ate_adj<-coef(fitadj)[2] #0.0682










