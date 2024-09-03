###########################  Q1 ##########################################
##########################################################################
##########################################################################
#The cars dataset in R gives the speed of cars and the distances taken to stop, which were recorded
#in 1920s.
#(a) Visualize the relationship between the two variables and find the correlation coefficient.
#(b) Fit a meaningful linear regression model and assess the adequacy of the fitted model.
#(c) Write down the fitted model.
#(d) What is the relationship between the R-squared value and the correlation coefficient between
#X and Y ? Does this relationship apply to all regression models?
##########################################################################
##########################################################################
##########################################################################
##########################################################################



# Load the cars dataset
data(cars)
head(data(cars))

# Reset graphics parameters to avoid "figure margins too large" error
par(mfrow=c(1,1), mar=c(5, 5, 2, 2))  # Adjust the margins as needed

# (a) Visualize the relationship and find the correlation coefficient
plot(cars$speed, cars$dist, 
     main="Scatter plot of Speed vs Stopping Distance",
     xlab="Speed (mph)", ylab="Stopping Distance (ft)",
     pch=19, col="blue")
abline(lm(dist ~ speed, data=cars), col="red")
correlation_coefficient <- cor(cars$speed, cars$dist)
print(paste("Correlation Coefficient:", correlation_coefficient))






model <- lm(dist ~ speed, data=cars)
summary(model)


coefficients <- summary(model)$coefficients
intercept <- coefficients[1,1]
slope <- coefficients[2,1]
fitted_model <- paste("dist =", round(intercept, 2), "+", round(slope, 2), "* speed")
print(fitted_model)



# (d) Relationship between R-squared and correlation coefficient
r_squared <- summary(model)$r.squared
relationship_statement <- paste("R-squared value is", round(r_squared, 2), "and the square of the correlation coefficient is", round(correlation_coefficient^2, 2))
print(relationship_statement)



######## outliers 
SR <- rstandard(model)  # Standardized residuals
plot(model$fitted.values,SR) # fitted values vs standardized residuals
abline(h=0,col="red")
which(SR>3 | SR<(-3))  ## outliers 


#####  Normality of residuals 
qqnorm(SR,ylab="Standardized Residuals")
qqline(SR,col="red")
shapiro.test(SR)

hist(SR,col=2) ## Check the normality


##### Goodness of fit
R2 = summary(model)$r.squared




















###########################  Q2 ##########################################
##########################################################################
##########################################################################
# The Western Collaborative Group Study (WCGS), a prospective cohort study, 
# recruited middleaged men (ages 39 to 59) who were employees of 10 
# California companies and collected data on
# 3154 individuals during the years 1960-1961. These subjects were 
# primarily selected to study the
# relationship between behavior pattern and the risk of coronary hearth 
# disease (CHD). A number
# of other risk factors were also measured. The dataset is available in the ‘epitools’ package in R.
# (a) Construct a contingency table for behavior pattern type and 
#  occurrence.
# (b) Estimate the ‘risk ratio’ and the ‘odds ratio’ considering behavior
# pattern type B as the
# reference level.
# (c) Conduct a suitable statistical test to check the independence 
# of the two variables.
##########################################################################
##########################################################################
##########################################################################
##########################################################################

library(epitools)

# Load the dataset
data(wcgs)
str(wcgs)

# Create a contingency table for behavior pattern type and CHD occurrence
contingency_table <- table(wcgs$dibpat0, wcgs$chd69)
print(contingency_table)
###### access values in contigency tatble and get rowsums and colsums
contingency_table[1,1]
contingency_table[1,2]
rowSums(contingency_table)
colSums(contingency_table)





#####################################################################
###  suppose that we have multiple rows and we want to keep the row2 
###   as it is and merge all the other rows to a one row 

##### Change the table 
# Specify the row to keep intact
#row_to_keep <- 2
#new_row <- colSums(contingency_table[-row_to_keep, ])
#contingency_table <- contingency_table[row_to_keep, , drop = FALSE]
#merged_contingency_table <- rbind(contingency_table, new_row)
#print("Merged Contingency Table:")
#print(merged_contingency_table)
########################################################################


riskratio(contingency_table)$measure
oddsratio(contingency_table)$measure





# Perform a statistical test for independence
# Assuming chi-square test for independence
expected(contingency_table)
chi_square_test <- chisq.test(contingency_table)

# Display the results of the chi-square test
print("Chi-Square Test for Independence:")
print(chi_square_test)













                       


###########################  Q3 ##########################################
##########################################################################
##########################################################################                                                                                   20on%20Delta%20Variant,a%20much%20higher%20risk%20population.). It has 286,166 rows
# 3 variables:
#• age group - Age of the person. Levels: under 50, 50 +.
#• vaccine status - Vaccination status of the person. 
#Levels: vaccinated, unvaccinated
#• outcome - Did the person die from the Delta variant? Levels: death and survived.
#(a) Load the dataset into R.
#(b) Construct a 2 × 2 table to summarize the variables vaccine status and outcome.
#(c) Calculate the ‘risk ratio’ to quantify the association between vaccine status and outcome.
#(d) Re-calculate the risk ratios in subgroups defined by the age group and comment on your
#findings.
##########################################################################
##########################################################################    
covid <- read.csv("C:/Users/Tharindu/OneDrive/Desktop/Causal Inference/covid.csv")
covid$outcome <- as.factor(covid$outcome)
covid$outcome <- relevel(covid$outcome,ref='survived')
tab2 <- table(covid$vaccine_status,covid$outcome)
tab2
riskratio(tab2)$measure




# (b) Construct a 2 × 2 table to summarize the variables vaccine status and outcome
table_vaccine_outcome <- table(covid$vaccine_status, covid$outcome)
print("2x2 Contingency Table for Vaccine Status and Outcome:")
print(table_vaccine_outcome)


# (c) Calculate the risk ratio to quantify the association between vaccine status and outcome
riskratio(table_vaccine_outcome)$measure



























#################################  Activity ##########################
##########################################################################
##########################################################################
##########################################################################
# Consider a sample size of n = 500. Generate Y (0) randomly from a
# standard normal distribution. [Use set.seed(2024)]
# Set average causal effect close to −0.5 by generating τi as
# τi = −0.5 + Yi(0). Then generate Yi(1) = Yi(0) + τi
.
# A perfect doctor scenario
# If the doctor already knows that the individual causal effect is
# positive, he will definitely assign the treatment to the patient.
# Allocate the treatment assignment accordingly and construct the
# observed outcome vector Y . Find the difference in means of the
# observed outcome for the treatment and control groups.

# A clueless doctor scenario
# A clueless doctor does not have any information about the individual
#  effects and assigns the treatment randomly. Allocate the
# treatment assignment accordingly and construct the observed
# outcome vector Y . Find the difference in means of the observed
# outcome for the treatment and control groups.
#########################################################################
#########################################################################


# Set the seed for reproducibility
set.seed(2024)

# Step 1: Generate Yi(0) from a standard normal distribution
n <- 500
Yi_0 <- rnorm(n)

# Step 2: Generate τi to set average causal effect close to -0.5
tau_i <- -0.5 + Yi_0

# Step 3: Generate Yi(1)
Yi_1 <- Yi_0 + tau_i

# Step 4: Perfect doctor scenario
A_perfect <- ifelse(tau_i > 0, 1, 0)

# Observed outcomes Y for perfect doctor scenario
Y_perfect <- ifelse(A_perfect == 1, Yi_1, Yi_0)

# Calculate difference in means of observed outcomes for treatment and control groups (perfect doctor)
mean_treated_perfect <- mean(Y_perfect[A_perfect == 1])
mean_control_perfect <- mean(Y_perfect[A_perfect == 0])
diff_means_perfect <- mean_treated_perfect - mean_control_perfect

# Step 5: Clueless doctor scenario
A_clueless <- sample(c(0, 1), n, replace = TRUE)

# Observed outcomes Y for clueless doctor scenario
Y_clueless <- ifelse(A_clueless == 1, Yi_1, Yi_0)

# Calculate difference in means of observed outcomes for treatment and control groups (clueless doctor)
mean_treated_clueless <- mean(Y_clueless[A_clueless == 1])
mean_control_clueless <- mean(Y_clueless[A_clueless == 0])
diff_means_clueless <- mean_treated_clueless - mean_control_clueless

# Print results
cat("Difference in means (Perfect doctor scenario):", diff_means_perfect, "\n")
cat("Difference in means (Clueless doctor scenario):", diff_means_clueless, "\n")


#######  If we have any influence over the treatment without assigning the treatments 
#  randomly  we wont get good  a good estimate for the causal effect . 








