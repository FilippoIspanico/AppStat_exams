# The dataset asthma.txt contains data regarding asthma prevalence across the 110 Italian provinces, identified by
# province id and grouped by their regional code region id.
# For each province, the dataset includes the following variables: average age, air pollution index, average
# number of sunny days per month, average tobacco consumption, average household income, average education
# level, and asthma prevalence rate, standardized per 10,000 residents. Additionally, the dataset includes a categorical
# variable urban indicating whether the province is categorized as urban or not.
# All numerical variables have been scaled to have a mean 0 and a standard deviation 1.

asthma = read.table("asthma.txt", header = TRUE)
plot(asthma[-7], col = as.numeric(as.factor(asthma$urban)))
# by looking at the plot we see that the variable asthma seems correlated with 
# tobacco consuption, and household income. Also pollution and age may be correlated.
asthma$urban = factor(asthma$urban)
m0 = lm(asthma ~ -1 + urban + age + pollution + sunny + income + education, data = asthma)
summary(m0)
# We can se that the linear model m0 contains significant variables(F-test p-value of 0)
# the adjusted R-squared is 0.8726, which tells us that the model is able to explain a good amount of the variance.
# the coefficients that are significantly different from zero are the ubran itercepts, pollution and income, and education. 
# the coefficients for pollution and income are negative, 
# which means that the higher the income the lower the asthma rate. 
# Also pollution and asthma are negatively correlated, which is 
# unintuitive.
# Urban categorical variable seem to affect significantly the asthma rate,
# in particular the urban provinces have a higher asthma rate. (their intercept is higher)
m0.1 = lm(asthma ~ urban + age + pollution + sunny + income + education, data = asthma)
summary(m0.1)
plot(m0.1)
sigma = summary(m0.1)$sigma
library(car)
vif(m0.1)
# we don't have problem as far as leverage points and normality is concerned, however, 
# residual seems to be heteroscedastic, which is a problem for the model. 
# in particular higher values of fitted values seem to be underestimated by the model.
# and have higher variance. 

# b)
# Can we affirm at 90% confidence level that the age has a positive e↵ect on asthma prevalence?

# no because in the summary of the model m0.1 the p-value for the t-test of the coefficient of age is 0.16
# which means that we cannot reject the null hypothesis that the coefficient is zero. Hence it cannot be said that
# age has a positive effect on asthma prevalence.

# provide an 95% confidence interval for the mean difference between the asthma prevalence in an urban province
# and in a non-urban one

confint(m0.1, "age", level= 0.90)
# if we decided in any case to keep age then its confidence intervals contains
# negative values, as expected. Hence we cannot say that age has a positive effect on asthma prevalence.
# 
# reducing the model: 
library(leaps)
regfit.fit = regsubsets(asthma ~ urban + age + pollution + sunny + income + education, data = asthma,
                         method=c("exhaustive")
                         )
plot(regfit.fit, scale = "Cp")
summary(regfit.full)
names(summary(regfit.fit))
summary(regfit.fit)$cp
summary(regfit.fit)$bic
summary(regfit.fit)$adjr2
names(summary(regfit.fit))
formula = asthma ~ urban + pollution + income + education
library(nlme) 

m1 = gls(formula, data = asthma, 
         correlation = corCompSymm(form = ~ 1|region_id))
summary(m1)

m2 = lme(formula,data = asthma ,random = ~1|region_id)  
summary(m2)
