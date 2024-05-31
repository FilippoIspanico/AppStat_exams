# The dataset asthma.txt contains data regarding asthma prevalence across the 110 Italian provinces, identified by
# province id and grouped by their regional code region id.
# For each province, the dataset includes the following variables: average age, air pollution index, average
# number of sunny days per month, average tobacco consumption, average household income, average education
# level, and asthma prevalence rate, standardized per 10,000 residents. Additionally, the dataset includes a categorical
# variable urban indicating whether the province is categorized as urban or not.
# All numerical variables have been scaled to have a mean 0 and a standard deviation 1.
# a) Implement the following linear regression model M0:
#   asthma =
#      and k the grouping variable induced by urban.
#   Report the estimates of the parameters of the model fitted with Ordinary Least Squares and verify the model
#   assumptions.
#   b) Can we affirm at 90% confidence level that the age has a positive e↵ect on asthma prevalence? Additionally,
# provide an 95% confidence interval for the mean di↵erence between the asthma prevalence in an urban province
# and in a non-urban one
# c) After having reduced the model M0, if appropriate, update it by introducing a compound-Symmetry Correlation
# Structure using the region as a grouping factor (model M1). Provide a 99% confidence interval for the
# parameters ⇢ and of the compound symmetry.
# d) From the possibly reduced version of the model M0, update it now by introducing a random intercept related
# to the regional grouping factor (model M2). What do you observe? Provide the estimate of the standard
# deviation of the random intercept along with the one of the error term.


asma = read.table("asthma.txt", header = TRUE)
attach(asma)
dummy_urban = as.numeric(as.factor(urban))-1 #1 = yes, 0 = No
M0 = lm(asthma ~ age + dummy_urban + pollution + sunny + income + education )
summary(M0)
plot(M0)

#b) No we cannot  affirm at 90% confidence level that the age has a positive effect on asthma prevalence
# since the null HO of the T test on the variable age cannot be rejected at level alpha = 0.1. 
# This means that we cannot reject the hypotesis that the age has no effect on astma, hence  we can't say it
# neither have a positive(or negative) effect. 

# However we can futher investigate the regressor that have positive/negetive effect by developing an improved 
# model and inspect its charasterictiscs. 

asthma_urban = asthma[which(urban == "Yes")]
asthma_no = asthma[which(urban == "No")]
diff = asthma_urban - asthma_no 


means = tapply(asthma, urban, mean)
mu = as.numeric( means[2] - means[1] ) 
table(urban)
n = 55 
alpha = 0.05
bonf.correction = qt(1-alpha/2, n -1 )

BCI = cbind(
  inf = mu - bonf.correction * sqrt(var(diff)/n),
  center = mu,
  sup = mu + bonf.correction *  sqrt(var(diff)/n)
)

BCI

# in 

