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

