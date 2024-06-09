# The files discomaniac.txt and lipsticks.txt contain the prices (in e) and the conditions of sales (a real number
#                                                                                                    from 0 to 12) of the same 20 LPs sold from two stores.
# a) Perform a statistical test of level 95% to verify if the mean prices and conditions of the two stores differ.
# b) Which are the assumptions of the previous test? Are they met?
#   c) Provide the plot of the confidence region at level 95% for the differences of the mean of prices and the conditions
# between the two stores.
# d) Provide four Bonferroni simultaneous confidence intervals of global level 95% for the means and the variances
# of the differences in prices and conditions. Interpret the results of the test at point (a) through the intervals.

disco = read.table("discomaniac.txt", header = T)
lipstick = read.table("lipsticks.txt", header = T)

diff = disco
diff$price = disco$price - lipstick$price
diff$media.condition = disco$media.condition - lipstick$media.condition
plot(diff$price, diff$condition, main = "Differences in prices and conditions", xlab = "Prices", ylab = "Conditions")
diff = diff[, c("price", "media.condition")]
# a)
x.mean = colMeans(diff)
S = cov(diff)
n = 20
p = 2
alpha = 0.05
cfr.fisher = (n-1)*p/(n-p)*qf(1-alpha, p, n-p)
T2 = n*(x.mean %*% solve(S) %*% x.mean)
T2>cfr.fisher
p_value = 1 - pf(T2*(n-p)/p/(n-1), p, n-p)
p_value


# b)
library(MVN)
mvn(diff)$multivariateNormality
# The assumption of multivariate normality is met. 

# c)
library(car)
plot(diff)
ellipse(center = x.mean, shape = S/n, radius = sqrt(cfr.fisher), col = "red")
abline(h = 0, lty = 3, col=  "gray")
abline(v = 0, lty = 3, col=  "gray")

# d) 

# Bonferroni simultaneous confidence intervals for the means

alpha = 0.05
k = 4

BCI_means = cbind(
  inf = x.mean - qt(1-alpha/2/k, n-1)*sqrt(diag(S)/n),
  center = x.mean,
  sup = x.mean + qt(1-alpha/2/k, n-1)*sqrt(diag(S))/n)
)
BCI_means

BCI_simultaneous = cbind(
  inf = x.mean - sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(diag(S)/n),
  center = x.mean,
  sup = x.mean + sqrt((n-1)*p/(n-p)*cfr.fisher)*sqrt(diag(S)/n)
)
BCI_simultaneous

BCI_var = cbind(
  inf = (n - 1) * diag(S) / qchisq(1 - alpha/(2*k), n - 1),
  center = diag(S) ,
  sup = (n - 1) * diag(S) / qchisq(alpha/(2*k), n - 1)
)  
BCI_var
