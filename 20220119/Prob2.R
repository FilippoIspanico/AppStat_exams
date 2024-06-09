fish = read.table("fish.txt", header = TRUE)
plot(fish[,1:2], col = as.numeric(factor(fish$abundance)))
legend("bottomleft", col = 1:2, pch = 16, legend = c("H", "L"))
# We can see that probably an LDA separation is enough (linear separation between calsses)
# It seems that both group are normal. We will check this assumption. 


# To use LDA we need to see if the variances in the two group is the same, 
# and group-normality 

H = fish[fish$abundance=="H",1:2]
L = fish[fish$abundance=="L",1:2]
library(MVN)
mvn(H)$multivariateNormality
mvn(L)$multivariateNormality
#group L is not normal, however LDA/QDA is very roubst to normal assumption (fisher's argument)
# if the variances are similar we can go on. 
muH = colMeans(H)
muL = colMeans(L)
cov(H)
cov(L)


par(mfrow = c(1,2))
image(cov(H), main = "cov(H)")
image(cov(L), main = "cov(L)")
par(mfrow = c(1,1))
# the covariance between x-y is very different, topright square in the plot..
# we can try to use qda. 
# estimates of parameters

# prior are estimated from data
p = table(fish$abundance)/250

library(MASS)
fit = qda(abundance ~ ., data = fish)
library(MiniR)
library(MASS)
library(ggplot2)
library(gridExtra)
plot_decision_boundaries(abundance ~ ., data = fish, fit)

fit.cv = qda(abundance ~ ., data = fish, CV = TRUE)

predictions = fit.cv$class
table(TRUTH = fish$abundance, PREDICT = predictions)
AER = 13/120 * p[1] + 12/130*p[2]
AER




# point C) ----------------------------------------------------------------
library(class)
set.seed(19)

min_aer = 1
best_model = NULL
optimal_k = NULL

for (k in 10:30){
  fit.knn = knn.cv(fish[,1:2], fish$abundance, k = 10)
  confusion = table(TRUTH = fish$abundance, PREDICT = fit.knn)
  AER = confusion[1, 2]*p[1]/120 + confusion[2,1]*p[2]/130
  if (AER<min_aer){
    best_model = fit.knn
    min_aer = AER
    optimal_k = k
  }
    
}

best_model
min_aer
optimal_k

# i choose QDA


