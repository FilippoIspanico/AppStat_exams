students = read.table("students.txt", header = TRUE)
plot(students[2:10], col = as.numeric(as.factor(students$gender)))
m0 = lm(watchtv ~ ., data = students)     
summary(m0)
plot(m0)

# before proceding I will try to improve this model.
# We have a leverage point, and homoschedasticty don't seem verified
# the problem with this observation seem the distance (25000 km). 
# We can try to remove this point and see how the model changes; however, if the probelm persists, we 
# we can also consider to use a log scale for the distance variable
students[12, ]
students.restriced = students[-12, ]
m0.1 = lm(watchtv ~ ., data = students.restriced)     
summary(m0.1)
plot(m0.1)
plot(students$watchtv, log(students$distance+1))
students.modified = students
students.modified$distance = log(students$distance+1)
m0.2 = lm(watchtv ~ ., data = students.modified)     
summary(m0.2)
library(car)
vif(m0)
# nothing every modification I do still result in lower R2

library(glmnet) 

x = model.matrix(m0)[,-1]
y = students$watchtv
fit.lasso = glmnet(x, y, lambda = 0.3)
coef.lasso <- predict(fit.lasso, s=0.3, type = "coefficients")
# the significant coefficients are: 
# siblings      0.4526208912
# computertime  0.1422680784


mse = function(x, y){
  return (mean((x-y)^2))
}

set.seed(20231108)
lambda.grid <- seq(from = 0.01,to = 10,length.out=100)
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid)
cv.lasso

bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso # this is the lambda that minimizes the MSE
plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)
cv.lasso$glmnet.fit
coef.lasso <- predict(cv.lasso, s=bestlam.lasso, type = "coeff")
coef.lasso
abline(h = 26.25)

