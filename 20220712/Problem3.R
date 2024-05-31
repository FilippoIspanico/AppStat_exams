rent = read.table("rent.txt", header = TRUE)
plot(rent)
# by observing the plot we see that the price is highly correlated (positively)
# with footage, as one would expect. All other variables seem quite uncorrelated
# apart from the variables transport & supermarket, that have high (positive) correlation.
# maybe keeping both variables in the model is not the best idea since we can expext 
# the variance of the related regressors are high. 

fit = lm(price ~ .*two.bathrooms, data = rent)
summary(fit)
coeff = coef(fit)
sigma = summary(fit)$sigma

plot(fit)
# from the plots we can see: 
# Residuals vs Fitted plot: no non-linear pattern in the residuals: we can assume 
#     residuals don't contain information. 

# QQ plot of the residuals. We can see that the residual have a normal distribuition. 
#     Theoretical assumptions on normality are met. (residuals distributed as normal)

# Scale - Location plot. Residuals are spread equally along the ranges of predictors.
            # We can assume homoscedasticity assumption is met. 

# Residuals vs Leverage. We cannor see any point with a problematic cook's distance. 
#         We can assume no leverage points are present in the model. 

library(car)
vif(fit)
# We have lot of regressors with high values of VIF! 
# We need to improve the model! Hence we perfome lasso regression. 
# C) 
library(glmnet)
y = rent$price
x = model.matrix(price ~ .*two.bathrooms, data = rent)[,-1]

fit.lasso = glmnet(x, y, lambda = 45)
summary(fit.lasso)


coef.lasso <- predict(fit.lasso, s=45, type = 'coefficients')
coef.lasso 
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]
plot(fit.lasso)
lambda.grid <- seq(100,0,length=100)
cv.lasso <- cv.glmnet(x,y,lambda=lambda.grid) # default: 10-fold CV
fit.lasso <- glmnet(x,y, lambda = lambda.grid) # default: alpha=1 -> lasso 
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

plot(cv.lasso)
abline(v=log(bestlam.lasso), lty=1)


coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 
selected =  coef.lasso[which(abs(coef.lasso)> 0.01)]

z0.new = c(30, 5, 5, 300, 100, 500, 100, "FALSE")
