dat = read.table("satisfaction.txt", header = TRUE)
plot(dat)
# from the plot we can see that:
# score, purch_amount and membership duration are highly correlated. We will look at vif values. 
# all other variables seems uncorrelated

m0 = lm(score ~ purch_amount + memb_duration + age, data = dat)
summary(m0)
# We see that the the p-value of the t-test has really low values only for purch_amount
# memb_durarion has low p-valye only beacause highly correlated with purch_amount. We should keepm just one of them 
# age, as in the plot, doest seem to be able to explain satisfaction score. 

coeff = coef(m0)
sigma = summary(m0)$sigma

# modelling score using a lm requirse independence, homoschedasticty of the residuals
# (and normality for confidence intervals)
# let's see them: 
plot(m0)
# hypotesis seem verified

library(car)
C = matrix(0, nrow = 2, ncol=4)
C[1, 3] = C[2, 4] = 1
linearHypothesis(m0, C)
# we reject at level 5% H0: we cannot assume are both 0!

vif(m0)

library(MiniR)
new.data = data.frame(cbind(dat$purch_amount , dat$memb_duration , dat$age))
colnames(new.data) = c("purch_amount", "memb_duration", "age")
boxplot(new.data)
pc.fit = princomp(scale(new.data))
pc.interpretation(pc.fit, data = new.data)
pc.scores = data.frame(pc.fit$scores[,1:2]) # we keep just the first two pcs
pc.scores$score = dat$score
plot(pc.scores)
m1 = lm(score ~ . , data = pc.scores)
summary(m1)
plot(m1)

beta.pca = coef(m1)[-1]
intercept <- coef(m1)[1]
loadings <- pc.fit$loadings[, 1:2]
beta.original <- as.vector(loadings %*% beta.pca)
beta.original


pca.regression = function(target, original_data, pca.qty, scale = TRUE, center = TRUE){
  
  r = ncol(original_data) # number of initial regressors (not including the intercept)
  sigmas = diag(cov(original_data))
  mu = colMeans(original_data)
  
  if (!scale){
    sigmas = rep(1, r)
  }
  
  if(!center){
    mu = rep(0, r)
  }
  
  pca.fit = princomp(scale(original_data, center = center, scale = scale))
  scores = pca.fit$scores[,1:pca.qty]
  loadings = pca.fit$loadings[,1:pca.qty]
  
  m0 = lm(target ~ scores)
  
  beta.pca = coef(m0)[-1]
  betas.tilde = loadings%*%beta.pca
  intercept = coef(m0)[1] - t(mu/sigmas)%*%betas.tilde
  betas.original = rep(0, r+1)
  betas.original[1] = intercept
  betas.original[1:r+1] = betas.tilde/sigmas
  
  return(betas.original)
}

pca.regression(dat$score, original_data, 2)


