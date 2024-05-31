dance = read.table("danceability.txt", header = TRUE)
plot(dance[1:4], col = as.numeric(as.factor(dance$genre)))
attach(dance)

fit.lm = lm(danceability ~ loudness + energy + tempo)
summary(fit.lm)

names(summary(fit.lm))
summary(fit.lm)$sigma

betas = coef(fit.lm)


# B
plot(fit.lm)


# C

delta.0 = c(0, 0)
C = matrix(0, nrow = 2, ncol = length(fit.lm$coefficients))
C[1, 2] = C[2, 3] = 1
C
library(car)


linearHypothesis(fit.lm, C, delta.0)
# we cannot take anything out of the model. 
par(mfrow = c(2, 2))
plot(fit.lm)



vif(fit.lm)
R2 = 1- 1/#vif(fit.lm)

fit_reduced = lm(danceability ~ tempo)
par(mfrow = c(1,1 ))
plot(danceability, tempo)

summary(fit_reduced)



#

regfit.full = regsubsets(danceability ~ loudness + energy + tempo, data = dance , method=c("backward"))
summary(regfit.full)
plot(regfit.full)
plot(fit2)
fit2 = lm(danceability ~ loudness + tempo)
fit3 = lm(danceability ~ energy + tempo)

summary(fit2)
summary(fit3)
anova(fit3, fit2)


library(leaps)
