# A) ----------------------------------------------------------------------
wine = read.table("wine.txt", header = TRUE)

plot(wine$sugar, wine$alcohol, col = as.numeric(as.factor(wine$type)), pch = 16)
legend("topleft", legend = unique(as.factor(wine$type)), pch = 16, col = 1:3)

fit = lm(alcohol ~ type + type:sugar, data = wine)

summary(fit)
coeff = coef(fit)
sigma = summary(fit)$sigma
sigma
coeff

plot(wine$sugar, wine$alcohol, col = as.numeric(as.factor(wine$type)), pch = 16)
legend("topleft", legend = unique(as.factor(wine$type)), pch = 16, col = 1:3)
abline(a = coeff[1], b = coeff[4], col = 1)
abline(a = coeff[1] + coeff[2], b = coeff[5], col = 2)
abline(a = coeff[1] + coeff[3], b = coeff[6], col = 3)


par(mfrow = c(2,2))
plot(fit)
par(mfrow=c(1,1))

# B) ----------------------------------------------------------------------


alpha = 0.01
C = matrix(0, nrow = 2, ncol = 6)
C
C[1, 2] = C[2, 3] = 1
C
delta.0 = c(0, 0)

linearHypothesis(fit, C, delta.0)
# we can remove them!

C = matrix(0, nrow = 3, ncol = 6)
delta.0 = c(0, 0, 0)
C[1, 4] = C[2, 5] = C[3, 6] = 1
C

linearHypothesis(fit, C, delta.0)
# We cannot remove them!


# Reduced model 

fit.reduced = lm(alcohol ~ type:sugar, data = wine)
summary(fit.reduced)

coeff = coef(fit.reduced)
plot(wine$sugar, wine$alcohol, col = as.numeric(as.factor(wine$type)), pch = 16)
legend("topleft", legend = unique(as.factor(wine$type)), pch = 16, col = 1:3)
coeff
abline(a = coeff[1], b = coeff[2], col = 1)
abline(a = coeff[1], b = coeff[3], col = 2)
abline(a = coeff[1], b = coeff[4], col = 3)


# C) ----------------------------------------------------------------------
alpha = 0.01
Z0.new = data.frame(sugar = 20, type = "Red")
predict(fit, Z0.new, interval='prediction', level=1-0.05)
