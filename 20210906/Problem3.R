boats = read.table("boats.txt", header = TRUE)
plot(boats[,1:6])
fit = lm(price ~ .  , data = boats)
summary(fit)
coef(fit)
par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))

