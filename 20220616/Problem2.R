music = read.table("musicCountry.txt", header = TRUE)
head(music)
plot(music[,1:2], col = as.numeric(factor(music$release.country)), pch = 16)
legend("topleft", legend = levels(factor(music$release.country)), pch = 16, col = 1:2, cex = 0.5)
# we can see that the varainces of the two groups are different
p = c(0.1, 0.9)
ger = music[music$release.country == "Germany",1:2]
us = music[music$release.country == "US",1:2]

mu.ger = colMeans(ger)
mu.us = colMeans(us)
S.ger = cov(ger)
S.us = cov(us)
# we will plot the covariances of the two groups
par(mfrow = c(1, 2))
image(S.ger, main = "Germany")
image(S.us, main = "US")
par(mfrow = c(1, 1))
# the variance of the term price seems differnet in the two groups, also the lenght
# lets verify normality
library(MVN)
mvn(ger)$multivariateNormality
mvn(us)$multivariateNormality
# both groups are normal, we can go on!

# I would go on with qda. 
library(MASS)
fit.qda = qda(release.country ~ ., data = music, prior = p)
library(MiniR)
library(MASS)
library(ggplot2)
library(gridExtra)
plot_decision_boundaries(release.country ~ ., data = music, fit.qda)

# b) 
predictions = predict(fit.qda, music[,1:2])$class
table(TRUTH = music$release.country, PREDICT = predictions)
AER = 9/(27+9) + 2/152
AER
# c)


# d)
Z0.new = data.frame(price = 50, average.length = 3.5)
predict(fit.qda, Z0.new)
# the model predicts that the new song is from the US
# with probability 0.92...

# e)
music$release.country = as.factor(music$release.country)

library(e1071)
svmfit = svm(release.country ~ price + average.length, data=music, kernel = "linear", cost = 1, scale = FALSE)
summary(svmfit)

plot(svmfit, data = music)
tune.out <- tune(svm, release.country ~ price + average.length, data=music, kernel = "linear",
                 ranges = list(cost=c(0.001 , 0.01, 1, 10,100) ))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod, music)
