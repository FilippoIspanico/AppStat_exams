library(MiniR)
food = read.table("food.txt", header = TRUE)
plot(food[1:4], col = as.numeric(as.factor(food$result)), pch = 16)
#legend("topleft", legend = c("high, low"), col = 1:2, pch = 16)
# by looking at the plot we can see that not always seems to exist a clear 
# direction in which classification is easier. The relation between brightness 
# and the other factors may be able to discriminate between high and low result. 
# Also other relations may discrimate data but maybe not good enough. 

# We could try to do pca on this dataset and see if the scores allows us to 
# reach a better discrimination
# First lets do a boxplot to see if variances are different 


boxplot(food[, 1:4])
boxplot(scale(food[, 1:4], scale = FALSE), col = "gold")

# There are some outliers on odor variable 

fit.pca = princomp(scale(food[,1:4]))
pc.interpretation(fit.pca, data = food[, 1:4])
biplot(fit.pca)
plot(fit.pca$scores[,1:2], col = as.numeric(as.factor(food$result)), pch = 16)
legend("topleft", legend = c("high", "low"), col = 1:2, pch = 16)

# we kept just the first two pca for the sake of simplicty. (There is now elbow 
# point and the varibaility explained by hte first pcs is not too little (70%))

# By looking at the plot see that PC1 offers a simple rule for discrimination:
# PC1<0 high quality
# PC1>0 low quality. 
# Interpretation of PC1: it is a contrast between taste-odor (positive weight)
# and smooth-bright (negative weight): when we have high level of smooth-bright
# and low levels of taste-odor we have high quality food. (A little odd but possible...
# maybe high values of odor(taste) is a smell(bad taste) (?))
# I will compute aper of this technique of classification. I will not run LDA
# since I want to obtain a baseline classifier based on just intuition. 
# 

# computing number of misclassification 

intuitive_data = data.frame(fit.pca$scores[,1:2])
intuitive_data$result = food$result
table(intuitive_data$result)
n12 = length(which(intuitive_data$Comp.1<0 & intuitive_data$result == "low"))
n21 = length(which(intuitive_data$Comp.1>0 & intuitive_data$result == "high"))
n11 = 30 - n12
n22 = 30 - n21
confusion_matrix = matrix(c(n11, n12, n21, n22), nrow = 2, ncol = 2, byrow = TRUE)
confusion_matrix

prior = c(1-0.1/100, 0.1/100)
tot = 500*prior[1] + 100000*prior[2]
prior_transformed = c(prior[1]*500/tot,  prior[2]*100000/tot)
APER = 3/30*prior[1] + 5/30*prior[2]
APER

# 10 % of aper. 

#let's now try LDA(QDA) on the same data
var(intuitive_data[which(intuitive_data$result == "low"), 1:2])
var(intuitive_data[which(intuitive_data$result == "high"), 1:2])

# variances are similar we can use lda

lda.fit = lda(result ~ ., data = intuitive_data, prior = prior_transformed)
library(MASS)
library(ggplot2)
library(gridExtra)
plot_decision_boundaries(result ~ ., data = intuitive_data, lda.fit)
predictions = predict(lda.fit, data.frame(intuitive_data[,1:2]))$class
predictions
table(TRUTH = intuitive_data$result, PREDICT = predictions)
APER = 3/30*prior[1] + 9/30*prior[2]
APER



# Considering the prior the best classifier based on PC directions 
# always predicts high (on our dataset)


# Lets see on original variables. 

Slow = var(food[which(food$result == "low"), 1:4])
Shigh = var(food[which(food$result == "high"), 1:4])
par(mfrow = c(1, 2))
image(Slow)
image(Shigh)
# the variances looks similar. 
library(MVN)
mvn.low = mvn(food[which(food$result == "low"), 1:4]) 
mvn.low$multivariateNormality

mvn.high = mvn(food[which(food$result == "high"), 1:4]) 
mvn.high$multivariateNormality
# we have one component in group low that does seem gaussian. However we go on since 
# LDA is robust wrt normality HP

original.lda = lda(result ~ ., data = food, prior = prior_transformed)
plot_decision_boundaries(result ~ ., data = food, original.lda)

predictions = predict(original.lda, data.frame(food[,1:4]))$class
predictions
table(TRUTH = food$result, PREDICT = predictions)
APER = 2/30*prior[1] + 16/30*prior[2]
APER
original.cv = lda(result ~ ., data = food, prior = prior_transformed, CV = TRUE)
table(TRUTH = food$result, PREDICT = original.cv$class)
