diet =read.table("diet.txt", header = TRUE)
attach(diet)
labels  = as.factor(interaction(vegetarian, vitamin))
plot(diet[1:2], col = as.numeric(labels))
legend("topleft", legend = unique(labels), col=1:4, pch = 16, cex = 0.5)


# the problem is a two way manova: given values for the two factors, 
# vegetarian(T/F), vitamin assumptions(T/F), see how the means of the
# health indicators changes. 

par(mfrow = c(3, 2))
boxplot(diet$pressure~vegetarian, main = "vegetarian vs pressure")
boxplot(diet$cholesterol~vegetarian, main = "vegetarian vs cholesterol")
boxplot(diet$pressure~vitamin, main = "vitamin vs pressure")
boxplot(diet$cholesterol~vitamin, main = "vitamin vs cholesterol")
boxplot(diet$pressure~interaction(vegetarian, vitamin), main = "interaction vs pressure")
boxplot(diet$cholesterol~interaction(vegetarian, vitamin), main = "interaction vs cholesterol")
par(mfrow = c(1, 1))

#from the boxlpot we can see that:
#1. vegetarian may have an effect on cholesterol but not on pressure
#2. vitamin intake may have an effect on pressure but not on cholesterol
#3. interaction term seems to be negligible (it seems an additive effect)

# let's perform a two way manvoa

fit.complete = manova(as.matrix(diet[,1:2]) ~ vegetarian + vitamin + vegetarian:vitamin)
summary(fit.complete)

summary.aov(fit.complete)

# as suspected let's remove the interaction term. 
fit.additive = manova(as.matrix(diet[,1:2]) ~ vegetarian + vitamin)
summary(fit.additive)
summary.aov(fit.additive)
# this is the model i would suggest at point C
# B. 
# verifing assumptions. 
for (i in 1:4){
  
  
  
}
