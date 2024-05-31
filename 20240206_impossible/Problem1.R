library(devtools)
install_github("FilippoIspanico/MiniR")
library(MiniR)

diet = read.table("diet.txt", header = TRUE)
attach(diet)
library(MASS)
manova.fit = manova(as.matrix(diet[,1:2]) ~ vegetarian + vitamin + vegetarian:vitamin, data = diet)
summary(manova.fit)

additive.fit = manova(as.matrix(diet[,1:2]) ~ vegetarian + vitamin, data = diet)
summary(additive.fit)

unique(interaction(vegetarian, vitamin))
labels = c("nothing", "vegetarian", "vitamins", "both")
interaction.new = factor(interaction(vegetarian, vitamin), levels = unique(interaction(vegetarian, vitamin)), labels = labels)


x11()
par(mfrow = c(3, 2))
boxplot(pressure~vegetarian, main = "Vegeterian vs pressure")
boxplot(cholesterol~vegetarian, main = "Vegeterian vs cholesterol")
boxplot(pressure~vitamin, main = "Vitamins vs pressure")
boxplot(cholesterol~vitamin, main = "Vitamins vs cholesterol")
boxplot(pressure ~ vegetarian:vitamin, main = "Interaction between vegetarian and vitamins on pressure")
boxplot(cholesterol ~ vegetarian:vitamin, main = "Interaction between vegetarian and vitamins on cholesterol")
par(mfrow = c(1, 1))

summary.aov(additive.fit)

# Provide Bonferroni intervals (global level 95%) for the e↵ects of the vegetarian diet and the vitamin intake on
# the health indicators. How would you describe the e↵ect of the vegetarian diet and vitamin intake on the health
#indicators?
# Bonferroni Cofindece Intervals for the effects of vegetarian and vitamins on health indicators
alpha = 0.95
k = 4
general_mean = colMeans(diet[,1:2])
n = 50
b = g = 2
# Vegetarian effect 

means_vegetarian_pressure = tapply(diet$pressure, diet$vegetarian, mean)
means_vegetarian_cholesterol = tapply(diet$cholesterol, diet$vegetarian, mean)
effect_vegetarian = cbind(means_vegetarian_pressure - general_mean[1], means_vegetarian_cholesterol - general_mean[2])
# add colnames to effect_vegetarian
colnames(effect_vegetarian) = c("pressure", "cholesterol")
effect_vegetarian

# Vitamin effect
means_vitamin_pressure = tapply(diet$pressure, diet$vitamin, mean)
means_vitamin_cholesterol = tapply(diet$cholesterol, diet$vitamin, mean)
effect_vitamin = cbind(means_vitamin_pressure - general_mean[1], means_vitamin_cholesterol - general_mean[2])
# add colnames to effect_vitamin
colnames(effect_vitamin) = c("pressure", "cholesterol")
effect_vitamin


# Bonferroni intervals
Spool = t(additive.fit$residuals)%*%additive.fit$residuals/additive.fit$df.residual
bonf.correction = qt(1-alpha/2/k, additive.fit$df.residual)


BCI_effect_vegetarian_FALSE_pressure = cbind(inf = effect_vegetarian[1, 1] - bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)), 
                       sup = effect_vegetarian[1, 1] + bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)))
BCI_effect_vegetarian_FALSE_cholesterol = cbind(inf = effect_vegetarian[1, 2] - bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)),
                                                sup = effect_vegetarian[1, 2] + bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)))
BCI_effect_vegetarian_TRUE_cholesterol = cbind(inf = effect_vegetarian[2, 2] - bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)),
                                               sup = effect_vegetarian[2, 2] + bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)))
BCI_effect_vegetarian_TRUE_pressure = cbind(inf = effect_vegetarian[2, 1] - bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)),
                                            sup = effect_vegetarian[2, 1] + bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)))

BCI_effect_vitamin_FALSE_pressure = cbind(inf = effect_vitamin[1, 1] - bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)),
                                          sup = effect_vitamin[1, 1] + bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)))
BCI_effect_vitamin_FALSE_cholesterol = cbind(inf = effect_vitamin[1, 2] - bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)),
                                             sup = effect_vitamin[1, 2] + bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)))
BCI_effect_vitamin_TRUE_cholesterol = cbind(inf = effect_vitamin[2, 2] - bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)),
                                            sup = effect_vitamin[2, 2] + bonf.correction*sqrt(Spool[2, 2]*(1/n/b + 1/n/b/g)))
BCI_effect_vitamin_TRUE_pressure = cbind(inf = effect_vitamin[2, 1] - bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)),                                        
                                         sup = effect_vitamin[2, 1] + bonf.correction*sqrt(Spool[1, 1]*(1/n/b + 1/n/b/g)))                                                                                        
# lets concatenate the results and put the correct row and columns names
BCI_effect_vegetarian = rbind(BCI_effect_vegetarian_FALSE_pressure, BCI_effect_vegetarian_TRUE_pressure, BCI_effect_vegetarian_FALSE_cholesterol, BCI_effect_vegetarian_TRUE_cholesterol)
rownames(BCI_effect_vegetarian) = c("FALSE_pressure", "TRUE_pressure", "FALSE_cholesterol", "TRUE_cholesterol")
colnames(BCI_effect_vegetarian) = c("inf", "sup")

BCI_effect_vitamin = rbind(BCI_effect_vitamin_FALSE_pressure, BCI_effect_vitamin_TRUE_pressure, BCI_effect_vitamin_FALSE_cholesterol, BCI_effect_vitamin_TRUE_cholesterol)
rownames(BCI_effect_vitamin) = c("FALSE_pressure", "TRUE_pressure", "FALSE_cholesterol", "TRUE_cholesterol")
colnames(BCI_effect_vitamin) = c("inf", "sup")