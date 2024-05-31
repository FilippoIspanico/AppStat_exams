# A)
monsters = read.table("dnd_monsters.txt", header = TRUE)
plot(monsters[,1:8], col = as.numeric(as.factor(monsters$size)))
# from this plot we can see:
# 1) hit points observations seem to have pretty high variance wrt other variables
# 2) we could be able to train a classifier on this data, however the best directions 
#     on which we could discriminate the observations are not clear. We may classify on
#     based on pca's results.

# To further inspect the variances we perform boxplot

par(mfrow = c(1,2))
boxplot(monsters[,1:8], col = "gold", main = "original_data")
# we can see that the box associated to hit-points variable is larger than the other ones. 
# --> we need to scale the variables otherwise first PCA will represent hit-points variables! 
boxplot(scale(monsters[,1:8], scale = FALSE), col = "gold", main = "centered_data")
par(mfrow = c(1,1))
# moreover we have outliers!


# B)
library(MiniR)
fit = princomp(scale(monsters[,1:8]))
summary(fit)


pc.interpretation(fit, data = monsters[,1:8])

# Interpretations of the PCs
# PC1: weighted average of all variables but "dexterity". High values of PC1 scores means
#     that the monster is very strong (in general sense, both physically and "psicologically" strong, if the variables have all same importance). 
#     It contains no info about monster's dexterity.
# PC2: Contains info about monster's dexterity (high dex --> high values of PC2 scores), along with a
#     positive weighted average of intelligence, wisdom & charisma (note how all are non phisical attributes). 
#     Physical attributes are considered as well in PC2 scores, but with negative values (even if almost 0).
#     This means that PC2 is a contrast between physical and non physical attributes! 
# 
# PC3: the opposite of PC2: PC3 evaluates positively physical attributes and negatively intellectual attributes. 
#     
#
# This means that based on PCA scores we could discriminate between strong and weak monsters (PC1 score) and 
#     physical vs intellectual monster (ex: we could discriminate a troll from a mage based on their PC2-PC3 scores)


# How may components to keep? 
# We may decide to keep just the first two PCA since:
# 1) Cumulative Proportion(PC1, PC2)=0.7837273 almost 80%
# 2) We can make 2D plots

# Or we could keep also PC3 hoping for a better discrimination of the results. 

# C)
plot(fit$scores, col = as.numeric(as.factor(monsters$size)), main = "PC 1-2 scores", pch = 16)
legend("topleft", legend = unique(c(as.factor(monsters$size))), col = as.numeric(as.factor(monsters$size)), cex = 0.5, pch = 16)

# We can see that tiny and small monsters have little pc1 scores & high pc2 scores, so I would guess
# are small but intellectual monsters. Gargantuan seem strong but they may not have high values of dexterity (or are dumb, or both)
# Large monsters seem generally strong; large and medium monsters seem more balanced, but vith higher variances then other groups. 


# D)
idxs = which(monsters$size == "Tiny" | monsters$size == "Huge")
data = monsters[idxs, ]
par(mfrow = c(1,1))
plot(data, col = as.numeric(as.factor(data$size)), main = "Tiny - Huge scatterplot")
plot(fit$scores[idxs, 1:2], col = as.numeric(as.factor(data$size)), pch = 16, main = "Tiny - Huge PC12 scores ")
legend("topleft", legend = unique(c(as.factor(data$size))), col = as.numeric(as.factor(data$size)), cex = 0.9, pch = 16)

scores = data.frame(fit$scores[idxs,1:2])
scores$size = data$size
library(e1071)
svmfit = svm(as.factor(scores$size) ~ . , data=scores, kernel = "linear", cost = 1, scale = FALSE)
summary(svmfit)
No_support_vectors = 5

plot(svmfit, scores, col =c("salmon", "light blue"), pch=19, asp=1)


# New prediction: 
z0.new = c(14, 50, 19, 10, 16, 8, 12, 13)



means = colMeans(data[,1:2])
var = sapply(data[,1:2], sd)
z0.new = (z0.new - means)/var
plot(fit$scores[idxs, 1:2], col = as.numeric(as.factor(data$size)), pch = 16, main = "Tiny - Huge PC12 scores ")
points(z0.new[1], z0.new[2], pch = 3)

z0.scores = (t(fit$loadings)%*%z0.new) 


pred = predict(svmfit, as.matrix(cbind(z0.scores[1], z0.scores[2])))
pred # Tiny
