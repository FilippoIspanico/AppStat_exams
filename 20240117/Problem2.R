library(devtools)
install_github("FilippoIspanico/MiniR")
library(MiniR)


aster = read.table("asteroids.txt", header = TRUE)
View(aster)
plot(aster)


aster.numeric = aster[, 1:8]
boxplot(aster.numeric)
# as we can see from the plot the variables SurfArea and ConvVolume have very high variance, 
# wrt to others variables. We therefore proceed by standardizing the variables. 
boxplot(scale(aster.numeric), scale = FALSE)

pca = princomp(scale(aster.numeric))
summary(pca)

pc.interpretation(pca, aster.numeric, number_pcs = 2)

biplot(pca)
plot(pca$scores, col = as.numeric(as.factor(aster$Type)))
# We can see that the first two principal components allows us to 
# discriminates well between the groups! This is is good if we would like to train
# a classifier on this data. 
