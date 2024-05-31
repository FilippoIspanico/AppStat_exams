nutrients = read.table("nutrients.txt", header = TRUE)
plot(nutrients)
# variances of varibles seem different, lets see

boxplot(scale(nutrients, scale = FALSE))
# we need to standardize the variables otherwise we will got the first pca representing
# just energy..

fit = princomp(scale(nutrients))

loadings = fit$loadings
loadings

# B)

library(MiniR)
pc.interpretation(fit, data  = nutrients)

# C) 
par(mfrow = c(1, 1))
biplot(fit)
# PC1: negative weight to proteic and fiber cereals, positive
# 
# 
#
# D)
screeplot(fit)

