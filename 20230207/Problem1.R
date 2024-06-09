# The file decathlon.txt reports the results of the 23 best ranked athletes at Men’s Decathlon during the 2016
# Summer Olympics in Rio de Janeiro. Note that variables corresponding to time measurements (100m, 400m,
#                                                                                            110m_hurdles, 1500m) are expressed in seconds while variables corresponding to length measurements (long_jump,
#                                                                                                                                                                                                shot_put, high_jump, discus_throw, pole_vault, javelin_throw) are expressed in meters.
# a) Considering a Principal Component Analysis of the dataset, decide whether it is appropriate to use the original
# variables or the standardized ones. Propose in addition a simple transformation to have all variables denoting
# better performance for higher values. Perform the Principal Component Analysis and report the loadings of
# the first principal component.

data = read.table("decathlon.txt", header = T)
head(data)
plot(data[,3:12], col = as.numeric(factor(data$country)), pch = 16)
# i will transform the time in velocity: in this way the higher the value the better the performance
data$X100m = 100/data$X100m
data$X400m = 400/data$X400m
data$X110m_hurdles = 110/data$X110m_hurdles
data$X1500m = 1500/data$X1500m

# we will now plot the data
boxplot(scale(data[,3:12], center = TRUE, scale = FALSE))
# we see that we need to standardize the data, since shot put, discus throw and javelin throw 
# have a much higher variance than the other variables. 

fit.pca = princomp(scale(data[,3:12]))
summary(fit.pca)
# we can see that we need at least 3 PCs to explain 70% of the variance.4 if we want to reach the 80% threshold 

library(MiniR)
pc.interpretation(fit.pca, data = data[,3:12])

#interpretation of the first 3 principal components
# PC1: is a weighted average of "running" and jumping variables: all velocity variables, high jump and long jump. 
# PC2: is a weighted average with negative weights of results in throwing events: shot put, discus throw, javelin throw. 
# Probably by looking at this first two PCs we are able to distinguish between sprinters and throwers.
# PC3: its seems a contrast between pole vault and highjump vs X100m (postive value)

# point c)
biplot(fit.pca, choices = 1:2)
biplot(fit.pca, choices = 2:3)
# from this plot we are able to distinguish between sprinters(high values of PC1 scores) and throwers (negative value of PC2).
# we repeat the biplot with the plot of second and third PC


# point d)
# see point a  for screeplot. 
# I will keep just the first three pcs, we could keep two, but we may lose informaion 
# on pale vault and high jump.

# point e)
# Project these results on the reduced space identified at point (d). Interpret.
# 100 metres
# Long jump
# Shot put
# High jump
# 400 metres
# 110 metres hurdles
# Discus throw
# Pole vault
# Javelin throw
# 1500 metres
# 10.81s
# 7.46m
# 14.56m
# 2.02m
# 47.90s
# 14.44s
# 43.04m
# 4.90m
# 57.24m
# 4’41.63s

Z0.new = data.frame(X100m = 100/10.81, long_jump = 7.46, shot_put = 14.56, high_jump = 2.02, X400m = 400/47.90, X110m_hurdles = 110/14.44, discus_throw = 43.04, pole_vault = 4.90, javelin_throw = 57.24, X1500m = 1500/281.63)
Z0.new = (Z0.new - colMeans(data[,3:12]))/sqrt(diag(var(data[,3:12])))
Z0.new = as.matrix(Z0.new)
Z0.new = Z0.new %*% fit.pca$loadings[,1:3]
biplot(fit.pca)
points(Z0.new[,1], Z0.new[,2], col = "blue", pch = 2, cex = 2)
# It seem a pretty balanced athlete, with a good performance in all the events but the throwing ones.