# The dataset gemstones.txt contains information on 300 different gemstones, each characterized by eight quantitative
# features, including area, perimeter, major axis length, minor axis length, eccentricity, convex area, equivalent
# diameter (diameter of the circle of the same area), and roundness (calculated with the formula 4⇡area/perimeter2 ),
# all obtained using high-resolution 2D imaging technology. The dataset also includes details about the type of
# gemstone.
# a) Conduct a Principal Component Analysis (PCA) of the dataset, focusing on the quantitative variables only.
# Determine whether it is more appropriate to use the original variables or the standardized ones and proceed
# accordingly.
# b) Report a plot of the loadings of the first two principal components and provide an interpretation. Report the
# scatter plot of the data along the first two principal components. Considering the categorical variable variety,
# interpret the results.
# c) For gemstones of the ruby variety, construct a 95% confidence region for the mean of the vector whose
# components are the first two principal components. Describe this region by providing its center coordinates,
# axis directions, and semi-axes lengths. Provide a plot of the region. Introduce and assess the hypothesis of
# normality, which will here be tested at the 1% significance level..

gemme = read.table("gemstones.txt", header = TRUE)
plot(gemme[1:8], col = as.numeric(as.factor(gemme$Type)))
boxplot(gemme[1:8])
#variances incredibily different! We need to scale our data!
boxplot(scale(gemme[1:8], scale = FALSE))
boxplot(scale(gemme[1:8]))
# We saw there are some outliers (eccetricty boxplot)
pc.fit = princomp(scale(gemme[1:8]))
library(MiniR)
pc.interpretation(pc.fit, gemme[,1:8])

# we keep just the first two PCs. 
# The first one is a weighted average of all components but Eccentricity and roundness. 
# The second one is a contrast of this last two components


biplot(pc.fit)
plot(pc.fit$scores, col = as.numeric(as.factor(gemme$Type)))
# We can see that the representation found by the PCA allows us to 
# easily discriminates the gems into their groups! We found a great trasnformation of 
# our data for which we could train a classifer. (we could do a LDA if hypotesis are satisfied)
dataset = data.frame(pc.fit$scores[,1:2])
dataset$Type = gemme$Type
lda.fit = lda(Type ~ ., data = dataset)
plot_decision_boundaries(Type ~ ., data = dataset, lda.fit)
 

# c) For gemstones of the ruby variety, construct a 95% confidence region for the mean of the vector whose
# components are the first two principal components. Describe this region by providing its center coordinates,
# axis directions, and semi-axes lengths. Provide a plot of the region. Introduce and assess the hypothesis of
# normality, which will here be tested at the 1% significance level..

alpha = 0.05
ruby.idx = which(gemme$Type == "ruby")
ruby.scores = pc.fit$scores[ruby.idx, 1:2]
plot(ruby.scores, pch = 16, main = "Ruby Scores on I and II loadings", col = "salmon")
means = colMeans(ruby.scores)
points(means[1], means[2], pch = 3)
S = var(ruby.scores)
n = nrow(ruby.scores)
p = 2
cfr.fisher = (n-1)*p/(n-p)*qf(1-alpha, p, n-p)
library(car)
ellipse(center=means, shape = S/n, radius = sqrt(cfr.fisher))
axis.directions = eigen(S)
axis.directions = axis.directions$vectors

arrows(means[1], means[2], means[1] + axis.directions[1, 1], means[2] + axis.directions[2, 1])
arrows(means[1], means[2], means[1] + axis.directions[1,2], means[2] + axis.directions[2,2])

