pesci = read.table("pinnanobilis.txt", header = TRUE)
plot(pesci)
# we would expect two clusters.  
D = dist(pesci)
fit = hclust(D)
plot(fit)
rect.hclust(fit, k = 2)
groups  = cutree(fit, 2)
table(groups)
plot(pesci, col = as.numeric(as.factor(groups)), pch = 16, main = "pinnanobilis (K=2)")
legend("topleft", cex = 0.5, legend = unique(as.factor(groups)), pch = 16, col = 1:2)
  
# point b)
library(MVN)
for (i in 1:2)
{
  datum = pesci[groups == i,]
  print(mvn(datum)$multivariateNormality)
  S = cov(datum)
  print(S)
}



manova.fit = manova(as.matrix(pesci) ~ groups)
summary(manova.fit)
summary.aov(manova.fit)

par(mfrow = c(1, 2))
boxplot(pesci$height ~ groups, main = "effect on height")
boxplot(pesci$width ~ groups, main = "effect on width")
par(mfrow = c(1, 1))


