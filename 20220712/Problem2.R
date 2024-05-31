demo = read.table("demogorgons.txt", header = TRUE)


plot(demo, main = "demogorgons attacks")

D = dist(demo)
fit = hclust(D, "average")
plot(fit)
rect.hclust(fit, 2)
groups = cutree(fit, 2)
table(groups)
plot(demo, main = "demogorgons attacks", col = as.numeric(as.factor(groups)))







# Point b) ----------------------------------------------------------------
# HP testing.
demo$groups = groups
group1 = demo[which(groups == "1"),]
group2 = demo[which(groups == "2"),]

library(MVN)
mvn1 = mvn(group1[,1:2])
mvn1$multivariateNormality

mvn2 = mvn(group2[,1:2])
mvn2$multivariateNormality

#Both groups are normally distributed. With same Sigma?

S1 = cov(group1[,1:2])
S2 = cov(group2[,1:2])
S1
S2

bartlett.test(demo[,1:2], groups)
par(mfrow=c(1, 2))
boxplot(demo$lat ~ groups)
boxplot(demo$lon ~ groups)
par(mfrow=c(1, 1))

fit = manova(as.matrix(demo[,1:2]) ~ groups )
summary.manova(fit)
summary.aov(fit)


coef(fit)



# Point c) ----------------------------------------------------------------


plot(demo, main = "demogorgons attacks", col = as.numeric(as.factor(groups)))

n1 = 36
n2 = 61
Spool = ((n1 -1)*S1 + (n2 - 1)*S2)/(n1+n2 -2) #we need to use spool since we tested that the variances are the same

p = 2

alpha = 0.05
cfr.1 = (n1 - 1)*p/(n1 - p) * qf(1-alpha, p, n1 - p)
cfr.2 = (n2 - 1)*p/(n2 - p) * qf(1-alpha, p, n2 - p)

mean1 = sapply(group1, mean)
mean2 = sapply(group2, mean)
library(car)
ellipse(center = mean1, shape = Spool/(n1+n2-2), radius = sqrt(cfr.1))
ellipse(center = mean2, shape = Spool/(n1+n2-2), radius = sqrt(cfr.2))

# Domanda! We should plot with alpha = 0.05/2 or alpha = 0.05: 
# the global confidence on the two regions is different from 95% if we use alpha = 0.05, no??



