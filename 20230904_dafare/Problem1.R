# The files wheelworks.txt and cyclecraft.txt contain the prices (in €) and the condition (a real number from
#    0 to 12) of 20 models of vintage bikes sold from two second-hand bike shops WheelWorks and CycleCraft.
# a) A friend of yours suggests that, in general, bikes are more affordable and in better condition at CycleCraft
# compared to WheelWorks. Do you agree with this statement? Support your answer with a statistical test at
# level 95%, reporting the p-value.
# b) Which are the assumptions of the previous test? Are they met?
#   c) Provide the plot of the confidence region at level 95% for the mean difference in price and condition between
# the two shops.
# d) Provide four Bonferroni simultaneous confidence intervals of global level 95% for the mean and the variance of
# the difference in price and condition. Interpret the results of the test at point (a) through the intervals.


alpha = 0.05
bikesA = read.table("wheelworks.txt", header = TRUE)
bikesB = read.table("cyclecraft.txt", header = TRUE)
par(mfrow=c(1,2))
plot(bikesA[,2:3])
plot(bikesB[,2:3])
par(mfrow = c(1,1))

new.dataA = data.frame(log(bikesA$price), bikesA$condition)
colnames(new.dataA) = c("log(price)", "condition")
new.dataB = data.frame(log(bikesB$price), bikesB$condition)
colnames(new.dataB) = c("log(price)", "condition")
par(mfrow=c(1,2))
plot(new.dataA, main = "A")
plot(new.dataB, main = "B")
par(mfrow = c(1,1))
library(MVN)
mvnA = mvn(new.dataA)
mvnB = mvn(new.dataB)
mvnA$multivariateNormality
mvnB$multivariateNormality
# the transformed data is normal!
# We will do the analysis on the log(price) not price
# We look at the boxcox transformation to see if we got a result similar to ours

lambda = powerTransform(bikesA[,2])
# we got -0.3986909: I think that using lambda=0 i.e. log transformation is a good compromise between 
# normality assumption and interpretability of the result!

