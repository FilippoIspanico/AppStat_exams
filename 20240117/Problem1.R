library(devtools)
install_github("FilippoIspanico/MiniR")
library(MiniR)


sharks = read.table("sharks.txt", header = TRUE)
View(sharks)
plot(sharks)

D = dist(sharks)
fit = hclust(D, method = "average")
plot(fit)
rect.hclust(fit, k = 4)
cluster = cutree(fit, 4)
table(cluster)
plot(sharks, col = cluster, pch = 16)
legend("topleft", legend = 1:4, col = 1:4, pch = 16)

means = matrix(0, 4, 2)
for (i in 1:4){
  group = sharks[which(cluster == i), ]
  means[i, ] = sapply(group, mean)
}
means
plot(sharks, col = cluster, pch = 16, main = "Sharks groups")
legend("topleft", legend = 1:4, col = 1:4, pch = 16)
points(means, pch = 5, col = "salmon")
table(cluster)

# Observing the clusering result it is clear the presence of four different sharks group. 
# The sharks have been classified by their size and, since we know sharks have very slow grow rate
# we could infer that the four groups could indicate sharks of different ages: 
# Group 1 - child, Group2 - young, group 3 - adult, group 4 - old. This hypotesis could 
# We can see very separated groups. If we infer that a sismic event severly affect the grow possibilities
# of young sharks, then the group separation can indicate the presence of 4 seismic events. 
k = 8 
alpha = 0.1 
library(MVN)
for(i in 1:4 )
{
  
    group = sharks[which(cluster == i), ]
    mvn.result = mvn(group)
    print(mvn.result$multivariateNormality)
    mean = means[i, 1]
    S = cov(group)
    print(S)
    n = nrow(group)
    cfr.t = qt(1-alpha/2/k, n-1)*sqrt(S[1, 1]/n)
    
    CI = cbind(
      inf = mean - cfr.t,
      center = mean, 
      sup = mean + cfr.t
    )
    
    BCI_var = cbind(
      inf = (n - 1) * S[1, 1] / qchisq(1 - alpha/(2*k), n - 1),
      center = S[1, 1] ,
      sup = (n - 1) * S[1, 1] / qchisq(alpha/(2*k), n - 1)
    )
      
    print(CI)
    print(BCI_var)
    print("---------------------------------------------------------")
}  

means = means[, ]

