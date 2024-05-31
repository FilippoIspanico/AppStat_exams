streaming = read.table("streaming.txt", header = TRUE)
plot(streaming)
# We see three clusters. 

D = dist(streaming)
fit = hclust(D, "single")
plot(fit)
rect.hclust(fit, k = 3)
groups = cutree(fit, k  = 3)
plot(streaming, col = as.numeric(as.factor(groups)), pch = 16)
legend("bottomright", legend = levels(as.factor(groups)), pch = 16, cex = 0.5,
       col = 1:3)

# B)
sizes = table(groups)
sizes
C = cophenetic(fit)
coeff = cor(C, D)
coeff
centroids = cbind(tapply(streaming$minutes, groups, mean), 
                tapply(streaming$artists, groups, mean))
points(centroids[,1], centroids[,2], pch = 3, col = "gold")


# interpretation: 
# we got three groups: 
# group 1, in black, correspond to user that uses the most the streaming service,
# they listen to different artists for an important amount of time.  
# group 2, in red, represent users that listen to a lot of artist for a limited amount of time. 
# group 3, in green correspond to users that listen only a few artist.The amount of time 
# they uses the service is limited. 


# Centroids can be seen as yellow arrows.
# Variances within group seem different! Let's check this assumption. 
alpha = 0.05 


S1 = cov(streaming[groups == 1, ])
S2 = cov(streaming[groups == 2, ])
S3 = cov(streaming[groups == 3, ])

par(mfrow = c(1, 3))
image(S1, main = "S1")
image(S2, main = "S2")
image(S3, main = "S3")
par(mfrow = c(1, 1))


plot(log(streaming$minutes), log(streaming$artists), col = as.numeric(as.factor(groups)), pch = 16)
legend("bottomright", legend = levels(as.factor(groups)), pch = 16, cex = 0.5,
       col = 1:3)


# Ponit C) 





alpha = 0.05 
k = 6 
CI = cbind()

library(MVN)
for(i in 1:3){
  subset_i = streaming[groups == i, ]
  S_i = cov(subset_i)
  n_i = nrow(subset_i)  
  means = sapply(subset_i, mean)
  print(mvn(subset_i)$multivariateNormality)
  correction = qt(1-alpha/2/k, n_i-1)
  
  
  CI_i = cbind(
    inf = means - correction*sqrt(diag(S_i)/(n_i)),  
    center = means,
    sup = means + correction*sqrt(diag(S_i)/(n_i))
  )
  rownames(CI_i) = c(paste("minutes_", i), paste("artist_i", i))
  
  CI = rbind(CI, CI_i)
}
CI
library(MiniR)
CI_minutes = CI[i%%2 == 0]
CI_artists = CI[i%%2 == 1]
plot.intervals(CI_minutes)
plot.intervals(CI_artists)