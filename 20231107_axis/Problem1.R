# In the remote wilderness of the Amazon Rainforest, there is a region known as the Jungle Sanctuary that
# has witnessed a long history of natural disturbances, including massive wildfires. The file kapok.txt contains
# information on 294 ancient Kapok trees, specifically recording their heights (in meters) and trunk diameters (in
#   meters).
# a) Conduct a cluster analysis of the Kapok trees using a hierarchical clustering method (Euclidean distance and
#  Ward linkage). Report the number of clusters you deem appropriate to represent the data, the centroids of
# the clusters, and their size. Additionally, provide a possible interpretation regarding the possible number of
# historical wildfires that acted the growth of the Jungle Sanctuary.
# b) Calculate Bonferroni intervals at a global significance level of 90% for both the mean and variances of the trunk
# diameters within each of the clusters identified in (a). Introduce and verify the appropriate assumptions.

kapok = read.table("kapok.txt", header = TRUE)
par(mfrow = c(1, 1))
plot(kapok)
# graphically we can see the presence of 5 cluster. 
D = dist(kapok)
fit = hclust(D, method = "ward.D2")
plot(fit)
rect.hclust(fit, k = 5)
groups = cutree(fit, k = 5)
plot(kapok, col = groups, pch = 16)
legend("topleft", legend = 1:5, col = 1:5, pch = 16)
table(groups) # size of the groups, note that the order of "age" of the groups is 2, 3, 1, 4, 5
mean_height = tapply(kapok$height, groups, mean)
mean_diameter = tapply(kapok$diameter, groups, mean)
means = cbind(mean_height, mean_diameter) # centroids 
points(means[,1], means[,2], col = "gold", pch = 3)

# If we HP that the number of new tree is constant along years, then we can conclude that 5 wildfares have been present. 
# 5 since we can see that no tree of height << 50 m exists

# b) Calculate Bonferroni intervals at a global significance level of 90% for both the mean and variances of the trunk
# diameters within each of the clusters identified in (a). Introduce and verify the appropriate assumptions.

alpha = 0.1
k = 2*5 
BCI_means = cbind()
BCI_var = cbind()
for (i in 1:5){
  
  n = table(groups)[i]
  data = kapok[which(groups == i), ]
  correction = qt(1-alpha/2/k, n-1)
  
  
  S = var(data)
  sii = S[2,2]
  
  CI = cbind(
    inf = mean_diameter[i] - correction*sqrt(sii/n),
    center = mean_diameter[i],
    sup = mean_diameter[i] + correction*sqrt(sii/n)
  )
  BCI_means = rbind(BCI_means, CI)
  
  
  CI_var = cbind( 
      inf = (n - 1) * sii / qchisq(1 - alpha/(2*k), n - 1),
      center = sii,
      sup = (n - 1) * sii / qchisq(alpha/(2*k), n - 1)
    )
  
  BCI_var = rbind(BCI_var, CI_var)
  
}
BCI_means
BCI_var
library(MiniR)


plot.intervals <- function(BonfCI = NULL, SimCI = NULL, delta.0, legend.position = 'bottomright') {
  
  
  if(is.null(BonfCI) && is.null(SimCI)){
    stop("Both BonfCI and SimCI cannot be NULL")
  }
  
  if(!is.null(BonfCI)){
    
    if (is.null(dim(BonfCI))) {
      BonfCI <- matrix(BonfCI, ncol = 3, byrow = TRUE)
    }
    
    n_intervals = nrow(BonfCI)
    
    matplot(t(matrix(1:n_intervals, n_intervals, 3)), t(BonfCI), type='b', pch='', xlim=c(1-.05,n_intervals+0.5), xlab='',
            ylab='', main='Confidence intervals')
    
    # Plotting the Bonferroni intervals
    segments(matrix(1:n_intervals, n_intervals, 1), BonfCI[,1], matrix(1:n_intervals, n_intervals, 1), BonfCI[,3],
             col='orange', lwd=2)
    points(1:n_intervals, BonfCI[,2], col='orange', pch=16)
    
    # Plotting delta.0 under H0 
    points(1:n_intervals+.02, delta.0, col='black', pch=16)
    
    # Plotting the simultaneous T2
    if(is.null(SimCI)){
      
      legend(legend.position, c('Bonf. IC'), col=c('orange'), lty=1, lwd=2)
      return()
    }
    
    segments(matrix(1:n_intervals+.1,n_intervals,1),SimCI[,1],matrix(1:n_intervals+.1,n_intervals,1),SimCI[,3], col='blue', lwd=2)
    points(1:n_intervals+.1,SimCI[,2], col='blue', pch=16)
    
    legend(legend.position, c('Bonf. IC', 'Sim-T2 IC'), col=c('orange', 'blue'), lty=1, lwd=2)
    
  }
  
  else{
    
    if (is.null(dim(SimCI))) {
      SimCI <- matrix(SimCI, ncol = 3, byrow = TRUE)
    }
    
    n_intervals = nrow(SimCI)
    
    
    matplot(t(matrix(1:n_intervals, n_intervals, 3)), t(SimCI), type='b', pch='', xlim=c(1-.05,n_intervals+0.5), xlab='',
            ylab='', main='Confidence intervals')
    segments(matrix(1:n_intervals,n_intervals,1),SimCI[,1],matrix(1:n_intervals,n_intervals,1),SimCI[,3], col='blue', lwd=2)
    points(1:n_intervals, SimCI[,2], col='blue', pch=16)
    
    legend(legend.position, c('Sim-T2 CI'), col=c('blue'), lty=1, lwd=2)
    
    points(1:n_intervals, delta.0, col='black', pch=16)
    
  }
}

plot.intervals(BCI_means, delta.0 = mean_diameter, legend.position = "topleft")

#Assumptions: 
#We need that observation in each group are normal
library(MVN)
for (i in 1:5)
{
  data = kapok[which(groups == i), ]
  mvn.i = mvn(data)
  print(mvn.i$multivariateNormality)
}

