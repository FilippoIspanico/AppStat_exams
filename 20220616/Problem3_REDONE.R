# We are interested in studing the danceability (Y) of a song with respect to other features of the song. The file
# danceability.txt contains the values of danceability (the higher the value, the easier it is to dance to this song),
# loudness, energy, tempo and genre of 400 songs.

dance = read.table("danceability.txt", header = TRUE)
head(dance)
plot(dance[,1:4], col = as.numeric(factor(dance$genre)), pch = 19, main = "Danceability vs Loudness")
# we can see that loudness and energy are positively correlated. However their relation with our
# target variable, dancability, seem weak (maybe postive correlation with energy?)
# there may be a negative correlation between dancability and tempo
m0 = lm(danceability ~ loudness + energy + tempo, data = dance)
summary(m0)
# we can see that: 
# the model is overall significant (F test has low p-value)
# tempo is significant and has a negative effect on danceability
# energy and loundness may or may not be significant, depending on the significance level we choose

# given the high correlation between loundness and energy, we may want to remove one of them
vif(m0)
# model's assumptions:
# residuals are normally distributed
# residuals are homoscedastic
# residuals are independent
plot(m0)
# all hp verified. 

# let's check if both energy and loudness are significant
C = matrix(0, nrow = 2, ncol = 4)
C[1, 2] = C[2, 3] = 1
C
library(car)
linearHypothesis(m0, C)
# we can reject the null hp! We need to keep at least one of the two variables in the model. 
# We can perform a backward selection to choose the best model
library(leaps)
backward.fit = regsubsets(danceability ~ loudness + energy + tempo, data = dance)
summary(backward.fit)
# the best model with two variables is the one composed of loudness and tempo. 
par(mfrow=c(1, 3))
plot(backward.fit, scale = "bic")
plot(backward.fit, scale = "Cp")
plot(backward.fit, scale = "adjr2")
par(mfrow=c(1, 1))


m1 = lm(danceability ~ loudness + tempo, data = dance)
summary(m1)
cov_matrix <- vcov(m1)
# we can see that: 
# the covariance matrix seems diagonal, so the estimented coefficients are uncorrelated
# here the model is still significant, and both variables are significant.
# we see that the coefficient of tempo is still negative (thanks to indepdence of B_hat if you want to improve
# the danceability of a song, you should decrease its tempo)
# and the coefficient of loudness is positive (if you want to improve the danceability
# of a song, you should increase its loudness, thanks to independence of B_hat)
plot(m1)
# assumption are verified. 



# lets see how the variable genre affects the danceability
boxplot(m1$residuals ~ genre, data = dance, main = "Danceability vs Genre")
abline(h= 0, col = "red")
# from this plot we see that, using the genre variable, 
# we can extract some information from the residuals. 
# For example, the for the box corresponding to genre "ambience", the whole interquartile range is below 0.
# this means that the model is overestimating the danceability of songs of genre "ambience"
# this situation is opposed to the genere "pop", wheere the model is underestimating the danceability of songs.
# the variance of the residuals seems similar for all genres.
# we will make now a new linaer mixed model with genre as a random effect. 
library(nlme)
m2 = lme(danceability ~ loudness + tempo, data = dance,random = ~1|genre)  
summary(m2)
ranef(m2)
# Report the dot plot of the estimated random intercepts. Net to the effect of fixed effect covariates, which is the
# genre associated to the highest danceability?
library(lattice)
dotplot(random.effects(m2))
abline(v = 0)



# Net to the effect of fixed effect covariates, which is the genre associated to the highest danceability?
# the genre associated to the highest danceability is R&B.
library(lme4)

m3 = lmer(danceability ~ loudness + tempo + (1|genre), data = dance)
summary(m3)
random.effects(m3) - random.effects(m2)
dotplot(random.effects(m3))


sigma2_eps = as.numeric(get_variance(m3, component = "residual"))
sigma2_b = as.numeric(get_variance(m3, component = "random"))
PVRE <- sigma2_b/(sigma2_b+sigma2_eps)
PVRE

lattice::dotplot(ranef(m3))
