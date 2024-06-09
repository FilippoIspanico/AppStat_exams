# A) ----------------------------------------------------------------------
magazza = read.table("StorageCentres.txt", header = TRUE)
plot(magazza)
attach(magazza)
# we see that size variable is correlated to both cost0 and cost. Cost0 and cost are also correlated. 
id.factor = factor(id_storage_centre)
time.factor = factor(time)
growth.factor = factor(growth)
m0 = lm(costs ~ time.factor + costs0 + time.factor:growth + size + rad_less_15_city, data = magazza)
summary(m0)

coeff = coef(m0)
sigma = summary(m0)$sigma
AIC(m0)

# B) ----------------------------------------------------------------------
plot(m0, which = 3)
# We should see an horizontal line, however this is not the case. 
boxplot(m0$residuals~time)
# from this plot we can see that, as time increases, the variance of the residual increases. 
# we cannot assume homoschedasticity
plot(time, m0$residuals)
p = 0.5
lines(time, sigma * time^p, col = "gray", lty = 3)
lines(time, -sigma * time^p, col = "gray", lty = 3)
# from this plot we can see that the variances seem to increase as a power of time. 
# since in m0 we did assume homoschedasticty we need to modify such model in order to consider this fact
library(nlme)
m1 = gls(costs ~ time + costs0 + time.factor:growth + size + rad_less_15_city,
        data = magazza,
        weights = varPower(form=~time)
)

summary(m1)
delta = 0.8870087

n = 40
r = 12
plot(time, abs(m0$residuals)^2/(n-(r+1)))
points(1:5, sqrt(tapply(abs(m1$residuals)^2/(n-(r+1)), time, mean)), pch = 5, col = "red")
lines(1:5, 1.89816 * (1:5)^delta, col = "gray", lty = 3)


m2 = gls(costs ~ time + costs0 + time.factor:growth + size + rad_less_15_city,
         data = magazza,
         weights = varPower(form=~time),
         correlation = corAR1(form = ~ 1|time)
         )
summary(m2)
