tatu = read.table("tattoo.txt", header = TRUE)

plot(tatu[,1:3], col = as.numeric(factor(tatu$method)))
tatu$method = factor(tatu$method)
m0 = lm(price ~ .*method, data = tatu)
summary(m0)
coef(m0)
sigma = summary(m0)$sigma
par(mfrow = c(2, 2))
plot(m0)
par(mfrow = c(1, 1))
# hypotesis of lm verified, also normality: we can do confidence intervals. 

library(car)

# point b)
C
C = matrix(0, nrow = 1, ncol = 6)
C[1, 4] = 1
linearHypothesis(m0, C)
# we do not reject the null hp of method = 0

# point c) 
# yes for the t test. 
# maybe we can remove the intercation along with methodmachine
C
C = matrix(0, nrow = 2, ncol = 6)
C[1, 4] =  C[2, 6] = 1
linearHypothesis(m0, C)
# we can remove them. 
#does ncolor have impact?
C
C = matrix(0, nrow = 3, ncol = 6)
C[1, 4] =  C[2, 6] = C[3, 3] = 1
linearHypothesis(m0, C)

# yes!

# point d)
m1 = lm(price ~ dimension + ncolors + dimension:method, data = tatu)
summary(m1)
coef(m1)
summary(m1)$sigma


# point e)
Z0.new = data.frame(dimension=0, ncolors=0, method = "machine")
predict(m1, Z0.new, interval = "confidence", level = 1-0.05/2)

Z0.new = data.frame(dimension=6.5, ncolors=1, method = "handmade")
predict(m1, Z0.new, interval = "confidence", level = 1-0.05/2)