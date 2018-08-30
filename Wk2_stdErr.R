# estimate std err of linear model off.

library(UsingR)

x = father.son$fheight
y = father.son$sheight

n = length(y)
N = 50

# take sample of size N and fit the linear model
set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight

betahat = lm(y~x)$coef


# fit model
fit = lm(y~x)
y_hat = fit$fitted.values #estimated (fitted) y ... y hat

residue = y - y_hat
ssr = t(residue)%*%residue # sum square residue


# Our estimate of sigma^2 will be the sum of squared 
#residuals divided by (N - p), the sample size minus 
#the number of terms in the model.

# sigma2 = SSR / 48

X = cbind(rep(1,N),x)
solve(t(X)%*%X)


# find std err of beta hat
diagss = diag(solve(t(X)%*%X))
sigma2 = ssr/(N-2)  # 2 term in the model
sqrt(diagss*sigma2)
