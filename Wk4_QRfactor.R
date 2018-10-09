n = 50; M = 500
x = seq(1,M,len=n)
X = cbind(1,x,x^2,x^3)
beta = matrix(c(1,1,1,1),4,1)
y = X%*%beta + rnorm(n,sd=1)

par(mar=c(1,1,1,1))
plot(x,y)

# to find beta_hat, we need to compute (t(X)X)^-1
# but it's unstable in this case and give error

log10(crossprod(X))  # very different item --> inverse close to div by zeoro

solve(crossprod(X)) # give error

solve(crossprod(X)) %*% crossprod(X,y) # give error

# QR factorization: decomposing a matrix into Q - orthogonal x R - triangular matrix

# R*beta = t(Q)*Y

QR = qr(X)
Q = qr.Q(QR)
R = qr.R(QR)
betahat = backsolve(R, crossprod(Q,y))

QR = qr(X)
betahat = solve.qr(QR, y) # calculate betahat

fitted = tcrossprod(Q)%*%y # fit y value
lines(x, fitted, col=2)

# standard error
df = length(y) - QR$rank
sigma2 = sum((y-fitted)^2)/df
var = sigma2*chol2inv(QR$qr)
SE = sqrt(diag(var))
cbind(betahat,SE)

summary(lm(y~X-1))$coef


