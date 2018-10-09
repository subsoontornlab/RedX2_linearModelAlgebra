sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))

X <- model.matrix( ~ sex + trt)

qr(X)$rank

Y <- 1:8

makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b

fitTheRest <- function(a,b) {
  Ystar <- makeYstar(a,b)
  Xrest <- X[,-c(2,5)]
  betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
  residuals <- Ystar - Xrest %*% betarest
  sum(residuals^2)
}

fitTheRest(1,2)

expand.grid(1:3,1:3)

# find combination with minimal rss
betas = expand.grid(-2:8,-2:8)

rss = apply(betas,1,function(x) fitTheRest(x[1],x[2]))

indexMin = which(rss == min(rss))
betas[indexMin,]

# visualise minimum
library(rafalib)
## plot the pairs what are minimum
themin=min(rss)
plot(betas[which(rss==themin),])



