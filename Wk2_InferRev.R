# falling object experiment
g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)

# suppose we don't know h0, v0, -0.5g
# y = Xβ + e
# β̂ =(X⊤X)−1X⊤Y ... least square error
X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)
beta = A%*%y
g_estimate = -2*beta[3]

# g_estimate monte carlo simulation
g_sim = function(T, nT){
  g = 9.8 ## meters per second
  h0 = 56.67
  v0 = 0
  tt = seq(0,T,len=nT)
  y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(nT,sd=1)
  X = cbind(1,tt,tt^2)
  A = solve(crossprod(X))%*%t(X)
  beta = A%*%y
  g_estimate = -2*beta[3]
}
  
set.seed(1)
gs = g_sim(3.4,25)

set.seed(1)
g_est_rep = replicate(100000, g_sim(3.4,25))
sd(g_est_rep)

# father and son
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)










fssampleB = function(N,fs){
  n = length(fs$sheight)
  index = sample(n,N)
  sampledat = fs[index,]
  x = sampledat$fheight
  y = sampledat$sheight
  betahat =  lm(y~x)$coef
  slope = betahat[2]
}

fs = father.son
N = 50
b1 = fssampleB(N,fs)

set.seed(1)
repb1 = replicate(10000, fssampleB(N,fs))
sd(repb1)


# We are defining a new concept: covariance. 
#The covariance of two lists of numbers X=X1,...,Xn
#and Y=Y1,...,Yn is mean( (Y - mean(Y))*(X-mean(X) ) ).
mean((y-mean(y))*(x-mean(x)))

