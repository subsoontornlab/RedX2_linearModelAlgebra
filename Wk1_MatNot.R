# create matrix
X = matrix(1:1000,100,10)
X[25,3]

x = 1:10
xmat = cbind(x, 2*x, 3*x, 4*x, 5*x)
sum(xmat[7,]) # sum of element in 7th row

matrix(1:60,20,3)
matrix(1:60,20,3,byrow=TRUE)
x=11:20;rbind(x,2*x,3*x)

x=1:40;matrix(3*x,20,2)