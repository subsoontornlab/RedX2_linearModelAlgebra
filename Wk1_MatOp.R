matrix(1,5)

#Solve the following system of equations using R:
# 3a + 4b - 5c + d = 10
# 2a + 2b + 2c - d = 5
# a -b + 5c - 5d = 7
# 5a + d = 4
# What is the solution for c:

#Ax = B

A = t(matrix(c(3, 4,-5, 1,
               2, 2, 2,-1,
               1,-1, 5,-5,
               5, 0, 0, 1),4,4))

B = matrix(c(10,5,7,4))

x = solve(A)%*%B
x

# matrix multiplication
a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
a%*%b
#element-wise vector multiplication
sum(a[3,]*b[,2])