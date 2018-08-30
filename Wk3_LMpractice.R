# show hat t-test is the same for LM and direct calculation
# number of samples
nx = 5
ny = 7
rep(c(0,1),c(nx,ny))
rep(1,nx + ny)
X = cbind(rep(1,nx + ny), rep(c(0,1),c(nx,ny)))

# using model matrix
group <- factor(c("control","control","highfat","highfat"))
model.matrix(~ group)

diet <- factor(c(1,1,1,1,2,2,2,2))
sex <- factor(c("f","f","m","m","f","f","m","m"))
model.matrix(~ diet + sex)


model.matrix(~ rep(c(0,1),c(nx,ny)))

##### 
t(X)%*%X