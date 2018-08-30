
# Suppose we are analyzing a set of 4 samples.
# The first two samples are from a treatment 
# group A and the second two samples are from
#a treatment group B. This design can be represented 
# with a model matrix like so:

X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")
X

# fitted parameters for linear model
beta <- c(5, 2)

# fitted value for A samples
X%*%beta


# Suppose now we are comparing two treatments B and C
# to a control group A, each with two samples. This design 
# can be represented with a model matrix like so:
X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

#Suppose that the fitted values for the linear model
#are given by:
  
beta <- c(10,3,-3)

X%*%beta

  

