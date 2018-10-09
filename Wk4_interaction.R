url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

spider$log2friction <- log2(spider$friction)

boxplot(log2friction ~ type*leg, data=spider)

fit3 = lm(log2friction ~ type*leg, data=spider)

summary(fit3)

#What is the t-value for the interaction of type push and leg L4? 
#If this t-value is sufficiently large, we would reject the null hypothesis 
#that the push vs pull effect on log2(friction) is the same in L4 as in L1.

# What is the F-value for all of the type:leg interaction terms, 
#in an analysis of variance? If this value is sufficiently large, 
#we would reject the null hypothesis that the push vs pull effect 
#on log2(friction) is the same for all leg pairs.

anova(fit3)


coefFit3 = summary(fit3)$coefficients

# L2 vs L1 estimate in log2friction for pull sample
coefFit3[3,1]

# L2 vs L1 estimate in log2friction for push sample
coefFit3[3,1]+coefFit3[6,1]


#*** performing a linear model on log(value)  -> beta represent fold change

## ---------------------------------------------
## ANOVA Monte Carlo Simulation

# suppose we have 4 group, 10 samples per group, 40 samples overall. 
N = 40
p = 4 
group = factor(rep(1:p, each=N/p))
X = model.matrix(~group)

# We will show here how to calculate the "F-value", and then we will
# use random number to observe the distribution of the F-value under 
# the null hypothesis.

Y = rnorm(N,mean=42,7)

# The base model we wil compare against is simply Y-hat = mean(Y), 
# which we will call mu0, and the initial sum of squares 
# is the Y values minus mu0:
  
mu0 = mean(Y)
initial.ss = sum((Y - mu0)^2) ## sum squre of all

# We then need to calculate the fitted values for each group,
# which is simply the mean of each group, and the residuals from this model, 
# which we will call "after.group.ss" for the sum of squares after 
#using the group information:
  
s = split(Y, group)
after.group.ss = sum(sapply(s, function(x) sum((x - mean(x))^2))) # sum square gr

# Then the explanatory power of the group variable is the initial 
# sum of squares minus the residual sum of squares:
  
(group.ss = initial.ss - after.group.ss)

# We calculate the mean of these values, but we divide by terms which remove 
# the number of fitted parameters. For the group sum of squares, 
# this is number of parameters used to fit the groups 
# (3, because the intercept is in the initial model). For the after group 
# sum of squares, this is the number of samples minus the number of
# parameters total (So N - 4, including the intercept).

group.ms = group.ss / (p - 1)
after.group.ms = after.group.ss / (N - p)

#The F-value is simply the ratio of these mean sum of squares.
f.value = group.ms / after.group.ms

# What's the point of all these calculations? The point is that, 
# after following these steps, the exact distribution of the F-value
# has a nice mathematical formula under the null hypothesis. 
# We will see this below.

# interaction exercise #5


N = 40
p = 4 
group = factor(rep(1:p, each=N/p))
X = model.matrix(~group)

repN = 1000

set.seed(1)

fval = numeric(repN)

for (i in array(1:repN)){
  Y = rnorm(N, mean = 42,7)
  mu0 = mean(Y)
  initial.ss = sum((Y-mu0)^2)
  s = split(Y, group)
  after.group.ss = sum(sapply(s, function(x) sum((x-mean(x))^2)))
  group.ss = initial.ss - after.group.ss
  group.ms = group.ss / (p-1)
  after.group.ms = after.group.ss / (N-p)
  f.value = group.ms / after.group.ms
  #print(f.value)
  #print("y")
  fval[i] = f.value
}

mean(fval)


# Plot the distribution of the 1000 F-values:
  
hist(fval, col="grey", border="white", breaks=50, freq=FALSE)

#Overlay the theoretical F-distribution, with parameters 
#df1=p - 1, df2=N - p.

xs = seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")

#This is the distribution which is used to calculate the p-values for the ANOVA table produced by anova(). 

