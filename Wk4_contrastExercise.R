# aside: factor in r: putting items in category
data = c(1,2,2,3,1,2,3,3,1,2,3,3,1)
fdata = factor(data)
fdata
rdata = factor(data,labels=c("I","II","III"))
rdata
levels(fdata) = c('I','II','III')
fdata
#######

# contrast exercise
species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))
model.matrix(~ species + condition)

# the contrast of (species=B and condition=control) 
#vs (species=A and condition=treatment)?
# ANS: [0 1 -1]
# model fit by R is: Intercept, speciesB, conditiontreated

# ---- Exercise 2:
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

head(spider)

fitTL = lm(friction ~ type + leg, data = spider)
summary(fitTL)

# The t-value for the contrast of leg pair L4 vs leg pair L2
#is constructed by taking the difference of the coefficients
#legL4 and legL2, and then dividing by the standard error of the dif

# standard error of the dif = sqrt(C*Sig*t(C))
# when Sig = covariance matrix; C = contrast matrix

# alternatively:
# Var(BL4 - BL2) = Var(BL4) + Var(BL2) - Cov(BL4, BL2)




X <- model.matrix(~ type + leg, data=spider)
(Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X))
C <- matrix(c(0,0,-1,0,1),1,5) # i.e. subtracting BL2 from BL4

dif = sqrt(C%*%Sigma%*%t(C))

Coeff = fitTL$coefficients

t4_2 = (Coeff[5] - Coeff[3])/dif


# approach #1 to find  variance of BL4-BL2 difference
dif1 = sqrt(C%*%Sigma%*%t(C))

# appoach #2
# Var(BL4 - BL2) = Var(BL4) + Var(BL2) - Cov(BL4, BL2)
betaVar = summary(fitTL)$coefficients[,2]**2
dif2  = sqrt(betaVar[3] + betaVar[5] - 2*Sigma[3,5])




# -------
# accessing coefficient table
summary(fitTL)$coefficients

#Estimate of beta
summary(fitTL)$coefficients[,1]

#std. Error of beta
summary(fitTL)$coefficients[,2]

