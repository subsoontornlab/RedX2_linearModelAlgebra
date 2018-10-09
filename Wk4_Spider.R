# examples of box plots and linear fit of two varibles

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1) # skip the first row

spidersave = spider

head(spider) # see data
dim(spider)

# Boxplots are a measure of how well distributed is 
#the data in a data set. It divides the data set into 
#three quartiles. This graph represents the minimum, 
#maximum, median, first quartile and third quartile 
#in the data set. 

# see different type of measurement we have
table(spider$leg,spider$type) 

# boxplot of friction coff from each leg pair
# arrange plot by leg first
boxplot(spider$friction ~ spider$type * spider$leg, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs")

# ** las=2 put label of boxplot vertically

# arrange plot by type first
boxplot(spider$friction ~ spider$leg*spider$type, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs")

# type alone
boxplot(spider$friction ~ spider$type, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs")

# leg alone
boxplot(spider$friction ~ spider$leg, 
        col=c("grey90","grey40"), las=2, 
        main="Comparison of friction coefficients of different leg pairs")



# linear model of L1 alone (push vs pull)
spider.sub <- spider[spider$leg == "L1",]
fit <- lm(friction ~ type, data=spider.sub)
summary(fit)
# get coeff alone
(coefs <- coef(fit))

#the coefs above are simply mean and diff of mean
s <- split(spider.sub$friction, spider.sub$type)
mean(s[["pull"]]) # intersect = mean of 'pull' coeff
mean(s[["push"]]) - mean(s[["pull"]]) #slope = diff of mean

# see design matrix
X <- model.matrix(~ type, data=spider.sub)
colnames(X)

# visualise design matrix
library(rafalib)
imagemat(X, main="Model matrix for linear model with one variable")



# split function divides data into group
spspi = split(spider.sub$friction, spider.sub$type)
# unsplit function reverses effect of split???
unspt = unsplit(spspi, 'type')


# visualise data with stripechart *****
set.seed(1) #same jitter in stripchart (on the side)
stripchart(split(spider.sub$friction, spider.sub$type), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,3), ylim=c(0,2))
a <- -0.25 # specify arrow location horizontally
lgth <- .1 # specify arrow head size
library(RColorBrewer)
cols <- brewer.pal(3,"Dark2")
abline(h=0)
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1]) # horizontal line - pull 
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
abline(h=coefs[1]+coefs[2],col=cols[2])  # horizontal line - pull 
legend("right",names(coefs),fill=cols,cex=.75,bg="white")


# -------------------------------------------
# linear model with two variables
X <- model.matrix(~ type + leg, data=spider)
colnames(X)
imagemat(X, main="Model matrix for linear model with two factors")

fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)
(coefs <- coef(fitTL)) #get model coeff

# solve system of eq with matrix
Y <- spider$friction
X <- model.matrix(~ type + leg, data=spider)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y
t(beta.hat)


#----------
# # visualise data (2 var) with stripechart *****
spider$group <- factor(paste0(spider$leg, spider$type))
stripchart(split(spider$friction, spider$group), 
           vertical=TRUE, pch=1, method="jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols <- brewer.pal(5,"Dark2")
abline(h=0) # zero base line
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
abline(h=coefs[1],col=cols[1])
arrows(3+a,coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
arrows(5+a,coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
arrows(7+a,coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(3+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3],lwd=3,col=cols[3])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[3]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(5+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4],lwd=3,col=cols[4])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[4]+coefs[2],lwd=3,col=cols[2],length=lgth)
segments(7+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5],lwd=3,col=cols[5])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[5]+coefs[2],lwd=3,col=cols[2],length=lgth)
legend("right",names(coefs),fill=cols,cex=.75,bg="white")



# organize friction data by group
head(spider)
s <- split(spider$friction, spider$group)
mean(s[["L1pull"]])
mean(s[["L1push"]])
coefs[1] + coefs[2]
# **numbers are just added up as we use 5 coeff to describe 8 groups

# here, coeff is wt avg determined by sample size
means <- sapply(s, mean)
##the sample size of push or pull groups for each leg pair
ns <- sapply(s, length)[c(1,3,5,7)]
(w <- ns/sum(ns))

sum(w * (means[c(2,4,6,8)] - means[c(1,3,5,7)]))
coefs[2]

#  ------contrasting coefficients
# recall coeff of our linear model:
coefs

#Here we have the intercept estimate, the push vs. pull estimated
#effect across all leg pairs, and the estimates for the L2 vs. 
#L1 effect, the L3 vs. L1 effect, and the L4 vs. L1 effect. 
#What if we want to compare two groups 
#and one of those groups is not L1? 
#The solution to this question is to use 'contrasts.'

# error *******
#install.packages('contrast')
#install.packages('quantreg')
#install.packages('rms')

library(rms)
library(plospline)
library(quantreg)
library(contrast) #Available from CRAN
L3vsL2 <- contrast(fitTL,list(leg="L3",type="pull"),list(leg="L2",type="pull"))
L3vsL2
# error *******


# ------------------------------------------------- ******
# interaction & contrast II
library(rafalib)
library(RColorBrewer)
cols <- brewer.pal(5,"Dark2")
a <- -0.25 # specify arrow location horizontally
lgth <- .1 # specify arrow head size

head(spider)
X = model.matrix(~ type + leg, data = spider) # model matrix
colnames(X)
head(X)
imagemat(X, main = "model matrix for linear model with 2 factors")

# fitting linear model
fit2 = lm(friction ~ type + leg, data = spider)
summary(fit2)
coefs = coef(fit2)

# examining coefficient
# make new column named 'group' in spider with leg and type
spider = spidersave
spider$group = factor(paste0(spider$leg, spider$type)) # paste0 used for concatenation without space
head(spider)
stripchart(split(spider$friction, spider$group), vertical = TRUE, 
           pch=1, method = "jitter", las=2, xlim = c(0,11),
           ylim = c(0,2))
cols = brewer.pal(5,"Dark2")
abline(h=0)



abline(h=coefs[1], col=cols[1]) # intercept is not the mean**

#L1pull
arrows(1+a,0,1+a,coefs[1],lwd=3,col=cols[1],length=lgth)
#L2pull
arrows(3+a, coefs[1],3+a,coefs[1]+coefs[3],lwd=3,col=cols[3],length=lgth)
#L3pull
arrows(5+a, coefs[1],5+a,coefs[1]+coefs[4],lwd=3,col=cols[4],length=lgth)
#L4pull
arrows(7+a, coefs[1],7+a,coefs[1]+coefs[5],lwd=3,col=cols[5],length=lgth)

#L1push
segments(1+a, coefs[1], 2+a, coefs[1], lwd=3,col=cols[2])
arrows(2+a,coefs[1],2+a,coefs[1]+coefs[2],lwd=3,col=cols[2],length=lgth)


#L2push
segments(3+a, coefs[1]+coefs[3], 4+a, coefs[1]+coefs[3], lwd=3,col=cols[2])
arrows(4+a,coefs[1]+coefs[3],4+a,coefs[1]+coefs[2]+coefs[3],lwd=3,col=cols[2],length=lgth)

#L3push
segments(5+a, coefs[1]+coefs[4], 6+a, coefs[1]+coefs[4], lwd=3,col=cols[2])
arrows(6+a,coefs[1]+coefs[4],6+a,coefs[1]+coefs[2]+coefs[4],lwd=3,col=cols[2],length=lgth)

#L4push
segments(7+a, coefs[1]+coefs[5], 8+a, coefs[1]+coefs[5], lwd=3,col=cols[2])
arrows(8+a,coefs[1]+coefs[5],8+a,coefs[1]+coefs[2]+coefs[5],lwd=3,col=cols[2],length=lgth)

library(contrast)

# **** coef != mean because we have 8 groups but only 5 parameters. 
# *** earlier we have 2 groups and 2 parameters.
mean(split(spider$friction, spider$group)[["L1pull"]]) #mean L1pull


## ----------------
# linear model with interaction

X = model.matrix(~ type + leg + type:leg, data = spider) # model matrix
#X = model.matrix(~ type*leg, data = spider) # ... alternative formula
colnames(X)
head(X)
imagemat(X, main = "model matrix for linear model with interaction")

fit3 = lm(friction ~ type + leg + type:leg, data=spider)
summary(fit3)
coefs = coef(fit3)

stripchart(split(spider$friction, spider$group), vertical = TRUE, 
           pch=1, method = "jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols = brewer.pal(8, "Dark2")
abline(h=0)
abline(h=coefs[1], col=cols[1])

# leg 1
arrows(1+a, 0, 1+a, coefs[1], lwd=3, col=cols[1], length=lgth)
arrows(2+a, coefs[1], 2+a, coefs[1]+coefs[2], lwd=3, col=cols[2], length=lgth)
segments(1+a, coefs[1], 2+a, coefs[1], lwd=3, col=cols[2])
#arrows(2+a, coefs[1]+coefs[2], 2+a, coefs[1]+coefs[2]+coefs[6], lwd=3, col=cols[6], length=lgth)

# leg 2
arrows(3+a, coefs[1], 3+a, coefs[1]+coefs[3], lwd=3, col=cols[3], length=lgth)
arrows(4+a, coefs[1]+coefs[3], 4+a, coefs[1]+coefs[3]+coefs[2], lwd=3, col=cols[2], length=lgth)
segments(3+a, coefs[1]+coefs[3], 4+a, coefs[1]+coefs[3], lwd=3, col=cols[2])
arrows(4+a, coefs[1]+coefs[3]+coefs[2], 4+a, coefs[1]+coefs[3]+coefs[2]+coefs[6], lwd=3, col=cols[6], length=lgth)

# leg 3
arrows(5+a, coefs[1], 5+a, coefs[1]+coefs[4], lwd=3, col=cols[4], length=lgth)
arrows(6+a, coefs[1]+coefs[4], 6+a, coefs[1]+coefs[4]+coefs[2], lwd=3, col=cols[2], length=lgth)
segments(5+a, coefs[1]+coefs[4], 6+a, coefs[1]+coefs[4], lwd=3, col=cols[2])
arrows(6+a, coefs[1]+coefs[4]+coefs[2], 6+a, coefs[1]+coefs[4]+coefs[2]+coefs[7], lwd=3, col=cols[7], length=lgth)

# leg 4
arrows(7+a, coefs[1], 7+a, coefs[1]+coefs[5], lwd=3, col=cols[5], length=lgth)
arrows(8+a, coefs[1]+coefs[5], 8+a, coefs[1]+coefs[5]+coefs[2], lwd=3, col=cols[2], length=lgth)
segments(7+a, coefs[1]+coefs[5], 8+a, coefs[1]+coefs[5], lwd=3, col=cols[2])
arrows(8+a, coefs[1]+coefs[5]+coefs[2], 8+a, coefs[1]+coefs[5]+coefs[2]+coefs[8], lwd=3, col=cols[8], length=lgth)

legend("right", names(coefs), fill=cols, cex=.75, bg = "white")

# p-value of interaction show whether difference in interaction in sig.

install.packages(multicomp)
library(multicomp)

#Q: if the push VS pull difference is different for different legs
anova(fit3)


##-----------------------------------------
# different specification of the same model
# just go from zero to specific group
a <- -0.25 # specify arrow location horizontally
lgth <- .1 # specify arrow head size

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
library(rafalib)
library(RColorBrewer)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)
spider$group = factor(paste0(spider$leg, spider$type))
X = model.matrix(~0 + group, data = spider)
colnames(X)
head(X)
imagemat(X, main = "Model matrix for linear model with interactions")

fit4 = lm(friction ~ 0+ group, data=spider)
summary(fit4)
coefs = coef(fit4)


stripchart(split(spider$friction, spider$group), vertical = TRUE, 
           pch=1, method = "jitter", las=2, xlim=c(0,11), ylim=c(0,2))
cols = brewer.pal(8, "Dark2")
abline(h=0)
for(i in 1:8){
  arrows(i+a, 0, i+a, coefs[i], lwd=3, col=cols[i], length=lgth)
}
legend("right", names(coefs), fill=cols, cex=0.75,bg="white")

# push vs pull difference for leg2
groupL2push.vs.pull = coefs[4] - coefs[3]