url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1) # skip the first row

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

# solve system of eq with matrix