url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

fit <- lm(friction ~ type + leg, data=spider)

betahat <- coef(fit)

Y <- matrix(spider$friction, ncol=1)

X <- model.matrix(~ type + leg, data=spider)

summary(fit)

QR = qr(X)
Q = qr.Q(QR)
R = qr.R(QR)

Q[1,1]
R[1,1]

QtY = crossprod(Q,Y)
QtY[1,1]

betahat2 = backsolve(R, crossprod(Q,Y))
betahat