# 4 treatments on 8 mices. Two mices per treatment
# treatment A, B use male; C, D use female.

set.seed(1)
Sex = c(0,0,0,0,1,1,1,1)
A   = c(1,1,0,0,0,0,0,0)
B   = c(0,0,1,1,0,0,0,0)
C   = c(0,0,0,0,1,1,0,0)
D   = c(0,0,0,0,0,0,1,1)

X = model.matrix(~Sex+A+B+C+D-1)

cat("ncol=",ncol(X), "rank=",qr(X)$rank,"\n") # calculate rank in qr decomp

###
#  4 treatments on 8 mices. Two mices per treatment
# each treatment use one male and one female

set.seed(1)
Sex = c(0,1,0,1,0,1,0,1)
A   = c(1,1,0,0,0,0,0,0)
B   = c(0,0,1,1,0,0,0,0)
C   = c(0,0,0,0,1,1,0,0)
D   = c(0,0,0,0,0,0,1,1)

X = model.matrix(~Sex+A+B+C+D-1)

cat("ncol=",ncol(X), "rank=",qr(X)$rank,"\n") # calculate rank in qr decomp