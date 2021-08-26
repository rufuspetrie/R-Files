######################### Packages #########################

# install.packages('randomForest')
# install.packages('neuralnet')
library(tidyverse)
library(neuralnet)
library(randomForest)
n = 1000
set.seed(0)

############################################################
# Draw observable explanatory variables
x1 = rgamma(n,2,1); x2 = rnorm(n,0,2);
x3 = rweibull(n,2,2); x4 = rlogis(n,2,1);
x5 = rbeta(n,2,1);
x = cbind(x1,x2,x3,x4,x5)
############################################################
# Transform into independent random variables
# Find the current correlation matrix
# Note that %*% is the matrix multiplication operator in R
c1 <- var(x)
# Cholesky decomposition to get independence
chol1 <- solve(chol(c1))
x <- x %*% chol1
############################################################
# Generate random correlation matrix
R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))
RtR <- R %*% t(R)
corr <- cov2cor(RtR)
# check that it is positive definite
sum((eigen(corr)$values>0))==ncol(x)
############################################################
# Transform according to this correlation matrix
x <- x %*% chol(corr)

y1 <- x[,1] + (x[,3]*x[,2]^2)/10 + (x[,1]*x[,4]*x[,5])/10
y2 <- y1 + rnorm(1000,0,1)
y3 <- log(abs(x[,1]^4)/10+abs(x[,2])+x[,3]^2)+x[,4]*x[,2]*sin(x[,5])+rnorm(1000,0,1)
