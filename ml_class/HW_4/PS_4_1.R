#load in random forests package
require("neuralnet")
require("randomForest") 

n = 1000
perc_train = .5
specification1 = T
error = F
sderror = .2
verbose = T

iters = 1
nn_error = numeric(iters)
rf_error = numeric(iters)
series_error = numeric(iters)

pb = txtProgressBar(min = 0, max = iters, initial = 0) 
for (k in 1:iters) {
  
#all covariates
x1 = rgamma(n,2,1); x2 = rnorm(n,0,2); 
x3 = rweibull(n,2,2); x4 = rlogis(n,2,1); 
x5 = rbeta(n,2,1); 
x = cbind(x1,x2,x3,x4,x5)
###############################################
#transform to independence
# find the current correlation matrix
c1 <- var(x)

# cholesky decomposition to get independence
chol1 <- solve(chol(c1))

x <-  x %*% chol1 
###############################################
#generate random correlation matrix

R <- matrix(runif(ncol(x)^2,-1,1), ncol=ncol(x))

RtR <- R %*% t(R) #always symmetric!

corr <- cov2cor(RtR) 

# check that it is positive definite
sum((eigen(corr)$values>0))==ncol(x)
################################################
#transform according to this correlation matrix
x <- x %*% chol(corr)
################################################
if (specification1) {
  y = x[,1] + ((x[,2]^2)/3)*x[,3]/3 + x[,4]*x[,1]*x[,5]/10 
} else {
  y = log(abs(x[,1]^4/10)+abs(x[,2])+x[,3]^2) + x[,4]*x[,2]*sin(x[,5])
}
if (error) {
#add in error
y = y + rnorm(n,0,sderror)
}
#print(paste("sd of y is ",sd(y)))
data = data.frame(y,x)

train = data[(1:(n*perc_train)),] #training data
test = data[((n*perc_train+1):nrow(data)),] #test data
###############################################################
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

data_n <- as.data.frame(scale(data,center = mins, scale = maxs - mins))

train_n = data_n[(1:(n*perc_train)),]
test_n = data_n[((n*perc_train+1):nrow(data_n)),]


xn = names(train_n[,2:ncol(train_n)])
f <- as.formula(paste("y ~", paste(xn, collapse = " + ")))
if (verbose) {
print("estimating neural net...")
}
nn <- neuralnet(f,data=train_n,hidden=c(64,32,16),linear.output=T#,
                #algorithm="backprop",learningrate=.001
)

pr.nn <- compute(nn,test_n[,2:ncol(test_n)])

pr.nn_ <- pr.nn$net.result*(max(data$y)-min(data$y))+min(data$y)
test.r <- (test_n$y)*(max(data$y)-min(data$y))+min(data$y)

nn_error[k] <- sum((test.r - pr.nn_)^2)/nrow(test_n)
#######################################################################
trainm = data.frame(train$y,poly(as.matrix(train[,2:ncol(train)]),degree=3,raw=T))
if (verbose) {
print("estimating series...")
}
linear_model = lm(trainm)

testm = data.frame(test$y,poly(as.matrix(test[,2:ncol(test)]),degree=3,raw=T))

series_error[k] = mean((test$y - predict(linear_model,testm))^2)
#######################################################################
#random forest
if (verbose) {
print("estimating random forest...")
}

rf = randomForest(x=train[,2:ncol(train)],y=train[,1],
                  xtest=test[,2:ncol(test)],ytest=test[,1],
                  ntree=1000,mtry=4)

rf_error[k] = mean((test$y - rf$test$predicted)^2)
################################################
setTxtProgressBar(pb,k)
}
print(paste("nn error is", mean(nn_error)))
print(paste("series error is", mean(series_error)))
print(paste("rf error is", mean(rf_error)))









