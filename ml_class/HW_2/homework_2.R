######################### Question 1 ##############################

rm(list=ls())
library(tidyverse)
library(knitr)
library(leaps)
library(glmnet)
load(file="airline_data_market_level.R")

##### Part a ##### 
load(file="lookup_and_hub_r.R")
lookup_and_hub <- lookup_and_hub %>% 
  transmute(Code,
            Hub = 1*(rowSums(.[4:134])>0))
datam <- left_join(datam,lookup_and_hub,
                   by=c("origin_airport_id"="Code"))
datam <- left_join(datam,lookup_and_hub,
                   by=c("dest_airport_id"="Code"))
datam <- datam %>%
  mutate(Hub = 1*((Hub.x+Hub.y)>0))
datam <- subset(datam,
             select=-c(Hub.x,Hub.y))
rm(lookup_and_hub)

##### Part b #####
load(file="vacations.R")
datam <- left_join(datam,vacations,
                   by=c("origin_city"="origin_cities"))
datam <- left_join(datam,vacations,
                   by=c("dest_city"="origin_cities"))
datam <- datam %>%
  mutate(Vacation_spot= 1*((vacation_spot.x+vacation_spot.y)>0))
datam <- subset(datam,
                select=-c(vacation_spot.x,vacation_spot.y))
rm(vacations)

##### Part c #####
load(file="data_income.R")
datam <- left_join(datam,msa_income,
                   by=c("origin_city"="city"))
datam <- left_join(datam,msa_income,
                   by=c("dest_city"="city"))
datam <- datam %>%
  mutate(Mean_income=sqrt(median_income.x*median_income.y))
datam <- subset(datam,
                select=-c(median_income.x,median_income.y))
rm(msa_income)

##### Part d #####
load(file="slot_controlled.R")
datam <- left_join(datam,slot_controlled,
                   by=c("origin_airport_id"="airport"))
datam <- left_join(datam,slot_controlled,
                   by=c("dest_airport_id"="airport"))
datam <- datam %>%
  mutate(Slot_controlled=1*((slot_controlled.x+slot_controlled.y)>0))
datam <- subset(datam,
                select=-c(slot_controlled.x,slot_controlled.y))
rm(slot_controlled)

##### Part e #####
invisible(datam[with(datam,order("origin_airport_id","dest_airport_id"))])

######################### Question 2 #########################

# Second argument denotes size of training set
set.seed(0)
train=sample(1:nrow(datam),nrow(datam)/2)
test=(-train)

######################### Question 3 #########################

# Help for this part:
# https://tomaztsql.wordpress.com/2016/01/11/playing-with-regression-prediction-and-mse-measure/
lm.fit=lm(num_carriers~Hub+Vacation_spot+Mean_income+Slot_controlled+
            average_distance_m+market_size, data=datam[train,])
print(summary(lm.fit)$adj.r.squared)
lm.pred=predict(lm.fit,data.frame(datam[test,]))
x=mean((datam[test,]$num_carriers-lm.pred)^2)
print(paste0("The mean squared error equals: ",x))

######################### Question 4 #########################

# Note: regular poly requires a model matrix:
lm.poly=lm(num_carriers~polym(Hub,Vacation_spot,Mean_income,Slot_controlled,
                              average_distance_m,market_size,degree=2,raw=TRUE),
           data=datam[train,])
print(summary(lm.poly)$adj.r.squared)
lm.pred=predict(lm.poly,data.frame(datam[test,]))
print(mean((datam[test,]$num_carriers-lm.pred)^2))

######################### Question 5 #########################

# Note: not all square terms are included because of the dummies
models <- regsubsets(num_carriers~polym(Hub,Vacation_spot,Mean_income,Slot_controlled,
                                        average_distance_m,market_size,degree=2,raw=TRUE),
                     data = datam[train,],method = "backward",nvmax=28)
models.summary = summary(models)
# Generate test matrix:
test.mat=model.matrix(num_carriers~polym(Hub,Vacation_spot,Mean_income,Slot_controlled,
                                         average_distance_m,market_size,degree=2,raw=TRUE),
                      data=datam[test,])
# By adjr2:
coefi=coef(models,id=which.max(models.summary$adjr2))
pred=test.mat[,names(coefi)]%*%coefi
print(mean((datam[test,]$num_carriers-pred)^2))
# By BIC:
coefi=coef(models,id=which.max(which.min(models.summary$bic)))
pred=test.mat[,names(coefi)]%*%coefi
print(mean((datam[test,]$num_carriers-pred)^2))

######################### Question 6 #########################

# Note: model matrices preserve train/test variable
x=model.matrix(num_carriers~Hub+Vacation_spot+Mean_income+Slot_controlled+
               average_distance_m+market_size,data=datam)[,-1]
y=datam$num_carriers

# alpha=0: ridge; alpha=1: lasso
nmse <- function(alp,lam){
  ridge.mod=glmnet(x[train,],y[train],alpha=alp,lambda=lam)
  ridge.pred=predict(ridge.mod,s=lam,newx=x[test,])
  return(mean((ridge.pred-y[test])^2))
}
errors <- matrix(c(nmse(0,0),nmse(0,1),nmse(0,2),
                   nmse(1,0),nmse(1,1),nmse(1,2)),
                 ncol=3,byrow=TRUE)
colnames(errors) <- c("lambda=0","lambda=1","lambda=2")
rownames(errors) <- c("ridge","lasso")
errors <- as.table(errors)

######################### Question 7 #########################

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
bestlam=cv.out$lambda.min
print(bestlam)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y[test])^2)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
bestlam
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
lasso.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y[test])^2)

######################### Question 8 #########################

# I think that the polynomial model is probably the most
# appropriate choice for this data. Neither the ridge nor
# the lasso have significant penalty parameters after CV,
# so the linear model is clearly underspecified. However,
# adjr2 and BIC selection for the polynomial model yielded
# worse results, so I would try CV selection for those.

######################### Question 9 #########################

# The code below adds random noise correlated with predictors
# To adjust the train/test split, I change the code in Question 3
# I couldn't get the models going with one input style, so I just
# pasted this code 5 times in the rmd file to make the output
datam$noise1 = datam$average_distance_m + rnorm(nrow(datam),.01)
datam$noise2 = datam$Hub + rnorm(nrow(datam),.01)
datam$noise3 = datam$Mean_income + rnorm(nrow(datam),.01)

##### 9.a.3 #####
lm.fit=lm(num_carriers~Hub+Vacation_spot+Mean_income+Slot_controlled+
            average_distance_m+market_size+noise1+noise2+noise3,
          data=datam[train,])
print(summary(lm.fit)$adj.r.squared)
lm.pred=predict(lm.fit,data.frame(datam[test,]))
print(mean((datam[test,]$num_carriers-lm.pred)^2))

##### 9.a.4 #####
lm.poly=lm(num_carriers~polym(Hub,Vacation_spot,Mean_income,Slot_controlled,
                              average_distance_m,market_size,noise1,noise2,
                              noise3,degree=2,raw=TRUE),
           data=datam[train,])
print(summary(lm.poly)$adj.r.squared)
lm.pred=predict(lm.poly,data.frame(datam[test,]))
print(mean((datam[test,]$num_carriers-lm.pred)^2))

##### 9.a.5 #####
models <- regsubsets(num_carriers~polym(Hub,Vacation_spot,Mean_income,Slot_controlled,
                                        average_distance_m,market_size,noise1,noise2,
                                        noise3,degree=2,raw=TRUE),
                     data = datam[train,],method = "backward",nvmax=54)
models.summary = summary(models)
# Generate test matrix:
test.mat=model.matrix(num_carriers~polym(Hub,Vacation_spot,Mean_income,Slot_controlled,
                                         average_distance_m,market_size,noise1,noise2,
                                         noise3,degree=2,raw=TRUE),
                      data=datam[test,])
# By adjr2:
coefi=coef(models,id=which.max(models.summary$adjr2))
pred=test.mat[,names(coefi)]%*%coefi
print(mean((datam[test,]$num_carriers-pred)^2))
# By BIC:
coefi=coef(models,id=which.max(which.min(models.summary$bic)))
pred=test.mat[,names(coefi)]%*%coefi
print(mean((datam[test,]$num_carriers-pred)^2))

##### 9.a.6 #####
x=model.matrix(num_carriers~Hub+Vacation_spot+Mean_income+Slot_controlled+
                 average_distance_m+market_size+noise1+noise2+noise3,data=datam)[,-1]
y=datam$num_carriers
nmse <- function(alp,lam){
  ridge.mod=glmnet(x[train,],y[train],alpha=alp,lambda=lam)
  ridge.pred=predict(ridge.mod,s=lam,newx=x[test,])
  return(mean((ridge.pred-y[test])^2))
}
errors <- matrix(c(nmse(0,0),nmse(0,1),nmse(0,2),
                   nmse(1,0),nmse(1,1),nmse(1,2)),
                 ncol=3,byrow=TRUE)
colnames(errors) <- c("lambda=0","lambda=1","lambda=2")
rownames(errors) <- c("ridge","lasso")
errors <- as.table(errors)

##### 9.a.7 #####
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
bestlam=cv.out$lambda.min
print(bestlam)
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=bestlam)
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y[test])^2)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
bestlam
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
lasso.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y[test])^2)

# r-squared increases, test MSE decreases as noise increases,
# training part of sample decreases

######################### Question 10 #########################

train=sample(1:nrow(datam),nrow(datam)/2)
test=(-train)

x=model.matrix(num_carriers~Hub+Vacation_spot+Mean_income+Slot_controlled+
                 average_distance_m+market_size,data=datam[train,])[,-1]
y=datam[train,]$num_carriers

models <- regsubsets(y~x,data=datam[train,],method="backward")
summary(models)
summary(models)$rsq
coef(models,id=6)