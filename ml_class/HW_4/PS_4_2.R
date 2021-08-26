#clear workspace
rm(list=ls())

#set working directory
setwd("~/Econ_690")

#load in market-airline level data
load(file="market_airline_level.R")

#load in market level data
load(file="market_level.R")

#load in packages
require("tidyverse")
require("LiblineaR")
require("glmnet")

#specify carrier
carrier = "AA"

ntest = round(nrow(datam)/2)
#############################################
#Start by looking at American Airlines
market_airline_only = datama[,c("origin_airport_id","dest_airport_id","ticket_carrier")]

market_airline_only_m = market_airline_only %>%
  group_by(origin_airport_id,dest_airport_id) %>%
  nest()

market_airline_only_m$carrier_in = unlist(lapply(market_airline_only_m[["data"]],function(m){
  return(1*(carrier%in%unlist(m[,"ticket_carrier"])))
}))
#############################################
datam = merge(datam,market_airline_only_m[,c("origin_airport_id","dest_airport_id","carrier_in")])

datam$num_competitors = datam$num_carriers - 1

set.seed(0)

test_ind = sample.int(nrow(datam),ntest)

datam_tr = datam[-test_ind,]

#lpm
lpm = lm(carrier_in~num_competitors,data=datam_tr)
datam_tr$lpm_fitted_values = predict(lpm,datam_tr)

#logit
logit = glm(carrier_in~num_competitors,data=datam_tr,family=binomial(link="logit"))
datam_tr$logit_fitted_values = predict.glm(logit,datam_tr,type="response")

#probit
probit = glm(carrier_in~num_competitors,data=datam_tr,family=binomial(link="probit"))
datam_tr$probit_fitted_values = predict.glm(probit,datam_tr,type="response")

#nonparametric estimates
nonpar = NULL
for (n in as.numeric(levels(as.factor(datam_tr$num_competitors)))) {
  nonpar = rbind(nonpar,c(n,sum(datam_tr$carrier_in[which(datam_tr$num_competitors==n)]==1)/
                            length(which(datam_tr$num_competitors==n))))
}
colnames(nonpar) = c("num_competitors","nonpar")

datam_tr = merge(datam_tr,nonpar,by=c("num_competitors"))

#plot
################################################
d = data.frame(rep(datam_tr$num_competitors,4),
               c(datam_tr$lpm_fitted_values,datam_tr$logit_fitted_values,datam_tr$probit_fitted_values,
                 datam_tr$nonpar),
               c(rep("lpm",nrow(datam_tr)),rep("logit",nrow(datam_tr)),rep("probit",nrow(datam_tr)),
                 rep("nonpar",nrow(datam_tr))))
colnames(d) = c("num_competitors","probabilities","model")
ggplot(d,aes(x=num_competitors,y=probabilities,color=model)) + geom_point() 
################################################


x =poly(as.matrix(datam[,c("num_competitors","average_distance_m",
                           "market_size","hub_route","vacation_route",
                           "slot_controlled","market_income")]),degree=2,raw=T)

y = as.matrix(datam$carrier_in)

x_tr = x[-test_ind,]; y_tr = y[-test_ind]
x_te = x[test_ind,]; y_te = y[test_ind]

#########
cv.out <- cv.glmnet(x_tr,y_tr,alpha=1,family="binomial",type.measure = "mse")
model = glmnet(x_tr,y_tr,family="binomial")

#average squared prediction error on test set
mean((datam[test_ind,"carrier_in"] - predict(lpm,datam[test_ind,]))^2)
mean((datam[test_ind,"carrier_in"] - predict.glm(logit,datam[test_ind,],
                                                 type="response"))^2)
mean((datam[test_ind,"carrier_in"] - predict.glm(probit,datam[test_ind,],
                                                 type="response"))^2)
mean((datam[test_ind,"carrier_in"] -
        sapply(datam[test_ind,"num_competitors"],function(x){
          return(nonpar[,2][which(nonpar[,1]==x)])
        })
)^2
)

mean((y_te - predict(model,data.matrix(x_te),type="response",
                     s=cv.out$lambda.min))^2)
#############################################
#Random forests
require("randomForest") #load in random forests package

rf = randomForest(x=x_tr,y=y_tr,
                  xtest=x_te,ytest=y_te,
                  ntree=1000)

rf_error = mean((y_te - rf$test$predicted)^2); rf_error
################################################


