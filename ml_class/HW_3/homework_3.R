######################### Question 0 #########################

rm(list=ls())
library(tidyverse)
library(glmnet)
library(knitr)
load(file="market_level.R")
load(file="market_airline_level.R")
set.seed(0)
train=sample(1:nrow(datam),nrow(datam)-1000)
test=(-train)

######################### Question 1 #########################

datama <- datama %>%
  group_by(origin_airport_id,dest_airport_id) %>%
  summarise(AA = 1*('AA' %in% ticket_carrier))
datam <- left_join(datam,datama,
                   by=c("origin_airport_id"="origin_airport_id",
                        "dest_airport_id"="dest_airport_id"))
datam <- datam %>% 
  mutate(competitors = ifelse(AA==1, num_carriers-1, num_carriers))
rm(datama)

x <- data.frame("competitors"=1:10)
lm.fit=lm(AA ~ competitors, data=datam[train,])
lm.preds=predict(lm.fit,newdata=x,type="response")
lm.preds <- data.frame("competitors"=1:10,probability=lm.preds)

######################### Question 2 #########################

lr.fit=glm(AA~competitors, data=datam[train,], family=binomial)
lr.preds=predict(lr.fit,newdata=x,type="response")
lr.preds <- data.frame("competitors"=1:10,probability=lr.preds)

######################### Question 3 #########################

pr.fit=glm(AA~competitors, data=datam[train,], family=binomial(link="probit"))
pr.preds=predict(pr.fit,newdata=x,type="response")
pr.preds <- data.frame("competitors"=1:10,probability=pr.preds)

######################### Question 4 #########################

freq <- datam[train,] %>%
  group_by(competitors) %>%
  summarise(probability = (sum(AA) / n() ))

######################### Question 5 ########################

ggplot() +
  geom_point(aes(x=competitors,y=probability),
            data=lm.preds) +
  geom_point(aes(x=competitors,y=probability),
            data=pr.preds,color='red',alpha=0.5) +
  geom_point(aes(x=competitors,y=probability),
             data=lr.preds,color='blue',alpha=0.5) +
  scale_x_discrete(limits=1:10) +
  ggtitle("Probability of market entry given number of competitors")

######################### Question 6 #########################

x=model.matrix(AA~polym(competitors,average_distance_m,market_size,hub_route,
                        vacation_route,slot_controlled,market_income,degree=2,
                        raw=TRUE),data=datam)[,-1]
y=datam$AA

cv.out=cv.glmnet(x[train,],y[train],alpha=1,family="binomial")
bestlam=(cv.out$lambda.min)

######################### Question 7 #########################

lm.pred=predict(lm.fit,data.frame(datam[test,]))
lm.SE=(sum((datam[test,]$AA-lm.pred)^2))

lr.pred=predict(lr.fit,data.frame(datam[test,]),type="response")
lr.SE=(sum((datam[test,]$AA-lr.pred)^2))

pr.pred=predict(pr.fit,data.frame(datam[test,]),type="response")
pr.SE=(sum((datam[test,]$AA-pr.pred)^2))

vect <- as.vector(freq[2])
datam$prediction=0
for(i in 1:10){
  datam$prediction[datam$competitors==i] <- as.numeric(vect[i,1])
}
bi.SE=(sum((datam[test,]$AA-datam[test,]$prediction)^2))

ll.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
ll.pred=predict(ll.mod,s=bestlam,newx=x[test,])
ll.SE=(sum((ll.pred-y[test])^2))