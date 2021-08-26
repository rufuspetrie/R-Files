######################### Part 1 #########################

library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
setwd("U:/HW_1")
data = read.csv(file="DB1B_MARKET.csv", header=TRUE, sep=",")
attach(data)

######################### Part 2 #########################

data = subset(data,TK_CARRIER_CHANGE==0)
data = subset(data,MARKET_FARE>=25 & MARKET_FARE<=2500)
data = data %>% group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID) %>% 
  mutate(route_passengers=sum(PASSENGERS))
data = subset(data,route_passengers>1800)

######################### Part 3 #########################

carrier_data = data %>% group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID,TICKET_CARRIER) %>% 
  summarise(mean_price=mean(MARKET_FARE),
            passenger_count=sum(PASSENGERS),
            mean_distance=mean(MARKET_DISTANCE))
attach(carrier_data)

HHI = carrier_data %>% group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID) %>%
  summarise(HHI=(sum((passenger_count/sum(passenger_count))^2)))

market_data = data %>% group_by(ORIGIN_AIRPORT_ID,DEST_AIRPORT_ID) %>% 
  summarise(Mean_Price=mean(MARKET_FARE),
            Mean_Distance=mean(MARKET_DISTANCE),
            Firms=n_distinct(TICKET_CARRIER))

market_data=left_join(market_data,HHI,
  by=c("ORIGIN_AIRPORT_ID"="ORIGIN_AIRPORT_ID","DEST_AIRPORT_ID"="DEST_AIRPORT_ID"))

get(load("~/populations.R")) 
market_data=left_join(market_data,populations,
  by=c("ORIGIN_AIRPORT_ID"="ORIGIN_AIRPORT_ID","DEST_AIRPORT_ID"="DEST_AIRPORT_ID"))
market_data = market_data %>% 
  mutate(market_size=sqrt(population_origin*population_dest))

######################### Part 4 #########################

voi = c("mean_price","passenger_count","mean_distance")
summary(carrier_data[vois])

VOI = c("Mean_Price","Mean_Distance","Firms","HHI","market_size")
summary(market_data[VOIS])

ggplot(market_data, aes(x=HHI, y=Mean_Price)) + 
  geom_point() + 
  geom_smooth(method="lm",se=FALSE) +
  ggtitle("Mean Price vs. HHI")

ggplot(market_data) + 
  geom_histogram(aes(x=Mean_Price,y=..density..), position="identity",color="white") + 
  geom_density(aes(x=Mean_Price,y=..density..),color="red") +
  ggtitle("Density of Mean Prices")

ggplot(market_data) + 
  geom_histogram(aes(x=HHI,y=..density..), position="identity",color="white") + 
  geom_density(aes(x=HHI,y=..density..),color="red") +
  ggtitle("Density of HHI")

# ggplot(market_data, aes(Mean_Price)) + geom_histogram()
# ggplot(market_data, aes(Mean_Price)) + geom_density()
# ggplot(market_data, aes(HHI)) + geom_histogram()
# ggplot(market_data, aes(HHI)) + geom_density()