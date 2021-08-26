#clear workspace
rm(list=ls())

#set working directory
setwd("~/Econ_690")

#load in market level data
load(file="PS4_mkt.R")
######################################################################
 x = datam[,c("average_price","average_distance","average_passengers","nonstop_miles","num_carriers",
             "hhi")]

#kmeans with 2 clusters
nk = 2
km = kmeans(x,centers=nk,iter.max=1000)

c1 = colMeans(x[which(km$cluster==1),])
c2 = colMeans(x[which(km$cluster==2),])
c1; c2

#kmeans with 4 clusters
nk = 4
km = kmeans(x,centers=nk,iter.max=1000)

#hierarchical clustering
ncut = 4
d = dist(x, method = "euclidean") # distance matrix
fit = hclust(d, method="ward") 
plot(fit)
groups = cutree(fit, k=ncut)

#check average characteristics of clusters
clusters  = c(1,2,3,4)
w = 1
ckm = x[which(groups==clusters[w]),]
cag = x[which(km$cluster==clusters[w]),]
colMeans(ckm)
colMeans(cag)
