dados=read.table("samu_dados.txt",header=T,sep=";")
X=dados[,-c(2,1)]
X
lab=dados[,1]
lab
dados=scale(X)
(c1<-kmeans(X,4))
plot(X,col=c1$cluster)
identify(X[,1],X[,2],labels=1:nrow(X))
points(cl$centers, col=1:2, pch=8, cex=2)

dados=read.table("samu_dados.txt",header=T,sep=";")
X=dados[,-c(2,1)]
X=scale(X)
Z
lab=dados[,1]
lab
(c1<-kmeans(X,2))
cbind(Z,c1$cluster)

#K-Means Clustering with 5 clusters
fit<-kmeans(X,3)

#Cluster Plot against lst 2 principal components

#vary parameters for most readable graph

library(cluster)
clusplot(X, fit$cluster, color=TRUE, shade=FALSE, labels=2, lines=0)
###
-Means Cluster Analysis
