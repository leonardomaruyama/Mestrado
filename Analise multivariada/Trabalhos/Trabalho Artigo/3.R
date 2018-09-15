dados=read.table("dados.csv",header=T,sep=";")
X=dados[,-c(1,1)]
X
lab=dados[,1]
lab
dados=scale(X)
(c1<-kmeans(X,5))
plot(X,col=c1$cluster)
identify(X[,1],X[,2],labels=1:nrow(X))
points(cl$centers, col=1:2, pch=8, cex=2)

dados=read.table("dados.csv",header=T,sep=";")
x=dados[,-c(1,1)]
x=scale(x)
lab=dados[,1]
lab
(c1<-kmeans(x,3))
cbind(x,c1$cluster)
a=cbind(x,c1$cluster)


#K-Means Clustering with 5 clusters
fit<-kmeans(x,3)

#Cluster Plot against lst 2 principal components

#vary parameters for most readable graph
a=x[,6]
b=x[,7]

library(cluster)
clusplot(x, fit$cluster, color=T, cex=.7, shade=FALSE,col.p=rep(c("black","black","black"),c(1,1,1)),plotchar=F, labels=3, pch=16, lines=0, lty=2, span=T)
identify(x[,6], x[,7], fit$cluster, labels=lab)
###
-Means Cluster Analysis
