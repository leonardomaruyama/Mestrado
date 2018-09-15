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
clusplot(X, fit$cluster, color=TRUE, plotchar = TRUE, diss = FALSE,
          s.X.2d = mkCheckX(X, diss), shade=FALSE, labels=2, lines=0)
###
-Means Cluster Analysis


clusplot(X, diss = FALSE,
          s.X.2d = mkCheckX(X, diss), stand = FALSE,
          lines = 2, shade = FALSE, color = FALSE,
          labels= 0, plotchar = TRUE,
          col.p = "dark green", col.txt = col.p,
cex = 1, 
cex.txt = cex,
          span = TRUE,
          add = FALSE,
          xlim = NULL, ylim = NULL,
          main = paste("CLUSPLOT(", deparse(substitute(X)),")"),
          sub = paste("These two components explain",
             round(100 * var.dec, digits = 2), "% of the point variability."),
          xlab = "Component 1", ylab = "Component 2")
