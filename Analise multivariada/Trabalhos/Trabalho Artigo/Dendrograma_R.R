dados=read.table("samu_dados.txt",header=T,sep=";")
dados
X=dados[,-c(2,1)]
X
lab=dados[,1]
lab
Z=scale(X) ##Padroniza as obs
D=dist(X,method='euclidean', diag=TRUE, upper=TRUE) #matriz de distancia
D
D1=dist(Z,method='euclidean', diag=TRUE, upper=TRUE) #matriz de distancia
D1
require(graphics)
layout(rbind(c(0,1,1,0), c(2,2,3,3)))
par(mai=c(0.7,0.65,0.30,0.05))
seg=hclust(D1, method="single")
seg1=hclust(D1, method="complete")
seg2=hclust(D1, method="average")
plot(seg,label=lab,hang=-1,ann=F)
title(main="Dendrograma-Vizinho mais próximo", ylab="Tempo atendimento entre regiões",xlab="")
rect.hclust(seg,k=6,border="red")
plot(seg1,label=lab,hang=-1,ann=F)
title(main="Dendrograma-Vizinho mais longe", ylab="Tempo atendimento entre regiões",xlab="")
rect.hclust(seg1,k=6,border="red")
plot(seg2,label=lab,hang=-1,ann=F)
title(main="Dendrograma-Vizinho mais média", ylab="Tempo atendimento entre regiões",xlab="")
rect.hclust(seg2,k=6,border="red")


dados=read.table("samu_dados.txt",header=T;sep=";")
#Z=scale(dados)
## Ward Hierarchical Clustering
d<-dist(Z,method="euclidean") #distance matrix
fit<-hclust(d, method="ward")
plot(fit)
groups <- cutree(fit, k=3) #cut tree into 3 clusters
#draw dendrogram with red borders around the 3 clusters
rect.hclust(fit,k=3,border="red")
par(mfrow=c(2,2)) #para mais gráficos
##
lab=dados[,1]
seg = hclust(dist(dados,method='euclidean'),method="ward")
plot(seg,label=lab,hang=-1)

pclust(seg,labels=lab,main="Dendrograma-Vizinho mais próximo", ylab="distância entre países",xlab="")
rect.hclust(seg,k=4,border="red")