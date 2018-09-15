## artigo final - Análise Multivariada

########################   Análise Agrupamento   ########################
dados=read.table("samu_dados3.txt", header=T, sep=";") #Leitura da tabela
x=dados[,-c(2,1)]
lab=dados[,1]
#Z=scale(dados)
z=scale(x)
## Ward Hierarchical Clustering
d<-dist(z,method="euclidean") #distance matrix
fit<-hclust(d, method="ward")
#par(mfrow=c(2,2)) #para mais gráficos

plot(fit,labels=lab,hang=-1)

#groups <- cutree(fit, k=3) #cut tree into 3 clusters
#draw dendrogram with red borders around the 3 clusters
##

seg = hclust(dist(dados,method='euclidean'),method="ward")
plot(seg,label=lab,hang=-1, xlab="", ylab="Distância entre regiões")
abline(60,0 , lty=2,col="red")
rect.hclust(seg,k=5,border="red")

plclust(seg, labels=lab,hang=-1, main="Dendrograma-Vizinho mais próximo", ylab="Distância entre regiões",xlab="")
#rect.hclust(seg,k=4,border="red")


kmeans(z, 5)
