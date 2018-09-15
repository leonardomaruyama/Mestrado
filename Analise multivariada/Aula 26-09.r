#Índice
dados=read.table("dados1.csv",header=T)
X=as.matrix(dados[,-1],12,3)
S=cov(X)
vp=eigen(S)
vvp=-as.matrix(vp$vectors,3)
y=t(t(vvp)%*%t(X))
y
plot(y[,1],y[,2],lwd=2,col="blue",xlab="1a Componente",ylab="2a Componente",main="Gráfico de escores da 1a 2a componente")
identify(y[,1],y[,2],labels=dados[,1])
p_var=vp$values/sum(vp$values)*100
plot(1:3,p_var,col="blue",lty=1,xlab="Ordem de autovalor",ylab="Variância total explicada")

p_var
cor(y,X)
### função que padroniza
pad=function(x){
(x-mean(x))/sd(x)
}
Z=apply(X,2,pad)
## correlação
R=cor(X)
vp=eigen(R)
p_var=vp$values/sum(vp$values)*100
points(1:3,p_var,xlab="Ordem de autovalor",ylab="Variância total explicada (%)",lwd=3,col="red",lty=2)

#legend(locator(1),c("COV","CORR"),lwd=c("blue","red"))
vvp=-as.matrix(vp$vectors,3)
vvp[,3]=-vvp[,3]
y=t(t(vvp)%*%t(Z))
y
plot(y[,1],y[,2],lwd=2,col="blue",xlab="1a Componente",ylab="2a Componente",main="Gráfico de escores padronizados da 1a 2a Componente")
identify(y[,1],y[,2],labels=dados[,1])
p_var=vp$values/sum(vp$values)*100
p_var
cor(y,X)

# PCA
pca=printcomp(X,cor=FALSE)
pca=printcomp(X,cor=T)
biplot(pca)
summary(pca)
loadings(pca)
coord=pca$scores
plot(coord[,1],coord[,2],lwd=2,col="blue",xlab="1a Componente",ylab="2a Componente",main="Gráfico de escores da 1a 2a componente")
identify(coord[,1],coord[,2],labels=dados[,1])
abline(v=0,h=0,lty=2,col="red")
