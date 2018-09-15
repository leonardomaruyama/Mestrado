#Índice
dados=read.table("dados1.txt",header=T)
X=as.matrix(dados[,-1],12,3)
S=cov(X)
vp=eigen(S)
vvp=-as.matrix(vp$vectors,3)
y=t(t(vvp)%*%t(X))
y
plot(y[,1],y[,2],lwd=2,col="blue",xlab="1a Componente",ylab="2a Componente",main="Gráfico de escores de 1a 2a componente")
identify(y[,1],y[,2],labels=dados[,1])
p_var=vp$values/sum(vp$values)*100
plot(1:3,p_var,col="blue",lty=1,xlab="Ordem de autovalor",ylab="Variância total 
