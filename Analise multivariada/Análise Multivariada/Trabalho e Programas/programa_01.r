bivar=read.table("Dados_empresas.txt",sep=";",dec=",",header=T)
bivar
attach(bivar)
#plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,800),xlim=c(2000,11000),ylab="Ganho L�quido",xlab="Ganho Bruto",lwd=3.5,col="blue")

matrizbivar=data.frame(Ganho_Bruto,Ganho_Liquido,Patrimonio)
matrizbivar
X=as.matrix(matrizbivar,3)
X
vmed=apply(X,2,mean)
S=cov(X)
n=nrow(X)
p=ncol(X)
dquad=numeric(n)
for (i in 1:n) {dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,]-vmed)}
prop=ppoints(n)
qui=qchisq(prop,p)

plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")
abline(0,1)
#require(car)
#data(Prestige)
#attach(Prestige)

#for (i in 1:5) { hist (Prestige[,i], main="", xlab="") }
#Grafico dispers�o
#Scartterplot3d

library(scatterplot3d)
scatterplot3d(Ganho_Liquido,Ganho_Bruto,Patrimonio,main="Diagrama de dispers�o")
pairs(~Ganho_Liquido+Ganho_Bruto+Patrimonio,data=X, main="Matriz de dispers�o")
boxplot(Ganho_Liquido)
boxplot(Ganho_Bruto)
boxplot(Patrimonio)
boxplot(X)

#Para o Patrim�nio existem mais dados acima da mediana do que abaixo, e n�o apresenta Outlayer