require(utils)
require(MASS)
dados=read.table("tabela4.txt",header=T,sep=";")
pairs(dados[c("x1","x2","x3","x4")], main="Diagrama de dispersão ", pch=42, bg=c("red", "yellow")[unclass(dados$grupo)])

#plot(dados[,1], dados[,2], lwd=3)
#raca$dados=as.factor(raca$dados)
fit=qda(grupo~x1+x2+x3+x4, prior=c(0.5,0.5), data=dados)
fit1=predict(fit,data=dados)
table(dados$grupo,fit1$class)
fit2=lda(grupo~x1+x2+x3+x4,prior=c(0.5,0.5)CV=T, method="moment", data=dados)
plot(fit, dimen=1, type="both")
library(klaR)
dados$grupo=as.factor(dados$grupo)
partimat(grupo~.,data=dados,method="qda")
library(MASS)
data(iris)
partimat(Species ~ ., data=iris, method="lda", plot.matrix=TRUE, imageplot=FALSE)
##Not run:
partimat(Species ~ ., data=iris, method="lda", plot.matrix=TRUE, imageplot=FALSE) #take some time ...

##
attach(dados)
m1=apply(dados[grupo==1,-1],2,mean)
m2=apply(dados[grupo==2,-1],2,mean)
n1=length(grupo[grupo==1])
n2=length(grupo[grupo==2])
S1=var(dados[1:n1,-1)
S2=var(dados[n1+1:n2,-1)
S=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
SS1=solve(S)

dm=m1-m2
fd=matrix((dm)%*%SS1,ncol(dados[,-1]),1)
med=matrix(0.5*(m1+m2),1,ncol(dados[,-1]}}
escore
classifa<-ifelse(escore>=mm,1,2)
table(classifica,grupo)

x0=c(-o.45,-0.41,1.09,0.45)
x0%*%fd
by1=matrix(m1,1,4)%*%fd #media da fd grupo 1
by2=matrix(m2,1,4)%*%fd #media da fd grupo 2
T2=(by1-by2)*n1*n2/(n1+n2)
p=length(dm)
F=(n1+n2-p-1)/(p*(n1+n2-2))*T2

pvalue=1-pf(F,p,n1+n2-2-1)
saida=cbind(F,pvalue)
colnames(saida)=c("F_obs","P-valor")
rownames(saida)=c("Valor")
cat("TESTE PARA A IGUALDADE DO VETOR DE MÉDIAS")

saida

library(MASS)
dados=read.table("exemplo1.txt",header=T,sep=";")
pairs(dados[c("x1","x2")],main="Diagrama de dispersão", pch=42, bg=c("red","yellow") [unclass(dados$grupo)])
library(klaR)
dados$grupo=as.factor(dados$grupo)
partimat(grupo~x1+x2,data=dados,method="lda",main="Função discriminante linear")
partimat(grupo~x1+x2,data=dados,method="qda",main="Função discriminante quadrática")
fit=lda(grupo~x1+x2,prior=c(1,1)/2,data=dados)
plot(fit)
fit1=predict(fit,data=dados)
table(dados$grupo,fit1$class)

