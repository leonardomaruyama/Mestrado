## artigo final - Análise Multivariada
library(ellipse)
library(ICSNP)
library(psych)
library(GPArotation)

dados=read.table("tabela.txt", header=T)
x=dados[,-1]
lab=dados[,1]
r=cor(x)
s=cov(x)

## Componentes Principais
vp=eigen(s)
vvp=-as.matrix(vp$vectors, 42)
y=t(t(vvp)%*%t(x))
p_var=vp$values/sum(vp$values)
plot(1:42, p_var, col="blue", lty=1, xlab="Ordem de autovalor", ylab="Variância total explicada")
plot(y[,1], y[,2], lwd=2, col="blue", main="Gráfico de escores das componentes 1 e 2")
identify(y[,1], y[,2], labels=lab)
cor(y,x)
##padronização
pad=function(x1){
(x1-mean(x1))/sd(x1)}
z=apply(x,2,pad)
# Correlação
vp2=eigen(r)
p_var2=vp2$values/sum(vp2$values)
vvp2=-as.matrix(vp2$vectors, 42)
y2=t(t(vvp2)%*%t(z))
plot(1:42, p_var2, col="blue", lty=1, xlab="Ordem de autovalor", ylab="Variância total explicada")
plot(y2[,1], y2[,2], lwd=2, col="blue", main="Gráfico de escores padronizados das componentes 1 e 2")
identify(y2[,1], y2[,2], labels=dados[,1])
cor(y2,x)

## Análise Fatorial
avalores=eigen(r)
vp3=avalores$values
p=col(x)
n=nrow(x)
vexp=100*(avalores$values/p)
print(round(cbind(vp3, vexp), 4))
plot(1:42, vp3, type="b", xlab="Ordem do fator", ylab="Autovalor", lwd=2, col="blue")
abline(h=1, lty=2)
autvet=as.matrix(avalores$vectors, 9)
svp=sqrt(vp3)
fit <- principal(r, nfactors=2, rotate="none")
print(fit, 3)
c1=fit$loadings[,1]
c2=fit$loadings[,2]
plot(c1, c2, lwd=3, ylab="cargas F2", xlab="cargas F1", ylim=c(-1,1), xlim=c(-1,1))
#identify(c1, c2, labels=lab)
fit <- principal(x, nfactors=2, rotate="varimax", scores=T)
scores_bart=fit$scores
plot(scores_bart[,1], scores_bart[,2], pch=20, xlab="Escores F1", ylab="Escores F2")
identify(scores_bart[,1], scores_bart[,2], labels=dados[,1])
abline(v=0.0, h=0.0, lty=2, col="red")


## Agrupamento
z=scale(x)  ##padroniza as observações
d=dist(x, method='euclidean', diag=TRUE, upper=TRUE) ## distâncias
d1=dist(z, method='euclidean', diag=TRUE, upper=TRUE) ## distâncias
require(graphics)
layout(rbind(c(0,1,1,0), c(2,2,3,3)))
par(mai=c(0.7, 0.65, 0.30, 0.05))
seg=hclust(d1, method="single")
seg1=hclust(d1, method="complete")
seg2=hclust(d1, method="average")
plot(seg2, label=lab, hang=-1, ann=F)
title(main="Dendrogama Distancia Media", ylab="distancias entre cidades")
plot(seg, label=lab, hang=-1, ann=F)
title(main="Dendrogama Vizinho Mais Próximo", ylab="distancias entre cidades")
plot(seg1, label=lab, hang=-1, ann=F)
title(main="Dendrogama Vizinho Mais Longe", ylab="distancias entre cidades")
