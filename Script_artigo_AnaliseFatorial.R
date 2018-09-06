dados=read.table("samu_dados3.txt",header=T,sep=";")
x=dados[,-c(2,1)]
lab=dados[,1]
dados=scale(x)

sapply(x,summary)
sapply(x,sd)

cor(x)

fit<-princomp(x, cor=TRUE)

summary(fit)

loadings(fit)

plot(fit,type="l",main="Variâncias", lty=2)
abline(1,0,lty=2,col="red")

fit$scores
biplot(fit)

require("psych")

cortest.bartlett(x)

fit<-principal(x, nfactors=2, rotate="none")
fit

fit<-principal(x, nfactors=2, rotate="varimax")
fit
print(fit,3)
require("GPArotation")

fit<-fa(x, 2, rotation="varimax")
print(fit, digits=2, cutoff=.0, sort=TRUE)

fit<-fa(x, nfactors=2, rotate="none")
print(fit, digits=2, cutoff=.0, sort=TRUE)

fit=principal(x,nfactors=2,rotate="varimax",scores=T)
scores.bart=fit$scores
plot(scores.bart[,1],scores.bart[,2],pch=16,cex=.7,xlab="Scores Fator 1",ylab="Scores Fator 2",ldw=3,col="blue")
identify(scores.bart[,1],scores.bart[,2],labels=lab)
abline(0,0,lty=2,col="red")

load<-fit$loadings
load

fit<-factanal(x, 2, rotation="none")

plot(load,type="n")
text(load, labels=names(x), cex=1)