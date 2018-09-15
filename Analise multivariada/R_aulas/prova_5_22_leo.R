#Exercício 5.22

dados=read.table("t1_19.txt",sep=";",header=T)
p=ncol(dados)
X=as.matrix(dados,p)
vmed=apply(X,2,mean)
S=cov(X)
n=nrow(X)
p=ncol(X)
dquad=numeric(n)
for(i in 1:n){dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,]-vmed)}
dquad=sort(dquad)
prop=ppoints(n)
qui=qchisq(prop,p)
plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")
abline(0,1)
######

dados=read.table("t1_19_m.txt",sep=";",header=T)
p=ncol(dados)
X=as.matrix(dados,p)
vmed=apply(X,2,mean)
S=cov(X)
n=nrow(X)
p=ncol(X)
dquad=numeric(n)
for(i in 1:n){dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,]-vmed)}
dquad=sort(dquad)
prop=ppoints(n)
qui=qchisq(prop,p)
plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")
abline(0,1)
######

dados=read.table("t1_19.txt",sep=";",header=T)
x=as.matrix(dados,1)
dados
xbar=apply(x,2,mean)
xbar
S=cov(dados)
IS=solve(S)
n=nrow(dados)
p=ncol(dados)
alpha=0.05
LI=numeric(p)
LS=numeric(p)

for(i in 1:p) {LI[i]=xbar[i]-sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))*sqrt(S[i,i]/n)
LS[i]=xbar[i]+sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))*sqrt(S[i,i]/n)}

# Intervalo T2-HOTT
IC=cbind(LI,LS)
IC

LIB=numeric(p)
LSB=numeric(p)

for(i in 1:p) {LIB[i]=xbar[i]-sqrt(qt(1-alpha/(2*p),n-1))*sqrt(S[i,i]/n)
LSB[i]=xbar[i]-sqrt(qt(1-alpha/(2*p),n-1))*sqrt(S[i,i]/n)}

# Intervalor de Bonferroni
ICB=cbind(LIB,LSB)
ICB

library(ICSNP)

dados=read.table("t1_19.txt",sep=";",header=T)
HotellingsT2(dados,mu=c(500,50,30),test="f")

# (b) Determinar o comprimento para os eixos
#S=var(dados)
#evalor=eigen(S)
#xbar=apply(x,2,mean)
#xbar
#n=nrow(dados)
#p=ncol(dados)
#CC=sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))*sqrt(evalor$values)
#auto_valor=evalor$vectors
