dados=read.table("t5_11.txt",sep=";",header=T)
x=as.matrix(dados,1)
dados
xbar=apply(x,2,mean)
xbar
S=cov(dados)
IS=solve(S)
n=nrow(dados)
p=ncol(dados)
alpha=0.05
mu=c(2000,10000)

#RC=function(mu=c(1,1)){
R=n*t(xbar-mu)%*%IS%*%(xbar-mu)
QRC=(n-1)*p*qf(1-alpha,p,n-p)/(n-p)
SR=cbind(R,rc=QRC)
if(R>QRC) {print("rejeita-se H0")} 
if(R<=QRC) {print("não rejeita-se H0")}
SR

library(ellipse)
plot(ellipse(S/n,centre=xbar,t=sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))),
type="l",lwd=2,ylab=expression(mu[2]),xlab=expression(mu[1]),ylim=c(7500,10100))
points(xbar[1],xbar[2],col="red",lwd=3)
points(2000,10000,col="blue",lwd=3)

p=ncol(dados)
X=as.matrix(dados,p)
vmed=apply(X,2,mean)
S=cov(X)
n=nrow(X)
p=ncol(X)
dquad=numeric(n)
for(i in 1:n) { dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,] - vmed)}
dquad=sort(dquad)
prop=ppoints(n)
qui=qchisq(prop,p)
plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")
abline(0,1)

