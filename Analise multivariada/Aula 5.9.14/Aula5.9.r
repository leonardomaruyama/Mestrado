#Exemplo de notas de teste n=10
dados1=read.csv("dados.csv",sep=";",header=T)
x=dados1[,-1]
p=length(x)
med=apply(x,2,mean)
s=var(x)
inv=solve(s)
mo=c(60,60,60,60)
dim(x)
n=nrow(x)
t2=n*t(med-mo)%*%inv%*%(med-mo)
alpha=0.05
t2a=((n-1)*p*qf(1-alpha,p,n-p))/(n-p)
colnames(t2)=c("T2")
S1=cbind(t2,rc=t2a)
if(t2>t2a){print("rejeita-se H0")}
if(t2<=t2a){print("Não Rejeita H0"}
S1

#Teste de hipóteses com ICSNP
#Cargar ICSNP
library(ICSNP)
dados1=read.csv("dados.csv",sep=";",header=T)
x=dados1[,-1]
HotellingsT2(x,mu=c(60,60,60,60),test="f")

##Verificar se o ponto mu=(13,4)' pertence a R(X)
x1=c(11,10,9)
x2=c(2,4,3)
x=cbind(x1,x2)
xbar=apply(x,2,mean)
s=var(x)
invs=solve(s)#inversa da matriz s
p=ncol(x)
n=nrow(x)
evalor=eigen(s)
fa=qf(0.05,2,1,lower.tail=F)
mu=c(13,4)

#RC=function(mu=c(13,4)){
t2=n*t(xbar-mu)%*%invs%*%(xbar-mu)
t2a=(((n-1)*p)/(n-p))*fa
colnames(t2)=c("T2")
S1=cbind(t2,rc=t2a)
if(t2>t2a){print("Rejeita-se H0")}
if (t2<=t2a){print("Não se rejeita H0")}

S1#ellipse
alpha=0.05
#carar ellipse
library(ellipse)
plot(ellipse(s/n,centre=xbar,t=sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))),type="l",lwd=2,ylab=expression(mu[2]),xlab=expression(mu[1]),ylim=c(-13,20))
points(xbar[1],xbar[2],col="red",lwd=3)
points(13,4,col="blue",lwd=3)
text(locator(n=1),c("(13,4)"))

