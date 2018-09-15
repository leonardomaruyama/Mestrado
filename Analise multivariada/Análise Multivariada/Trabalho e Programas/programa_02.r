dados1=read.csv("dados.csv",sep=";",header=T)
dados1
x=dados1[,-1]
x
p=length(x)
p
med=apply(x,2,mean)
med
s=var(x)
s
inv=solve(s) 				#determina a matriz inversa
inv
mo=c(60,60,60,60) 			#determina Mi0
dim(x)					#dimensão da matriz
n=nrow(x)
n
t2=n*t(med-mo)%*%inv%*%(med-mo)	#Formula definida por Hottelling
t2
alpha=0.05
t2a=((n-1)*p*qf(1-alpha,p,n-p))/(n-p) #Classifica o nível de significância
t2a
colnames(t2)=c("T2")
S1=cbind(t2,rc=t2a)
S1
if(t2>t2a) {print("rejeita-se H0")}
if(t2<=t2a) {print("não rejeita-se H0")}
S1
#teste de hipóteses com ICSNP
#carga ICSNP
library(ICSNP)
dados1=read.csv("dados.csv",sep=";",header=T)
x=dados1[,-1]
HotellingsT2(x,mu=c(60,60,60,60),test="f")

##verificar se o ponto mu=(13,4)' pertence a R(X)
x1=c(11,10,9)
x2=c(2,4,3) 
x=cbind(x1,x2)
x
xbar=apply(x,2,mean)
xbar
s=var(x)
invs=solve(s) #inversa da matriz s
p=ncol(x)
n=nrow(x)
evalor=eigen(s)
fa=qf(0.05,2,1,lower.tail=F)
mu=c(13,4)
#RC=function(mu=c(13,4)){
t2=n*t(xbar-mu)%*%invs%*%(xbar-mu)	#Formula definida por Hottelling
t2
t2a=(((n-1)*p)/(n-p))*fa 
t2a
colnames(t2)=c("T2")
S1=cbind(t2,rc=t2a)
if(t2>t2a) {print("rejeita-se H0")}
if(t2<=t2a) {print("não rejeita-se H0")}
S1 #ellipse
alpha=0.05
#carga ellipse
library(ellipse)
plot(ellipse(s/n,centre=xbar,t=sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))),type="l",lwd=2,ylab=expression(mu[2]),xlab=expression(mu[1]),ylim=c(-13,20))
points(xbar[1],xbar[2],col="red",lwd=3)
points(13,4,col="blue",lwd=3)
text(locator(n=1),c("(13,4)"))
