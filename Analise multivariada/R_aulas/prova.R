dados1=read.table("t5_11.txt",sep=";",header=T)
dados=as.matrix(dados1,1)
dados
xbar=mean(dados)
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
SR=cbind(R=R,RC=QRC)
if(R>QRC) {print("rejeita-se H0")} 
if(R<=QRC) {print("não rejeita-se H0")}
SR


library(ellipse)
plot(ellipse(s/n,centre=xbar,t=sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))),
type="l",lwd=2,ylab=expression(mu[2]),xlab=expression(mu[1]),ylim=c(-13,20))
points(xbar[1],xbar[2],col="red",lwd=3)
points(2000,10000,col="blue",lwd=3)