##Gráfico do QQ-Chi-Square
p=ncol(dados1)
X=as.matrix(dados1,p)
vmed=apply(dados1,2,mean)
S=cov(X)
n=nrow(X)
p=ncol(X)
dquad=numeric(n)
for(i in 1:n){dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,]-vmed)}
dquad=sort(dquad)
prop=ppoints(n)
qui=qchisq(prop,p)
plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")

xbar=mean(dados)
S=cov(dados)
IS=solve(S)
n=nrow(dados)
p=ncol(dados)
alpha=0.05
LI=numeric(p)
LS=numeric(p)
for(i in 1:p){LI[i]=xbar[i]-sqrt((n-1)*p*qf(1-alpha,p,n-p)/(n-p))*sqrt(S[i,i]/n)
LS[1]=xbar[i]+sqrt((n-1)*p*qf(1-alpha,p,n-p))*sqrt(S[i,i]/n)}
#Intervalo T2-HOTT
IC=cbind(LI,LS)
IC
LIB=numeric(p)
LSB=numeric(p)
for(i in 1:p){LIB[i]=xbar[i]-sqrt(qt(1-alpha/(2*p),n-1))*sqrt(S[i,i]/n)
LSB[i]=xbar[i]+sqrt(qt(1-alpha/(2*p),n-1))*sqrt(S[i,i]/n}
#intervalo de Bonferroni
ICB=cbind(LIB,LSB)
ICB
library (ICSNP)
dados=(read.table("endereço da tabela")
HotellingsT2(dados,mu=c(500,50,30),test="f")

#(b) determinar o comprimento da direções para os eixos
S=var(dados)
evalo=eigen(S)
xbar=mean(dados)
n=nrow(dados)
p=ncol(dados)
#Semi-eixos da elipse
CC=sqrt((n-1)*p*qf(1-alpha,p,n,n-p)/(n-p))*sqrt(evalor$values)
autl_valor=evalor$vectors