#Exercício 5.22

#A

dados1=read.table("t1_19.txt",sep=";",header=T)
p=ncol(dados1)
X=as.matrix(dados1,p)
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

# Sem Outlier

dados1=read.table("t1_19_out.txt",sep=";",header=T)
p=ncol(dados1)
X=as.matrix(dados1,p)
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




xbar=mean(dados)
S=cov(dados)
IS=solve(S)
alpha=0.05
LI=numeric(p)
LS=numeric(p)
for(i in 1:p){LI[i]=xbar[i]-sqrt((n-1)*p*qf(1-alpha,p,n-p))*sqrt(S[i,i]/n)
LS[i]=xbar[i]+sqrt((n-1)*p*qf(1-alpha)