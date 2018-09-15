#exercicio 6.18
dados=read.csv("t6_9.csv",sep=";",header=T)
Z=as.matrix(dados[,-4],3)
g=(dados[,4])
hettelingsT2(Z~g,mu=c(0,0,0))

x=dados[1:24,-4]
n1=nrow(x)
y=dados[25:48,-4]
n2=nrow(y)
xbar1=mean(x)
S1=cov(x)
xbar2=mean(y)
S2=cov(y)
p=ncol(x)
DIF=xbar1-xbar2
Sp=((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)
alpha=0.05
c2=(n1+n2-2)*p*qf(1-alpha,p,n1+n2-p-1)/(n1+n2-p-1)

LI

