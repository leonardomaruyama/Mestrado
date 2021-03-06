dados=read.table("forca_trabalho.txt",header=T,sep=";",dec=",")
X=dados[,-c(1,2)]
R=cor(X)
S=cov(X)
print(S,3)
avalores=eigen(R)
vp=avalores$values
p=ncol(X)
n=nrow(X)
vexp=100*(avalores$values/p)
print(round(cbind(vp,vexp),4))
plot(1:9,vp,type="b",xlab="Ordem do valor",ylab="Autovalor",lwd=2,col="blue")
abline(h=1,lty=2)
autvet=as.matrix(avalores$vectors,9)
svp=sqrt(vp)

L=cbind(autvet[,1]*svp[1],autvet[,2]*svp[2],autvet[,3]*svp[3],autvet[,4]*svp[4],autvet[,5]*svp[5],autvet[,6]*svp[6],autvet[,7]*svp[7],autvet[,8]*svp[8],autvet[,9]*svp[9])
hi=diag(L%*%t(L))##Comunalidade
psi=diag(diag(R-L%*%t(L)),9,9)##Vari�ncia Espec�fica
saida=cbind(L,hi,1-hi)
round(saida,3)
plot(L[,1],L[,2],lwd=3,ylab="Cargas do F2",xlab="Cargas do F1",ylim=c(-1,1),xlim=c(-1,1))
#abline(h=0,v=0,lty=2,col="blue")
lab=c("AGR","MIN","FAB","FEA","COM","SER","FIN","SSP","TC")
identify(L[,1],L[,2],labels=lab)
##
MRES=R-(L%*%t(L)+psi)

