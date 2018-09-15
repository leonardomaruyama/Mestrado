bivar=read.table("Dados_empresas.txt",sep=";",dec=",",header=T)
bivar
attach(bivar)

matrizbivar=data.frame(Ganho_Bruto,Ganho_Liquido,Patrimonio)
matrizbivar
X=as.matrix(matrizbivar,3)
X
vmed=apply(X,2,mean)
S=cov(X)
n=nrow(X)
p=ncol(X)
dquad=numeric(n)
for (i in 1:n) {dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,]-vmed)}
prop=ppoints(n)
qui=qchisq(prop,p)

plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")
abline(0,1)

#Esse comando nos fornece um diagrama de dispers�o, da avalia��o de normalidade bivariada,
tal dispers�o nos indica falta de normalidade, uma vez que a nuvem de pontos n�o se ajustou a
uma reta.

library(scatterplot3d)
scatterplot3d(Ganho_Liquido,Ganho_Bruto,Patrimonio,main="Diagrama de dispers�o")
pairs(~Ganho_Liquido+Ganho_Bruto+Patrimonio,data=X, main="Matriz de dispers�o")

#Esse comando nos permite observar 2 a 2, a dispers�o dos dados, o que nos permite observar, que 
dentre os dados considerados, ganho bruto x patrim�nio s�o os que aprensentam maior correla��o
uma vez que a n�vem de pontos, � a que mais se ajusta a uma reta, sendo o patrim�nio x ganho liquido
o que menos tem correla��o, uma vez que a dispers�o dos dados est� em formato de circunfer�ncia.

boxplot(Ganho_Liquido)
boxplot(Ganho_Bruto)

#Esses dois comandos nos mostra que h� um outlier acima dos demais valores. Enquanto o quartil acima e abaixo da mediana 
(2 e 3 quartil) apresentam tamanhos similares, o primeiro quartil � bastante pequeno em rela��o, principalmente ao quarto.
Sendo que a principal diferen�a entre os dois est� na ordem dos valores observados, bem mais que na rela��o entre a dispers�o
 dos valores (rela��o entre os quartis e mediana).

boxplot(Patrimonio)

#Primeiro, segundo e quarto quartil s�o bem menores que o terceiro, n�o apresenta outliers.

boxplot(X)

#Esse comando nos permite observar o boxplot (gr�fico que mostra a dispers�o dos dados por meio de mediana, e quartil),
de todas as s�ries de dados, de forma que se permite observar o grau de grandeza dos valores das s�ries (mediana e quartis) 
uma em rela��o �s outras.

