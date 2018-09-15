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

#Esse comando nos fornece um diagrama de dispersão, da avaliação de normalidade bivariada,
tal dispersão nos indica falta de normalidade, uma vez que a nuvem de pontos não se ajustou a
uma reta.

library(scatterplot3d)
scatterplot3d(Ganho_Liquido,Ganho_Bruto,Patrimonio,main="Diagrama de dispersão")
pairs(~Ganho_Liquido+Ganho_Bruto+Patrimonio,data=X, main="Matriz de dispersão")

#Esse comando nos permite observar 2 a 2, a dispersão dos dados, o que nos permite observar, que 
dentre os dados considerados, ganho bruto x patrimônio são os que aprensentam maior correlação
uma vez que a núvem de pontos, é a que mais se ajusta a uma reta, sendo o patrimônio x ganho liquido
o que menos tem correlação, uma vez que a dispersão dos dados está em formato de circunferência.

boxplot(Ganho_Liquido)
boxplot(Ganho_Bruto)

#Esses dois comandos nos mostra que há um outlier acima dos demais valores. Enquanto o quartil acima e abaixo da mediana 
(2 e 3 quartil) apresentam tamanhos similares, o primeiro quartil é bastante pequeno em relação, principalmente ao quarto.
Sendo que a principal diferença entre os dois está na ordem dos valores observados, bem mais que na relação entre a dispersão
 dos valores (relação entre os quartis e mediana).

boxplot(Patrimonio)

#Primeiro, segundo e quarto quartil são bem menores que o terceiro, não apresenta outliers.

boxplot(X)

#Esse comando nos permite observar o boxplot (gráfico que mostra a dispersão dos dados por meio de mediana, e quartil),
de todas as séries de dados, de forma que se permite observar o grau de grandeza dos valores das séries (mediana e quartis) 
uma em relação às outras.

