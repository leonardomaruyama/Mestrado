#Resist�ncia
dados=read.table("Resistencia.csv",header=T,sep=";",dec=".")
attach(dados)
dados
names(dados)

#Resumo dos dados de resit�ncia
summary(Resistencia)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  43.00   46.05   48.15   47.52   48.92   51.00 

#Medida qualitativa quanto a espessura da resist�ncia
summary(as.factor(Espessura))
10 11 12 13 14 15 16 
 3  4  5  1  4  2  1 

#Medida qualitativa quanto ao tipo de cola da resist�ncia
summary(as.factor(Tipo_de_cola))
1 2 3 4 
5 5 5 5 

#Vari�ncia da resist�ncia
var(Resistencia)
[1] 4.820289

#Mediana da resist�ncia
median(Resistencia)
[1] 48.15

#Quantil da resist�ncia
quantile(Resistencia, c(0.25,0.5,0.75))
   25%    50%    75% 
46.050 48.150 48.925 


#Propor��o da espessura da resist�ncia
prop.table(table(Espessura))
Espessura
  10   11   12   13   14   15   16 
0.15 0.20 0.25 0.05 0.20 0.10 0.05 

#Propor��o da espessura da resist�ncia e do tipo de cola
prop.table(table(Espessura,Tipo_de_cola))
         Tipo_de_cola
Espessura    1    2    3    4
       10 0.00 0.05 0.05 0.05
       11 0.00 0.05 0.10 0.05
       12 0.10 0.10 0.00 0.05
       13 0.05 0.00 0.00 0.00
       14 0.10 0.05 0.05 0.00
       15 0.00 0.00 0.05 0.05
       16 0.00 0.00 0.00 0.05

#Gr�fico de frequ�ncia para espessura da resist�ncia
barplot(table(Espessura),
col=c("green","red","yellow","orange","blue","purple","gray"),
space=.8, width=c(.2,.2),
main="Propor��o das resist�ncia pela espessura", 
xlab="Espessura", ylab="Propor��o de resit�ncia")
text(locator(n=7),c("15%","20%","25%","5%","20%","10%","5%"))

#Gr�fico de caixas (boxplot)
boxplot(Resistencia~Espessura, xlab="Resist�ncia", 
col=c("green","red","yellow","orange","blue","purple","gray"))

#Gr�fico de pontos
stripchart(Resistencia~Espessura, xlab="Resist�ncia", 
ylab="Espessura", pch=20, method="stack", col=2:4)