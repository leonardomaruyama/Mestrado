#Resistência
dados=read.table("Resistencia.csv",header=T,sep=";",dec=".")
attach(dados)
dados
names(dados)

#Resumo dos dados de resitência
summary(Resistencia)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  43.00   46.05   48.15   47.52   48.92   51.00 

#Medida qualitativa quanto a espessura da resistência
summary(as.factor(Espessura))
10 11 12 13 14 15 16 
 3  4  5  1  4  2  1 

#Medida qualitativa quanto ao tipo de cola da resistência
summary(as.factor(Tipo_de_cola))
1 2 3 4 
5 5 5 5 

#Variância da resistência
var(Resistencia)
[1] 4.820289

#Mediana da resistência
median(Resistencia)
[1] 48.15

#Quantil da resistência
quantile(Resistencia, c(0.25,0.5,0.75))
   25%    50%    75% 
46.050 48.150 48.925 


#Proporção da espessura da resistência
prop.table(table(Espessura))
Espessura
  10   11   12   13   14   15   16 
0.15 0.20 0.25 0.05 0.20 0.10 0.05 

#Proporção da espessura da resistência e do tipo de cola
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

#Gráfico de frequência para espessura da resistência
barplot(table(Espessura),
col=c("green","red","yellow","orange","blue","purple","gray"),
space=.8, width=c(.2,.2),
main="Proporção das resistência pela espessura", 
xlab="Espessura", ylab="Proporção de resitência")
text(locator(n=7),c("15%","20%","25%","5%","20%","10%","5%"))

#Gráfico de caixas (boxplot)
boxplot(Resistencia~Espessura, xlab="Resistência", 
col=c("green","red","yellow","orange","blue","purple","gray"))

#Gráfico de pontos
stripchart(Resistencia~Espessura, xlab="Resistência", 
ylab="Espessura", pch=20, method="stack", col=2:4)