#Análise dos dados de octanagem
dados=read.table("Octanagem.csv",header=T,dec=".")
attach(dados)
dados

#Resumo dos dados de octanagem
summary(dados)
       x         
 Min.   : 83.40  
 1st Qu.: 88.60  
 Median : 90.40  
 Mean   : 90.53  
 3rd Qu.: 92.20  
 Max.   :100.30 

#Variância dos dados de octanagem
var(dados)
       x
x 8.4402

#Mediana da octanagem
median(x)

#Quantil da resistência
quantile(x, c(0.25,0.5,0.75))
 25%  50%  75% 
88.6 90.4 92.2 

n=length(x)
n


table(x)
 83.4  84.3  85.3  86.7  87.4  87.5  87.6  87.7  87.8  87.9  88.2  88.3  88.4 
    1     2     1     3     1     1     1     1     1     1     1     3     1 
 88.5  88.6  88.7  88.9    89  89.2  89.3  89.6  89.7  89.8  89.9    90  90.1 
    2     2     1     1     1     1     2     1     1     2     2     1     3 
 90.3  90.4  90.5  90.6  90.7  90.8  90.9    91  91.1  91.2  91.5  91.6  91.8 
    1     3     1     1     1     1     1     3     3     2     1     2     2 
 92.2  92.3  92.6  92.7    93  93.2  93.3  93.4  93.7  94.2  94.4  94.7  96.1 
    3     1     1     3     1     1     2     1     1     2     1     1     1 
 96.5  98.8 100.3 
    1     1     1 

freqa=table(x)
freqr=sort(table(x))/n*100
barplot(freqa)

table(x)/n*100

posb=barplot(freqr, xlab="Octanagem", ylab="Porcentagem")
text(posb, freqr/2, freqr)
box()

library(qcc)
pareto.chart(freqa, main="", xlab="Octanagem", ylab="Frequência")

pie(freqr)