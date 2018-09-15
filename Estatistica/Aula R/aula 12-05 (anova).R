####Autora: Profa: Gladys D.C. Barriga################################
####Ano de 2009#####################################################
 setwd("I:R_Aulas")
getwd()
dados = read.table("dadosA.txt",  dec = ",",header=T)   #leitura de Dados em arquivo externo
attach(dados)
names(dados)
## Medidas de posição e dispersão
mean(Salario)
var(Salario)
sd(Salario)
median(Salario)
quantile(Salario,c(0.25,0.5,0.75))
### Mudulo que determinas as Medidas de Posição e dispersão###################
meuResumo <- function(x) {
 s <- c(length(x),mean(x, na.rm = T),median(x, na.rm = T),
 sd(x, na.rm = T), var(x, na.rm = T), length(which(is.na(x))))
 names(s) <- c("n","média","mediana" , "desvio Padrão", "variância","NA")
 return(s)
 }
meuResumo(Salario)

### Tabela de frequencia para dados qualitativos ##################
table(EsT_Civ) 
table(EsT_Civ,G_Int)        
table( EsT_Civ,G_Int, R_proced)
## Tabela de proporções
prop.table(table(EsT_Civ,G_Int))
prop.table(table(EsT_Civ,G_Int),1)
prop.table(table(EsT_Civ,G_Int),2)
# Gráfico de Barras
barplot(table(EsT_Civ), col=c("green","red"),
ylim=c(0,25), space=.8, width=c(.2,.2),
main="Proporção de funcionario por estado civil",
xlab="Estado Civil", ylab="Proporção de funcionários")
text(locator(n=2),c("56%","44%"))
## ou
barplot(table(EsT_Civ), ylim=c(0,25), space=.8, width=c(.2,.2),col=c("green","red"),legend=c("56%","44%"),main="Proporção de funcionario por estado civil",
xlab="Estado Civil", ylab="Proporção de funcionários")
## Diagrama Circular ou Pizza
pie(table(G_Int) ,col=c("red","yellow","green"),labels=c("1 Grau (33,3%)","2 Grau(50,0%)", "Superior(15,7%)"))
title(main="Distribuição dos Funcionários por Grau de Instrução")

pie(table(EsT_Civ),col=c("red","yellow"),labels=c("Casado (56%)","Solteiro(44%)"
,main="Distribuição dos Funcionários por Estado Civil")  )
## Grafíco de dados discretos
plot(table(N_filhos),xlab="Número de filhos",ylab="Número de Funcionários")

#Gráfico de Histograma#
hist(Salario, breaks =c(4,8,12,16,20,24),xlab="salário",ylab="Densidade",prob=T,col="yellow",
 main="Dist. de salário dos funcionários  da empresa",ylim=c(0,0.1),
labels=c("27,8%","33,3%","22,2%","13,8%","2,8%"))  
## Alternativamente
h=hist(Salario, breaks =c(4,8,12,16,20,24),xlab="Salário",ylab="Número de Funcionários",col="yellow",
 main="Dist. de salário dos funcionários  da empresa",axes=F, right = F,
labels=c("27,8%","33,3%","22,2%","13,8%","2,8%"))
 
 axis(1,h$breaks)
 axis(2,h$counts)

## Tabela de Frequencia
TDF=hist(Salario, breaks =c(4,8,12,16,20,24), right = F,plot=F)
fabs=TDF$counts    #Frequencia absoluta
fr=fabs/length(Salario) # Frequencia relativa
saida=cbind(fabs,fr)
dimnames(saida)=list(c("4|-8","8|-12","12|-16","16|-20","20|-24"),c("f", "fr"))
saida 
## Tabela de Frequência usando a função cut
fabs=table(cut(Salario, breaks=seq(4,24,4), labels=c("4|-8","8|-12","12|-16","16|-20","20|-24"), right=F))
fr=fabs/length(Salario)
saida=cbind(fabs,fr)
dimnames(saida)=list(c("4|-8","8|-12","12|-16","16|-20","20|-24"),c("f", "fr"))
saida
## Histograma e TDF usando a library agricolae
library(agricolae)
h<-hist(Salario, breaks=c(4,8,12,16,20,24), prob=TRUE,right = F)
plot(h,col="yellow",xlim=c(0,26),labels=c("27,8%","33,3%","22,2%","13,8%","2,8%"),
xlab="Salário",ylab="Número de funcionários", main="Dist. de salário dos funcionários  da empresa")
polygon.freq(h,col="blue")
table.freq(h)           ##
stat.freq(h)
graph.freq(h)
h<- graph.freq(Salario, breaks=c(4,8,12,16,20,24),axes=FALSE, frequency=1, ylab="frequency",col="yellow")
axis(1,h$breaks)
axis(2,h$counts)

 ## Gráfico de Freq. Acumulada relativa (ogiva)
 ojiva.freq(h,type="b",col="green",main="Ogiva",lwd=3,xlab="Salario",ylab="Probabilidade")

## Dados de Octanagem
dados=scan("comb.txt", dec=",")  #Montgomery  e Runger pag. 124
 summary(dados)
 quantile(dados, prob=c(0.1, 0.4, 0.7, 0.9))
 stripchart(dados,xlab="Octanagem", pch= 20, method ="stack") 
 abline(h = 0.98)
 points(mean(dados), 0.93, pch = 17, col = "red", cex = 3)

hist(dados, main = "", xlab = "Octanagem", ylab = "Densidade", freq = FALSE, nclass = 6, ylim=c(0,0.16))
lines(density(dados), col = "blue")
par(mfrow=c(2,2))
hist(dados, main = "k=25", xlab = "Octanagem", ylab = "Densidade", freq = FALSE, nclass =25,col="yellow")
hist(dados, main = "k=15", xlab = "Octanagem", ylab = "Densidade", freq = FALSE, nclass =15 , col="green")
hist(dados, main = "k=5", xlab = "Octanagem", ylab = "Densidade", freq = FALSE, nclass =5 ,col="red")
hist(dados, main = "Sturges", xlab = "Octanagem", ylab = "Densidade", freq = FALSE, breaks="Sturges", col="purple" )
#
dados <- read.table("Tabela_12_3.txt",header=T )  # Montgomere pag. 307
names(dados)
attach(dados)
boxplot(Ris~Tec, ylab="Resistência")
##
dados=read.table("Tabela_13_1.txt",dec=",",header=T)     #Dados do livro Montgomery pag 307 
attach(dados)
names(dados)
Concentrac=as.factor(dados[,2])
Resist=dados[,1]
boxplot(Resist~Concentrac, ylab="Resistência (psi)", xlab="Concentração de madeira lei (%)")
 ######## Diagrama de pontos
 stripchart(Salario, xlab="Salario", pch= 20, method ="stack")
 abline(h=0.97) 
 points(mean(Salario), 0.93, pch = 17, col = "red", cex = 2)
  stripchart(Salario~G_Int, xlab="Salario", pch= 20, method ="stack", col=2:4)
  
  
## Diagrama de Caixas
boxplot(Salario,col="yellow",ylab="Salario")
boxplot(Salario,notch=TRUE, xlab="Salario",horizontal = T,col="green")
boxplot(Salario~EsT_Civ, xlab="Salario",col=c("green","yellow"))
 boxplot(Salario~G_Int, xlab="Grau de Instrução", names=c("1 Grau", "2 Grau", "Superior"),
 ylab="Salario",main="Diagrama de caixas dos salarios dos func. por grau de instrução")

######## Diagrama de ramo e folhas ##########################################
stem(Salario)

 ######## Modulo para análise de dados bivariados ###########################
### Leitura de dados do excel
dados1 <- read.csv("lamina5.csv", sep=";",dec=",",header=T )
names(dados1)
x=dados1[,2]
y=dados1[,4]

plot(x, y, xlab = "Espessura", ylab =    "Resistência", pch = 20)
abline(h=mean(y), v=mean(x))
points(mean(x),mean(y),pch=5,col="red")
## Alternativamente
cola=as.factor(dados1[,3])
 cores = rainbow(length(levels(cola)))
 plot(x, y, xlab = "Espessura", ylab = "Resistência", pch = 20, col = cores[cola])
 legend("bottomleft", levels(cola), pch = 20, col = cores) 
 ## Mais de um gráfico por pagina
 par(mfrow=c(2, 2))
#Figura 1
 barplot(table(EsT_Civ), col=c("green","red"),  
ylim=c(0,25), space=.8, width=c(.2,.2),xlab="Estado Civil", ylab="Proporção de funcionários")
text(locator(n=2),c("56%","44%"))
# Figura 2
pie(table(EsT_Civ),col=c("red","yellow"),labels=c("Casado (56%)","Solteiro(44%)"))
 # Figura 3
 hist(Salario, breaks =c(4,8,12,16,20,24),xlab="salário",ylab="Densidade",prob=T,col="yellow",
 ylim=c(0,0.1),  labels=c("27,8%","33,3%","22,2%","13,8%","2,8%"),main="")
# Figura 4
 boxplot(Salario~EsT_Civ, ylab="Salario",col=c("green","yellow"))
 
 
  ### Variaveis Quantitativos vs Variáveis Qualitativos #####################
by(Salario,EsT_Civ, summary)
boxplot(Salario~EsT_Civ,col=c(2,3))
points(by(Salario, EsT_Civ,mean),col=4,pch = 18)
title("Boxplot dos salários x Estado cívil")
by(Salario,R_proced,summary)
by(Salario,G_Int,summary)
boxplot(Salario~R_proced,col=c(4,2,3))
points(by(Salario,R_proced,summary),col=5)
title("Boxplot dos salários x Região de Procedencia",col=1)
by(Salario,G_Int,summary)
boxplot(Salario~G_Int,col=c(3,5,7))
points(by(Salario,G_Int,mean),col=4)
title("Boxplot dos salários x Grau de Instrução")
#### Probabilidades e números aleatórios #####################
x=rnorm(200,4,2)
hist(x,probability=TRUE,col="lightblue", main="Normal(4,2)",ylab="Densidade",
ylim=c(0,0.25),xlim=c(-3,12))
curve(dnorm(x,4,2),add=T)

 ## Grafico da Distribuição Binomail
 x=0:10
plot(x, dbinom(x,10,0.5),type="h",ylab="função de probabilidade")
title(main="B(10,0.5))
##### A Função Sample
x=c(0,3,4,10,18,100,200,6000)
sample(x)
### Teste T para uma amostra
Viscosidade=c(13.3, 14.5, 15.3, 15.3, 14.3, 14.8, 15.2, 14.5, 14.6,
 14.1, 14.3, 16.1, 13.1, 15.5, 12.6, 14.6, 14.3, 15.4, 15.2, 16.8,
  14.9, 13.7, 15.2, 14.5, 15.3, 15.6, 15.8, 13.3, 14.1, 15.4, 15.2, 15.2, 15.9,
   16.5, 14.8, 15.1, 17.0 ,14.9, 14.8, 14.0)
t.test(Viscosidade, alternative="two.side", mu=15.5)
### Teste t-Student para 2 amostras
Maquina1=c(30.9, 30.9, 30.8, 30.7, 30.9, 30.6, 30.8, 30.9, 30.7, 30.9, 30.7,31.0)
Maquina2=c(30.8, 30.9, 30.7, 30.5, 30.5, 30.6, 30.7, 30.3, 30.6 , 30.7)
t.test(Maquina1,Maquina2, alternative="two.side",var.equal=T,conf.level = 0.99)
 ### Teste F para testar a razão de variâncias
var.test(Maquina1,Maquina2, alternative="two.side",conf.level = 0.99)
                                                                                  


## Teste para dados pareados
A=c(7.5, 4.6, 5.7, 4.3,5.8,3.2, 6.1,5.6,3.4,6.5)
B=c(5.2,4.1, 4.3, 4.7, 3.2, 4.9,5.2,4.4,5.7,6.0)  
 t.test(A,B, alternative="greater",paired = T,mu=0.2,conf.level = 0.90)
 ### Teste de normalidade
  shapiro.test(rnorm(10,5,2))
  shapiro.test(Viscosidade)
  ks.test(rnorm(10,5,2), "pnorm",5,2)
  ## Gráfico de QQ normal
  x=rnorm(30,5,2)
  qqnorm(x)
  qqline(x)
## Q-Q plot para os dados de viscosidade
 qqnorm(Viscosidade, main="Normal Q-Q plot para os dados de viscosidae")
 qqline(Viscosidade)
### Análise regressão
dados_reg=read.table("Tabela_11_1.txt",dec=",", header=T)
attach(dados_reg)
names(dados_reg)
plot(N_Hidroc,Pureza ,xlab="Nível de Hidrocarboneto (%)",ylab="Pureza de oxigênio(%)",
 pch=20,col="red")
fit=lm(Pureza~N_Hidroc)
fit
summary(fit)
anova(fit)
abline(fit)
###Estimativa para a resposta média x=1
x.data=data.frame(N_Hidroc=c(1))
predict(fit,x.data, interval="confidence" ) 
###Estimativa para uma observação futura x=1
x.data=data.frame(N_Hidroc=c(1))
(val_pred=predict(fit,x.data, interval="prediction" )) 

### Bandas de confiança para resposta média
x.data=data.frame(N_Hidroc=seq(min(N_Hidroc), max(N_Hidroc),0.01))
val_pred=predict(fit,x.data, interval="confidence" ) 

LINF= val_pred[,2]
LSUP=val_pred[,3]
plot(N_Hidroc,Pureza ,xlab="Nível de Hidrocarboneto (%)",ylab="Pureza de oxigênio(%)",
 pch=20,col="red")
 points(x.data$N_Hidroc,val_pred[,1],lty=1,lwd=1,type="l")

points(x.data$N_Hidroc,LINF,lty=2,lwd=1,type="l")
points(x.data$N_Hidroc,LSUP,lty=2,lwd=1,type="l")
## Bandas de confiança para valores preditos
val_pred=predict(fit,x.data, interval="prediction" ) 
points(x.data$N_Hidroc,val_pred[,1],lty=1,lwd=2,type="l")
points(x.data$N_Hidroc,val_pred[,2],lty=2,lwd=2,col="blue",type="l")
points(x.data$N_Hidroc,val_pred[,3],lty=2,lwd=2,col="blue",type="l")
 text(locator(1),"y=14.957x+ 74.340")
 ## Análise de  residuos
V_ajustados=fitted(fit)
res=residuals(fit)
res_padr=rstandard(fit)
res_stud=rstudent(fit)
par(mfrow=c(2,2))
plot(V_ajustados,res, pch=20)
abline(h=0,lty=2)
plot(V_ajustados,res_padr, pch=20)
abline(h=0,lty=2)
plot(V_ajustados,res_stud, pch=20)
abline(h=0,lty=2)
qqnorm(res_padr, pch=20)
qqline(res_padr)
par(mfrow=c(2,2))
plot(fit)      #plota os  residuos
shapiro.test(res)# verifica a normalidade dos resíduos
## Análise de    variância de um fator  #######################################
dados=read.table("Tabela_13_1.txt",dec=",",header=T)     #Dados do livro Montgomery 
attach(dados)
names(dados)
Concentrac=as.factor(dados[,2])
Resist=dados[,1]  
### Gráfico de Boxplot
medias=tapply(Resist, Concentrac,mean)  ## Calcula as medias por tratamento
dp=tapply(Resist, Concentrac,sd)  ## Calcula os desvio padrão por tratamento
desc=cbind(medias,dp)
dimnames(desc)=list(Concentração=c("5 %","10 %","10 %","15 %"), c("Media","Desvio padrão"))
desc
boxplot(Resist~Concentrac, ylab="Resistência à tensão(psi)", xlab="Concentração de madeira lei (%)",col="green")
points(medias,col="red")                    ## coloca as médias no gráfico de boxplot
## Tabela de  anova
ajusta=aov(Resist~Concentrac)
ajusta
summary(ajusta) #Da a tabela de anova
anova(ajusta)   # Tabela anova
 ## Análise de  residuos
V_ajustados=fitted(ajusta)
res=residuals(ajusta)
res_padr=rstandard(ajusta)
res_stud=rstudent(ajusta)
par(mfrow=c(2,2))
plot(V_ajustados,res, pch=20)
abline(h=0,lty=2)
plot(V_ajustados,res_padr, pch=20)
abline(h=0,lty=2)
plot(V_ajustados,res_stud, pch=20)
abline(h=0,lty=2)
qqnorm(res_padr, pch=20)
qqline(res_padr)
par(mfrow=c(2,2))
plot(ajusta)

 ## Comparação múltipla ####
 TukeyHSD(ajusta,  ordered = TRUE)
plot(TukeyHSD(ajusta))
#ajusta1=oneway.test(Resist~Concentrac,var.equal=TRUE)
## Análise de    variância de dois fatore  #######################################
dados=read.table("Tabela_13_12.txt",dec=",",header=T)     #Dados do livro Montgomery 
Resist=dados[,1]
Fator1=as.factor(dados[,2])
Fator2=as.factor(dados[,3])
 medias=tapply(Resist, Fator1,mean)  ## Calcula as medias por tratamento
dp=tapply(Resist, Fator1,sd)  ## Calcula os desvio padrão por tratamento
desc=cbind(medias,dp)
dimnames(desc)=list(Tipo=c(" 1"," 2"," 3"," 3"),c("Media","Desvio padrão"))
desc
boxplot(Resist~Fator1, ylab="Resistência do tecido", xlab="Tipo de produto químico",col="green")
points(medias,col="red") 
ajusta=aov(Resist~Fator1+Fator2)
ajusta
summary(ajusta) #Da a tabela de anova
anova(ajusta)
TukeyHSD(ajusta, "Fator1", ordered = TRUE)
plot(TukeyHSD(ajusta, "Fator1", ordered = TRUE))
#Análise residual
par(mfrow=c(2,2))
plot(ajusta)