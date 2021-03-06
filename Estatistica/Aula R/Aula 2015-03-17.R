#Medidas de resumo
dados=read.table("tabela1.csv",header=T,sep=";",dec=".")
attach(dados)
names(dados)
summary(Salario)
summary(No_filhos)
summary(Estado_civil)
summary(as.factor(No_filhos))

#Medidas de posi��o e dispers�o
mean(No_filhos)
mean(No_filhos,na.rm=T)

#Vari�ncia
var(No_filhos)
var(No_filhos,na.rm=T)

#Desvio padr�o
sd(No_filhos)
sd(No_filhos,na.rm=T)

#Mediana
median(Salario)

#Quantil
quantile(Salario,c(0.25,0.5,0.75))


#Escrevendo fun��es
meuResumo<-function(x){
s<-c(length(x),mean(x,na.rm=T),median(x,na.rm=T),
sd(x,na.rm=T),var(x,na.rm=T),length(which(is.na(x))))
names(s)<-c("n","m�dia","mediana","desvio padr�o","vari�ncia","NA")
return(s)
}

meuResumo(Salario)

meuResumo(No_filhos)


#Tabelas
table(Estado_civil)
table(Estado_civil,Grau_Instrucao)
table(Estado_civil,Grau_Instrucao,Regiao_procedencia)

#Propor��es
prop.table(table(Estado_civil))
prop.table(table(Estado_civil,Grau_Instrucao))
prop.table(table(Estado_civil,Grau_Instrucao),1)
prop.table(table(Estado_civil,Grau_Instrucao),2)

#Gr�fico de barras
barplot(table(Estado_civil),col=c("green","red"),
ylim=c(0,25),space=.8,width=c(.2,.2), 
main="Propor��o de funcion�rios por estado civil",
xlab="Estado civil", ylab="Propor��o de funcion�rios")
text(locator(n=2),c("56%","44%"))

#Diagrama de pizza
pie(table(Grau_Instrucao),col=c("red","yellow","green"),
labels=c("1 Grau(33,3%)","2 Grau(50%)","Superior(15,7%)"))
title(main="Distribui��o dos funcion�rios por grau de instru��o")

#Histrograma
hist(Salario, breaks=c(4,8,12,16,20,24),xlab="Densidade",prob=T,col="yellow",
main="Dist. de sal�rio dos funcion�rios da empresa",ylim=c(0,0.1), 
labels=c("27.8%","33.3%","22.2%","13.8%","2.8%"))


#Tabela de frequ�ncia (TDF)
TDF=hist(Salario,breaks=c(4,8,12,16,20,24), right=F, plot=F)
fabs=TDF$counts #Frequ�ncia absoluta
fr=fabs/length(Salario) #Frequ�ncia relativa
saida=cbind(fabs,fr)
dimnames(saida)=list(c("4|-8", "8|-12", "12|-16", "16|-20", "20|-24"),c("f","fr"))
saida

#Fun��o cut
fabs=table(cut(Salario, breaks=c(4,8,12,16,20,24), labels=c("4|-8", "8|-12", "12|-16", "16|-20", "20|-24"), right=F))
fr=fabs/length(Salario)
saida=cbind(fabs,fr)
dimnames(saida)=list(c("4|-8", "8|-12", "12|-16", "16|-20", "20|-24"),c("f","fr"))
saida

#boxplot
boxplot(Salario,col="yellow")
boxplot(Salario, notch=TRUE, xlab="Salario", horizontal=T, col="green")
boxplot(Salario~Estado_civil, xlab="Salario", col=c("green","yellow"))
boxplot(Salario~Grau_Instrucao, xlab="Grau de instru��o", names=c("1 Grau","2 Grau", "Superior"), ylab="Salario")

