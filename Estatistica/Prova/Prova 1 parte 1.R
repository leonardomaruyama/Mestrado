#Prova
#item 1
 
dados=read.table("exer1.txt",header=T,sep=";",dec=".")
attach(dados)
dados
names(dados)

summary(dados)

sort(Direito))

sd(Direito)

sd(Pol�tica)

sd(Estat�stica)


moda<-function(d)
 {
 if ((is.vector(d) || is.matrix(d) || is.factor(d)==TRUE) &&
 (is.list(d)==FALSE))
 {
 dd<-table(d)
 valores<-which(dd==max(dd))
 vmodal<-0
 for(i in 1:(length(valores)))
 if (i==1) vmodal<-as.numeric(names(valores[i]))
 else
 vmodal<-c(vmodal,as.numeric(names(valores[i])))
 if (length(vmodal)==length(dd))
 print("conjunto sem valor modal")
 else return(vmodal)
 }
 else print("o par�metro deve ser um vetor ou uma matriz")
 } 

sort(Pol�tica)

moda(sort(Direito))
moda(sort(Pol�tica))
moda(sort(Estat�stica))

boxplot(Direito)

boxplot(Pol�tica)

boxplot(Estat�stica)

boxplot(Estat�stica~Se��o)

prop.table(table(Se��o, Estat�stica))

table(Ingl�s)
prop.table(table(Se��o, Ingl�s))