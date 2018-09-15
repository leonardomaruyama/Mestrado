#Prova
#item 1
 
dados=read.table("exer1.txt",header=T,sep=";",dec=".")
attach(dados)
dados
names(dados)

summary(dados)

sort(Direito))

sd(Direito)

sd(Política)

sd(Estatística)


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
 else print("o parâmetro deve ser um vetor ou uma matriz")
 } 

sort(Política)

moda(sort(Direito))
moda(sort(Política))
moda(sort(Estatística))

boxplot(Direito)

boxplot(Política)

boxplot(Estatística)

boxplot(Estatística~Seção)

prop.table(table(Seção, Estatística))

table(Inglês)
prop.table(table(Seção, Inglês))