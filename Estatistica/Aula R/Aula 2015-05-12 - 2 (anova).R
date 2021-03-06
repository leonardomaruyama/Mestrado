dados=read.table("dados_anova2.csv",header=T,sep=";",dec=".")
attach(dados)
names(dados)
Produto=as.factor(dados[,2])
Tecido=as.factor(dados[,3])
Resistencia=dados[,1]

######### Resistencia X Produto #########
## Calcula as medias por tratamento
medias=tapply(Resistencia, Produto,mean) 

## Calcula os desvio padr�o por tratamento
dp=tapply(Resistencia, Produto, sd) 

desc=cbind(medias,dp)

#dimnames(desc)=list(Produto=c("5 %","10 %","10 %","15 %"), c("Media","Desvio padr�o"))

desc

boxplot(Resistencia~Produto, ylab="Resist�ncia do tecido", xlab="Tipo de produto qu�mico",col="green")

points(medias,col="red")

ajusta=aov(Resistencia~Produto)
ajusta

#Apresenta a tabela anova
summary(ajusta)

# Tabela anova
anova(ajusta) 


# An�lise de res�duos
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

#plot(ajusta)

shapiro.test(res)
#se p-value maior que n�vel de significancia aceita valores <> 0, caso contr�rio aceita-se valores tamb�m = 0 

TukeyHSD(ajusta, "Produto", ordered = TRUE)

plot(TukeyHSD(ajusta, "Produto", ordered = TRUE))



######### Resistencia X Tecido #########
## Calcula as medias por tratamento
medias=tapply(Resistencia, Tecido ,mean) 

## Calcula os desvio padr�o por tratamento
dp=tapply(Resistencia, Tecido , sd) 

desc=cbind(medias,dp)

#dimnames(desc)=list(Produto=c("5 %","10 %","10 %","15 %"), c("Media","Desvio padr�o"))

desc

boxplot(Resistencia~Tecido, ylab="Resist�ncia do tecido", xlab="Tipo de tecido",col="green")

points(medias,col="red")

ajusta=aov(Resistencia~Tecido )
ajusta

#Apresenta a tabela anova
summary(ajusta)

# Tabela anova
anova(ajusta) 


# An�lise de res�duos
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

#plot(ajusta)

shapiro.test(res)
#se p-value maior que n�vel de significancia aceita valores <> 0, caso contr�rio aceita-se valores tamb�m = 0 

TukeyHSD(ajusta, ordered = TRUE)

plot(TukeyHSD(ajusta))