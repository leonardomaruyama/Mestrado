dados=read.table("dados_anova.csv",header=T,sep=";")
attach(dados)
names(dados)
Concentracao=as.factor(dados[,2])
Resistencia=dados[,1]

## Calcula as medias por tratamento
medias=tapply(Resistencia, Concentracao,mean) 

## Calcula os desvio padr�o por tratamento
dp=tapply(Resistencia, Concentracao, sd) 

desc=cbind(medias,dp)

dimnames(desc)=list(Concentra��o=c("5 %","10 %","10 %","15 %"), c("Media","Desvio padr�o"))

desc

boxplot(Resistencia~Concentracao, ylab="Resist�ncia � tens�o(psi)", xlab="Concentra��o de madeira lei (%)",col="green")

points(medias,col="red")

ajusta=aov(Resistencia~Concentracao)
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