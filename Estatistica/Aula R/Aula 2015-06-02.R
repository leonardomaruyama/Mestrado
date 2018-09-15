x = c(16,31,38,39,37,36,36,22,10)
y = c(290,374,393,425,406,370,365,320,269)

#Diagrama de dispersão
plot(x,y,xlab="Temperatura", ylab="Consumo", col="blue", lwd=3)
#Pode-se analisar descritivamente que os pontos estão distribuidos em uma reta 
#imaginária, então há evidências de que existe uma correlação entre as variáveis

#Calcula correlação
cor(x,y)
#Existe um forte correlação entre x e y