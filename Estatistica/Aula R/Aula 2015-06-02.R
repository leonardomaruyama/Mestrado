x = c(16,31,38,39,37,36,36,22,10)
y = c(290,374,393,425,406,370,365,320,269)

#Diagrama de dispers�o
plot(x,y,xlab="Temperatura", ylab="Consumo", col="blue", lwd=3)
#Pode-se analisar descritivamente que os pontos est�o distribuidos em uma reta 
#imagin�ria, ent�o h� evid�ncias de que existe uma correla��o entre as vari�veis

#Calcula correla��o
cor(x,y)
#Existe um forte correla��o entre x e y