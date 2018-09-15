#Principais Modelos Discretos
#Exemplo 2
1/2
pbinom(6,10,0.5,lower.tail=F)
pbinom(6,10,0.2,lower.tail=F)
200*0.0008643584
dbinom(6,10,0.2)

x=0:10
y=dbinom(x,10,0.2)
cbind(x,y)

barplot(y,names.arg=x,main="Distribuição B(10,0.2)")

plot(x,y,type="h",ylab="função de probabiidade")

1-pbinom(5,10,0.2)


#Exemplo 4
#Empresa recebe em média 3 chamadas a cada 4 minutos
3/4

#Calcular a probabilidade de receber 2 chamadas em 2 minutos
Mi=(3/4)*2
Mi

x=dpois(0,Mi)
y=dpois(2,Mi)
z=dpois(1,Mi)

x+y+z

ppois(2,Mi)

plot(dpois(1:100, lambda=10), type="h", ylab="Probabilidade",main="Distribuição Poisson")
plot(dpois(1:100, lambda=20), type="h", ylab="Probabilidade",main="Distribuição Poisson")
plot(dpois(1:100, lambda=30), type="h", ylab="Probabilidade",main="Distribuição Poisson")
plot(dpois(1:100, lambda=40), type="h", ylab="Probabilidade",main="Distribuição Poisson")

#Principais Modelos Contínuos
#Distribuição Exponencial
pexp(150,1/100,lower.tail=F)
pexp(20,1/100,lower.tail=F)

#Modelo Weibull
x=24*365 #Durabilidade de 1 ano em horas (= 8760 horas)
pweibull(x,0.5,100000,lower.tail=F)

#Função de densidade de probabilidade
help(Weibull)
x=seq(1,50,length=200) #é o mesmo que x=1:50
y=dweibull(x,0.5,100000)
plot(x,y,type="l",lwd=2,col="yellow")