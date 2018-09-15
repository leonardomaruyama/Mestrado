#Teste de hipótese e intervalo de confiança

Viscosidade = c(	13.3, 14.5, 15.3, 15.3, 14.3, 14.8, 15.2, 14.5, 
			14.6,	14.1, 14.3, 16.1, 13.1, 15.5, 12.6, 14.6, 
			14.3, 15.4, 15.2, 16.8, 14.9, 13.7, 15.2, 14.5, 
			15.3, 15.6, 15.8,	13.3, 14.1, 15.4, 15.2, 15.2, 
			15.9, 16.5, 14.8, 15.1, 17.0, 14.9, 14.8, 14.0
		   )
t.test(Viscosidade, alternative="two.side", mu=15.5)

#------------------------------------------------------------------------------------
#Teste de hipótese e intervalo de confiança

Maquina1 = c(30.9, 30.9, 30.8, 30.7, 30.9, 30.6, 30.8, 30.9, 30.7, 30.9, 30.7, 31.0) 
Maquina2 = c(30.8, 30.9, 30.7, 30.5, 30.5, 30.6, 30.7, 30.3, 30.6, 30.7)

#supor igualdade de variância (var.equal = T ou F) 
#Para confirmar isto é necessário fazer o teste de variância
t.test(Maquina1,Maquina2, alternative="two.side",var.equal=T,conf.level = 0.99)
t.test(Maquina1,Maquina2, alternative="two.side",var.equal=F,conf.level = 0.99)

#Teste de variância (para verificar a igualdade p-value deve ser maior que o erro considerado)
var.test(Maquina1,Maquina2, alternative="two.side",conf.level = 0.99)

#------------------------------------------------------------------------------------

A = c(7.5, 4.6, 5.7, 4.3, 5.8, 3.2, 6.1, 5.6, 3.4, 6.5)
B = c(5.2, 4.1, 4.3, 4.7, 3.2, 4.9, 5.2, 4.4, 5.7, 6.0)

t.test(A,B, alternative="greater",paired = T,mu=0.2,conf.level = 0.99)

#------------------------------------------------------------------------------------

#Teste de normalidade
shapiro.test(rnorm(10,5,2))
shapiro.test(Viscosidade)
shapiro.test(Maquina1)
shapiro.test(Maquina2)

ks.test(rnorm(10,5,2), "pnorm",5,2)
ks.test(Viscosidade,"pnorm", mean(Viscosidade), sd(Viscosidade))

#------------------------------------------------------------------------------------

#QQ normal
qqnorm(Viscosidade, main="Normal Q-Q plot para os dados de viscosidade")
qqline(Viscosidade)