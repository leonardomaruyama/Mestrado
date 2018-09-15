require(utils)
require(MASS)
dados=read.table("dados_artigo.txt",header=T,sep=";")
dados
pairs(dados[c("x1","x2","x3","x4")], main="Diagrama de dispersão", pch=22, bg=c("red", "yellow"))

#plot(dados[,1], dados[,2], lwd=3)