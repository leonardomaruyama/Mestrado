dados=read.table("dados_ARM.csv",header=T,sep=";")
dados
library(scatterplot3d)
attach(dados)

scatterplot3d(X1,X2,Y, main="Diagrama de dispersão",
zlab="Consumo",xlab=expression(x[1]),ylab=expression(x[2]),lwd=3)
pairs(~ Y + X1 + X2,data=dados, main="Matriz de diagrama de dispersão", lwd=3)
fit=lm(Y ~ 1 + X1 + X2,data=dados)
summary(fit)

anova(fit)

res=residuals(fit) # residuals
resp = rstandard(fit)#standardresiduals
par(mfrow=c(2,2))
par(mai = c(0.85,0.85,0.30,0.05))
# Margins: inf, left, sup and right
qqnorm(resp, main =)
qqline(resp)
plot(X1,resp, ylab = "Residualpadranizado", xlab = expression(x[1]))
abline(h=0, lty=2)
plot(X2,resp, ylab = "Residualpadranizado", xlab = expression(x[2]))
abline(h=0,lty=2)

yhat = predict(fit)
plot(yhat,resp, ylab = "Residualpadranizado", xlab = "Valorespreditos")
abline(h=0,lty=2)
