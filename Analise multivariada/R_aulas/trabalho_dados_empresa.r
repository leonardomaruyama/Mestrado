
R version 3.1.1 (2014-07-10) -- "Sock it to Me"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R é um software livre e vem sem GARANTIA ALGUMA.
Você pode redistribuí-lo sob certas circunstâncias.
Digite 'license()' ou 'licence()' para detalhes de distribuição.

R é um projeto colaborativo com muitos contribuidores.
Digite 'contributors()' para obter mais informações e
'citation()' para saber como citar o R ou pacotes do R em publicações.

Digite 'demo()' para demonstrações, 'help()' para o sistema on-line de ajuda,
ou 'help.start()' para abrir o sistema de ajuda em HTML no seu navegador.
Digite 'q()' para sair do R.

> bivar=read.table("Dados_empresas.txt",sep=";",dec=",",header=T)
Erro em file(file, "rt") : não é possível abrir a conexão
Além disso: Mensagens de aviso perdidas:
In file(file, "rt") :
  não foi possível abrir o arquivo 'Dados_empresas.txt': No such file or directory
> bivar=read.table("Dados_empresas.txt",sep=";",header=T)
Erro em file(file, "rt") : não é possível abrir a conexão
Além disso: Mensagens de aviso perdidas:
In file(file, "rt") :
  não foi possível abrir o arquivo 'Dados_empresas.txt': No such file or directory
> bivar=read.table("Dados_empresas.txt",sep=";",dec=",",header=T)
> bivar
   Empresa Ganho_Bruto Ganho_Liquido Patrimonio
1       E1        9993           564      17683
2       E2        8776           389      17359
3       E3       13572          1103      18597
4       E4        6455           743       8745
5       E5        5129           203      14397
6       E6        5432           215       3467
7       E7        3807           385       4679
8       E8        3423           187       6754
9       E9        3708           127       2275
10     E10        3294           297       6754
11     E11        5433           432       5583
12     E12        6287           451       8972
> attach(bivar)
> plot(xj1,xj2,ylim=c(-12,10),xlim=c(-2,12),ylab=expression(X[2]),xlab=expression(X[1]),lwd=2.5,col="blue")
Erro em plot(xj1, xj2, ylim = c(-12, 10), xlim = c(-2, 12), ylab = expression(X[2]),  : 
  objeto 'xj1' não encontrado
> plot(x1j,x2j,ylim=c(-12,10),xlim=c(-2,12),ylab=expression(X[2]),xlab=expression(X[1]),lwd=2.5,col="blue")
Erro em plot(x1j, x2j, ylim = c(-12, 10), xlim = c(-2, 12), ylab = expression(X[2]),  : 
  objeto 'x1j' não encontrado
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(-12,10),xlim=c(-2,12),ylab=expression(X[2]),xlab=expression(X[1]),lwd=2.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(1000,15000),xlim=c(-2,12),ylab="Ganho Líquido",xlab=,lwd=2.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,1000),xlim=c(1000,15000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=2.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,1000),xlim=c(1000,15000),ylab="Ganho Líquido",xlab="Ganho Bruto",col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,1000),xlim=c(1000,15000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=3.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,1000),xlim=c(1000,15000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=2.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,800),xlim=c(1000,15000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=3.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,800),xlim=c(2000,12000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=3.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,800),xlim=c(2000,11000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=3.5,col="blue")
> plot(Ganho_Bruto,Ganho_Liquido,ylim=c(100,800),xlim=c(2000,11000),ylab="Ganho Líquido",xlab="Ganho Bruto",lwd=3.5,col="blue")
> X=as.matrix(bivar,2)
> vmed=apply(X,2,mean)
Mensagens de aviso perdidas:
1: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
2: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
3: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
4: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
> vmed=apply(X,3,mean)
Erro em if (d2 == 0L) { : valor ausente onde TRUE/FALSE necessário
> X=as.matrix(bivar,3)
> vmed=apply(X,3,mean)
Erro em if (d2 == 0L) { : valor ausente onde TRUE/FALSE necessário
> vmed=apply(X,2,mean)
Mensagens de aviso perdidas:
1: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
2: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
3: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
4: In mean.default(newX[, i], ...) :
  argumento não é numérico nem lógico: retornando NA
> vmed=apply(X,3,mean)
Erro em if (d2 == 0L) { : valor ausente onde TRUE/FALSE necessário
> X
      Empresa Ganho_Bruto Ganho_Liquido Patrimonio
 [1,] "E1"    " 9993"     " 564"        "17683"   
 [2,] "E2"    " 8776"     " 389"        "17359"   
 [3,] "E3"    "13572"     "1103"        "18597"   
 [4,] "E4"    " 6455"     " 743"        " 8745"   
 [5,] "E5"    " 5129"     " 203"        "14397"   
 [6,] "E6"    " 5432"     " 215"        " 3467"   
 [7,] "E7"    " 3807"     " 385"        " 4679"   
 [8,] "E8"    " 3423"     " 187"        " 6754"   
 [9,] "E9"    " 3708"     " 127"        " 2275"   
[10,] "E10"   " 3294"     " 297"        " 6754"   
[11,] "E11"   " 5433"     " 432"        " 5583"   
[12,] "E12"   " 6287"     " 451"        " 8972"   
> X=as.matrix(bivar,2)
> X
      Empresa Ganho_Bruto Ganho_Liquido Patrimonio
 [1,] "E1"    " 9993"     " 564"        "17683"   
 [2,] "E2"    " 8776"     " 389"        "17359"   
 [3,] "E3"    "13572"     "1103"        "18597"   
 [4,] "E4"    " 6455"     " 743"        " 8745"   
 [5,] "E5"    " 5129"     " 203"        "14397"   
 [6,] "E6"    " 5432"     " 215"        " 3467"   
 [7,] "E7"    " 3807"     " 385"        " 4679"   
 [8,] "E8"    " 3423"     " 187"        " 6754"   
 [9,] "E9"    " 3708"     " 127"        " 2275"   
[10,] "E10"   " 3294"     " 297"        " 6754"   
[11,] "E11"   " 5433"     " 432"        " 5583"   
[12,] "E12"   " 6287"     " 451"        " 8972"   
> X=as.matrix(bivar,3)
> X
      Empresa Ganho_Bruto Ganho_Liquido Patrimonio
 [1,] "E1"    " 9993"     " 564"        "17683"   
 [2,] "E2"    " 8776"     " 389"        "17359"   
 [3,] "E3"    "13572"     "1103"        "18597"   
 [4,] "E4"    " 6455"     " 743"        " 8745"   
 [5,] "E5"    " 5129"     " 203"        "14397"   
 [6,] "E6"    " 5432"     " 215"        " 3467"   
 [7,] "E7"    " 3807"     " 385"        " 4679"   
 [8,] "E8"    " 3423"     " 187"        " 6754"   
 [9,] "E9"    " 3708"     " 127"        " 2275"   
[10,] "E10"   " 3294"     " 297"        " 6754"   
[11,] "E11"   " 5433"     " 432"        " 5583"   
[12,] "E12"   " 6287"     " 451"        " 8972"   
> X=as.matrix(bivar,4)
> X
      Empresa Ganho_Bruto Ganho_Liquido Patrimonio
 [1,] "E1"    " 9993"     " 564"        "17683"   
 [2,] "E2"    " 8776"     " 389"        "17359"   
 [3,] "E3"    "13572"     "1103"        "18597"   
 [4,] "E4"    " 6455"     " 743"        " 8745"   
 [5,] "E5"    " 5129"     " 203"        "14397"   
 [6,] "E6"    " 5432"     " 215"        " 3467"   
 [7,] "E7"    " 3807"     " 385"        " 4679"   
 [8,] "E8"    " 3423"     " 187"        " 6754"   
 [9,] "E9"    " 3708"     " 127"        " 2275"   
[10,] "E10"   " 3294"     " 297"        " 6754"   
[11,] "E11"   " 5433"     " 432"        " 5583"   
[12,] "E12"   " 6287"     " 451"        " 8972"   
> mean(bivar)
[1] NA
Mensagens de aviso perdidas:
In mean.default(bivar) : argumento não é numérico nem lógico: retornando NA
> teste=data.frame("Ganho_Bruto")
> teste
  X.Ganho_Bruto.
1    Ganho_Bruto
> teste=data.frame(Ganho_Bruto)
> teste
   Ganho_Bruto
1         9993
2         8776
3        13572
4         6455
5         5129
6         5432
7         3807
8         3423
9         3708
10        3294
11        5433
12        6287
> matrizbivar=data.frame(Ganho_Bruto,Ganho_Liquido)
> X=as.matrix(matrizbivar,2)
> X
      Ganho_Bruto Ganho_Liquido
 [1,]        9993           564
 [2,]        8776           389
 [3,]       13572          1103
 [4,]        6455           743
 [5,]        5129           203
 [6,]        5432           215
 [7,]        3807           385
 [8,]        3423           187
 [9,]        3708           127
[10,]        3294           297
[11,]        5433           432
[12,]        6287           451
> vmed=apply(X,2,mean)
> S=cov(X)
> vmed
  Ganho_Bruto Ganho_Liquido 
    6275.7500      424.6667 
> S
              Ganho_Bruto Ganho_Liquido
Ganho_Bruto     9617361.7     707387.73
Ganho_Liquido    707387.7      76269.52
> n=nrow(X)
> n
[1] 12
> p=ncol(X)
> p
[1] 2
> dquad=numeric(n)
> dquad
 [1] 0 0 0 0 0 0 0 0 0 0 0 0
> for (i in 1:n) {dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%(X[i,]-vmed)}
> prop=ppoints(n)
> qui=qchisq(prop,p)
> plot(dquad,qui)
> plot(dquad,qui,ylab=expression(d[j]^2))
> plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]))
> plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3)
> plot(dquad,qui,ylab=expression(d[j]^2),xlab=expression(q[j]),lwd=3,col="blue")
> abline(0,1)
> requires(car)
Erro: não foi possível encontrar a função "requires"
> require(car)
Carregando pacotes exigidos: car
> local({pkg <- select.list(sort(.packages(all.available = TRUE)),graphics=TRUE)
+ if(nchar(pkg)) library(pkg, character.only=TRUE)})
> require(car)
> data(Prestige)
> attach(Prestige)
The following object is masked from package:datasets:

    women

> par(mfrow=c(3,4))
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm(Prestige[,i], main="", xlab="")}
Erro: símbolo inesperado in "for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm"
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") }
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm (Prestige[,i], main="", xlab=""}
Erro: símbolo inesperado in "for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm"
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qnorm (Prestige[,i], main="", xlab=""}
Erro: símbolo inesperado in "for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qnorm"
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qnorm (Prestige[,i], main="", xlab="")}
Erro: símbolo inesperado in "for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qnorm"
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm (Prestige[,i], main="", xlab="")}
Erro: símbolo inesperado in "for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm"
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm(Prestige[,i], main="", xlab="")}
Erro: símbolo inesperado in "for (i in 1:5) { hist (Prestige[,i], main="", xlab="") qqnorm"
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="")}
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="")}
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="")}
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="")}
> for (i in 1:5) { hist (Prestige[,i], main="", xlab="")}
> matrizbivar=data.frame(Ganho_Bruto,Ganho_Liquido,Patrimonio)
> matrizbivar
   Ganho_Bruto Ganho_Liquido Patrimonio
1         9993           564      17683
2         8776           389      17359
3        13572          1103      18597
4         6455           743       8745
5         5129           203      14397
6         5432           215       3467
7         3807           385       4679
8         3423           187       6754
9         3708           127       2275
10        3294           297       6754
11        5433           432       5583
12        6287           451       8972
> X=as.matrix(matrizbivar,3)
> vmed=apply(X,3,mean)
Erro em if (d2 == 0L) { : valor ausente onde TRUE/FALSE necessário
> vmed=apply(X,2,mean)
> matrizbivar=data.frame(Ganho_Bruto,Ganho_Liquido,Patrimonio)
> matrizbivar
   Ganho_Bruto Ganho_Liquido Patrimonio
1         9993           564      17683
2         8776           389      17359
3        13572          1103      18597
4         6455           743       8745
5         5129           203      14397
6         5432           215       3467
7         3807           385       4679
8         3423           187       6754
9         3708           127       2275
10        3294           297       6754
11        5433           432       5583
12        6287           451       8972
> X=as.matrix(matrizbivar,3)
> X
      Ganho_Bruto Ganho_Liquido Patrimonio
 [1,]        9993           564      17683
 [2,]        8776           389      17359
 [3,]       13572          1103      18597
 [4,]        6455           743       8745
 [5,]        5129           203      14397
 [6,]        5432           215       3467
 [7,]        3807           385       4679
 [8,]        3423           187       6754
 [9,]        3708           127       2275
[10,]        3294           297       6754
[11,]        5433           432       5583
[12,]        6287           451       8972
> vmed=apply(X,#,mean)


+ vmed=apply(X,#,mean)> > q
function (save = "default", status = 0, runLast = TRUE) 
.Internal(quit(save, status, runLast))
<bytecode: 0x000000000a73df68>
<environment: namespace:base>
> vmed=apply(X,2,mean)
> vmed
  Ganho_Bruto Ganho_Liquido    Patrimonio 
    6275.7500      424.6667     9605.4167 
> scov(X)
Erro: não foi possível encontrar a função "scov"
> S=cov(X)
> S
              Ganho_Bruto Ganho_Liquido Patrimonio
Ganho_Bruto     9617361.7     707387.73 15050142.7
Ganho_Liquido    707387.7      76269.52   933835.1
Patrimonio     15050142.7     933835.06 34403683.7
> n=nrow(X)
> n
[1] 12
> p=ncol(X)
> dqaud=numeric(n)
> rm(dqaud)
> dquad=numeric(n)
> for (i in 1:n) {dquad[i]=t(X[i,]-vmed)%*%solve(S)%*%solve(S)%*%(X[i,]-vmed)}
> prop=ppoints(n)
> qui=qchisq(prop,p)
> plot(dquad,qui)
> plot(dquad,qui)
> 
