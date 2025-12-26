###########################################################################
##################################################
###########################################################################

getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")
dadosCONV=read.table("bov_mais_CONV.csv", header=T, sep=";", dec=",")
dadosDOP=read.table("bov_mais_DOP.csv", header=T, sep=";", dec=",")

dados$Dest_abate2<-as.numeric(factor(dados$Dest_abate))-1

#custo total por dia
shapiro.test(dados$custototal.dia)
lillieTest(dados$custototal.dia)

library(fBasics)
basicStats(dados$custototal.dia)
basicStats(dadosDOP$custototal.dia)
basicStats(dadosCONV$custototal.dia)
names(table(dados$custototal.dia))[table(dados$custototal.dia) == max(table(dados$custototal.dia))]
boxplot(dados$custototal.dia~dados$Dest_abate, main= "Custo por dia de produ??o por destino de abate", ylab= "Peso da carca?a em Kg", xlab = "Destino de abate")

t.test(dados$custototal.dia ~ dados$Dest_abate, alternative = c("two.sided"))

library(ltm)
biserial.cor(dados$custototal.dia, dados$Dest_abate2)
