##########################Observação outliers modelo geral
getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")


dados2<-dados[-c(2,3,264,265,266,267,268,333,337,338,348,360,363,703,710),]

modcustodiaENTB2<-lm(custototal.dia~peso_ent+VG_cap_crescimento+VG_capacidade_maternal+VG_carcaca_dia_idade, data = dados2)
summary(modcustodiaENTB2)
library(fBasics)
basicStats(dados$custototal.dia) #média=2,18
basicStats(dados$Diasengorda)   #média = 139 dias

dados[2,] #ficou 29 dias na engorda Custo/dia=1,63

dados[3,] #ficou 29 dias na engorda e custo/dia = 1,577

dados[263,]

dados[264,] #m?dia de custo por dia = 1,502
#Sendo que ficou 146 dias na engorda

dados[265,] #146 dias na engorda custo/dia = 1,35

dados[266,]

dados[267,]

dados[268,]

dados[333,]

dados[337,]

dados[338,]

dados[348,]

dados[360,]

dados[363,]

dados[710,]

#Tenho 4 animais que foram ao abate DOP e 11 animais que foram ao abate convencional
#sao formados por outlier inferior e superior de custototal por dia e dias na engorda

plot(dados$Diasengorda, dados$custototal.dia)
