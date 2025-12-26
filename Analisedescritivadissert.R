#####################################################################################
##############ANALISE DESCRITIVA PARA DISSERTAção####################################
#####################################################################################


getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=TRUE, sep=";", dec=",", fileEncoding = "latin1")
dadosCONV=read.table("bov_mais_CONV.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")
dadosDOP=read.table("bov_mais_DOP.csv", header=T, sep=";", dec=",", fileEncoding = "latin1")

dados$Dest_abate2<-as.numeric(factor(dados$Dest_abate))-1
table(dados$Dest_abate)
round(prop.table(table(dados$Dest_abate))*100, digits = 2)
table(dados$Dest_abate2)
library(fBasics)


#IDADE A ENTRADA

tapply(dados$Idadeent_meses,dados$Dest_abate, summary)
# os animais DOP vão um pouco mais tarde que os convencionais
#o que descreve o ato do produtor selecionar os animais que vão ficar para o selo DOP

basicStats(dados$Idadeent_meses)
basicStats(dadosDOP$Idadeent_meses)
basicStats(dadosCONV$Idadeent_meses)

t.test(dados$Idadeent_meses, dados$Dest_abate2) #significativo
wilcox.test(dados$Idadeent_meses~dados$Dest_abate2) #o teste não parametrico confirma
#A idade a entrada do animal é uma característica com diferenças de média e mediana significativa quanto ao grupo de abate

boxplot(dadosDOP$Idadeent_meses,dadosCONV$Idadeent_meses, main= "Idade a entrada por destino de abate", ylab = "Idade a entrada", xlab= "Abate DOP Vs. Abate Convencional")
grafico1<-hist(dados$Idadeent_meses, main = "Idade na entrada dos currais",xlab = "Idade (em meses)", ylab = "Frequencias")
min(dados$Idadeent_meses)
seq(1,5,length=22)
xajust<-seq(min(dados$Idadeent_meses, na.rm=T),  
            max(dados$Idadeent_meses, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$Idadeent_meses,na.rm=T),
              sd=sd(dados$Idadeent_meses,na.rm=T))
hist(dados$Idadeent_meses, freq = F, main = "Idade a entrada", xlab = "Idade (em meses)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

lillieTest(dados$Idadeent_meses)
# a idade não segue uma distribuição normal

#PESO 

tapply(dados$peso_ent, dados$Dest_abate, summary)
basicStats(dados$peso_ent)
basicStats(dadosDOP$peso_ent)
basicStats(dadosCONV$peso_ent)

grafico2<-hist(dados$peso_ent, main = "Peso na entrada dos currais",xlab = "Peso (em kg)", ylab = "Frequencias")
min(dados$peso_ent)
seq(1,5,length=22)
xajust<-seq(min(dados$peso_ent, na.rm=T),  
            max(dados$peso_ent, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$peso_ent,na.rm=T),
              sd=sd(dados$peso_ent,na.rm=T))
hist(dados$peso_ent, freq = F, main = "Peso na entrada", xlab = "Peso (em kg)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$peso_ent,dados$Dest_abate2)

boxplot(dadosDOP$peso_ent,dadosCONV$peso_ent, main= "Peso a entrada por destino de abate", ylab = "Peso a entrada", xlab= "Abate DOP Vs. Abate Convencional")

library(ltm)
biserial.cor(dados$peso_ent, dados$Dest_abate2)
#Nao apresentam relacao de associacao entre peso e destino de abate


#IDADE A SAIDA
basicStats(dados$Idadesaida_meses)
basicStats(dadosDOP$Idadesaida_meses)
basicStats(dadosCONV$Idadesaida_meses)

grafico3<-hist(dados$Idadesaida_meses, main = "Idade na saída",xlab = "Idade (em meses)", ylab = "Frequencias")
min(dados$Idadesaida_meses)
seq(1,5,length=22)
xajust<-seq(min(dados$Idadesaida_meses, na.rm=T),  
            max(dados$Idadesaida_meses, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$Idadesaida_meses,na.rm=T),
              sd=sd(dados$Idadesaida_meses,na.rm=T))
hist(dados$Idadesaida_meses, freq = F, main = "Idade na saída", xlab = "Idade (em meses)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)


t.test(dados$Idadesaida_meses, dados$Dest_abate2)

biserial.cor(dados$Idadesaida_meses, dados$Dest_abate) #-0.7912027

boxplot(dadosDOP$Idadesaida_meses,dadosCONV$Idadesaida_meses, main= "Idade a saída por destino de abate", ylab = "Idade a saída", xlab= "Abate DOP Vs. Abate Convencional")


#PESO A SAIDA

tapply(dados$peso_saida, dados$Dest_abate, summary)
basicStats(dados$peso_saida)
basicStats(dadosDOP$peso_saida)
basicStats(dadosCONV$peso_saida)


grafico4<-hist(dados$peso_saida, main = "Peso na saída",xlab = "Peso (em kg)", ylab = "Frequencias")
min(dados$peso_saida)
seq(1,5,length=22)
xajust<-seq(min(dados$peso_saida, na.rm=T),  
            max(dados$peso_saida, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$peso_saida,na.rm=T),
              sd=sd(dados$peso_saida,na.rm=T))
hist(dados$peso_saida, freq = F, main = "Peso na saída", xlab = "Peso (em kg)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$peso_saida, dados$Dest_abate2)

biserial.cor(dados$peso_saida, dados$Abatedestino2) #-0.5730065

boxplot(dadosDOP$peso_saida,dadosCONV$peso_saida, main= "Peso a saída por destino de abate", ylab = "Peso a saída", xlab= "Abate DOP Vs. Abate Convencional")

#DIAS NA ENGORDA

basicStats(dados$Diasengorda)
basicStats(dadosDOP$Diasengorda)
basicStats(dadosCONV$Diasengorda)
tapply(dados$Diasengorda, dados$Dest_abate, summary)

grafico5<-hist(dados$Diasengorda, main = "Dias na engorda",xlab = "Dias na engorda", ylab = "Frequencias")
min(dados$Diasengorda)
seq(1,5,length=22)
xajust<-seq(min(dados$Diasengorda, na.rm=T),  
            max(dados$Diasengorda, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$Diasengorda,na.rm=T),
              sd=sd(dados$Diasengorda,na.rm=T))
hist(dados$Diasengorda, freq = F, main = "Histograma dos dias na engorda", xlab = "Dias na engorda", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$Diasengorda,dados$Dest_abate2)

boxplot(dadosDOP$Diasengorda,dadosCONV$Diasengorda, main= "Dias na engorda por destino de abate", ylab = "Dias na engorda", xlab= "Abate DOP Vs. Abate Convencional")

#GMD
tapply(dados$GMD, dados$Dest_abate, summary)
basicStats(dados$GMD)
basicStats(dadosDOP$GMD)
basicStats(dadosCONV$GMD)

grafico6<-hist(dados$GMD, main = "Ganho médio diário",xlab = "Ganho médio diário (kg/dia)", ylab = "Frequencias")
min(dados$GMD)
seq(1,5,length=22)
xajust<-seq(min(dados$GMD, na.rm=T),  
            max(dados$GMD, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$GMD,na.rm=T),
              sd=sd(dados$GMD,na.rm=T))
hist(dados$GMD, freq = F, main = "Histograma ganho médio diário (GMD)", xlab = "Ganho médio diário (kg/dia)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$GMD,dados$Dest_abate2)

biserial.cor(dados$GMD, dados$Dest_abate)

boxplot(dadosDOP$GMD,dadosCONV$GMD, main= "Ganho médio diário por destino de abate", ylab = "Ganho médio diário", xlab= "Abate DOP Vs. Abate Convencional")

#Peso aos 210 dias
basicStats(dados$P210)
basicStats(dadosDOP$P210)
basicStats(dadosCONV$P210)
tapply(dados$P210, dados$Dest_abate, summary)

grafico7<-hist(dados$P210, main = "P210",xlab = "Peso aos 210 dias", ylab = "Frequencias")
min(dados$P210)
seq(1,5,length=22)
xajust<-seq(min(dados$P210, na.rm=T),  
            max(dados$P210, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$P210,na.rm=T),
              sd=sd(dados$P210,na.rm=T))
hist(dados$P210, freq = F, main = "Histograma do Peso aos 210 dias", xlab = "Peso aos 210 dias", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$P210,dados$Dest_abate2)
t.test(dadosDOP$P210,dadosCONV$P210)

biserial.cor(dados$P210, dados$Dest_abate, use = "complete.obs") #0.1725363
str(dados$P210)
str(dados$Dest_abate)

boxplot(dadosDOP$P210 ,dadosCONV$P210, main= "Peso aos 210 dias por destino de abate", ylab = "Peso aos 210 dias (Kg)", xlab= "Abate DOP Vs. Abate Convencional")

#Peso da mae aos 210 dias
basicStats(dados$mae_p210)
basicStats(dadosDOP$mae_p210)
basicStats(dadosCONV$mae_p210)
t.test(dados$mae_p210,dados$Dest_abate2)

#Peso do pai aos 210 dias
basicStats(dados$Pai_p210)
basicStats(dadosDOP$Pai_p210)
basicStats(dadosCONV$Pai_p210)
t.test(dados$Pai_p210,dados$Dest_abate2)

#peso da carcaça Vs Peso liquido carcaça
basicStats(dados$pesodacarcaca)
basicStats(dadosDOP$pesodacarcaca)
basicStats(dadosCONV$pesodacarcaca)
tapply(dados$pesodacarcaca, dados$Dest_abate, summary)

basicStats(dados$Peso_liq_carcaca)

t.test(dados$pesodacarcaca, dados$Peso_liq_carcaca) #p-value = 0.2643
#nao existe diferença entre peso da carcaça e peso liquido da carcaça
#sera realizado a analise com base no PEso da carcaça

lillieTest(dados$pesodacarcaca)

wilcox.test(dados$pesodacarcaca, dados$Dest_abate2)
t.test(dados$pesodacarcaca, dados$Dest_abate2)

grafico8<-hist(dados$pesodacarcaca, main = "Peso da carcaça",xlab = "Peso da carcaça", ylab = "Frequencias")
min(dados$pesodacarcaca)
seq(1,5,length=22)
xajust<-seq(min(dados$pesodacarcaca, na.rm=T),  
            max(dados$pesodacarcaca, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$pesodacarcaca,na.rm=T),
              sd=sd(dados$pesodacarcaca,na.rm=T))
hist(dados$pesodacarcaca, freq = F, main = "Histograma do Peso da carcaça", xlab = "Peso da carcaça em kg", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)


biserial.cor(dados$pesodacarcaca, dados$Dest_abate)
boxplot(dadosDOP$pesodacarcaca ,dadosCONV$pesodacarcaca, main= "Peso da carcaça por destino de abate", ylab = "Peso da carcaça (Kg)", xlab= "Abate DOP Vs. Abate Convencional")

#Categorizando as faixas de peso da carcaça
summary(dados$pesodacarcaca)
dados$pesocarccat<-cut(dados$pesodacarcaca, breaks=c(min(dados$pesodacarcaca), 120,140,160, 180, max(dados$pesodacarcaca)), include.lowest=T)
table(dados$pesocarccat)
table(dados$pesocarccat, dados$Dest_abate2)
round(prop.table(table(dados$pesocarccat, dados$Dest_abate2))*100, digits=2)
chisq.test(dados$pesocarccat, dados$Dest_abate2)


#Profilaxia
basicStats(dados$profilaxia)
basicStats(dadosDOP$profilaxia)
basicStats(dadosCONV$profilaxia)
summary(dados$profilaxia)

t.test(dadosDOP$profilaxia,dadosCONV$profilaxia) #rejeita-se a igualdade das médias, o que é curioso pelo fato do custo ser fixo

chisq.test(dados$profilaxia, dados$Dest_abate)
biserial.cor(dados$profilaxia, dados$Dest_abate) #-0.1799359

boxplot(dadosDOP$profilaxia ,dadosCONV$profilaxia, main= "Custo com profilaxia por destino de abate", ylab = "Custo com profilaxia (euros)", xlab= "Abate DOP Vs. Abate Convencional")
#

#Taxa promert
summary(dados$servico_Promert)
basicStats(dados$servico_Promert)
basicStats(dadosDOP$servico_Promert)
basicStats(dadosCONV$servico_Promert)

#Custo de funcionamento
basicStats(dados$custos_func)
basicStats(dadosDOP$custos_func)
basicStats(dadosCONV$custos_func)

t.test(dados$custos_func,dados$Dest_abate2)

chisq.test(dados$custos_func, dados$Dest_abate)
biserial.cor(dados$custos_func, dados$Dest_abate)

boxplot(dadosDOP$custos_func ,dadosCONV$custos_func, main= "Custo com funcionamento por destino de abate", ylab = "Custo com funcionamento (euros)", xlab= "Abate DOP Vs. Abate Convencional")

#custo de concentrado
basicStats(dados$custo_concentrado)
basicStats(dadosDOP$custo_concentrado)
basicStats(dadosCONV$custo_concentrado)

t.test(dadosDOP$custo_concentrado,dadosCONV$custo_concentrado)

chisq.test(dados$custo_concentrado, dados$AbateDestino) #
biserial.cor(dados$custo_concentrado, dados$Abatedestino2)

boxplot(dadosDOP$custo_concentrado ,dadosCONV$custo_concentrado, main= "Custo concentrado por destino de abate", ylab = "Custo com concentrado (euros)", xlab= "Abate DOP Vs. Abate Convencional")

#custo de palha
basicStats(dados$custo_palha)
basicStats(dadosDOP$custo_palha)
basicStats(dadosCONV$custo_palha)

t.test(dadosDOP$custo_palha,dadosCONV$custo_palha)

chisq.test(dados$custo_palha, dados$AbateDestino) 
biserial.cor(dados$custo_palha, dados$Abatedestino2)

boxplot(dadosDOP$custo_palha ,dadosCONV$custo_palha, main= "Custo com palha por destino de abate", ylab = "Custo com palha (euros)", xlab= "Abate DOP Vs. Abate Convencional")

#custo alimentacao
dados$custoaliment = dados$custo_concentrado+dados$custo_palha
dadosDOP$custoaliment = dadosDOP$custo_concentrado+dadosDOP$custo_palha
dadosCONV$custoaliment = dadosCONV$custo_concentrado+dadosCONV$custo_palha

basicStats(dados$custoaliment)
basicStats(dadosDOP$custoaliment)
basicStats(dadosCONV$custoaliment)

grafico9<-hist(dados$custoaliment, main = "Custo alimentacao",xlab = "Custo com alimentacao", ylab = "Frequencias")
min(dados$custoaliment)
seq(1,5,length=22)
xajust<-seq(min(dados$custoaliment, na.rm=T),  
            max(dados$custoaliment, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$custoaliment,na.rm=T),
              sd=sd(dados$custoaliment,na.rm=T))
hist(dados$custoaliment, freq = F, main = "Histograma do custo com alimentação", xlab = "Custo com alimentação em euros", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$custoaliment,dados$Dest_abate2)
biserial.cor(dados$custoaliment, dados$Dest_abate2)

boxplot(dadosDOP$custoaliment ,dadosCONV$custoaliment, main= "Custo com alimentação por destino de abate", ylab = "Custo com alimentação (euros)", xlab= "Abate DOP Vs. Abate Convencional")

#custo individual com transporte
basicStats(dados$custo_individual_transporte)
basicStats(dadosDOP$custo_individual_transporte)
basicStats(dadosCONV$custo_individual_transporte)

t.test(dadosDOP$custo_individual_transporte,dadosCONV$custo_individual_transporte)

grafico10<-hist(dados$custo_individual_transporte, main = "Custo individual com transporte",xlab = "Custo individual com transporte (euro/animal)", ylab = "Frequencias")
min(dados$custo_individual_transporte)
seq(1,5,length=22)
xajust<-seq(min(dados$custo_individual_transporte, na.rm=T),  
            max(dados$custo_individual_transporte, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$custo_individual_transporte,na.rm=T),
              sd=sd(dados$custo_individual_transporte,na.rm=T))
hist(dados$custo_individual_transporte, freq = F, main = "Histograma do custo individual com transporte", xlab = "Custo individual com transporte (euro/animal)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

chisq.test(dados$custo_individual_transporte, dados$Dest_abate) 
biserial.cor(dados$custo_individual_transporte, dados$Dest_abate2)

boxplot(dadosDOP$custo_individual_transporte ,dadosCONV$custo_individual_transporte, main= "Custo individual com transporte por destino de abate", ylab = "Custo com transporte (euros)", xlab= "Abate DOP Vs. Abate Convencional")

#outros custos
basicStats(dados$outros_custos)
basicStats(dadosDOP$outros_custos)
basicStats(dadosCONV$outros_custos)

t.test(dados$outros_custos,dados$Dest_abate2)

chisq.test(dados$outros_custos, dados$Dest_abate) 
biserial.cor(dados$outros_custos, dados$Dest_abate2)

boxplot(dadosDOP$outros_custos ,dadosCONV$outros_custos, main= "Outros custo por destino de abate", ylab = "Outros custos (euros)", xlab= "Abate DOP Vs. Abate Convencional")


#Custo total

basicStats(dados$total_custos)
basicStats(dadosDOP$total_custos)
basicStats(dadosCONV$total_custos)
tapply(dados$total_custos, dados$Dest_abate, summary)

grafico11<-hist(dados$total_custos, main = "Custo total de produção",xlab = "Custo total de produção(euro)", ylab = "Frequencias")
min(dados$total_custos)
seq(1,5,length=22)
xajust<-seq(min(dados$total_custos, na.rm=T),  
            max(dados$total_custos, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$total_custos,na.rm=T),
              sd=sd(dados$total_custos,na.rm=T))
hist(dados$total_custos, freq = F, main = "Histograma do custo total de produção", xlab = "Custo total de produção (euro)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

lillieTest(dados$total_custos)
t.test(dados$total_custos,dados$Dest_abate2)
wilcox.test(dados$custototal~dados$Dest_abate2)

chisq.test(dados$total_custos, dados$Dest_abate2) 
biserial.cor(dados$total_custos, dados$Dest_abate)
table(dados$total_custos, dados$Dest_abate)

boxplot(dadosDOP$total_custos ,dadosCONV$total_custos, main= "Custo total por destino de abate", ylab = "Custo total (euros)", xlab= "Abate DOP Vs. Abate Convencional")

#correlação custo total com as variáveis de produção
attach(dados)
cor.test(Idadeent_meses, total_custos, method = "pearson") #-0.2548727
cor.test(peso_ent, total_custos, method = "pearson") #-0.3641564
cor.test(Idadesaida_meses, total_custos, method = "pearson") #0.718684
cor.test(peso_saida, total_custos, method = "pearson") #0.7998594
cor.test(Diasengorda, total_custos, method = "pearson") #0.9575669
cor.test(GMD, total_custos, method = "pearson") #0.02512924
cor.test(dados$P210, dados$total_custos, method = "pearson") #-0.1997296 
cor.test(pesodacarcaca, total_custos, method = "pearson") #0.8288582
cor.test(custos_func, total_custos, method = "pearson") #0.9575669
cor.test(custoaliment, total_custos, method = "pearson") #0.9967652 
cor.test(custo_individual_transporte, total_custos, method = "pearson") #0.6511612
cor.test(outros_custos, total_custos, method = "pearson")#-0.352223
cor.test(liquido_recria, total_custos, method = "pearson")#-0.2640309
cor.test(preco_kg_carcaca, total_custos, method = "pearson") #0.4668317 



#valor liquido recria 
basicStats(dados$liquido_recria)
basicStats(dadosDOP$liquido_recria)
basicStats(dadosCONV$liquido_recria)
summary(dados$liquido_recria)
tapply(dados$liquido_recria, dados$Dest_abate, summary)

grafico12<-hist(dados$liquido_recria, main = "Lucro líquido",xlab = "Lucro líquido da produção (euro)", ylab = "Frequencias")
min(dados$liquido_recria)
seq(1,5,length=22)
xajust<-seq(min(dados$liquido_recria, na.rm=T),  
            max(dados$liquido_recria, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$liquido_recria,na.rm=T),
              sd=sd(dados$liquido_recria,na.rm=T))
hist(dados$liquido_recria, freq = F, main = "Histograma do lucro líquido de pprodução", xlab = "Lucro líquido da pprodução (euro)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

lillieTest(dados$liquido_recria)
t.test(dados$liquido_recria,dados$Dest_abate2)

chisq.test(dados$liquido_recria, dados$Dest_abate2) 
biserial.cor(dados$liquido_recria, dados$Dest_abate2, use = "complete.obs")

boxplot(dadosDOP$liquido_recria ,dadosCONV$liquido_recria, main= "Valor líquido da recria por destino de abate", ylab = "Valor líquido da recria (euros)", xlab= "Abate DOP Vs. Abate Convencional")

cor.test(Idadeent_meses, liquido_recria, method = "pearson") #0.3259271
cor.test(peso_ent, liquido_recria, method = "pearson") #0.6281395
cor.test(Idadesaida_meses, liquido_recria, method = "pearson") #-0.1338496
cor.test(peso_saida, liquido_recria, method = "pearson") #0.2933334 
cor.test(Diasengorda, liquido_recria, method = "pearson") #-0.4037852
cor.test(GMD, liquido_recria, method = "pearson") #0.3505898 
cor.test(dados$P210, dados$liquido_recria, method = "pearson") #0.3953079 
cor.test(pesodacarcaca, liquido_recria, method = "pearson") #0.2919563
cor.test(custos_func, liquido_recria, method = "pearson") #-0.4037852
cor.test(custoaliment, liquido_recria, method = "pearson") #-0.2503664 
cor.test(custo_individual_transporte, liquido_recria, method = "pearson") #-0.04242552
cor.test(outros_custos, liquido_recria, method = "pearson")#0.6158209
cor.test(total_custos, liquido_recria, method = "pearson")#-0.2640309
cor.test(preco_kg_carcaca, liquido_recria, method = "pearson") #0.4183001 

#--------------------------------------------------------------------------
#custo alimentação por dia
dados$custoalimdia=dados$custodeconcentradopordia+dados$custodepalhapordia
basicStats(dados$custoalimdia)
dadosDOP$custoalimdia=dadosDOP$custodeconcentradopordia+dadosDOP$custodepalhapordia
basicStats(dadosDOP$custoalimdia)
dadosCONV$custoalimdia=dadosCONV$custodeconcentradopordia+dadosCONV$custodepalhapordia
basicStats(dadosCONV$custoalimdia)

t.test(dadosDOP$custoalimdia,dadosCONV$custoalimdia)

biserial.cor(dados$custoalimdia, dados$Abatedestino2)
boxplot(dadosDOP$custoalimdia,dadosCONV$custoalimdia, main= "Custo com alimentação por dia", ylab = "Custo com alimentação/dia (euros)", xlab= "Abate DOP Vs. Abate Convencional")



#--------------------------------------------------------------------------------------------------

#Custo de concentrado por dia
basicStats(dados$custodeconcentradopordia)
basicStats(dadosDOP$custodeconcentradopordia)
basicStats(dadosCONV$custodeconcentradopordia)

t.test(dadosDOP$custodeconcentradopordia,dadosCONV$custodeconcentradopordia)
boxplot(dadosDOP$custodeconcentradopordia,dadosCONV$custodeconcentradopordia, main= "Custo com concentrado/dia por destino de abate", ylab = "Custo com concentrado euro/dia", xlab= "Abate DOP Vs. Abate Convencional")

#custo de palha por dia
basicStats(dados$custodepalhapordia)
basicStats(dadosDOP$custodepalhapordia)
basicStats(dadosCONV$custodepalhapordia)

t.test(dadosDOP$custodepalhapordia,dadosCONV$custodepalhapordia)
boxplot(dadosDOP$custodepalhapordia,dadosCONV$custodepalhapordia, main= "Custo com palha/dia por destino de abate", ylab = "Custo com palha euro/dia", xlab= "Abate DOP Vs. Abate Convencional")
#Está como uma constante

#Outros custos por dia
basicStats(dados$outroscustospordia)
basicStats(dadosDOP$outroscustospordia)
basicStats(dadosCONV$outroscustospordia)

t.test(dadosDOP$outroscustospordia,dadosCONV$outroscustospordia)
boxplot(dadosDOP$outroscustospordia,dadosCONV$outroscustospordia, main= "Outros custos/dia por destino de abate", ylab = "Outros custos euro/dia", xlab= "Abate DOP Vs. Abate Convencional")

#Custo de transporte por kg de carcaça
basicStats(dados$custotransporteporkgcarcaça)
basicStats(dadosDOP$custotransporteporkgcarcaça)
basicStats(dadosCONV$custotransporteporkgcarcaça)

t.test(dadosDOP$custotransporteporkgcarcaça,dadosCONV$custotransporteporkgcarcaça)
boxplot(dadosDOP$custotransporteporkgcarcaça,dadosCONV$custotransporteporkgcarcaça, main= "custo transporte/kg de carcaça por destino de abate", ylab = "custo transporte euro/kg de carcaça", xlab= "Abate DOP Vs. Abate Convencional")

#Custo total por dia
basicStats(dados$custototal.dia)
basicStats(dadosDOP$custototal.dia)
basicStats(dadosCONV$custototal.dia)
str(dados$custototal.dia)
str(dados$Dest_abate)
t.test(dados$custototal.dia, dados$Dest_abate2)
biserial.cor(dados$custototal.dia, dados$Dest_abate)
lillieTest(dados$custototal.dia)

grafico1<-hist(dados$custototal.dia, main = "Custo total por dia de produção",xlab = "Custo total por dia de produção (euro)", ylab = "Frequencias")
min(dados$custototal.dia)
seq(1,5,length=22)
xajust<-seq(min(dados$custototal.dia, na.rm=T),  
            max(dados$custototal.dia, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$custototal.dia,na.rm=T),
              sd=sd(dados$custototal.dia,na.rm=T))
hist(dados$custototal.dia, freq = F, main = "Histograma do custo total por dia de produção", xlab = "Custo total por dia de produção (euro)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)


#Custo total = Custo fixo + custo variavel
dados$custofixo<-(dados$profilaxia+dados$servico_Promert+dados$custotransporte.kgcarcaca)
summary(dados$custofixo)

#Custo variaveis sao:Custo alimentacao, Custo funcionamento e outros custos
#se meu custo total por dia é de 2,18$
#E meu custo fixo por dia é 0,31$
#Entao meu scuto variavel é de 1,87$ em media





#Lucro liquido por dia
dados$Liquido_dia<-(dados$liquido_recria/dados$Diasengorda)
dadosDOP$Liquido_dia<-(dadosDOP$liquido_recria/dadosDOP$Diasengorda)
dadosCONV$Liquido_dia<-(dadosCONV$liquido_recria/dadosCONV$Diasengorda)

basicStats(dados$Liquido_dia)
basicStats(dadosDOP$Liquido_dia)
basicStats(dadosCONV$Liquido_dia)
t.test(dadosDOP$Liquido_dia, dadosCONV$Liquido_dia)

#--------------------------------------------------------------------------


#Rendimento de carcaça
basicStats(dados$Rendimento_carcaca)
basicStats(dadosDOP$Rendimento_carcaca)
basicStats(dadosCONV$Rendimento_carcaca)
lillieTest(dados$Rendimento_carcaca)
t.test(dados$Rendimento_carcaca, dados$Dest_abate2)
#--------------------------------------------------------------------------

#Preco/Kg de carcaça
basicStats(dados$preco_kg_carcaca)
basicStats(dadosDOP$preco_kg_carcaca)
basicStats(dadosCONV$preco_kg_carcaca)

grafico13<-hist(dados$preco_kg_carcaca, main = "Preço por kg de carcaça",xlab = "Preço por kg de carcaça (euro/kg)", ylab = "Frequencias")
min(dados$preco_kg_carcaca)
seq(1,5,length=22)
xajust<-seq(min(dados$preco_kg_carcaca, na.rm=T),  
            max(dados$preco_kg_carcaca, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$preco_kg_carcaca,na.rm=T),
              sd=sd(dados$preco_kg_carcaca,na.rm=T))
hist(dados$preco_kg_carcaca, freq = F, main = "Histograma do Preço por kg de carcaça", xlab = "Preço por kg de carcaça (euro/kg)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

round(prop.table((table(dados$Dest_abate, dados$preco_kg_carcaca))*100), digits = 2)
t.test(dados$preco_kg_carcaca,dados$Dest_abate2)
boxplot(dadosDOP$preco_kg_carcaca,dadosCONV$preco_kg_carcaca, main= "Preço/kg de carcaça por destino de abate", ylab = "Preço em euro/kg de carcaça", xlab= "Abate DOP Vs. Abate Convencional")


#Valor da carcaça
basicStats(dados$valor_carcaca)
basicStats(dadosDOP$valor_carcaca)
basicStats(dadosCONV$valor_carcaca)

t.test(dados$valor_carcaca,dados$Dest_abate2)
boxplot(dadosDOP$valor_carcaca,dadosCONV$valor_carcaca, main= "Valor da carcaça por destino de abate", ylab = "Valor da carcaça", xlab= "Abate DOP Vs. Abate Convencional")

grafico14<-hist(dados$valor_carcaca, main = "Valor da carcaça",xlab = "Valor da carcaça (euro)", ylab = "Frequencias")
min(dados$valor_carcaca)
seq(1,5,length=22)
xajust<-seq(min(dados$valor_carcaca, na.rm=T),  
            max(dados$valor_carcaca, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$valor_carcaca,na.rm=T),
              sd=sd(dados$valor_carcaca,na.rm=T))
hist(dados$valor_carcaca, freq = F, main = "Histograma do valor da carcaça", xlab = "Valor da carcaça (euro)", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)


#VG capacidade maternal
basicStats(dados$VG_capacidade_maternal)
basicStats(dadosDOP$VG_capacidade_maternal)
basicStats(dadosCONV$VG_capacidade_maternal)
summary(dados$VG_capacidade_maternal)
tapply(dados$VG_capacidade_maternal, dados$Dest_abate, summary)
plot(dados$VG_capacidade_maternal)

grafico15<-hist(dados$VG_capacidade_maternal, main = "Valor genetico da capacidade maternal",xlab = "Valor genetico da capacidade maternal", ylab = "Frequencias")
min(dados$VG_capacidade_maternal)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_capacidade_maternal, na.rm=T),  
            max(dados$VG_capacidade_maternal, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_capacidade_maternal,na.rm=T),
              sd=sd(dados$VG_capacidade_maternal,na.rm=T))
hist(dados$VG_capacidade_maternal, freq = F, main = "Histograma valor genetico da capacidade maternal", xlab = "Valor genetico da capacidade maternal", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)
lillieTest(dados$VG_capacidade_maternal)
wilcox.test(dados$VG_capacidade_maternal)
t.test(dados$VG_capacidade_maternal, dados$Dest_abate2)
boxplot(dadosDOP$VG_capacidade_maternal,dadosCONV$VG_capacidade_maternal, main= "VG capacidade maternal por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

biserial.cor(dados$VG_capacidade_maternal, dados$Dest_abate2)
cor.test(dados$VG_capacidade_maternal, dados$total_custos, method = "pearson") #0.1149163 
cor.test(dados$VG_capacidade_maternal, dados$liquido_recria, method = "pearson") #0.04996825

#VG capacidade de crescimento
basicStats(dados$VG_cap_crescimento)
basicStats(dadosDOP$VG_cap_crescimento)
basicStats(dadosCONV$VG_cap_crescimento)

plot(dados$VG_cap_crescimento)

t.test(dados$VG_cap_crescimento,dados$Dest_abate2)
boxplot(dadosDOP$VG_cap_crescimento,dadosCONV$VG_cap_crescimento, main= "VG capacidade de crescimento por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

grafico16<-hist(dados$VG_cap_crescimento, main = "Valor genetico da capacidade de crescimento",xlab = "Valor genetico da capacidade de crescimento", ylab = "Frequencias")
min(dados$VG_cap_crescimento)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_cap_crescimento, na.rm=T),  
            max(dados$VG_cap_crescimento, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_cap_crescimento,na.rm=T),
              sd=sd(dados$VG_cap_crescimento,na.rm=T))
hist(dados$VG_cap_crescimento, freq = F, main = "Histograma valor genetico da capacidade de crescimento", xlab = "Valor genetico da capacidade de crescimento", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

biserial.cor(dados$VG_cap_crescimento, dados$Dest_abate2) #0.08470386
cor.test(dados$VG_cap_crescimento, dados$total_custos, method = "pearson") #-0.09605221
cor.test(dados$VG_cap_crescimento, dados$liquido_recria, method = "pearson") #0.1661854


#VG GMD teste de performance
basicStats(dados$VG_gmd_estacao)
basicStats(dadosDOP$VG_gmd_estacao)
basicStats(dadosCONV$VG_gmd_estacao)

grafico17<-hist(dados$VG_gmd_estacao, main = "Valor genetico GMD em teste de performance",xlab = "Valor genetico GMD em teste de performance", ylab = "Frequencias")
min(dados$VG_gmd_estacao)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_gmd_estacao, na.rm=T),  
            max(dados$VG_gmd_estacao, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_gmd_estacao,na.rm=T),
              sd=sd(dados$VG_gmd_estacao,na.rm=T))
hist(dados$VG_gmd_estacao, freq = F, main = "Histograma valor genetico GMD em teste de performance", xlab = "Valor genetico GMD em teste de performance", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

biserial.cor(dados$VG_gmd_estacao, dados$Dest_abate2) #0.04030436
cor.test(dados$VG_gmd_estacao, dados$total_custos, method = "pearson") #-0.03795138
cor.test(dados$VG_gmd_estacao, dados$liquido_recria, method = "pearson") #0.04109777

t.test(dados$VG_gmd_estacao,dados$Dest_abate2)
boxplot(dadosDOP$VG_gmd_estacao,dadosCONV$VG_gmd_estacao, main= "VG GMD em teste de performance por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

#VG Peso de carcaça por dia idade
basicStats(dados$VG_carcaca_dia_idade)
basicStats(dadosDOP$VG_carcaca_dia_idade)
basicStats(dadosCONV$VG_carcaca_dia_idade)

grafico18<-hist(dados$VG_carcaca_dia_idade, main = "Valor genetico do Peso de carcaça 
                                                          por dia de idade",xlab = "Valor genetico do peso de carcaça por dia de idade", ylab = "Frequencias")
min(dados$VG_carcaca_dia_idade)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_carcaca_dia_idade, na.rm=T),  
            max(dados$VG_carcaca_dia_idade, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_carcaca_dia_idade,na.rm=T),
              sd=sd(dados$VG_carcaca_dia_idade,na.rm=T))
hist(dados$VG_carcaca_dia_idade, freq = F, main = "Histograma valor genetico peso de carcaça/dia de idade", xlab = "Valor genetico peso de carcaça por dia de idade", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dados$VG_carcaca_dia_idade,dados$Dest_abate2)
boxplot(dadosDOP$VG_carcaca_dia_idade,dadosCONV$VG_carcaca_dia_idade, main= "VG peso de carcaça/dia idade por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

biserial.cor(dados$VG_carcaca_dia_idade, dados$Dest_abate2) #0.1215377
cor.test(dados$VG_carcaca_dia_idade, dados$total_custos, method = "pearson") #0.09406781
cor.test(dados$VG_carcaca_dia_idade, dados$liquido_recria, method = "pearson") #0.2943919


#consumo alim residual

basicStats(dados$VG_consumo_alim_residual)
basicStats(dadosDOP$VG_consumo_alim_residual)
basicStats(dadosCONV$VG_consumo_alim_residual)

grafico19<-hist(dados$VG_consumo_alim_residual, main = "Valor genetico do consumo de alimento residual",xlab = "Valor genetico do consumo de alimento residual", ylab = "Frequencias")
min(dados$VG_consumo_alim_residual)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_consumo_alim_residual, na.rm=T),  
            max(dados$VG_consumo_alim_residual, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_consumo_alim_residual,na.rm=T),
              sd=sd(dados$VG_consumo_alim_residual,na.rm=T))
hist(dados$VG_consumo_alim_residual, freq = F, main = "Histograma VG consumo de alimento residual", xlab = "Valor genetico do consumo de alimento residual", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dadosDOP$VG_consumo_alim_residual,dadosCONV$VG_consumo_alim_residual)
boxplot(dadosDOP$VG_consumo_alim_residual,dadosCONV$VG_consumo_alim_residual, main= "VG consumo alimento residual por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

biserial.cor(dados$VG_consumo_alim_residual, dados$Dest_abate2) #0.007
cor.test(dados$VG_consumo_alim_residual, dados$total_custos, method = "pearson") #0.04713239
cor.test(dados$VG_consumo_alim_residual, dados$liquido_recria, method = "pearson") #-0.02627665


#intervalo entre partos
basicStats(dados$VG_Intervalo_Entre_partos)
basicStats(dadosDOP$VG_Intervalo_Entre_partos)
basicStats(dadosCONV$VG_Intervalo_Entre_partos)
summary(dados$VG_Intervalo_Entre_partos)
tapply(dados$VG_Intervalo_Entre_partos, dados$Dest_abate, summary)
plot(dados$VG_Intervalo_Entre_partos)

grafico20<-hist(dados$VG_Intervalo_Entre_partos, main = "Valor genetico do intervalo entre partos",xlab = "Valor genetico do intervalo entre partos", ylab = "Frequencias")
min(dados$VG_Intervalo_Entre_partos)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_Intervalo_Entre_partos, na.rm=T),  
            max(dados$VG_Intervalo_Entre_partos, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_Intervalo_Entre_partos,na.rm=T),
              sd=sd(dados$VG_Intervalo_Entre_partos,na.rm=T))
hist(dados$VG_Intervalo_Entre_partos, freq = F, main = "Histograma VG do intervalo entre partos", xlab = "Valor genetico do intervalo entre partos", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

lillieTest(dados$VG_Intervalo_Entre_partos)
wilcox.test(dados$VG_Intervalo_Entre_partos)
t.test(dados$VG_Intervalo_Entre_partos,dados$Dest_abate2)
boxplot(dadosDOP$VG_Intervalo_Entre_partos,dadosCONV$VG_Intervalo_Entre_partos, main= "VG intervalo entre partos por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

biserial.cor(dados$VG_Intervalo_Entre_partos, dados$Dest_abate2) #-0.14351
cor.test(dados$VG_Intervalo_Entre_partos, dados$total_custos, method = "pearson") #0.1099231 
cor.test(dados$VG_Intervalo_Entre_partos, dados$liquido_recria, method = "pearson") #-0.07017704


#indice de conversao
basicStats(dados$VG_indice_conversao)
basicStats(dadosDOP$VG_indice_conversao)
basicStats(dadosCONV$VG_indice_conversao)

plot(dados$VG_indice_conversao)

grafico21<-hist(dados$VG_indice_conversao, main = "Valor genetico do índice de conversao alimentar",xlab = "Valor genetico indice de conversao alimentar", ylab = "Frequencias")
min(dados$VG_indice_conversao)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_indice_conversao, na.rm=T),  
            max(dados$VG_indice_conversao, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_indice_conversao,na.rm=T),
              sd=sd(dados$VG_indice_conversao,na.rm=T))
hist(dados$VG_indice_conversao, freq = F, main = "Histograma VG do indice de conversao alimentar", xlab = "Valor genetico do indice de conversao alimentar", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dadosDOP$VG_indice_conversao,dadosCONV$VG_indice_conversao)
boxplot(dadosDOP$VG_indice_conversao,dadosCONV$VG_indice_conversao, main= "VG indice de conversao por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

biserial.cor(dados$VG_indice_conversao, dados$Dest_abate2) #-0.03751955
cor.test(dados$VG_indice_conversao, dados$total_custos, method = "pearson") #0.03928506  
cor.test(dados$VG_indice_conversao, dados$liquido_recria, method = "pearson") #-0.07381705


#longevidade produtiva
basicStats(dados$VG_longevidade_produtiva)
basicStats(dadosDOP$VG_longevidade_produtiva)
basicStats(dadosCONV$VG_longevidade_produtiva)

plot(dados$VG_longevidade_produtiva)

grafico22<-hist(dados$VG_longevidade_produtiva, main = "Valor genetico longevidade produtiva",xlab = "Valor genetico longevidade produtiva", ylab = "Frequencias")
min(dados$VG_longevidade_produtiva)
seq(1,5,length=22)
xajust<-seq(min(dados$VG_longevidade_produtiva, na.rm=T),  
            max(dados$VG_longevidade_produtiva, na.rm=T),
            length=50)
yajust<-dnorm(xajust,
              mean=mean(dados$VG_longevidade_produtiva,na.rm=T),
              sd=sd(dados$VG_longevidade_produtiva,na.rm=T))
hist(dados$VG_longevidade_produtiva, freq = F, main = "Histograma VG longevidade produtiva", xlab = "Valor genetico longevidade produtiva", ylab = "densidade")
lines(xajust, yajust, col="red", lwd=2)

t.test(dadosDOP$VG_longevidade_produtiva,dadosCONV$VG_longevidade_produtiva)
boxplot(dadosDOP$VG_longevidade_produtiva,dadosCONV$VG_longevidade_produtiva, main= "VG longevidade produtiva por destino de abate", ylab = "Valor genetico", xlab= "Abate DOP Vs. Abate Convencional")

biserial.cor(dados$VG_longevidade_produtiva, dados$Dest_abate2) #-0.03458004
cor.test(dados$VG_longevidade_produtiva, dados$total_custos, method = "pearson") #-0.01071049  
cor.test(dados$VG_longevidade_produtiva, dados$liquido_recria, method = "pearson") #0.05110431


#Valor/Kg de peso vivo
dados$valorporkgPV<-(dados$liquido_recria/dados$peso_ent)
basicStats(dados$valorporkgPV)
hist(dados$valorporkgPV)

dadosDOP$ValorporKgPVDOP<-(dadosDOP$liquido_recria/dadosDOP$peso_ent)
basicStats(dadosDOP$ValorporKgPVDOP)

dadosCONV$ValorporKgPVCONV<-(dadosCONV$liquido_recria/dadosCONV$peso_ent)
basicStats(dadosCONV$ValorporKgPVCONV)

t.test(dadosDOP$ValorporKgPVDOP,dadosCONV$ValorporKgPVCONV)
boxplot(dadosDOP$ValorporKgPVDOP,dadosCONV$ValorporKgPVCONV, main= "Valor por Kg de peso vivo a entrada", ylab = "Valor por Kg de peso vivo", xlab= "Abate DOP Vs. Abate Convencional")

##########INDICE DE CONVERSAO - concentrado/dias na engorda####################
dados$ingestaomedia=(dados$consumo_concen)/dados$Diasengorda #indice de conversao de concentrado
dadosDOP$ingestaomedia=(dadosDOP$consumo_concen)/dadosDOP$Diasengorda
dadosCONV$ingestaomedia=dadosCONV$consumo_concen/dadosCONV$Diasengorda
summary(dados$ingestaomedia)
summary(dados$GMD)

#5.799/1.135 = 7,78 kg de materia seca para obter 1 kg de peso animal
#ou ainda = 1/7,78 = 0,13 kg para cada kg consumido

summary(dadosDOP$ingestaomedia)
summary(dadosDOP$GMD)
#6.085/1.145 = 5,31441 kg de materia seca consumida para o animal obter 1 kg
#ou ainda 1/5.31441 = 0,18816 kg de peso obtido para cada kg de alimento consumido

summary(dadosCONV$ingestaomedia)
summary(dadosCONV$GMD)
#5.483/1.123 = 4,8824 kg de materia seca consumida para o animal obter 1 kg
#ou ainda 1/4.8824 = 0,20 kg de peso obtido para cada kg de alimento

#Correlacao entre as principais variaveis estudadas

varinteresse<- data.frame(dados$Idadeent_meses,dados$peso_ent,dados$Rendimento_carcaca,dados$preco_kg_carcaca, dados$pesodacarcaca, dados$Diasengorda, dados$custototal.dia, dados$liquido_recria)
varinteresse<-na.omit(varinteresse)
Corvarinteresse<-round(cor(varinteresse, method = "pearson"),2)
str(varinteresse)

install.packages("ggcorrplot")
library(ggcorrplot)

# Correlacao
ggcorrplot(Corvarinteresse, hc.order = T, 
           type = "full",show.legend = TRUE,
           lab = TRUE, show.diag = T,
           lab_size = 3, lab_col = "black",
           method="square", outline.color = "black",
           colors = c("red", "white", "blue"), tl.cex = 10,
           title ="", legend.title = "Correlação", 
           ggtheme=theme_bw, digits = 2)

varinteresse2<- data.frame(dados$custototal.dia, dados$P210, dados$mae_p210, dados$Pai_p210,dados$VG_capacidade_maternal, dados$VG_cap_crescimento, dados$VG_carcaca_dia_idade, dados$VG_consumo_alim_residual, dados$VG_gmd_estacao, dados$VG_indice_conversao,dados$VG_Intervalo_Entre_partos, dados$VG_longevidade_produtiva)
varinteresse2<-na.omit(varinteresse2)
Corvarinteresse2<-round(cor(varinteresse2, method = "pearson"),2)
str(varinteresse2)

# Correlação
ggcorrplot(Corvarinteresse2, hc.order = T, 
           type = "full",show.legend = TRUE,
           lab = TRUE, show.diag = T,
           lab_size = 3, lab_col = "black",
           method="square", outline.color = "black",
           colors = c("red", "white", "green"), tl.cex = 10,
           title ="", legend.title = "Correla��o", 
           ggtheme=theme_bw, digits = 2)

