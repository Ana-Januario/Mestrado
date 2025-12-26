################################################################
######################TESTES####################################
################################################################
getwd()

library(readxl)
dados=read.table("bov_mais_RECRIAalterado.csv", header=T, sep=";", dec=",")
dadosCONV=read.table("bov_mais_CONV.csv", header=T, sep=";", dec=",")
dadosDOP=read.table("bov_mais_DOP.csv", header=T, sep=";", dec=",")

#DICOTOMIZANDO DESTINO DE ABATE PARA 0 E 1
dados$Dest_abate2<-as.numeric(factor(dados$Dest_abate))-1
table(dados$Dest_abate)
table(dados$Dest_abate2)
library(fBasics)

#CRIANDO A VARIAVEL CUSTO DE ALIMENTAÇÃO= CONCENTRADO+PALHA
dados$custoaliment = dados$custo_concentrado+dados$custo_palha
dadosDOP$custoaliment = dadosDOP$custo_concentrado+dadosDOP$custo_palha
dadosCONV$custoaliment = dadosCONV$custo_concentrado+dadosCONV$custo_palha

#CATEGORIZANDO AS FAIXAS DE PESO DA CARCAÇA
dados$pesocarccat<-cut(dados$pesodacarcaca, breaks=c(min(dados$pesodacarcaca), 120,140,160, 180, max(dados$pesodacarcaca)), include.lowest=T)
levels(dados$pesocarccat)<-c("menor que 120 kg","de 120 a 140 kg","140 a 160 kg","160 a 180 kg", "maior que 180 kg")
table(dados$pesocarccat)

stripchart(dados$total_custos~dados$pesocarccat, vertical=T, ylab = "Custo total de produção", xlab="categorias de peso da carcaça")
table(dados$pesocarccat, dados$preco_kg_carcaca)
barplot(table(dados$pesocarccat, dados$preco_kg_carcaca), beside=T)
str(dados$preco_kg_carcaca)

(dados$precokgcarccat<-factor(dados$pesocarccat, labels = c("3.60", "3.60","3.85","4.00","4.15")))
round(prop.table(table(dados$precokgcarccat))*100, digits = 2)
round(prop.table(table(dados$precokgcarccat, dados$Dest_abate))*100, digits = 2)

table(dados$preco_kg_carcaca)

shapiro.test(dados$total_custos[dados$pesocarccat=="[99.3,120]"])
shapiro.test(dados$total_custos[dados$pesocarccat=="(120,140]"]) #não passou
shapiro.test(dados$total_custos[dados$pesocarccat=="(140,160]"])
shapiro.test(dados$total_custos[dados$pesocarccat=="(160,180]"])
shapiro.test(dados$total_custos[dados$pesocarccat=="(180,274]"]) #não passou

library(moments)
anscombe.test(dados$total_custos[dados$pesocarccat=="(120,140]"])
agostino.test(dados$total_custos[dados$pesocarccat=="(120,140]"])

library(car)
leveneTest(total_custos~pesocarccat, data=dados)

#analise não parametrica
kruskal.test(total_custos~pesocarccat, data=dados)
library(FSA)
dunnTest(total_custos~pesocarccat, data=dados, method = "holm")
median(dados$total_custos[dados$pesocarccat=="[99.3,120]"])
median(dados$total_custos[dados$pesocarccat=="(120,140]"]) 
median(dados$total_custos[dados$pesocarccat=="(140,160]"])
median(dados$total_custos[dados$pesocarccat=="(160,180]"])
median(dados$total_custos[dados$pesocarccat=="(180,274]"]) 

##########################ANOVA#############################

shapiro.test(dados$total_custos)
library(moments)
agostino.test(dados$total_custos, alternative = c("two.sided")) #rejeita-se normalidade : TLC
library(car)
leveneTest(dados$total_custos, dados$pesocarccat)



anovapesocat<-aov(dados$total_custos~ dados$pesocarccat)
summary(anovapesocat)
TukeyHSD(anovapesocat)
